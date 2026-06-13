package io.kaitai.struct.languages

import io.kaitai.struct.CppRuntimeConfig._
import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CppTranslator, TypeDetector}

class CppCompiler(
  typeProvider: ClassTypeProvider,
  config: RuntimeConfig
) extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with AllocateAndStoreIO
    with UniversalDoc
    with SwitchIfOps
    with EveryReadIsExpression
    with FetchInstances {
  import CppCompiler._

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  override val translator = new CppTranslator(typeProvider, importListSrc, importListHdr, config)
  val outSrcHeader = new StringLanguageOutputWriter(indent)
  val outHdrHeader = new StringLanguageOutputWriter(indent)
  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (outSrcHeader.result + importListSrc.result + outSrc.result),
      outFileNameHeader(className) -> (outHdrHeader.result + importListHdr.result + outHdr.result)
    )
  }

  sealed trait AccessMode
  case object PrivateAccess extends AccessMode
  case object PublicAccess extends AccessMode

  var accessMode: AccessMode = PublicAccess
  var writeFooterNeedsFetchInstances: Boolean = false

  override def indent: String = "    "

  override def fileHeader(topClassName: String): Unit = {
    outSrcHeader.puts(s"// $headerComment")
    outSrcHeader.puts

    importListSrc.addLocal(outFileNameHeader(topClassName))

    if (config.cppConfig.usePragmaOnce) {
      outHdrHeader.puts("#pragma once")
    } else {
      outHdrHeader.puts(s"#ifndef ${defineName(topClassName)}")
      outHdrHeader.puts(s"#define ${defineName(topClassName)}")
    }
    outHdrHeader.puts
    outHdrHeader.puts(s"// $headerComment")
    outHdrHeader.puts
    // Forward declaration of the top-level class defined later in this header
    // file. It's important to do this before printing `importListHdr` because
    // it contains `#include`s of header files of external .ksy modules that
    // could circularly import this header file (which does nothing because of
    // header guards, though, so if the other header files that tried to include
    // this one refer to our top-level class by name, which is very likely, then
    // we need to ensure that the C++ compiler has already seen the following
    // forward declaration).
    outHdrHeader.puts(s"class ${type2class(topClassName)};")
    outHdrHeader.puts

    importListHdr.addKaitai("kaitai/kaitaistruct.h")
    importListHdr.addSystem("stdint.h")

    config.cppConfig.pointers match {
      case UniqueAndRawPointers =>
        importListHdr.addSystem("memory")
      case RawPointers =>
        // no extra includes
    }

    // API compatibility check
    val minVer = KSVersion.minimalRuntime.toInt
    outHdr.puts
    outHdr.puts(s"#if KAITAI_STRUCT_VERSION < ${minVer}L")
    outHdr.puts(
      "#error \"Incompatible Kaitai Struct C++/STL API: version " +
        KSVersion.minimalRuntime + " or later is required\""
    )
    outHdr.puts("#endif")

    config.cppConfig.namespace.foreach { (namespace) =>
      outSrc.puts(s"namespace $namespace {")
      outSrc.inc
      outHdr.puts(s"namespace $namespace {")
      outHdr.inc
    }
  }

  override def fileFooter(topClassName: String): Unit = {
    config.cppConfig.namespace.foreach { (_) =>
      outSrc.dec
      outSrc.puts("}")
      outHdr.dec
      outHdr.puts("}")
    }

    if (!config.cppConfig.usePragmaOnce) {
      outHdr.puts
      outHdr.puts(s"#endif  // ${defineName(topClassName)}")
    }
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    importListHdr.addLocal(outFileNameHeader(extType.name.head))

  override def classHeader(name: List[String]): Unit = {
    val className = types2class(List(name.last))

    outHdr.puts
    outHdr.puts(s"class $className : public $kstructName {")
    outHdr.inc
    accessMode = PrivateAccess
    ensureMode(PublicAccess)

    /*
    outHdr.puts(s"static ${type2class(name)} from_file(std::string ${attrReaderName("file_name")});")

    outSrc.puts
    outSrc.puts(s"${type2class(name)} ${type2class(name)}::from_file(std::string ${attrReaderName("file_name")}) {")
    outSrc.inc
    outSrc.puts("std::ifstream* ifs = new std::ifstream(file_name, std::ifstream::binary);")
    outSrc.puts("kaitai::kstream *ks = new kaitai::kstream(ifs);")
    outSrc.puts(s"return new ${type2class(name)}(ks);")
    outSrc.dec
    outSrc.puts("}")
    */
  }

  override def classFooter(name: List[String]): Unit = {
    outHdr.dec
    outHdr.puts("};")
  }

  override def classForwardDeclaration(name: List[String]): Unit = {
    outHdr.puts(s"class ${types2class(name)};")
  }

  def importDataType(dt: DataType) = {
    dt match {
      case ut: UserType =>
        val classSpec = ut.classSpec.get
        if (classSpec.isTopLevel)
          importListSrc.addLocal(outFileNameHeader(classSpec.name.head))
      case _ => // no extra imports required
    }
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val (endianSuffixHdr, endianSuffixSrc)  = if (isHybrid) {
      (", int p_is_le = -1", ", int p_is_le")
    } else {
      ("", "")
    }

    val paramsArg = Utils.join(params.map { case (p) =>
      importDataType(p.dataType)
      s"${kaitaiType2NativeType(p.dataType)} ${paramName(p.id)}"
    }, "", ", ", ", ")

    val classNameBrief = types2class(List(name.last))

    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kaitaiType2NativeType(KaitaiStreamType)
    val tParent = kaitaiType2NativeType(parentType)
    val tRoot = kaitaiType2NativeType(CalcUserType(rootClassName, None))

    // Parent type might be declared somewhere else - in this case we need to include it
    importDataType(parentType)

    if (config.readWrite) {
      ensureMode(PrivateAccess)
      outHdr.puts("bool m__dirty;")
      ensureMode(PublicAccess)
    }

    outHdr.puts
    outHdr.puts(s"$classNameBrief($paramsArg" +
      s"$tIo $pIo, " +
      s"$tParent $pParent = $nullPtr, " +
      s"$tRoot $pRoot = $nullPtr$endianSuffixHdr);"
    )

    outSrc.puts
    outSrc.puts(s"${types2class(name)}::$classNameBrief($paramsArg" +
      s"$tIo $pIo, " +
      s"$tParent $pParent, " +
      s"$tRoot $pRoot$endianSuffixSrc) : $kstructName($pIo) {"
    )
    outSrc.inc

    handleAssignmentSimple(ParentIdentifier, pParent)
    handleAssignmentSimple(RootIdentifier, if (name == rootClassName) {
      s"${pRoot} ? ${pRoot} : this"
    } else {
      pRoot
    })

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        ensureMode(PrivateAccess)
        outHdr.puts("int m__is_le;")
        handleAssignmentSimple(EndianIdentifier, if (isHybrid) "p_is_le" else "-1")
        ensureMode(PublicAccess)
      case _ =>
        // no _is_le variable
    }

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readWrite) {
      outSrc.puts("m__dirty = false;")
    }
  }

  override def classConstructorFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def classDestructorHeader(name: List[String], parentType: DataType, topClassName: List[String]): Unit = {
    if (config.cppConfig.pointers == CppRuntimeConfig.RawPointers) {
      ensureMode(PrivateAccess)
      outHdr.puts("void _clean_up();")
    }
    ensureMode(PublicAccess)
    outHdr.puts(s"~${types2class(List(name.last))}();")

    outSrc.puts
    if (config.cppConfig.pointers == CppRuntimeConfig.RawPointers) {
      outSrc.puts(s"${types2class(name)}::~${types2class(List(name.last))}() {")
      outSrc.inc
      outSrc.puts("_clean_up();")
      outSrc.dec
      outSrc.puts("}")
      outSrc.puts
      outSrc.puts(s"void ${types2class(name)}::_clean_up() {")
      outSrc.inc
    } else {
      outSrc.puts(s"${types2class(name)}::~${types2class(List(name.last))}() {}")
    }
  }

  override def classDestructorFooter: Unit = {
    if (config.cppConfig.pointers != CppRuntimeConfig.RawPointers)
      return

    classConstructorFooter
  }

  override def runRead(name: List[String]): Unit = {
    val wrapToTryCatch = (config.cppConfig.pointers == CppRuntimeConfig.RawPointers)
    if (wrapToTryCatch) {
      outSrc.puts
      outSrc.puts("try {")
      outSrc.inc
    }
    outSrc.puts("_read();")
    if (wrapToTryCatch) {
      outSrc.dec
      outSrc.puts("} catch(...) {")
      outSrc.inc
      outSrc.puts("_clean_up();")
      outSrc.puts("throw;")
      outSrc.dec
      outSrc.puts("}")
    }
  }

  override def runReadCalc(): Unit = {
    outSrc.puts
    outSrc.puts("if (m__is_le == -1) {")
    outSrc.inc
    importListSrc.addKaitai("kaitai/exceptions.h")
    outSrc.puts(s"throw ${ksErrorName(UndecidedEndiannessError)}" +
      "(\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\");")
    outSrc.dec
    outSrc.puts("} else if (m__is_le == 1) {")
    outSrc.inc
    outSrc.puts("_read_le();")
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    outSrc.puts("_read_be();")
    outSrc.dec
    outSrc.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }

    ensureMode(if (config.autoRead) PrivateAccess else PublicAccess)

    outHdr.puts(s"void _read$suffix();")
    outSrc.puts
    outSrc.puts(s"void ${types2class(typeProvider.nowClass.name)}::_read$suffix() {")
    outSrc.inc
  }

  override def readFooter(): Unit = {
    if (config.readWrite) {
      outSrc.puts("m__dirty = false;")
    }
    outSrc.dec
    outSrc.puts("}")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
    declareNullFlag(attrName, attrType, isNullable)
  }

  def ensureMode(newMode: AccessMode): Unit = {
    if (accessMode != newMode) {
      outHdr.dec
      outHdr.puts
      outHdr.puts(newMode match {
        case PrivateAccess => "private:"
        case PublicAccess => "public:"
      })
      outHdr.inc
      accessMode = newMode
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PublicAccess)
    val ret = nonOwningPointer(privateMemberName(attrName), attrType)
    outHdr.puts(s"${kaitaiType2NativeType(attrType.asNonOwning())} ${publicMemberName(attrName)}() const { return $ret; }")
  }

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PublicAccess)
    val setDirty = if (isStreamType(attrType)) "" else "m__dirty = true; "
    val clearNull = if (isNullable && !needsDestruction(attrType)) s"${nullFlagForName(attrName)} = false; " else ""
    val invalidateValueInstances = typeProvider.nowClass.instances.collect {
      case (instName, _: ValueInstanceSpec) => s"${calculatedFlagForName(instName)} = false; "
    }.mkString
    val disableParseInstanceWriteback = attrName match {
      case instName: InstanceIdentifier =>
        typeProvider.nowClass.instances.get(instName) match {
          case Some(pi: ParseInstanceSpec) if isCurrentStreamLookaheadInstance(pi) =>
            s"${enabledFlagForName(instName)} = false; "
          case _ =>
            ""
        }
      case _ =>
        ""
    }
    val cacheInstance = attrName match {
      case instName: InstanceIdentifier if typeProvider.nowClass.instances.contains(instName) =>
        s"${calculatedFlagForName(instName)} = true; "
      case _ =>
        ""
    }
    outHdr.puts(s"void set_${idToStr(attrName)}(${kaitaiType2NativeType(attrType)} _v) { $setDirty$clearNull$invalidateValueInstances$disableParseInstanceWriteback$cacheInstance${privateMemberName(attrName)} = ${stdMoveWrap("_v")}; }")
  }

  private def isCurrentStreamLookaheadInstance(instSpec: ParseInstanceSpec): Boolean =
    instSpec.io.isEmpty && instSpec.pos.contains(Ast.expr.Attribute(Ast.expr.Name(Ast.identifier("_io")), Ast.identifier("pos")))

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    writeFooterNeedsFetchInstances = endian.isEmpty
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }

    ensureMode(endian match {
      case Some(_) => PrivateAccess
      case None => PublicAccess
    })

    outHdr.puts(s"void _write$suffix();")
    outSrc.puts
    outSrc.puts(s"void ${types2class(typeProvider.nowClass.name)}::_write$suffix() {")
    outSrc.inc
  }

  override def writeFooter(): Unit = {
    if (writeFooterNeedsFetchInstances)
      outSrc.puts("_fetch_instances();")
    outSrc.puts("m__dirty = false;")
    outSrc.dec
    outSrc.puts("}")
  }

  override def fetchInstancesHeader(): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts("void _fetch_instances();")
    outSrc.puts
    outSrc.puts(s"void ${types2class(typeProvider.nowClass.name)}::_fetch_instances() {")
    outSrc.inc
  }

  override def fetchInstancesFooter(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    val base = expression(baseExpr)
    if (exprType.asCombined != dataType) {
      importListSrc.addSystem("stdexcept")
      val castType = kaitaiType2NativeType(dataType.asNonOwning())
      val expr = nonOwningPointer(base, exprType)
      outSrc.puts("{")
      outSrc.inc
      outSrc.puts(s"$castType _switch_obj = dynamic_cast<$castType>($expr);")
      outSrc.puts(s"if (_switch_obj == $nullPtr) {")
      outSrc.inc
      outSrc.puts("""throw std::runtime_error("switch object type mismatch in _fetch_instances");""")
      outSrc.dec
      outSrc.puts("}")
      outSrc.puts("_switch_obj->_fetch_instances();")
      outSrc.dec
      outSrc.puts("}")
    } else {
      val expr = nonOwningPointer(base, dataType)
      outSrc.puts(s"$expr->_fetch_instances();")
    }
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${publicMemberName(instName)}();")

  override def runWriteCalc(): Unit = {
    outSrc.puts("if (m__is_le == -1) {")
    outSrc.inc
    importListSrc.addKaitai("kaitai/exceptions.h")
    outSrc.puts(s"throw ${ksErrorName(UndecidedEndiannessError)}" +
      "(\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\");")
    outSrc.dec
    outSrc.puts("} else if (m__is_le == 1) {")
    outSrc.inc
    outSrc.puts("_write_le();")
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    outSrc.puts("_write_be();")
    outSrc.dec
    outSrc.puts("}")
  }

  override def checkHeader(): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts("void _check();")
    outSrc.puts
    outSrc.puts(s"void ${types2class(typeProvider.nowClass.name)}::_check() {")
    outSrc.inc
  }

  override def checkFooter(): Unit = {
    outSrc.puts("m__dirty = false;")
    outSrc.dec
    outSrc.puts("}")
  }

  override def writeInstanceFooter(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def checkInstanceFooter(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def attrWrite(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
    val io = attr match {
      case pis: ParseInstanceSpec =>
        val io = pis.io.map(useIO).getOrElse(normalIO)
        pis.pos.foreach { pos =>
          pushPos(io)
          seek(io, pos)
        }
        io
      case _ =>
        normalIO
    }

    attr.cond.repeat match {
      case RepeatExpr(repeatExpr) =>
        attr.cond.ifExpr match {
          case Some(ifExpr) =>
            outSrc.puts(s"if (${expression(ifExpr)}) {")
            outSrc.inc
            attrWriteRepeatExpr(attr, id, repeatExpr, defEndian)
            outSrc.dec
            outSrc.puts("}")
          case None =>
            attrWriteRepeatExpr(attr, id, repeatExpr, defEndian)
        }
        attr match {
          case pis: ParseInstanceSpec if pis.pos.isDefined => popPos(io)
          case _ =>
        }
        return
      case repUntil: RepeatUntil =>
        attr.cond.ifExpr match {
          case Some(ifExpr) =>
            outSrc.puts(s"if (${expression(ifExpr)}) {")
            outSrc.inc
            attrWriteRepeatUntil(attr, id, repUntil, defEndian)
            outSrc.dec
            outSrc.puts("}")
          case None =>
            attrWriteRepeatUntil(attr, id, repUntil, defEndian)
        }
        attr match {
          case pis: ParseInstanceSpec if pis.pos.isDefined => popPos(io)
          case _ =>
        }
        return
      case RepeatEos =>
        attr.cond.ifExpr match {
          case Some(ifExpr) =>
            outSrc.puts(s"if (${expression(ifExpr)}) {")
            outSrc.inc
            attrWriteRepeatEos(attr, id, defEndian)
            outSrc.dec
            outSrc.puts("}")
          case None =>
            attrWriteRepeatEos(attr, id, defEndian)
        }
        attr match {
          case pis: ParseInstanceSpec if pis.pos.isDefined => popPos(io)
          case _ =>
        }
        return
      case NoRepeat =>
    }

    val fixedEndian = defEndian match {
      case Some(fe: FixedEndian) => Some(fe)
      case None => None
      case _ => None
    }

    attr.cond.ifExpr match {
      case Some(ifExpr) =>
        outSrc.puts(s"if (${expression(ifExpr)}) {")
        outSrc.inc
        emitWriteExpr(attr, attr.dataType, privateMemberName(id), fixedEndian, io)
        outSrc.dec
        outSrc.puts("}")
      case None =>
        emitWriteExpr(attr, attr.dataType, privateMemberName(id), fixedEndian, io)
    }

    attr match {
      case pis: ParseInstanceSpec if pis.pos.isDefined =>
        popPos(io)
      case _ =>
    }
  }

  override def attrCheck(attr: AttrLikeSpec, id: Identifier): Unit = {
    val io = attr match {
      case pis: ParseInstanceSpec => pis.io.map(useIO).getOrElse(normalIO)
      case _ => normalIO
    }

    attr.cond.repeat match {
      case RepeatExpr(repeatExpr) =>
        attr.cond.ifExpr match {
          case Some(ifExpr) =>
            val isSetExpr = attrIsSetExpr(attr, id)
            importListSrc.addSystem("stdexcept")
            outSrc.puts(s"if (${expression(ifExpr)}) {")
            outSrc.inc
            outSrc.puts(s"if (!($isSetExpr)) {")
            outSrc.inc
            outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field is not set");""")
            outSrc.dec
            outSrc.puts("}")
            attrCheckRepeatExpr(attr, id, repeatExpr)
            outSrc.dec
            outSrc.puts("} else {")
            outSrc.inc
            outSrc.puts(s"if ($isSetExpr) {")
            outSrc.inc
            outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field should be absent");""")
            outSrc.dec
            outSrc.puts("}")
            outSrc.dec
            outSrc.puts("}")
          case None =>
            attrCheckRepeatExpr(attr, id, repeatExpr)
        }
        return
      case repUntil: RepeatUntil =>
        attr.cond.ifExpr match {
          case Some(ifExpr) =>
            val isSetExpr = attrIsSetExpr(attr, id)
            importListSrc.addSystem("stdexcept")
            outSrc.puts(s"if (${expression(ifExpr)}) {")
            outSrc.inc
            outSrc.puts(s"if (!($isSetExpr)) {")
            outSrc.inc
            outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field is not set");""")
            outSrc.dec
            outSrc.puts("}")
            attrCheckRepeatUntil(attr, id, repUntil)
            outSrc.dec
            outSrc.puts("} else {")
            outSrc.inc
            outSrc.puts(s"if ($isSetExpr) {")
            outSrc.inc
            outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field should be absent");""")
            outSrc.dec
            outSrc.puts("}")
            outSrc.dec
            outSrc.puts("}")
          case None =>
            attrCheckRepeatUntil(attr, id, repUntil)
        }
        return
      case RepeatEos =>
        attr.cond.ifExpr match {
          case Some(ifExpr) =>
            val isSetExpr = attrIsSetExpr(attr, id)
            importListSrc.addSystem("stdexcept")
            outSrc.puts(s"if (${expression(ifExpr)}) {")
            outSrc.inc
            outSrc.puts(s"if (!($isSetExpr)) {")
            outSrc.inc
            outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field is not set");""")
            outSrc.dec
            outSrc.puts("}")
            attrCheckRepeatEos(attr, id)
            outSrc.dec
            outSrc.puts("} else {")
            outSrc.inc
            outSrc.puts(s"if ($isSetExpr) {")
            outSrc.inc
            outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field should be absent");""")
            outSrc.dec
            outSrc.puts("}")
            outSrc.dec
            outSrc.puts("}")
          case None =>
            attrCheckRepeatEos(attr, id)
        }
        return
      case NoRepeat =>
    }

    attr.cond.ifExpr match {
      case Some(ifExpr) =>
        val isSetExpr = attrIsSetExpr(attr, id)
        importListSrc.addSystem("stdexcept")
        outSrc.puts(s"if (${expression(ifExpr)}) {")
        outSrc.inc
        outSrc.puts(s"if (!($isSetExpr)) {")
        outSrc.inc
        outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field is not set");""")
        outSrc.dec
        outSrc.puts("}")
        emitCheckExpr(attr, attr.dataType, privateMemberName(id), io)
        attr.valid.foreach(valid => attrValidate(attr, valid, true))
        outSrc.dec
        outSrc.puts("} else {")
        outSrc.inc
        outSrc.puts(s"if ($isSetExpr) {")
        outSrc.inc
        outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: conditional field should be absent");""")
        outSrc.dec
        outSrc.puts("}")
        outSrc.dec
        outSrc.puts("}")
        return
      case None =>
    }

    attr.dataType match {
      case _ =>
    }

    emitCheckExpr(attr, attr.dataType, privateMemberName(id), io)
    attr.valid.foreach(valid => attrValidate(attr, valid, true))
  }

  private def attrAssertUserTypePresent(attr: AttrLikeSpec, id: Identifier, expr: String): Unit = {
    importListSrc.addSystem("stdexcept")
    outSrc.puts(s"if ($expr == $nullPtr) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: nested object is not set");""")
    outSrc.dec
    outSrc.puts("}")
  }

  private def attrAssertRepeatFieldPresent(attr: AttrLikeSpec, id: Identifier): Unit = {
    importListSrc.addSystem("stdexcept")
    outSrc.puts(s"if (${privateMemberName(id)} == $nullPtr) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: repeated field is not set");""")
    outSrc.dec
    outSrc.puts("}")
  }

  private def attrWriteRepeatExpr(attr: AttrLikeSpec, id: Identifier, repeatExpr: Ast.expr, defEndian: Option[Endianness]): Unit = {
    if (attr.valid.nonEmpty) {
      throw new NotImplementedError(s"C++ read-write prototype does not support validations on repeated fields yet: ${attr.path.mkString("/")}")
    }

    val fixedEndian = defEndian match {
      case Some(fe: FixedEndian) => Some(fe)
      case None => None
      case _ => None
    }
    val vecType = s"std::vector<${kaitaiType2NativeType(attr.dataType)}>"
    val itemExpr = "(*it)"

    attrAssertRepeatFieldPresent(attr, id)
    outSrc.puts(s"for ($vecType::const_iterator it = ${privateMemberName(id)}->begin(); it != ${privateMemberName(id)}->end(); ++it) {")
    outSrc.inc
    emitWriteExpr(attr, attr.dataType, itemExpr, fixedEndian, normalIO)
    outSrc.dec
    outSrc.puts("}")
  }

  private def attrCheckRepeatExpr(attr: AttrLikeSpec, id: Identifier, repeatExpr: Ast.expr): Unit = {
    if (attr.valid.nonEmpty) {
      throw new NotImplementedError(s"C++ read-write prototype does not support validations on repeated fields yet: ${attr.path.mkString("/")}")
    }

    attrAssertRepeatFieldPresent(attr, id)
    outSrc.puts(s"if (${privateMemberName(id)}->size() != static_cast<std::size_t>(${expression(repeatExpr)})) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: repeat-expr size mismatch");""")
    outSrc.dec
    outSrc.puts("}")

    val vecType = s"std::vector<${kaitaiType2NativeType(attr.dataType)}>"
    outSrc.puts(s"for ($vecType::const_iterator it = ${privateMemberName(id)}->begin(); it != ${privateMemberName(id)}->end(); ++it) {")
    outSrc.inc
    emitCheckExpr(attr, attr.dataType, "(*it)", normalIO)
    outSrc.dec
    outSrc.puts("}")
  }

  private def attrWriteRepeatUntil(attr: AttrLikeSpec, id: Identifier, repUntil: RepeatUntil, defEndian: Option[Endianness]): Unit = {
    if (attr.valid.nonEmpty) {
      throw new NotImplementedError(s"C++ read-write prototype does not support validations on repeated fields yet: ${attr.path.mkString("/")}")
    }

    val fixedEndian = defEndian match {
      case Some(fe: FixedEndian) => Some(fe)
      case None => None
      case _ => None
    }

    emitWriteRepeatLoop(attr, id, fixedEndian)
  }

  private def attrCheckRepeatUntil(attr: AttrLikeSpec, id: Identifier, repUntil: RepeatUntil): Unit = {
    if (attr.valid.nonEmpty) {
      throw new NotImplementedError(s"C++ read-write prototype does not support validations on repeated fields yet: ${attr.path.mkString("/")}")
    }

    attrAssertRepeatFieldPresent(attr, id)
    importListSrc.addSystem("stdexcept")
    importListSrc.addSystem("cstddef")
    outSrc.puts(s"if (${privateMemberName(id)}->empty()) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: repeat-until field must not be empty");""")
    outSrc.dec
    outSrc.puts("}")

    emitCheckRepeatLoop(attr, id) {
      outSrc.puts(s"const bool _is_last = (i == ${privateMemberName(id)}->size() - 1);")
      outSrc.puts(s"if ((${repeatUntilExpr(id, attr.dataType, repUntil)}) != _is_last) {")
      outSrc.inc
      outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: repeat-until condition mismatch");""")
      outSrc.dec
      outSrc.puts("}")
    }
  }

  private def attrWriteRepeatEos(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
    if (attr.valid.nonEmpty) {
      throw new NotImplementedError(s"C++ read-write prototype does not support validations on repeated fields yet: ${attr.path.mkString("/")}")
    }

    val fixedEndian = defEndian match {
      case Some(fe: FixedEndian) => Some(fe)
      case None => None
      case _ => None
    }

    attrAssertRepeatFieldPresent(attr, id)
    fixedSerializedSizeExpr(attr.dataType) match {
      case Some(itemSizeExpr) =>
        importListSrc.addSystem("stdexcept")
        outSrc.puts(s"const std::size_t _remaining = static_cast<std::size_t>(${normalIO}->size() - ${normalIO}->pos());")
        outSrc.puts(s"const std::size_t _actual = ${privateMemberName(id)}->size() * static_cast<std::size_t>($itemSizeExpr);")
        outSrc.puts("if (_remaining != 0 && _remaining != _actual) {")
        outSrc.inc
        outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: expected: " + kaitai::kstream::to_string(_remaining) + ", actual: " + kaitai::kstream::to_string(_actual));""")
        outSrc.dec
        outSrc.puts("}")
      case None if attr.dataType.isInstanceOf[UserType] || attr.dataType.isInstanceOf[UserTypeInstream] =>
        // No fixed-size shortcut is available for plain repeated user types.
        // For repeat:eos, writing the provided elements is still well-defined.
      case None =>
        throw new NotImplementedError(s"C++ read-write prototype does not support repeat: eos for field type `${attr.dataType}` yet: ${attr.path.mkString("/")}")
    }

    emitWriteRepeatLoop(attr, id, fixedEndian)
  }

  private def attrCheckRepeatEos(attr: AttrLikeSpec, id: Identifier): Unit = {
    if (attr.valid.nonEmpty) {
      throw new NotImplementedError(s"C++ read-write prototype does not support validations on repeated fields yet: ${attr.path.mkString("/")}")
    }

    attrAssertRepeatFieldPresent(attr, id)
    emitCheckRepeatLoop(attr, id) {}
  }

  private def emitWriteRepeatLoop(attr: AttrLikeSpec, id: Identifier, fixedEndian: Option[FixedEndian]): Unit = {
    val vecType = s"std::vector<${kaitaiType2NativeType(attr.dataType)}>"
    val itemExpr = "(*it)"

    attrAssertRepeatFieldPresent(attr, id)
    outSrc.puts(s"for ($vecType::const_iterator it = ${privateMemberName(id)}->begin(); it != ${privateMemberName(id)}->end(); ++it) {")
    outSrc.inc
    emitWriteExpr(attr, attr.dataType, itemExpr, fixedEndian, normalIO)
    outSrc.dec
    outSrc.puts("}")
  }

  private def emitCheckRepeatLoop(attr: AttrLikeSpec, id: Identifier)(extraBody: => Unit): Unit = {
    val vecType = s"std::vector<${kaitaiType2NativeType(attr.dataType)}>"
    val itemExpr = "(*it)"
    outSrc.puts(s"for ($vecType::const_iterator it = ${privateMemberName(id)}->begin(); it != ${privateMemberName(id)}->end(); ++it) {")
    outSrc.inc
    outSrc.puts(s"const std::size_t i = static_cast<std::size_t>(it - ${privateMemberName(id)}->begin());")
    outSrc.puts(s"const ${kaitaiType2NativeType(attr.dataType.asNonOwning())} ${translator.doName(Identifier.ITERATOR)} = ${nonOwningPointer(itemExpr, attr.dataType)};")
    emitCheckExpr(attr, attr.dataType, itemExpr, normalIO)
    extraBody
    outSrc.dec
    outSrc.puts("}")
  }

  private def repeatUntilExpr(id: Identifier, dataType: DataType, repUntil: RepeatUntil): String = {
    val prevIteratorType = typeProvider._currentIteratorType
    typeProvider._currentIteratorType = Some(dataType)
    try {
      expression(repUntil.expr)
    } finally {
      typeProvider._currentIteratorType = prevIteratorType
    }
  }

  private def fixedSerializedSizeExpr(dataType: DataType): Option[String] = dataType match {
    case Int1Type(_) => Some("1")
    case IntMultiType(_, width, _) => Some(width.width.toString)
    case FloatMultiType(width, _) => Some(width.width.toString)
    case et: EnumType =>
      fixedSerializedSizeExpr(et.basedOn)
    case bt: BytesLimitType if bt.terminator.isEmpty && bt.padRight.isEmpty && bt.process.isEmpty =>
      Some(expression(bt.size))
    case st: StrFromBytesType =>
      st.bytes match {
        case bt: BytesLimitType if bt.terminator.isEmpty && bt.padRight.isEmpty && bt.process.isEmpty =>
          Some(expression(bt.size))
        case _ =>
          None
      }
    case ut: UserTypeFromBytes =>
      rawSubstreamExpectedSizeExpr(ut.bytes)
    case ut: CalcUserTypeFromBytes =>
      rawSubstreamExpectedSizeExpr(ut.bytes)
    case _ =>
      None
  }

  private def rawSubstreamExpectedSizeExpr(bytesType: BytesType): Option[String] = bytesType match {
    case bt: BytesLimitType if bt.terminator.isEmpty && bt.padRight.isEmpty && bt.process.isEmpty =>
      Some(expression(bt.size))
    case _ =>
      None
  }

  private def prepareUserTypeFromBytesIO(attr: AttrLikeSpec, bytesType: BytesType): (String, String) = {
    if (attr.cond.repeat != NoRepeat) {
      throw new NotImplementedError(s"C++ read-write prototype does not support repeated raw-substream user types yet: ${attr.path.mkString("/")}")
    }

    val rawId = RawIdentifier(attr.id)
    val rawExpr = privateMemberName(rawId)
    val sizeExpr = rawSubstreamExpectedSizeExpr(bytesType).getOrElse {
      throw new NotImplementedError(s"C++ read-write prototype does not support this raw substream shape yet: ${attr.path.mkString("/")}")
    }
    outSrc.puts(s"$rawExpr = std::string(static_cast<std::string::size_type>($sizeExpr), '\\0');")
    (allocateIO(rawId, NoRepeat), sizeExpr)
  }

  private def assertRawSubstreamSize(attr: AttrLikeSpec, io: String, sizeExpr: String, isPointer: Boolean = true): Unit = {
    importListSrc.addSystem("stdexcept")
    val access = if (isPointer) "->" else "."
    outSrc.puts(s"if ($io${access}pos() != static_cast<uint64_t>($sizeExpr)) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: serialized size mismatch");""")
    outSrc.dec
    outSrc.puts("}")
    outSrc.puts(s"if ($io${access}to_byte_array().size() != static_cast<std::string::size_type>($sizeExpr)) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: raw buffer size mismatch");""")
    outSrc.dec
    outSrc.puts("}")
  }

  private def emitRepeatedRawSubstreamWrite(attr: AttrLikeSpec, bytesType: BytesType, userExpr: String, io: String): Unit = {
    val sizeExpr = rawSubstreamExpectedSizeExpr(bytesType).getOrElse {
      throw new NotImplementedError(s"C++ read-write prototype does not support this repeated raw substream shape yet: ${attr.path.mkString("/")}")
    }
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts(s"std::string _raw = std::string(static_cast<std::string::size_type>($sizeExpr), '\\0');")
    outSrc.puts(s"$kstreamName _io_raw(_raw);")
    outSrc.puts(s"$userExpr->_set_io(&_io_raw);")
    outSrc.puts(s"$userExpr->_write();")
    assertRawSubstreamSize(attr, "_io_raw", sizeExpr, isPointer = false)
    outSrc.puts(s"${io}->write_bytes(_io_raw.to_byte_array());")
    outSrc.dec
    outSrc.puts("}")
  }

  private def emitRepeatedRawSubstreamCheck(attr: AttrLikeSpec, bytesType: BytesType, userExpr: String): Unit = {
    val sizeExpr = rawSubstreamExpectedSizeExpr(bytesType).getOrElse {
      throw new NotImplementedError(s"C++ read-write prototype does not support this repeated raw substream shape yet: ${attr.path.mkString("/")}")
    }
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts(s"std::string _raw = std::string(static_cast<std::string::size_type>($sizeExpr), '\\0');")
    outSrc.puts(s"$kstreamName _io_raw(_raw);")
    outSrc.puts(s"$userExpr->_set_io(&_io_raw);")
    outSrc.puts(s"$userExpr->_check();")
    outSrc.dec
    outSrc.puts("}")
  }

  private def emitSwitchCaseExpr(
    attr: AttrLikeSpec,
    caseType: DataType,
    assignType: DataType,
    expr: String,
    fixedEndian: Option[FixedEndian],
    io: String,
    emitLeaf: (String, DataType) => Unit
  ): Unit = {
    caseType match {
      case _: BytesType if switchBytesOnlyAsRaw =>
        emitLeaf(privateMemberName(RawIdentifier(attr.id)), caseType)
      case _: UserType | _: UserTypeFromBytes | _: CalcUserTypeFromBytes if assignType.asCombined != caseType =>
        importListSrc.addSystem("stdexcept")
        val castType = kaitaiType2NativeType(caseType.asNonOwning())
        val switchExpr = nonOwningPointer(expr, assignType)
        outSrc.puts("{")
        outSrc.inc
        outSrc.puts(s"$castType _switch_obj = dynamic_cast<$castType>($switchExpr);")
        outSrc.puts(s"if (_switch_obj == $nullPtr) {")
        outSrc.inc
        outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: switch object type mismatch");""")
        outSrc.dec
        outSrc.puts("}")
        emitLeaf("_switch_obj", caseType.asNonOwning())
        outSrc.dec
        outSrc.puts("}")
      case _ =>
        emitLeaf(expr, caseType)
    }
  }

  private def emitSwitchWrite(attr: AttrLikeSpec, st: SwitchType, expr: String, fixedEndian: Option[FixedEndian], io: String): Unit = {
    switchCases[DataType](attr.id, st.on, st.cases,
      (caseType) => emitSwitchCaseExpr(attr, caseType, st.combinedType, expr, fixedEndian, io,
        (caseExpr, concreteType) => emitWriteExpr(attr, concreteType, caseExpr, fixedEndian, io)
      ),
      (caseType) => emitSwitchCaseExpr(attr, caseType, st.combinedType, expr, fixedEndian, io,
        (caseExpr, concreteType) => emitWriteExpr(attr, concreteType, caseExpr, fixedEndian, io)
      )
    )
  }

  private def emitSwitchCheck(attr: AttrLikeSpec, st: SwitchType, expr: String, io: String): Unit = {
    switchCases[DataType](attr.id, st.on, st.cases,
      (caseType) => emitSwitchCaseExpr(attr, caseType, st.combinedType, expr, None, io,
        (caseExpr, concreteType) => emitCheckExpr(attr, concreteType, caseExpr, io)
      ),
      (caseType) => emitSwitchCaseExpr(attr, caseType, st.combinedType, expr, None, io,
        (caseExpr, concreteType) => emitCheckExpr(attr, concreteType, caseExpr, io)
      )
    )
  }

  private def emitWriteExpr(attr: AttrLikeSpec, dataType: DataType, expr: String, fixedEndian: Option[FixedEndian], io: String): Unit = {
    dataType match {
      case rt: ReadableType =>
        outSrc.puts(s"${io}->write_${rt.apiCall(fixedEndian)}($expr);")
      case BitsType1(bitEndian) =>
        outSrc.puts(s"${io}->write_bits_int_${bitEndian.toSuffix}(1, (($expr) ? 1 : 0));")
      case BitsType(width, bitEndian) =>
        outSrc.puts(s"${io}->write_bits_int_${bitEndian.toSuffix}($width, $expr);")
      case et: EnumType =>
        et.basedOn match {
          case rt: ReadableType =>
            outSrc.puts(s"${io}->write_${rt.apiCall(fixedEndian)}(static_cast<${kaitaiType2NativeType(et.basedOn)}>($expr));")
          case BitsType(width, bitEndian) =>
            outSrc.puts(s"${io}->write_bits_int_${bitEndian.toSuffix}($width, static_cast<uint64_t>($expr));")
          case _ =>
            throw new NotImplementedError(s"C++ read-write prototype does not support enum base type `${et.basedOn}` yet: ${attr.path.mkString("/")}")
        }
      case _: BytesType =>
        outSrc.puts(s"${io}->write_bytes($expr);")
      case st: StrFromBytesType =>
        st.bytes match {
          case bt: BytesLimitType =>
            emitStringBytesLimitWrite(attr, expr, bt, io)
          case _ =>
            outSrc.puts(s"${io}->write_bytes($expr);")
        }
      case _: StrType =>
        outSrc.puts(s"${io}->write_bytes($expr);")
      case ut: UserTypeFromBytes =>
        val userExpr = nonOwningPointer(expr, ut)
        attrAssertUserTypePresent(attr, attr.id, userExpr)
        if (attr.cond.repeat == NoRepeat) {
          val (subIo, sizeExpr) = prepareUserTypeFromBytesIO(attr, ut.bytes)
          outSrc.puts(s"$userExpr->_set_io($subIo);")
          outSrc.puts(s"$userExpr->_write();")
          assertRawSubstreamSize(attr, subIo, sizeExpr)
          outSrc.puts(s"${privateMemberName(RawIdentifier(attr.id))} = $subIo->to_byte_array();")
          outSrc.puts(s"${io}->write_bytes(${privateMemberName(RawIdentifier(attr.id))});")
        } else {
          emitRepeatedRawSubstreamWrite(attr, ut.bytes, userExpr, io)
        }
      case ut: CalcUserTypeFromBytes =>
        val userExpr = nonOwningPointer(expr, ut)
        attrAssertUserTypePresent(attr, attr.id, userExpr)
        if (attr.cond.repeat == NoRepeat) {
          val (subIo, sizeExpr) = prepareUserTypeFromBytesIO(attr, ut.bytes)
          outSrc.puts(s"$userExpr->_set_io($subIo);")
          outSrc.puts(s"$userExpr->_write();")
          assertRawSubstreamSize(attr, subIo, sizeExpr)
          outSrc.puts(s"${privateMemberName(RawIdentifier(attr.id))} = $subIo->to_byte_array();")
          outSrc.puts(s"${io}->write_bytes(${privateMemberName(RawIdentifier(attr.id))});")
        } else {
          emitRepeatedRawSubstreamWrite(attr, ut.bytes, userExpr, io)
        }
      case ut: UserType =>
        val userExpr = nonOwningPointer(expr, ut)
        attrAssertUserTypePresent(attr, attr.id, userExpr)
        outSrc.puts(s"$userExpr->_set_io(${io});")
        outSrc.puts(s"$userExpr->_write();")
      case st: SwitchType =>
        emitSwitchWrite(attr, st, expr, fixedEndian, io)
      case _ =>
        throw new NotImplementedError(s"C++ read-write prototype does not support field type `${dataType}` yet: ${attr.path.mkString("/")}")
    }
  }

  private def emitCheckExpr(attr: AttrLikeSpec, dataType: DataType, expr: String, io: String): Unit = {
    dataType match {
      case ut: UserTypeFromBytes =>
        val userExpr = nonOwningPointer(expr, ut)
        attrAssertExprPresent(attr, userExpr, "nested object is not set")
        if (attr.cond.repeat == NoRepeat) {
          val (subIo, _) = prepareUserTypeFromBytesIO(attr, ut.bytes)
          outSrc.puts(s"$userExpr->_set_io($subIo);")
          outSrc.puts(s"$userExpr->_check();")
        } else {
          emitRepeatedRawSubstreamCheck(attr, ut.bytes, userExpr)
        }
      case ut: CalcUserTypeFromBytes =>
        val userExpr = nonOwningPointer(expr, ut)
        attrAssertExprPresent(attr, userExpr, "nested object is not set")
        if (attr.cond.repeat == NoRepeat) {
          val (subIo, _) = prepareUserTypeFromBytesIO(attr, ut.bytes)
          outSrc.puts(s"$userExpr->_set_io($subIo);")
          outSrc.puts(s"$userExpr->_check();")
        } else {
          emitRepeatedRawSubstreamCheck(attr, ut.bytes, userExpr)
        }
      case ut: UserType =>
        val userExpr = nonOwningPointer(expr, ut)
        attrAssertExprPresent(attr, userExpr, "nested object is not set")
        outSrc.puts(s"$userExpr->_set_io(${io});")
        outSrc.puts(s"$userExpr->_check();")
      case st: SwitchType =>
        emitSwitchCheck(attr, st, expr, io)
      case bt: BytesLimitType =>
        attrCheckFixedSizeExpr(attr, expr, bt.size)
      case st: StrFromBytesType =>
        st.bytes match {
          case bt: BytesLimitType =>
            emitStringBytesLimitCheck(attr, expr, bt)
          case _ =>
        }
      case _ =>
    }
  }

  private def emitStringBytesLimitWrite(attr: AttrLikeSpec, expr: String, bt: BytesLimitType, io: String): Unit = {
    val term = bt.terminator.map { terminator =>
      if (terminator.length == 1) {
        terminator.head & 0xff
      } else {
        throw new NotImplementedError(s"C++ read-write prototype does not support multibyte string terminators yet: ${attr.path.mkString("/")}")
      }
    }

    if (bt.terminator.isEmpty && bt.padRight.isEmpty) {
      outSrc.puts(s"${io}->write_bytes($expr);")
      return
    }

    val sizeExpr = expression(bt.size)
    val bufName = s"_buf${privateMemberName(attr.id)}"
    val padByte = bt.padRight.orElse(term.map(_ => 0)).getOrElse(0)
    outSrc.puts(s"std::string $bufName = $expr;")
    term.foreach { t =>
      if (!bt.include) {
        outSrc.puts(s"if ($bufName.size() < static_cast<std::string::size_type>($sizeExpr)) {")
        outSrc.inc
        outSrc.puts(s"""$bufName += std::string(1, static_cast<char>($t));""")
        outSrc.dec
        outSrc.puts("}")
      }
    }
    outSrc.puts(s"if ($bufName.size() < static_cast<std::string::size_type>($sizeExpr)) {")
    outSrc.inc
    outSrc.puts(s"$bufName.append(static_cast<std::string::size_type>($sizeExpr) - $bufName.size(), static_cast<char>($padByte));")
    outSrc.dec
    outSrc.puts("}")
    outSrc.puts(s"${io}->write_bytes($bufName);")
  }

  private def emitStringBytesLimitCheck(attr: AttrLikeSpec, expr: String, bt: BytesLimitType): Unit = {
    importListSrc.addSystem("stdexcept")
    val sizeExpr = expression(bt.size)
    if (bt.terminator.isEmpty && bt.padRight.isEmpty) {
      attrCheckFixedSizeExpr(attr, expr, bt.size)
      return
    }

    outSrc.puts(s"if ($expr.size() > static_cast<std::string::size_type>($sizeExpr)) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: size mismatch");""")
    outSrc.dec
    outSrc.puts("}")

    bt.terminator.foreach { terminator =>
      if (terminator.length != 1) {
        throw new NotImplementedError(s"C++ read-write prototype does not support multibyte string terminators yet: ${attr.path.mkString("/")}")
      }
      if (!bt.include) {
        val term = terminator.head & 0xff
        outSrc.puts(s"if ($expr.find(static_cast<char>($term)) != std::string::npos) {")
        outSrc.inc
        outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: terminator must not appear in value");""")
        outSrc.dec
        outSrc.puts("}")
      }
    }
  }

  private def attrIsSetExpr(attr: AttrLikeSpec, id: Identifier): String =
    if (attr.cond.repeat != NoRepeat || needsDestruction(attr.dataType)) {
      s"${nonOwningPointer(privateMemberName(id), attr.dataType)} != $nullPtr"
    } else {
      s"!${nullFlagForName(id)}"
    }

  private def attrAssertExprPresent(attr: AttrLikeSpec, expr: String, message: String): Unit = {
    importListSrc.addSystem("stdexcept")
    outSrc.puts(s"if ($expr == $nullPtr) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: $message");""")
    outSrc.dec
    outSrc.puts("}")
  }

  private def attrCheckFixedSize(attr: AttrLikeSpec, id: Identifier, expectedSize: Ast.expr): Unit = {
    attrCheckFixedSizeExpr(attr, privateMemberName(id), expectedSize)
  }

  private def attrCheckFixedSizeExpr(attr: AttrLikeSpec, expr: String, expectedSize: Ast.expr): Unit = {
    importListSrc.addSystem("stdexcept")
    outSrc.puts(s"if ($expr.size() != static_cast<std::string::size_type>(${expression(expectedSize)})) {")
    outSrc.inc
    outSrc.puts(s"""throw std::runtime_error("${attr.path.mkString("/", "/", "")}: size mismatch");""")
    outSrc.dec
    outSrc.puts("}")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    // All docstrings would be for public stuff, so it's safe to start it here
    ensureMode(PublicAccess)

    outHdr.puts
    outHdr.puts( "/**")

    doc.summary.foreach(docStr => outHdr.putsLines(" * ", docStr))

    doc.ref.foreach {
      case TextRef(text) =>
        outHdr.putsLines(" * ", s"\\sa $text")
      case UrlRef(url, text) =>
        outHdr.putsLines(" * ", s"\\sa $url $text")
    }

    outHdr.puts( " */")
  }

  override def attrInit(attr: AttrLikeSpec): Unit = {
    if (attr.isNullable && !needsDestruction(attr.dataTypeComposite))
      outSrc.puts(s"${nullFlagForName(attr.id)} = true;")

    // Only owning raw pointers (used in C++98) must be zero-initialized. Fields
    // of type `std::unique_ptr` (used for all pointers in C++11) don't need to
    // be initialized to `nullptr`, as this is the default behavior.
    if (config.cppConfig.pointers != CppRuntimeConfig.RawPointers)
      return

    // If the data type needs destruction, it means that it's a pointer type and
    // we want to zero-initialize all pointers - see
    // https://github.com/kaitai-io/kaitai_struct/issues/244
    if (needsDestruction(attr.dataTypeComposite))
      outSrc.puts(s"${privateMemberName(attr.id)} = $nullPtr;")
  }

  override def attrDestructor(attr: AttrLikeSpec, id: Identifier): Unit = {
    if (config.cppConfig.pointers != CppRuntimeConfig.RawPointers)
      return

    (ExtraAttrs.forAttr(attr, this) ++ List(attr)).foreach { (attr) =>
      destructMember(attr.id, attr.dataType, attr.isArray)
    }
  }

  def destructMember(id: Identifier, innerType: DataType, isArray: Boolean): Unit = {
    val ptr = privateMemberName(id)
    val innerNeedsDestruct = needsDestruction(innerType)

    if (!isArray && !innerNeedsDestruct)
      return

    if (isArray && innerNeedsDestruct) {
      outSrc.puts(s"if ($ptr) {")
      outSrc.inc
      destructVector(kaitaiType2NativeType(innerType), ptr)
      outSrc.puts(s"delete $ptr;")
      outSrc.dec
      outSrc.puts("}")
    } else {
      outSrc.puts(s"delete $ptr;")
    }
  }

  def needsDestruction(t: DataType): Boolean = {
    val combinedType = t match {
      case st: SwitchType => combineSwitchType(st)
      case other => other
    }
    combinedType match {
      case _: UserType | _: ArrayTypeInStream | KaitaiStructType | AnyType | OwnedKaitaiStreamType => true
      case _ => false
    }
  }

  def isStreamType(t: DataType): Boolean = {
    t match {
      case at: ArrayType => isStreamType(at.elType)
      case KaitaiStreamType | OwnedKaitaiStreamType => true
      case _ => false
    }
  }

  /**
    * Generates std::vector contents destruction loop.
    * @param elType element type, i.e. XXX in `std::vector&lt;XXX&gt;`
    * @param arrVar variable name that holds pointer to std::vector
    */
  def destructVector(elType: String, arrVar: String): Unit = {
    outSrc.puts(s"for (std::vector<$elType>::iterator it = $arrVar->begin(); it != $arrVar->end(); ++it) {")
    outSrc.inc
    outSrc.puts("delete *it;")
    outSrc.dec
    outSrc.puts("}")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    outSrc.puts("if (m__is_le == 1) {")
    outSrc.inc
    leProc()
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    beProc()
    outSrc.dec
    outSrc.puts("}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, rep: RepeatSpec): String = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$kstreamName::$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName::process_zlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName::process_rotate_left($srcExpr, $expr)"
      case ProcessCustom(name, args) =>
        val procClass = name.map((x) => type2class(x)).mkString("::")
        val procName = s"_process_${idToStr(varSrc)}"

        importListSrc.addLocal(outFileNameHeader(name.last))

        val argList = args.map(expression).mkString(", ")
        var argListInParens = if (argList.nonEmpty) s"($argList)" else ""
        outSrc.puts(s"$procClass $procName$argListInParens;")
        s"$procName.decode($srcExpr)"
    }
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)
    val ioId = IoStorageIdentifier(id)

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(id, rep)
    }

    val newStreamRaw = s"new $kstreamName($args)"

    val ioName = rep match {
      case NoRepeat =>
        val newStream = (
          if (config.cppConfig.pointers != CppRuntimeConfig.RawPointers)
            s"${kaitaiType2NativeType(OwnedKaitaiStreamType)}($newStreamRaw)"
          else
            newStreamRaw
        )
        outSrc.puts(s"${privateMemberName(ioId)} = $newStream;")
        config.cppConfig.pointers match {
          case RawPointers =>
            privateMemberName(ioId)
          case UniqueAndRawPointers =>
            s"${privateMemberName(ioId)}.get()"
        }
      case _ =>
        val localIO = s"io_${idToStr(id)}"
        outSrc.puts(s"$kstreamName* $localIO = $newStreamRaw;")
        if (config.cppConfig.pointers == CppRuntimeConfig.UniqueAndRawPointers) {
          outSrc.puts(s"${privateMemberName(ioId)}->emplace_back($localIO);")
        } else {
          outSrc.puts(s"${privateMemberName(ioId)}->push_back($localIO);")
        }
        localIO
    }

    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName->at($memberName->size() - 1)"
    }
  }

  override def useIO(ioEx: Ast.expr): String = {
    outSrc.puts(s"$kstreamName *io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    outSrc.puts(s"std::streampos _pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    outSrc.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    outSrc.puts(s"$io->seek(_pos);")

  override def instanceClear(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${calculatedFlagForName(instName)} = false;")

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${calculatedFlagForName(instName)} = true;")

  override def condIfSetNull(id: Identifier, dataType: DataType): Unit = {
    // We'll use null flags only for primitive types, not pointers. Pointers in
    // C++ can already represent the "null" state on their own, so null flags
    // are redundant and only complicate things, see
    // https://github.com/kaitai-io/kaitai_struct/issues/1223#issuecomment-2727742506
    if (needsDestruction(dataType))
      return
    outSrc.puts(s"${nullFlagForName(id)} = true;")
  }

  override def condIfSetNonNull(id: Identifier, dataType: DataType): Unit = {
    if (needsDestruction(dataType))
      return
    outSrc.puts(s"${nullFlagForName(id)} = false;")
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    outSrc.puts(s"if (${expression(expr)}) {")
    outSrc.inc
  }

  override def condIfFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    importListSrc.addSystem("cstddef")
    outSrc.puts(s"for (std::size_t i = 0; i < ${privateMemberName(id)}->size(); ++i) {")
    outSrc.inc
  }

  override def condRepeatCommonFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    importListHdr.addSystem("vector")

    outSrc.puts(s"${privateMemberName(id)} = ${newVector(dataType)};")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts("int i = 0;")
    outSrc.puts(s"while (!$io->is_eof()) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push_back(${stdMoveWrap(expr)});")
  }

  override def condRepeatEosFooter: Unit = {
    outSrc.puts("i++;")
    outSrc.dec
    outSrc.puts("}")
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    val lenVar = s"l_${idToStr(id)}"
    outSrc.puts(s"const int $lenVar = ${expression(repeatExpr)};")
    outSrc.puts(s"for (int i = 0; i < $lenVar; i++) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatExprFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts("int i = 0;")
    outSrc.puts(s"${kaitaiType2NativeType(dataType.asNonOwning())} ${translator.doName("_")};")
    outSrc.puts("do {")
    outSrc.inc
  }

  private val ReStdUniquePtr = "^std::unique_ptr<(.*?)>\\((.*?)\\)$".r
  private val ReLocalIdentifier = "^[A-Za-z_][A-Za-z0-9_]*$".r

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("std::string ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }

    val (wrappedTempVar, rawPtrExpr) = if (config.cppConfig.pointers == UniqueAndRawPointers) {
      expr match {
        case ReStdUniquePtr(cppClass, innerExpr) =>
          (s"std::move(std::unique_ptr<$cppClass>($tempVar))", innerExpr)
        case ReLocalIdentifier() if !isRaw =>
          (s"std::move($expr)", s"$expr.get()")
        case _ =>
          (tempVar, expr)
      }
    } else {
      (tempVar, expr)
    }

    outSrc.puts(s"$typeDecl$tempVar = $rawPtrExpr;")

    outSrc.puts(s"${privateMemberName(id)}->push_back($wrappedTempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    outSrc.puts("i++;")
    outSrc.dec
    outSrc.puts(s"} while (!(${expression(untilExpr)}));")
    outSrc.dec
    outSrc.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    outSrc.puts(s"${kaitaiType2NativeType(dataType)} $id = $expr;")

  override def blockScopeHeader: Unit = {
    outSrc.puts("{")
    outSrc.inc
  }
  override def blockScopeFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall(defEndian)}()"
      case blt: BytesLimitType =>
        s"$io->read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io->read_bytes_term($term, $include, $consume, $eosError)"
        } else {
          s"$io->read_bytes_term_multi(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}(1)"
      case BitsType(width: Int, bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => nullPtr
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", m__is_le"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        config.cppConfig.pointers match {
          case RawPointers =>
            s"new ${types2class(t.name)}($addParams$io$addArgs)"
          case UniqueAndRawPointers =>
            // C++14
            //s"std::make_unique<${types2class(t.name)}>($addParams$io$addArgs)"
            s"std::unique_ptr<${types2class(t.name)}>(new ${types2class(t.name)}($addParams$io$addArgs))"
        }
    }
  }

  def newVector(elType: DataType): String = {
    val cppElType = kaitaiType2NativeType(elType)
    config.cppConfig.pointers match {
      case RawPointers =>
        s"new std::vector<$cppElType>()"
      case UniqueAndRawPointers =>
        s"std::unique_ptr<std::vector<$cppElType>>(new std::vector<$cppElType>())"
        // TODO: C++14 with std::make_unique
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName::bytes_terminate($expr1, $t, $include)"
        } else {
          s"$kstreamName::bytes_terminate_multi($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType.asCombined != dataType) {
      s"static_cast<${kaitaiType2NativeType(dataType.asNonOwning())}>(${nonOwningPointer(id, assignType)})"
    } else {
      id
    }
    outSrc.puts(s"$expr->_read();")
  }

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    outSrc.puts("try {")
    outSrc.inc
    tryBlock()
    outSrc.dec
    outSrc.puts("} catch(...) {")
    outSrc.inc
    finallyBlock()
    outSrc.puts("throw;")
    outSrc.dec
    outSrc.puts("}")
    finallyBlock()
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    outSrc.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"case ${expression(condition)}: {")
    outSrc.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"case ${expression(condition)}: {")
    outSrc.inc
  }

  override def switchCaseEnd(): Unit = {
    outSrc.puts("break;")
    outSrc.dec
    outSrc.puts("}")
  }

  override def switchElseStart(): Unit = {
    outSrc.puts("default: {")
    outSrc.inc
  }

  override def switchEnd(): Unit =
    outSrc.puts("}")

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts(s"${kaitaiType2NativeType(onType)} on = ${expression(on)};")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"if (on == ${expression(condition)}) {")
    outSrc.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"else if (on == ${expression(condition)}) {")
    outSrc.inc
  }

  override def switchIfCaseEnd(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    outSrc.puts("else {")
    outSrc.inc
  }

  override def switchIfEnd(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  //</editor-fold>

  override def switchBytesOnlyAsRaw = true

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"bool ${calculatedFlagForName(attrName)};")
    outHdr.puts(s"${kaitaiType2NativeType(attrType, true)} ${privateMemberName(attrName)};")
    declareNullFlag(attrName, attrType, isNullable)
  }

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"bool ${writeFlagForName(attrName)};")
    outHdr.puts(s"bool ${enabledFlagForName(attrName)};")
  }

  override def instanceWriteFlagInit(attrName: InstanceIdentifier): Unit = {
    outSrc.puts(s"${writeFlagForName(attrName)} = false;")
    val enabledByDefault = typeProvider.nowClass.instances.get(attrName) match {
      case Some(pi: ParseInstanceSpec) if isCurrentStreamLookaheadInstance(pi) => "false"
      case _ => "true"
    }
    outSrc.puts(s"${enabledFlagForName(attrName)} = $enabledByDefault;")
  }

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${writeFlagForName(instName)} = ${enabledFlagForName(instName)};")

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${writeFlagForName(instName)} = false;")

  override def instanceEnabledSetter(instName: InstanceIdentifier): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"void set_${idToStr(instName)}_enabled(bool _v) { m__dirty = true; ${enabledFlagForName(instName)} = _v; }")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(dataType.asNonOwning(), true)} ${publicMemberName(instName)}();")

    outSrc.puts
    outSrc.puts(s"${kaitaiType2NativeType(dataType.asNonOwning(), true)} ${types2class(className)}::${publicMemberName(instName)}() {")
    outSrc.inc
  }

  override def instanceFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    outSrc.puts(s"if (${calculatedFlagForName(instName)})")
    outSrc.inc
    instanceReturn(instName, dataType, false)
    outSrc.dec
  }

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    outSrc.puts(s"if (${writeFlagForName(instName)})")
    outSrc.inc
    outSrc.puts(s"_write_${idToStr(instName)}();")
    outSrc.dec
  }

  override def instanceReturnNullIfDisabled(instName: InstanceIdentifier): Unit = {
    outSrc.puts(s"if (!${enabledFlagForName(instName)})")
    outSrc.inc
    outSrc.puts(s"return ${disabledReturnExprForType(typeProvider.nowClass.instances(instName).dataTypeComposite)};")
    outSrc.dec
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"void _write_${idToStr(instName)}();")
    outSrc.puts
    outSrc.puts(s"void ${types2class(typeProvider.nowClass.name)}::_write_${idToStr(instName)}() {")
    outSrc.inc
    instanceClearWriteFlag(instName)
  }

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    outSrc.puts(s"if (${enabledFlagForName(instName)}) {")
    outSrc.inc
  }

  override def instanceHasValueIfHeader(instName: InstanceIdentifier): Unit = {
    outSrc.puts(s"if (${calculatedFlagForName(instName)}) {")
    outSrc.inc
  }

  override def instanceHasValueIfFooter(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit =
    outSrc.puts(s"return ${nonOwningPointer(privateMemberName(instName), attrType)};")

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    if (attrDebugNeeded(instName))
      attrDebugStart(instName, dataType, None, NoRepeat)
    val valExpr = expression(value)
    val isOwningInExpr = dataType match {
      case ct: ComplexDataType => ct.isOwningInExpr
      case _ => false
    }
    val valExprConverted = if (isOwningInExpr) {
      config.cppConfig.pointers match {
        case RawPointers =>
          valExpr
        case UniqueAndRawPointers =>
          s"$valExpr.get()"
      }
    } else {
      valExpr
    }
    handleAssignmentSimple(instName, valExprConverted)
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"void _invalidate_${idToStr(instName)}() { ${calculatedFlagForName(instName)} = false; }")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(BigInt, EnumValueSpec)]): Unit = {
    val enumClass = types2class(List(enumName))

    outHdr.puts
    outHdr.puts(s"enum $enumClass {")
    outHdr.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        outHdr.puts(s"${value2Const(enumName, label.name)} = ${translator.doIntLiteral(id)},")
      }
    }
    enumColl.last match {
      case (id, label) =>
        outHdr.puts(s"${value2Const(enumName, label.name)} = ${translator.doIntLiteral(id)}")
    }

    outHdr.dec
    outHdr.puts("};")

    outHdr.puts(s"static bool _is_defined_$enumClass($enumClass v);")
    importListHdr.addSystem("set")
    val inClassRef = types2class(curClass)
    val enumClassAbs = s"$inClassRef::$enumClass"
    val valuesSetAbsRef = s"$inClassRef::_values_$enumClass"
    ensureMode(PrivateAccess)
    // NOTE: declaration and definition must be separate in this case,
    // see https://stackoverflow.com/a/12856069
    outHdr.puts(s"static const std::set<$enumClass> _values_$enumClass;")
    if (config.cppConfig.useListInitializers) {
      outSrc.puts(s"const std::set<$enumClassAbs> $valuesSetAbsRef{")
      outSrc.inc
      enumColl.foreach { case (_, label) =>
        outSrc.puts(s"$inClassRef::${value2Const(enumName, label.name)},")
      }
      outSrc.dec
      outSrc.puts("};")
    } else {
      outHdr.puts(s"static std::set<$enumClass> _build_values_$enumClass();")

      outSrc.puts(s"std::set<$enumClassAbs> $inClassRef::_build_values_$enumClass() {")
      outSrc.inc
      outSrc.puts(s"std::set<$enumClassAbs> _t;")
      enumColl.foreach { case (_, label) =>
        outSrc.puts(s"_t.insert($inClassRef::${value2Const(enumName, label.name)});")
      }
      outSrc.puts("return _t;")
      outSrc.dec
      outSrc.puts("}")
      outSrc.puts(s"const std::set<$enumClassAbs> $valuesSetAbsRef = $inClassRef::_build_values_$enumClass();")
    }
    ensureMode(PublicAccess)
    outSrc.puts(s"bool $inClassRef::_is_defined_$enumClass($enumClassAbs v) {")
    outSrc.inc
    outSrc.puts(s"return $valuesSetAbsRef.find(v) != $valuesSetAbsRef.end();")
    outSrc.dec
    outSrc.puts("}")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    ensureMode(PublicAccess)
    // _to_string() method
    outHdr.puts(s"std::string _to_string() const;")
    outSrc.puts
    outSrc.puts(s"std::string ${types2class(typeProvider.nowClass.name)}::_to_string() const {")
    outSrc.inc
    outSrc.puts(s"return ${translator.translate(toStringExpr)};")
    outSrc.dec
    outSrc.puts("}")

    // operator<< that trivially calls ._to_string()
    outHdr.puts(s"friend std::ostream& operator<<(std::ostream& os, const ${types2class(typeProvider.nowClass.name)}& obj);")
    outSrc.puts
    outSrc.puts(s"std::ostream& operator<<(std::ostream& os, const ${types2class(typeProvider.nowClass.name)}& obj) {")
    outSrc.inc
    outSrc.puts("os << obj._to_string();")
    outSrc.puts("return os;")
    outSrc.dec
    outSrc.puts("}")
  }

  def value2Const(enumName: String, label: String) = Utils.upperUnderscoreCase(enumName + "_" + label)

  def defineName(className: String) = Utils.upperUnderscoreCase(className) + "_H_"

  /**
    * Returns name of a member that stores "calculated flag" for a given lazy
    * attribute. That is, if it's true, then calculation have already taken
    * place and we need to return already calculated member in a getter, or,
    * if it's false, we need to calculate / parse it first.
    * @param ksName attribute ID
    * @return calculated flag member name associated with it
    */
  def calculatedFlagForName(ksName: Identifier) =
    s"f_${idToStr(ksName)}"

  def writeFlagForName(ksName: Identifier) =
    s"w_${idToStr(ksName)}"

  def enabledFlagForName(ksName: Identifier) =
    s"e_${idToStr(ksName)}"

  /**
    * Returns name of a member that stores "null flag" for a given attribute,
    * that is, if it's true, then associated attribute is null.
    * @param ksName attribute ID
    * @return null flag member name associated with it
    */
  def nullFlagForName(ksName: Identifier) =
    s"n_${idToStr(ksName)}"

  override def idToStr(id: Identifier): String = CppCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def privateMemberName(id: Identifier): String = CppCompiler.privateMemberName(id)


  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"p_${idToStr(id)}"

  def declareNullFlag(attrName: Identifier, attrType: DataType, isNullable: Boolean) = {
    if (isNullable) {
      // See the comment in condIfSetNull() - we use null flags only for
      // primitive types. For pointers, we simply check whether it's a null
      // pointer.
      if (!needsDestruction(attrType)) {
        outHdr.puts(s"bool ${nullFlagForName(attrName)};")
        ensureMode(PublicAccess)
        outHdr.puts(s"bool _is_null_${idToStr(attrName)}() { ${publicMemberName(attrName)}(); return ${nullFlagForName(attrName)}; };")
        ensureMode(PrivateAccess)
      } else {
        ensureMode(PublicAccess)
        outHdr.puts(s"bool _is_null_${idToStr(attrName)}() { return !${publicMemberName(attrName)}(); };")
        ensureMode(PrivateAccess)
      }
    }
  }

  override def type2class(className: String): String = CppCompiler.type2class(className)

  def kaitaiType2NativeType(attrType: DataType, absolute: Boolean = false): String =
    CppCompiler.kaitaiType2NativeType(config.cppConfig, importListHdr, attrType, absolute)

  def nullPtr: String = config.cppConfig.pointers match {
    case RawPointers => "0"
    case UniqueAndRawPointers => "nullptr"
  }

  def nonOwningPointer(id: String, attrType: DataType): String = {
    config.cppConfig.pointers match {
      case RawPointers =>
        id
      case UniqueAndRawPointers =>
        attrType match {
          case st: SwitchType =>
            nonOwningPointer(id, combineSwitchType(st))
          case t: ComplexDataType =>
            if (t.isOwning) {
              s"$id.get()"
            } else {
              id
            }
          case _ =>
            id
        }
    }
  }

  def stdMoveWrap(expr: String): String = config.cppConfig.pointers match {
    case UniqueAndRawPointers => s"std::move($expr)"
    case _ => expr
  }

  def disabledReturnExprForType(attrType: DataType): String =
    if (needsDestruction(attrType)) {
      nullPtr
    } else attrType match {
      case _: StrType | _: BytesType => "std::string()"
      case _: BooleanType => "false"
      case _: EnumType => s"static_cast<${kaitaiType2NativeType(attrType.asNonOwning())}>(0)"
      case _ => "0"
    }

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "std::ifstream::failure"
    case UndecidedEndiannessError => "kaitai::undecided_endianness_error"
    case ConversionError => "std::invalid_argument"
    case validationErr: ValidationError =>
      val cppType = kaitaiType2NativeType(validationErr.dt, true)
      val cppErrName = validationErr match {
        case _: ValidationNotEqualError => "validation_not_equal_error"
        case _: ValidationLessThanError => "validation_less_than_error"
        case _: ValidationGreaterThanError => "validation_greater_than_error"
        case _: ValidationNotAnyOfError => "validation_not_any_of_error"
        case _: ValidationNotInEnumError => "validation_not_in_enum_error"
        case _: ValidationExprError => "validation_expr_error"
      }
      s"kaitai::$cppErrName<$cppType>"
  }

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr] = None
  ): Unit =
    attrValidate(attr, s"!(${translator.translate(checkExpr)})", err, useIo, actual, expected)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    useIo: Boolean
  ): Unit = {
    val enumSpec = et.enumSpec.get
    val inClassRef = types2class(enumSpec.name.dropRight(1))
    val enumNameStr = type2class(enumSpec.name.last)
    attrValidate(attr, s"!$inClassRef::_is_defined_$enumNameStr(${translator.translate(valueExpr)})", err, useIo, valueExpr, None)
  }

  private def attrValidate(
    attr: AttrLikeSpec,
    failCondExpr: String,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr]
  ): Unit = {
    val errArgsStr = expected.map(expression) ++ List(
      expression(actual),
      if (useIo) expression(Ast.expr.InternalName(IoIdentifier)) else nullPtr,
      expression(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    )
    importListSrc.addKaitai("kaitai/exceptions.h")
    outSrc.puts(s"if ($failCondExpr) {")
    outSrc.inc
    outSrc.puts(s"throw ${ksErrorName(err)}(${errArgsStr.mkString(", ")});")
    outSrc.dec
    outSrc.puts("}")
  }
}

object CppCompiler extends LanguageCompilerStatic
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CppCompiler(tp, config)

  def typeToFileName(topClassName: String): String = topClassName
  def outFileNameSource(className: String): String = typeToFileName(className) + ".cpp"
  def outFileNameHeader(className: String): String = typeToFileName(className) + ".h"

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case NamedIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
      case IoStorageIdentifier(inner) => s"_io_${idToStr(inner)}"
    }

  def privateMemberName(id: Identifier): String = s"m_${idToStr(id)}"

  override def kstructName = "kaitai::kstruct"
  override def kstreamName = "kaitai::kstream"

  def kaitaiType2NativeType(config: CppRuntimeConfig, importListHdr: CppImportList, attrType: DataType, absolute: Boolean = false): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "uint64_t"

      case _: BooleanType => "bool"
      case CalcIntType => "int32_t"
      case CalcFloatType => "double"

      case _: StrType => "std::string"
      case _: BytesType => "std::string"

      case t: UserType =>
        val typeStr = types2class(if (absolute) {
          t.classSpec.get.name
        } else {
          t.name
        })
        config.pointers match {
          case RawPointers => s"$typeStr*"
          case UniqueAndRawPointers =>
            if (t.isOwning) s"std::unique_ptr<$typeStr>" else s"$typeStr*"
        }

      case t: EnumType =>
        types2class(if (absolute) {
          t.enumSpec.get.name
        } else {
          t.owner :+ t.name
        })

      case at: ArrayType => {
        importListHdr.addSystem("vector")
        val vecType = s"std::vector<${kaitaiType2NativeType(config, importListHdr, at.elType, absolute)}>"
        (at, config.pointers) match {
          case (_: ArrayTypeInStream, UniqueAndRawPointers) =>
            s"std::unique_ptr<$vecType>"
          case _ =>
            s"$vecType*"
        }
      }
      case OwnedKaitaiStreamType => config.pointers match {
        case RawPointers => s"$kstreamName*"
        case UniqueAndRawPointers => s"std::unique_ptr<$kstreamName>"
      }
      case KaitaiStreamType => s"$kstreamName*"
      case KaitaiStructType => config.pointers match {
        case RawPointers => s"$kstructName*"
        case UniqueAndRawPointers => s"std::unique_ptr<$kstructName>"
      }
      case CalcKaitaiStructType(_) => s"$kstructName*"

      case st: SwitchType =>
        kaitaiType2NativeType(config, importListHdr, combineSwitchType(st), absolute)
    }
  }

  /**
    * C++ does not have a concept of AnyType, and common use case "lots of
    * incompatible UserTypes for cases + 1 BytesType for else" combined would
    * result in exactly AnyType - so we try extra hard to avoid that here with
    * this pre-filtering. In C++, "else" case with raw byte array would
    * be available through _raw_* attribute anyway.
    *
    * @param st switch type to combine into one overall type
    * @return
    */
  def combineSwitchType(st: SwitchType): DataType = {
    val ct1 = TypeDetector.combineTypes(
      st.cases.filterNot {
        case (caseExpr, _: BytesType) => caseExpr == SwitchType.ELSE_CONST
        case _ => false
      }.values
    )
    if (st.isOwning) {
      ct1
    } else {
      ct1.asNonOwning()
    }
  }

  def types2class(typeName: Ast.typeId) = {
    typeName.names.map(type2class).mkString(
      if (typeName.absolute) "::" else "",
      "::",
      ""
    )
  }

  def types2class(components: List[String]) =
    components.map(type2class).mkString("::")

  def type2class(name: String) = Utils.lowerUnderscoreCase(name) + "_t"
}
