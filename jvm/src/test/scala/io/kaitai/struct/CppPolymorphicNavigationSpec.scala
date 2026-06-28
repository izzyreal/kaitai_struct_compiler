package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class CppPolymorphicNavigationSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("ternary over different user types falls back to kaitai::kstruct in C++") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/expr_io_ternary.ksy", config)
    assertNoNonStyleProblems(problems)
    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap

    files("expr_io_ternary.cpp") should include ("static_cast<kaitai::kstruct*>(obj1())")
    files("expr_io_ternary.cpp") should include ("static_cast<kaitai::kstruct*>(obj2())")
    files("expr_io_ternary.cpp") should include ("m_one_or_two_obj = ((flag() == 64) ? (static_cast<kaitai::kstruct*>(obj1())) : (static_cast<kaitai::kstruct*>(obj2())));")
  }

  test("parent navigation through polymorphic parents uses virtual base dispatch in runtime") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/nav_parent_switch_cast.ksy", config)
    assertNoNonStyleProblems(problems)
    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap

    files("nav_parent_switch_cast.cpp") should include ("m_flag = static_cast<nav_parent_switch_cast_t::foo_t*>(_parent()->_parent())->flag();")

    val source = Source.fromFile("../runtime/cpp_stl/kaitai/kaitaistruct.h")
    val runtimeHeader =
      try source.mkString
      finally source.close()
    runtimeHeader should include ("virtual kstruct *_parent() const { return nullptr; }")
  }
}
