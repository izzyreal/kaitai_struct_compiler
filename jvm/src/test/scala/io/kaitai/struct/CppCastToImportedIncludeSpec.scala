package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppCastToImportedIncludeSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(runtime = RuntimeConfig(autoRead = false))

  test("cpp_stl includes imported user type headers for cast-only usage") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/cast_to_imported.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap

    files("cast_to_imported.cpp") should include ("#include \"hello_world.h\"")
    files("cast_to_imported.cpp") should include ("static_cast<hello_world_t*")
  }

  test("cpp_stl includes imported user type headers for instance return types") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/cast_to_imported2.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap

    files("cast_to_imported2.h") should include ("#include \"hello_world.h\"")
    files("cast_to_imported2.h") should include ("hello_world_t* hw();")
  }
}
