package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppArrayParamUserTypeOwnershipSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("array params of user types keep owned element storage in C++") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/params_pass_array_usertype.ksy", config)
    assertNoNonStyleProblems(problems)
    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap

    files("params_pass_array_usertype.h") should include ("param_type_t(std::vector<std::unique_ptr<block_t>>* p_bar")
    files("params_pass_array_usertype.h") should include ("std::vector<std::unique_ptr<block_t>>* bar() const")
    files("params_pass_array_usertype.cpp") should include ("new param_type_t(blocks(), m__io, this, m__root)")
  }

  test("borrowed array params accept temporary literal arrays in C++11") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/params_pass_array_io.ksy", config)
    assertNoNonStyleProblems(problems)
    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap

    files("params_pass_array_io.cpp") should include (".release(), m__io, this, m__root")
    files("params_pass_array_io.h") should include ("param_type_t(std::vector<kaitai::kstream*>* p_arg_streams")
  }
}
