package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteRepeatExprBytesSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  test("cpp_stl_11 emits repeat_expr_bytes_write read-write API") {
    val config = CLIConfig(
      runtime = RuntimeConfig(
        autoRead = false,
        readWrite = true,
        zeroCopySubstream = false,
        cppConfig = CppRuntimeConfig().copyAsCpp11()
      )
    )

    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/repeat_expr_bytes_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("repeat_expr_bytes_write.cpp")

    source should include ("for (std::vector<std::string>::const_iterator it = m_names->begin(); it != m_names->end(); ++it)")
    source should include ("for (std::vector<std::string>::const_iterator it = m_payloads->begin(); it != m_payloads->end(); ++it)")
    source should include ("(*it).size() != static_cast<std::string::size_type>(4)")
    source should include ("(*it).size() != static_cast<std::string::size_type>(3)")
  }
}
