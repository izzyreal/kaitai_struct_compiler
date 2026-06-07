package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteRepeatExprUserSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  test("cpp_stl_11 emits repeat_expr_user_write read-write API") {
    val config = CLIConfig(
      runtime = RuntimeConfig(
        autoRead = false,
        readWrite = true,
        zeroCopySubstream = false,
        cppConfig = CppRuntimeConfig().copyAsCpp11()
      )
    )

    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/repeat_expr_user_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("repeat_expr_user_write.h")
    val source = files("repeat_expr_user_write.cpp")

    header should include ("void set_entries(")
    source should include ("for (std::vector<std::unique_ptr<entry_t>>::const_iterator it = m_entries->begin(); it != m_entries->end(); ++it)")
    source should include ("(*it).get()->_check();")
    source should include ("(*it).get()->_set_io(m__io);")
    source should include ("(*it).get()->_write();")
  }
}
