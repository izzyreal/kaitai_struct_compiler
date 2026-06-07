package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteSwitchUserSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits switch user-type write dispatch") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/switch_user_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("switch_user_write.cpp")

    source should include ("switch (kind()) {")
    source should include ("dynamic_cast<body_one_t*>(m_body.get())")
    source should include ("dynamic_cast<body_two_t*>(m_body.get())")
    source should include ("switch object type mismatch")
    source should include ("_switch_obj->_write();")
    source should include ("_switch_obj->_check();")
  }
}
