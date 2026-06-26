package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteIoLookaheadSwitchSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits cached lookahead-driven switch dispatch") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/io_lookahead_switch_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("io_lookahead_switch_write.h")
    val source = files("io_lookahead_switch_write.cpp")

    header should include ("void set_status_byte_lookahead(uint8_t _v) { m__dirty = true; f_actual_kind = false; f_using_prev = false; e_status_byte_lookahead = false; f_status_byte_lookahead = true;")
    source should include ("e_status_byte_lookahead = m__io != nullptr;")
    source should include ("w_status_byte_lookahead = false;")
    source should include ("if (!(using_prev())) {")
    source should include ("switch (actual_kind()) {")
    source should include ("dynamic_cast<body_one_t*>(m_body.get())")
    source should include ("status_byte_lookahead() < 128")
  }
}
