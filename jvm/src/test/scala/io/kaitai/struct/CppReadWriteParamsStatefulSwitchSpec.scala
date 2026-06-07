package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteParamsStatefulSwitchSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits stateful parameterized switch dispatch") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/params_stateful_switch_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("params_stateful_switch_write.h")
    val source = files("params_stateful_switch_write.cpp")

    header should include ("event_t(uint8_t p_prev_kind, kaitai::kstream* p__io")
    source should include ("switch (actual_kind()) {")
    source should include ("dynamic_cast<body_one_t*>(m_body.get())")
    source should include ("dynamic_cast<body_two_t*>(m_body.get())")
    source should include ("events()->at(i - 1)->next_kind()")
  }
}
