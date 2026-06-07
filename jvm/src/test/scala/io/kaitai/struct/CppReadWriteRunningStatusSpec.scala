package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteRunningStatusSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits running-status-style dispatch from nullable header presence") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/running_status_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("running_status_write.h")
    val source = files("running_status_write.cpp")

    header should include ("bool _is_null_event_header()")
    source should include ("m_using_prev = _is_null_event_header();")
    source should include ("switch (actual_kind()) {")
    source should include ("dynamic_cast<body_one_t*>(m_body.get())")
  }
}
