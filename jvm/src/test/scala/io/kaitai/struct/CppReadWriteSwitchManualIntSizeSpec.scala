package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteSwitchManualIntSizeSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits fetch-instances switch dispatch through unique_ptr storage") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats/switch_manual_int_size.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("switch_manual_int_size.cpp")

    source should include ("dynamic_cast<chunk_meta_t*>(m_body.get())")
    source should include ("dynamic_cast<chunk_dir_t*>(m_body.get())")
    source should include ("switch object type mismatch in _fetch_instances")
  }
}
