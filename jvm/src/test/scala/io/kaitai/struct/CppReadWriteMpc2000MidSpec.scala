package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc2000MidSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC2000 MID-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc2000mid_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc2000mid_write.h")
    val source = files("mpc2000mid_write.cpp")

    header should include ("#include \"standard_midi_file_with_running_status_write.h\"")
    header should include ("void set_tracks(std::unique_ptr<std::vector<std::unique_ptr<standard_midi_file_with_running_status_write_t::track_t>>> _v)")
    header should include ("standard_midi_file_with_running_status_write_t::meta_event_body_t* first_meta();")
    header should include ("std::vector<std::unique_ptr<standard_midi_file_with_running_status_write_t::track_event_t>>* meta_events();")
    source should include ("(*it).get()->_write();")
    source should include ("void mpc2000mid_write_t::_write_sequencer()")
  }
}
