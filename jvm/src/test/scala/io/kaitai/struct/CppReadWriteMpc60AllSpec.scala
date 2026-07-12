package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc60AllSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC60 ALL-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc60all_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc60all_write.h")
    val source = files("mpc60all_write.cpp")

    header should include ("class mpc60all_write_t")
    header should include ("void set_sequences(std::unique_ptr<std::vector<std::unique_ptr<sequence_t>>> _v)")
    header should include ("void set_sequences_terminator(std::string _v)")
    header should include ("void set_songs(std::unique_ptr<std::vector<std::unique_ptr<song_t>>> _v)")
    header should include ("void set_track_headers(std::unique_ptr<std::vector<std::unique_ptr<track_header_t>>> _v)")
    header should include ("void set_steps(std::unique_ptr<std::vector<std::unique_ptr<song_step_t>>> _v)")
    source should include ("void mpc60all_write_t::_write()")
    source should include ("(*it).get()->_write();")
    source should include ("void mpc60all_write_t::event_t::_write()")
    source should include ("void mpc60all_write_t::song_t::_write()")
  }
}
