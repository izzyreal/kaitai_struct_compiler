package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc2000xlAllSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a reduced real-layout MPC2000XL ALL write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc2000xlall_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc2000xlall_write.h")
    val source = files("mpc2000xlall_write.cpp")

    header should include ("class mpc2000xlall_write_t")
    header should include ("void set_sequences(std::unique_ptr<std::vector<std::unique_ptr<sequence_t>>> _v)")
    header should include ("void set_events(std::unique_ptr<std::vector<std::unique_ptr<event_t>>> _v)")
    header should include ("void set_note_event(std::unique_ptr<note_event_t> _v)")
    source should include ("void mpc2000xlall_write_t::_write()")
    source should include ("void mpc2000xlall_write_t::sequence_body_t::_write()")
    source should include ("void mpc2000xlall_write_t::event_t::_write()")
  }
}
