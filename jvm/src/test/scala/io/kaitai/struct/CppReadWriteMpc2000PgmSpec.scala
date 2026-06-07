package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc2000PgmSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC2000 PGM-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc2000pgm_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc2000pgm_write.h")
    val source = files("mpc2000pgm_write.cpp")

    header should include ("void set_sound_names(std::unique_ptr<std::vector<std::string>> _v)")
    header should include ("void set_note_parameters(std::unique_ptr<std::vector<std::unique_ptr<note_t>>> _v)")
    header should include ("void set_pad_mixers(std::unique_ptr<std::vector<std::unique_ptr<pad_mixer_t>>> _v)")
    header should include ("void set_pad_to_note_mapping(std::unique_ptr<std::vector<int8_t>> _v)")
    source should include ("m__io->write_u2le(m_sound_count);")
    source should include ("m__io->write_bytes((*it));")
    source should include ("(*it).get()->_write();")
  }
}
