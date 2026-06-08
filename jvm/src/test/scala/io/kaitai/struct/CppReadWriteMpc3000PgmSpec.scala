package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc3000PgmSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC3000 PGM-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc3000pgm_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc3000pgm_write.h")
    val source = files("mpc3000pgm_write.cpp")

    header should include ("class mpc3000pgm_write_t")
    header should include ("void set_sound_assignments(std::unique_ptr<std::vector<std::unique_ptr<sound_assignment_t>>> _v)")
    header should include ("void set_mixer_screens(std::unique_ptr<std::vector<std::unique_ptr<mixer_screen_t>>> _v)")
    source should include ("void mpc3000pgm_write_t::_write()")
    source should include ("m__io->write_u4le((*it));")
  }
}
