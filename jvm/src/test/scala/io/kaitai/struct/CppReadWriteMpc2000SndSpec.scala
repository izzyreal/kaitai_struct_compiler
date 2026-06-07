package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteMpc2000SndSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits a real MPC2000 SND-like write path") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/mpc2000snd_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("mpc2000snd_write.h")
    val source = files("mpc2000snd_write.cpp")

    header should include ("void set_name(std::string _v)")
    header should include ("void set_frames(std::unique_ptr<std::vector<int16_t>> _v)")
    source should include ("m__io->write_bits_int_be(1, ((m_stereo) ? 1 : 0));")
    source should include ("m__io->write_u4le(m_frame_count);")
    source should include ("m__io->write_s2le((*it));")
  }
}
