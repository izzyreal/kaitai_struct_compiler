package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteBitsSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits bitfield write calls") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/bits_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("bits_write.cpp")

    source should include ("m__io->write_bits_int_be(1, ((m_bits_a) ? 1 : 0));")
    source should include ("m__io->write_bits_int_be(3, m_bits_b);")
    source should include ("m__io->write_u1(m_after);")
  }

  test("cpp_stl_11 emits little-endian bitfield write calls") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/bits_write_le.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("bits_write_le.cpp")

    source should include ("m__io->write_bits_int_le(1, ((m_bits_a) ? 1 : 0));")
    source should include ("m__io->write_bits_int_le(11, m_large_bits_2);")
    source should include ("m__io->write_u1(m_after);")
  }
}
