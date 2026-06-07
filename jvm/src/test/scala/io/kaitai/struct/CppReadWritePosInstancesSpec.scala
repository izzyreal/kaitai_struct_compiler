package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWritePosInstancesSpec extends AnyFunSuite with Matchers with CppReadWriteSpecSupport {
  KSVersion.current = Version.version

  private val config = CLIConfig(
    runtime = RuntimeConfig(
      autoRead = false,
      readWrite = true,
      zeroCopySubstream = false,
      cppConfig = CppRuntimeConfig().copyAsCpp11()
    )
  )

  test("cpp_stl_11 emits pos instance writeback") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/pos_instance_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("pos_instance_write.h")
    val source = files("pos_instance_write.cpp")

    header should include ("void _fetch_instances();")
    header should include ("void _write_tail();")
    source should include ("w_tail = e_tail;")
    source should include ("std::streampos _pos = m__io->pos();")
    source should include ("m__io->seek(1);")
    source should include ("m__io->write_u1(m_tail);")
  }

  test("cpp_stl_11 emits explicit io + pos instance writeback") {
    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/io_pos_instance_write.ksy", config)
    assertNoNonStyleProblems(problems)

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val source = files("io_pos_instance_write.cpp")

    source should include ("kaitai::kstream *io = _root()->_io();")
    source should include ("std::streampos _pos = io->pos();")
    source should include ("io->seek(1);")
    source should include ("io->write_u1(m_tail);")
  }
}
