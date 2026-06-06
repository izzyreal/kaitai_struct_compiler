package io.kaitai.struct

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.formats.{JavaKSYParser}
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.format.KSVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CppReadWriteHelloWorldSpec extends AnyFunSuite with Matchers {
  KSVersion.current = Version.version

  test("cpp_stl_11 emits hello_world_write read-write API") {
    val config = CLIConfig(
      runtime = RuntimeConfig(
        autoRead = false,
        readWrite = true,
        zeroCopySubstream = false,
        cppConfig = CppRuntimeConfig().copyAsCpp11()
      )
    )

    val (specsOpt, problems) = JavaKSYParser.localFileToSpecs("../tests/formats_rw/hello_world_write.ksy", config)
    problems shouldBe empty

    val compiled = Main.compile(specsOpt.get, specsOpt.get.firstSpec, CppCompiler, config.runtime)
    val files = compiled.files.map(file => file.fileName -> file.contents).toMap
    val header = files("hello_world_write.h")
    val source = files("hello_world_write.cpp")

    header should include ("void set_one(")
    header should include ("void _check();")
    header should include ("void _write();")
    source should include ("m__io->write_u1(m_one);")
  }
}
