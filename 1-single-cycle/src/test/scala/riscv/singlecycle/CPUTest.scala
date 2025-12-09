// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.singlecycle

import java.nio.ByteBuffer
import java.nio.ByteOrder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import peripheral.InstructionROM
import peripheral.Memory
import peripheral.ROMLoader
import riscv.core.CPU
import riscv.core.ProgramCounter
import riscv.Parameters
import riscv.TestAnnotations

class TestTopModule(exeFilename: String) extends Module {
  val io = IO(new Bundle {
    val mem_debug_read_address  = Input(UInt(Parameters.AddrWidth))
    val regs_debug_read_address = Input(UInt(Parameters.PhysicalRegisterAddrWidth))
    val regs_debug_read_data    = Output(UInt(Parameters.DataWidth))
    val mem_debug_read_data     = Output(UInt(Parameters.DataWidth))
  })

  val mem             = Module(new Memory(8192))
  val instruction_rom = Module(new InstructionROM(exeFilename))
  val rom_loader      = Module(new ROMLoader(instruction_rom.capacity))

  rom_loader.io.rom_data     := instruction_rom.io.data
  rom_loader.io.load_address := Parameters.EntryAddress
  instruction_rom.io.address := rom_loader.io.rom_address

  val CPU_clkdiv = RegInit(UInt(2.W), 0.U)
  val CPU_tick   = Wire(Bool())
  val CPU_next   = Wire(UInt(2.W))
  CPU_next   := Mux(CPU_clkdiv === 3.U, 0.U, CPU_clkdiv + 1.U)
  CPU_tick   := CPU_clkdiv === 0.U
  CPU_clkdiv := CPU_next

  withClock(CPU_tick.asClock) {
    val cpu = Module(new CPU)
    cpu.io.debug_read_address  := 0.U
    cpu.io.instruction_valid   := rom_loader.io.load_finished
    mem.io.instruction_address := cpu.io.instruction_address
    cpu.io.instruction         := mem.io.instruction

    when(!rom_loader.io.load_finished) {
      rom_loader.io.bundle <> mem.io.bundle
      cpu.io.memory_bundle.read_data := 0.U
    }.otherwise {
      rom_loader.io.bundle.read_data := 0.U
      cpu.io.memory_bundle <> mem.io.bundle
    }

    cpu.io.debug_read_address := io.regs_debug_read_address
    io.regs_debug_read_data   := cpu.io.debug_read_data
  }

  mem.io.debug_read_address := io.mem_debug_read_address
  io.mem_debug_read_data    := mem.io.debug_read_data
}

class RSqrtTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly calculate reciprocal square root" in {
    test(new TestTopModule("rsqrt-c.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 200) {
          c.clock.step(1000)
          c.io.mem_debug_read_address.poke((i * 4).U)
        }

        val rsqrtVectors: Seq[(UInt, UInt)] = Seq(
          1.U   -> 65536.U,
          4.U   -> 32768.U,
          16.U  -> 16384.U,
          20.U  -> 14654.U,
          30.U  -> 11965.U,
          100.U -> 6553.U,
          120.U -> 5982.U,
          130.U -> 5747.U,
          0.U   -> 0xffffffffL.U,   // 4294967295 (for x=0)
          0xffffffffL.U -> 1.U      // x=4294967295, y=1
        )

        var i = 4
        // var j = 8
        // var k = 16

        println("========== rsqrt() test start =========")

        for ((input, expected) <- rsqrtVectors) {
          println(f"Accessing memory address $i")
          c.io.mem_debug_read_address.poke((i).U)
          c.clock.step()
          c.io.mem_debug_read_data.expect(expected, s"rsqrt($input) should be $expected")
          val result = c.io.mem_debug_read_data.peek().litValue
          println(f"rsqrt(${input.litValue}%d) = ${result}%d")

          // println(f"Accessing memory address $j")

          // c.io.mem_debug_read_address.poke((j).U)
          // c.clock.step()
          // val elapse_cycle = c.io.mem_debug_read_data.peek().litValue
          // println(f"Elapsed cycles: $elapse_cycle%d")

          // println(f"Accessing memory address $k")

          // c.io.mem_debug_read_address.poke((k).U)
          // c.clock.step()
          // val elapse_instruction = c.io.mem_debug_read_data.peek().litValue
          // println(f"Elapsed instructions: $elapse_instruction%d")

          i += 4
          // j += 20
          // k += 20
        }
  }
}
}

class FibonacciTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly execute recursive Fibonacci(10) program" in {
    test(new TestTopModule("fibonacci.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 50) {
        c.clock.step(1000)
        c.io.mem_debug_read_address.poke((i * 4).U) // Avoid timeout
      }

      c.io.mem_debug_read_address.poke(4.U)
      c.clock.step()
      c.io.mem_debug_read_data.expect(55.U)
    }
  }
}

class QuicksortTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly execute Quicksort algorithm on 10 numbers" in {
    test(new TestTopModule("quicksort.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 50) {
        c.clock.step(1000)
        c.io.mem_debug_read_address.poke((i * 4).U) // Avoid timeout
      }
      for (i <- 1 to 10) {
        c.io.mem_debug_read_address.poke((4 * i).U)
        c.clock.step()
        c.io.mem_debug_read_data.expect((i - 1).U)
      }
    }
  }
}

class ByteAccessTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly handle byte-level store/load operations (SB/LB)" in {
    test(new TestTopModule("sb.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 500) {
        c.clock.step()
        c.io.mem_debug_read_address.poke((i * 4).U) // Avoid timeout
      }
      c.io.regs_debug_read_address.poke(5.U)
      c.io.regs_debug_read_data.expect(0xdeadbeefL.U)
      c.io.regs_debug_read_address.poke(6.U)
      c.io.regs_debug_read_data.expect(0xef.U)
      c.io.regs_debug_read_address.poke(1.U)
      c.io.regs_debug_read_data.expect(0x15ef.U)
    }
  }
}


