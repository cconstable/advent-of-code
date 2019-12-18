#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

var input = IO.get()
  .components(separatedBy: ",")
  .compactMap(trim >>> Int.init)

///-------------------------------------------------------------------------
/// Part 1

do {
  var machine = Machine(input, [1])
  let output = machine.runProgram()
  print(output)
}

///-------------------------------------------------------------------------
/// Part 2

do {
  var machine = Machine(input, [2])
  let output = machine.runProgram()
  print(output)
}