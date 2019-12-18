#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

var input = IO.get()
  .components(separatedBy: ",")
  .compactMap(trim >>> Int.init)

//-------------------------------------------------------------------------
// Part 1

do {
  let program: Program = input
  let machine = Machine(program, [])

  var o = [Int]()
  while !machine.didHalt {
    o.append(machine.runProgram())
  }

  let numOfBricks = chunks(o, 3)
    .filter { $0.count > 1 }
    .filter { $0[2] == 2 }
    .count

  print(numOfBricks)
}

//-------------------------------------------------------------------------
// Part 2

do {
  var program: Program = input
  program[0] = 2

  let machine = Machine(program, [])
  var score = 0

  var paddleLoc = Point(x: 0, y: 0)
  var ballLoc = Point(x: 0, y: 0)

  while !machine.didHalt {
    let x = machine.runProgram()
    let y = machine.runProgram()
    let z = machine.runProgram()

    if x == -1 {
      score = z
    } else if x == -2 {
      if paddleLoc.x < ballLoc.x {
        machine.addInput(1)
      } else if paddleLoc.x > ballLoc.x {
        machine.addInput(-1)
      } else {
        machine.addInput(0)
      }
    } else if z == 3 {
      paddleLoc = Point(x: x, y: y)
    } else if z == 4 {
      ballLoc = Point(x: x, y: y)
    }
  }

  print(score)
}