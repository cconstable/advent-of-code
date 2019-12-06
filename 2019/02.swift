#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

let input = IO.get()
  .components(separatedBy: ",")
  .compactMap(trim >>> Int.init)

/// -------------------------------------------------------------------------
///  Part 1
///
///  This is essentially a turing machine with a mutable tape. We are making 
///  some big assumptions, namely, that the input program is always well-formed
///  and free from errors.
///
///  This isn't a particularly interesting solution but I think it's much more
///  readability than more clever alternatives.
/// -------------------------------------------------------------------------

func getInstruction(_ pointer: Int, forTape tape: [Int]) -> [Int] {
  let startIndex = pointer * 4
  let endIndex = startIndex + 3
  return Array(tape[startIndex...endIndex])
}

func runProgram(_ tape: [Int]) -> Int {
  var mutableTape = tape
  var ip = 0
  while true {
    let instruction = getInstruction(ip, forTape: mutableTape)
    switch instruction[0] {
    case 1:
      mutableTape[instruction[3]] = mutableTape[instruction[1]] + mutableTape[instruction[2]]
    case 2:
      mutableTape[instruction[3]] = mutableTape[instruction[1]] * mutableTape[instruction[2]]
    case 99:
      return mutableTape[0]
    default:
      return -1  // error
    }
    ip += 1
  }

  return -1  // unreachable
}

func updateFirstInstruction(_ a: Int, _ b: Int) -> ([Int]) -> [Int] {
  return { tape in
    var mutableTape = tape
    mutableTape[1] = a
    mutableTape[2] = b
    return mutableTape
  }
}

IO.printLn <| (input |> updateFirstInstruction(12, 2) |> runProgram)

/// -------------------------------------------------------------------------
///  Part 2
///
///  We need to find which values of the first two instructions give us a result
///  of 19690720. We can create a lazy cartesian product sequence and pass those
///  in to our existing algorithm. This is `product` in the AOCUtils lib.
///
///  This is the gist:
///  `list1.lazy.flatMap { x in list2.lazy.map { y in (x, y) } }`
/// -------------------------------------------------------------------------

IO.printLn
  <| product(0...99, 0...99).first {
    (input |> updateFirstInstruction($0.0, $0.1) |> runProgram) == 19_690_720
  }
  