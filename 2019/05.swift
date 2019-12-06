#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

let rawInput = """
  3,225,1,225,6,6,1100,1,238,225,104,0,1001,191,50,224,101,-64,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,2,150,218,224,1001,224,-1537,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1002,154,5,224,101,-35,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1102,76,17,225,1102,21,44,224,1001,224,-924,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,101,37,161,224,101,-70,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,102,46,157,224,1001,224,-1978,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,5,29,225,1101,10,7,225,1101,43,38,225,1102,33,46,225,1,80,188,224,1001,224,-73,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1101,52,56,225,1101,14,22,225,1101,66,49,224,1001,224,-115,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1101,25,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,1002,223,2,223,1005,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,344,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,434,101,1,223,223,1008,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226
  """

let input =
  rawInput
    .components(separatedBy: ",")
    .compactMap(trim >>> Int.init)

let maxParemeterLength = 3
enum Instruction {
  case start
  case add(Int, Int, Int)
  case mult(Int, Int, Int)
  case input(Int)
  case output(Int)
  case jumpIfTrue(Int, Int)
  case jumpIfFalse(Int, Int)
  case lessThan(Int, Int, Int)
  case equals(Int, Int, Int)
  case halt

  func length() -> Int {
    switch self {
    case .start:
      return 0
    case .add(_, _, _):
      return 4
    case .mult(_, _, _):
      return 4
    case .input(_):
      return 2
    case .output(_):
      return 2
    case .jumpIfTrue(_, _):
      return 3
    case .jumpIfFalse(_, _):
      return 3
    case .lessThan(_, _, _):
      return 4
    case .equals(_, _, _):
      return 4
    case .halt:
      return 1
    }
  }
}

enum ParameterMode: Int {
  case position = 0
  case immediate = 1
}

func getParamModes(_ input: Int) -> [ParameterMode] {
  let values = input / 100
  let digits = String(values).compactMap { ParameterMode(rawValue: $0.wholeNumberValue!) }
  return padLeft(.position, maxParemeterLength - digits.count, digits).reversed()
}

func getInstruction(_ ip: Int, _ curInstruction: Instruction, forTape tape: [Int]) -> (
  Int, Instruction
) {
  var i = ip
  switch curInstruction {

  case .jumpIfTrue(_, _), .jumpIfFalse(_, _):
    break
  default:
    i += curInstruction.length()
  }
  let opCode = tape[i] % 100
  let paramModes = getParamModes(tape[i])

  switch opCode {
  case 1:
    let a = paramModes[0] == .position ? tape[tape[i + 1]] : tape[i + 1]
    let b = paramModes[1] == .position ? tape[tape[i + 2]] : tape[i + 2]
    return (i, .add(a, b, tape[i + 3]))
  case 2:
    let a = paramModes[0] == .position ? tape[tape[i + 1]] : tape[i + 1]
    let b = paramModes[1] == .position ? tape[tape[i + 2]] : tape[i + 2]
    return (i, .mult(a, b, tape[i + 3]))
  case 3:
    return (i, .input(tape[i + 1]))
  case 4:
    return (i, .output(tape[i + 1]))
  case 5:
    let a = paramModes[0] == .position ? tape[tape[i + 1]] : tape[i + 1]
    let b = paramModes[1] == .position ? tape[tape[i + 2]] : tape[i + 2]
    return (i, .jumpIfTrue(a, b))
  case 6:
    let a = paramModes[0] == .position ? tape[tape[i + 1]] : tape[i + 1]
    let b = paramModes[1] == .position ? tape[tape[i + 2]] : tape[i + 2]
    return (i, .jumpIfFalse(a, b))
  case 7:
    let a = paramModes[0] == .position ? tape[tape[i + 1]] : tape[i + 1]
    let b = paramModes[1] == .position ? tape[tape[i + 2]] : tape[i + 2]
    return (i, .lessThan(a, b, tape[i + 3]))
  case 8:
    let a = paramModes[0] == .position ? tape[tape[i + 1]] : tape[i + 1]
    let b = paramModes[1] == .position ? tape[tape[i + 2]] : tape[i + 2]
    return (i, .equals(a, b, tape[i + 3]))
  case 99:
    return (i, .halt)
  default:
    fatalError("bad opcode: \(tape[i])")
  }
}

func runProgram(_ input: Int, _ tape: [Int]) -> Int {
  var output: Int = 0
  var instruction = Instruction.start
  var mutableTape = tape
  var ip = 0
  while true {
    (ip, instruction) = getInstruction(ip, instruction, forTape: mutableTape)
    switch instruction {
    case let .add(a, b, c):
      mutableTape[c] = a + b
    case let .mult(a, b, c):
      mutableTape[c] = a * b
    case let .input(a):
      mutableTape[a] = input
    case let .output(a):
      output = mutableTape[a]
    case let .jumpIfTrue(a, b):
      if (a != 0) {
        ip = b
      } else {
        ip += instruction.length()
      }
    case let .jumpIfFalse(a, b):
      if (a == 0) {
        ip = b
      } else {
        ip += instruction.length()
      }
    case let .lessThan(a, b, c):
      if (a < b) {
        mutableTape[c] = 1
      } else {
        mutableTape[c] = 0
      }
    case let .equals(a, b, c):
      if (a == b) {
        mutableTape[c] = 1
      } else {
        mutableTape[c] = 0
      }
    case .halt:
      return output
    case .start:
      fatalError()
    }
  }
}

func updateFirstInstruction(_ a: Int, _ b: Int) -> ([Int]) -> [Int] {
  return { tape in
    var mutableTape = tape
    mutableTape[1] = a
    mutableTape[2] = b
    return mutableTape
  }
}

IO.printLn <| runProgram(5, input)
