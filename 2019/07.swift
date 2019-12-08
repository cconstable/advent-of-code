#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

///-------------------------------------------------------------------------
/// This problem involved piping input/output between different instances
/// of the Intcode computer. I wrote an implementation of Heap's algorithm
/// (in AOCUtils) for getting permutations that is used later.
/// ------------------------------------------------------------------------

let input =
  (head <| IO.getLines())
    .components(separatedBy: ",")
    .compactMap(trim >>> Int.init)

typealias Program = [Int]
let program: Program = input

enum Instruction: Int {
  case add = 1
  case mult = 2
  case input = 3
  case output = 4
  case jumpIfTrue = 5
  case jumpIfFalse = 6
  case lessThan = 7
  case equals = 8
  case halt = 99
  case start
  case unknown

  func length() -> Int {
    switch self {
    case .unknown:
      return 0
    case .start:
      return 0
    case .add:
      return 4
    case .mult:
      return 4
    case .input:
      return 2
    case .output:
      return 2
    case .jumpIfTrue:
      return 3
    case .jumpIfFalse:
      return 3
    case .lessThan:
      return 4
    case .equals:
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

let maxParameterLength = 3
func getParamModes(_ input: Int) -> [ParameterMode] {
  let values = input / 100
  let digits = String(values).compactMap { ParameterMode(rawValue: $0.wholeNumberValue!) }
  return padLeft(.position, maxParameterLength - digits.count, digits).reversed()
}

/// A machine can execute a program. It is synchronous and pauses on output.
class Machine {
  var program: Program
  var ip = 0
  var curInstruction = Instruction.start
  private(set) var didHalt = false
  private var input: [Int]
  private var output = 0

  init(_ program: Program, _ input: [Int]) {
    self.program = program
    self.input = input
  }

  func addInput(_ input: Int) {
    self.input.append(input)
  }

  func getParam(_ modes: [ParameterMode], _ n: Int) -> Int {
    return modes[n] == .position ? program[program[ip + n + 1]] : program[ip + n + 1]
  }

  func getNextInstruction() -> Instruction {
    let opCode = program[ip] % 100
    return Instruction(rawValue: opCode) ?? .unknown
  }

  func runProgram() -> Int {
    while true {
      curInstruction = getNextInstruction()
      let paramModes = getParamModes(program[ip])

      switch curInstruction {
      case .add:
        let a = getParam(paramModes, 0)
        let b = getParam(paramModes, 1)
        let c = program[ip + 3]
        program[c] = a + b
        ip += curInstruction.length()
      case .mult:
        let a = getParam(paramModes, 0)
        let b = getParam(paramModes, 1)
        let c = program[ip + 3]
        program[c] = a * b
        ip += curInstruction.length()
      case .input:
        let a = program[ip + 1]
        program[a] = input[0]
        input = Array(input.dropFirst())
        ip += curInstruction.length()
      case .output:
        let a = program[ip + 1]
        output = program[a]
        ip += curInstruction.length()

        return output
      case .jumpIfTrue:
        let a = getParam(paramModes, 0)
        let b = getParam(paramModes, 1)
        if (a != 0) {
          ip = b
        } else {
          ip += curInstruction.length()
        }
      case .jumpIfFalse:
        let a = getParam(paramModes, 0)
        let b = getParam(paramModes, 1)
        if (a == 0) {
          ip = b
        } else {
          ip += curInstruction.length()
        }
      case .lessThan:
        let a = getParam(paramModes, 0)
        let b = getParam(paramModes, 1)
        let c = program[ip + 3]
        if (a < b) {
          program[c] = 1
        } else {
          program[c] = 0
        }
        ip += curInstruction.length()
      case .equals:
        let a = getParam(paramModes, 0)
        let b = getParam(paramModes, 1)
        let c = program[ip + 3]
        if (a == b) {
          program[c] = 1
        } else {
          program[c] = 0
        }
        ip += curInstruction.length()
      case .halt:
        didHalt = true
        return output
      default:
        fatalError("bad instuctions: \(curInstruction) at \(ip)")
      }
    }
  }
}

///-------------------------------------------------------------------------
/// Part 1
/// ------------------------------------------------------------------------

do {
  var phaseValues = [0, 1, 2, 3, 4]
  let phasePerms = perm(&phaseValues, phaseValues.count)

  let outputs: [Int] = phasePerms.map { phases in
    var output = 0
    for i in 0..<5 {
      output = Machine(program, [phases[i], output]).runProgram()
    }

    return output
  }

  print(outputs.max()!)
}

///-------------------------------------------------------------------------
/// Part 2
///
/// No need for any concurrent programming (yet).
/// ------------------------------------------------------------------------

do {
  var phaseValues = [5, 6, 7, 8, 9]
  let phasePerms = perm(&phaseValues, phaseValues.count)

  let outputs: [Int] = phasePerms.map { phases in
    let amps: [Machine] = phases.map { phase in
      let m = Machine(program, [phase])
      return m
    }

    var curAmp = 0  // currently running amp index
    var output = 0  // output of previous amp
    while true {
      let amp = amps[curAmp]
      amp.addInput(output)
      output = amp.runProgram()

      if (curAmp == 4) && amp.didHalt {
        break
      }
      curAmp = (curAmp + 1) % 5
    }
    return output
  }

  print(outputs.max()!)
}
