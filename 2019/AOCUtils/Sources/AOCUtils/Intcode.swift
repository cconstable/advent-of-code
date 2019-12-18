//
//  Intcode.swift
//  
//
//  Created by Chris Constable on 12/14/19.
//

import Foundation

public typealias Program = [Int]

public enum Instruction: Int {
    case add = 1
    case mult = 2
    case input = 3
    case output = 4
    case jumpIfTrue = 5
    case jumpIfFalse = 6
    case lessThan = 7
    case equals = 8
    case relBase = 9
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
        case .relBase:
            return 2
        case .halt:
            return 1
        }
    }
}

public enum ParameterMode: Int {
    case position = 0
    case immediate = 1
    case relative = 2
}

let maxParameterLength = 3
func getParamModes(_ input: Int) -> [ParameterMode] {
    let values = input / 100
    let digits = String(values).compactMap { ParameterMode(rawValue: $0.wholeNumberValue!) }
    return padLeft(.position, maxParameterLength - digits.count, digits).reversed()
}

/// A machine can execute a program. It is synchronous and pauses on output.
public struct Machine {
    var program: SparseArray<Int>
    var ip = 0
    var relativeBase = 0
    var curInstruction = Instruction.start
    public private(set) var didHalt = false
    private var input: [Int]
    private var output = 0
    
    public init(_ program: Program, _ input: [Int]) {
        self.program = SparseArray(0, program)
        self.input = input
    }
    
    public mutating func addInput(_ input: Int) {
        self.input.append(input)
    }
    
    func getParamIndex(_ modes: [ParameterMode], _ n: Int) -> Int {
        if (modes[n] == .position) {
            return program[ip + n + 1]
        } else if (modes[n] == .immediate) {
            return ip + n + 1
        } else {
            return program[ip + n + 1] + relativeBase
        }
    }
    
    func getNextInstruction() -> Instruction {
        let opCode = program[ip] % 100
        return Instruction(rawValue: opCode) ?? .unknown
    }
    
    public mutating func runProgram() -> Int {
        while true {
            curInstruction = getNextInstruction()
            let paramModes = getParamModes(program[ip])
            
            switch curInstruction {
            case .add:
                let a = getParamIndex(paramModes, 0)
                let b = getParamIndex(paramModes, 1)
                let c = getParamIndex(paramModes, 2)
                program[c] = program[a] + program[b]
                ip += curInstruction.length()
            case .mult:
                let a = getParamIndex(paramModes, 0)
                let b = getParamIndex(paramModes, 1)
                let c = getParamIndex(paramModes, 2)
                program[c] = program[a] * program[b]
                ip += curInstruction.length()
            case .input:
                let a = getParamIndex(paramModes, 0)
                if input.count == 0 {
                    return -2 // return and ask for more input
                } else {
                    program[a] = input[0]
                    input = Array(input.dropFirst())
                    ip += curInstruction.length()
                }
            case .output:
                let a = getParamIndex(paramModes, 0)
                output = program[a]
                ip += curInstruction.length()
                return output
            case .jumpIfTrue:
                let a = getParamIndex(paramModes, 0)
                let b = getParamIndex(paramModes, 1)
                if (program[a] != 0) {
                    ip = program[b]
                } else {
                    ip += curInstruction.length()
                }
            case .jumpIfFalse:
                let a = getParamIndex(paramModes, 0)
                let b = getParamIndex(paramModes, 1)
                if (program[a] == 0) {
                    ip = program[b]
                } else {
                    ip += curInstruction.length()
                }
            case .lessThan:
                let a = getParamIndex(paramModes, 0)
                let b = getParamIndex(paramModes, 1)
                let c = getParamIndex(paramModes, 2)
                if (program[a] < program[b]) {
                    program[c] = 1
                } else {
                    program[c] = 0
                }
                ip += curInstruction.length()
            case .equals:
                let a = getParamIndex(paramModes, 0)
                let b = getParamIndex(paramModes, 1)
                let c = getParamIndex(paramModes, 2)
                if (program[a] == program[b]) {
                    program[c] = 1
                } else {
                    program[c] = 0
                }
                ip += curInstruction.length()
            case .relBase:
                let a = getParamIndex(paramModes, 0)
                relativeBase += program[a]
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
