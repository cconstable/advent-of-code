
import AOCUtils  // ./AOCUtils
import Foundation

let rawInput = """
3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,28,1,1104,0,10,1006,0,71,2,1002,5,10,2,1008,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,66,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,87,1006,0,97,2,1002,6,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,116,1006,0,95,1,1009,10,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,145,1,1002,19,10,2,1109,7,10,1006,0,18,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,179,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,200,1,1105,14,10,1,1109,14,10,2,1109,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,235,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,257,2,101,9,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,282,2,1109,19,10,1,105,0,10,101,1,9,9,1007,9,1033,10,1005,10,15,99,109,633,104,0,104,1,21102,937268368140,1,1,21102,328,1,0,1106,0,432,21102,1,932700599052,1,21101,0,339,0,1105,1,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,209421601831,1,21102,1,386,0,1106,0,432,21102,235173604443,1,1,21102,1,397,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21101,825439855372,0,1,21102,1,420,0,1106,0,432,21101,0,988220907880,1,21102,431,1,0,1106,0,432,99,109,2,22101,0,-1,1,21101,40,0,2,21102,1,463,3,21102,453,1,0,1106,0,496,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1102,1,0,458,109,-2,2106,0,0,0,109,4,2102,1,-1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,22102,1,-3,1,21202,-2,1,2,21102,1,1,3,21101,532,0,0,1105,1,537,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,21201,-4,0,-4,1106,0,628,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,579,0,1106,0,537,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,21201,-1,0,1,21102,1,620,0,105,1,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0
"""

let rawInput2 = """
1102,34915192,34915192,7,4,7,99,0
"""

var input = rawInput
    .components(separatedBy: ",")
    .compactMap(trim >>> Int.init)

for _ in 0..<50000 {
    input.append(0)
}

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

enum ParameterMode: Int {
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
class Machine {
    var program: Program
    var ip = 0
    var relativeBase = 0
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
    
    func runProgram() -> Int {
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
                program[a] = input[0]
                input = Array(input.dropFirst())
                ip += curInstruction.length()
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

///-------------------------------------------------------------------------
/// Part 1
/// ------------------------------------------------------------------------

let black = 0
let white = 1

let left = 0
let right = 1

enum Direction {
    case up
    case left
    case right
    case down
}

var curDir = Direction.up
var curPoint = Point(x: 0, y: 0)
var points = [Point:Int]()

let machine = Machine(program, [1])
while(!machine.didHalt) {
    let color = machine.runProgram()
    points[curPoint] = color
    let nextDir = machine.runProgram()
    if nextDir == left {
        switch curDir {
        case .up:
            curPoint = Point(x: curPoint.x - 1, y: curPoint.y)
            curDir = .left
        case .down:
            curPoint = Point(x: curPoint.x + 1, y: curPoint.y)
            curDir = .right
        case .left:
            curPoint = Point(x: curPoint.x, y: curPoint.y - 1)
            curDir = .down
        case .right:
            curPoint = Point(x: curPoint.x, y: curPoint.y + 1)
            curDir = .up
        }
    } else {
        switch curDir {
        case .up:
            curPoint = Point(x: curPoint.x + 1, y: curPoint.y)
            curDir = .right
        case .down:
            curPoint = Point(x: curPoint.x - 1, y: curPoint.y)
            curDir = .left
        case .left:
            curPoint = Point(x: curPoint.x, y: curPoint.y + 1)
            curDir = .up
        case .right:
            curPoint = Point(x: curPoint.x, y: curPoint.y - 1)
            curDir = .down
        }
    }
    if let nextSpace = points[curPoint] {
        machine.addInput(nextSpace)
    } else {
        machine.addInput(0)
    }
}

var pts = [(Point, Int)]()
for (key, value) in points {
    pts.append((key, value))
}
pts = pts.map { (Point(x: $0.0.x, y: abs($0.0.y)), $0.1) }

var image = [
    Array(repeating: 0, count: 50),
    Array(repeating: 0, count: 50),
    Array(repeating: 0, count: 50),
    Array(repeating: 0, count: 50),
    Array(repeating: 0, count: 50),
    Array(repeating: 0, count: 50),
]

pts.forEach {
    image[$0.0.y][$0.0.x] = $0.1
}

image.forEach {
    print($0)
}

//var curDir = Direction.up
//var curPoint = Point(x: 0, y: 0)
//var points = [Point:Int]()
//
//let machine = Machine(program, [0])
//while(!machine.didHalt) {
//    let color = machine.runProgram()
//    points[curPoint] = color
//    let nextDir = machine.runProgram()
//    if nextDir == left {
//        switch curDir {
//        case .up:
//            curPoint = Point(x: curPoint.x - 1, y: curPoint.y)
//            curDir = .left
//        case .down:
//            curPoint = Point(x: curPoint.x + 1, y: curPoint.y)
//            curDir = .right
//        case .left:
//            curPoint = Point(x: curPoint.x, y: curPoint.y - 1)
//            curDir = .down
//        case .right:
//            curPoint = Point(x: curPoint.x, y: curPoint.y + 1)
//            curDir = .up
//        }
//    } else {
//        switch curDir {
//        case .up:
//            curPoint = Point(x: curPoint.x + 1, y: curPoint.y)
//            curDir = .right
//        case .down:
//            curPoint = Point(x: curPoint.x - 1, y: curPoint.y)
//            curDir = .left
//        case .left:
//            curPoint = Point(x: curPoint.x, y: curPoint.y + 1)
//            curDir = .up
//        case .right:
//            curPoint = Point(x: curPoint.x, y: curPoint.y - 1)
//            curDir = .down
//        }
//    }
//    if let nextSpace = points[curPoint] {
//        machine.addInput(nextSpace)
//    } else {
//        machine.addInput(0)
//    }
//}
//
//print(points.count)
