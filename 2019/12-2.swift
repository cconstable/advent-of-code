

import AOCUtils  // ./AOCUtils
import Foundation

let rawInput = """
<x=-7, y=17, z=-11>
<x=9, y=12, z=5>
<x=-9, y=0, z=-4>
<x=4, y=6, z=0>
"""

// Unfortunately regex is in a sad state in Swift right now
// and I would not like to pull in any external libs
func makePoint(_ string: String) -> Point3 {
    let range = NSRange(location: 0, length: string.count)
    let regex = try! NSRegularExpression(pattern: "-?\\d+")
    let results = regex.matches(in: string, options: [], range: range)
    let x = Int(result[0])
    print(results.count)
    print(string.s)
    return Point3(x: 0, y: 0, z: 0)
}

let moonPositions = (rawInput |> lines)
    .map(makePoint)

exit(0)
//
//let posMoons = [
//    Point3(x: -7, y: 17, z: -11),
//    Point3(x: 9, y: 12, z: 5),
//    Point3(x: -9, y: 0, z: -4),
//    Point3(x: 4, y: 6, z: 0)
//]
//
//func calcGravity(_ pos: [Point3], _ prevVel: [Point3]) -> [Point3] {
//    let pandv = Array(zip(pos, prevVel))
//    return pandv.map { pav in
//        let p1 = pav.0
//        var vel = pav.1
//        pos.forEach { p2 in
//            if(p1.x < p2.x) {
//                vel = Point3(x: vel.x + 1, y: vel.y, z: vel.z)
//            } else if(p1.x > p2.x) {
//                vel = Point3(x: vel.x - 1, y: vel.y, z: vel.z)
//            }
//
//            if(p1.y < p2.y) {
//                vel = Point3(x: vel.x, y: vel.y + 1, z: vel.z)
//            } else if(p1.y > p2.y) {
//                vel = Point3(x: vel.x, y: vel.y - 1, z: vel.z)
//            }
//
//            if(p1.z < p2.z) {
//                vel = Point3(x: vel.x, y: vel.y, z: vel.z + 1)
//            } else if(p1.z > p2.z) {
//                vel = Point3(x: vel.x, y: vel.y, z: vel.z - 1)
//            }
//
//        }
//        return vel
//    }
//}
//
////var pe = 0
////var ke = 0
////var positions = posMoons
////var velocity = [Point3].init(repeating: Point3(x: 0, y: 0, z: 0), count: 4)
////
////print("p: \(positions)")
////print("v: \(velocity)")
////
////for _ in 0...999 {
////    velocity = calcGravity(positions, velocity)
////    let t = zip(positions, velocity).map {
////        return Point3(x: $0.0.x + $0.1.x, y: $0.0.y + $0.1.y, z: $0.0.z + $0.1.z)
////    }
////    positions = Array(t)
////
////    print("p: \(positions)")
////    print("v: \(velocity)")
////}
////
////print(positions)
////print(velocity)
////
////let t = zip(positions, velocity).map {
////    return (abs($0.0.x) + abs($0.0.y) + abs($0.0.z)) * (abs($0.1.x) + abs($0.1.y) + abs($0.1.z))
////}
////print(Array(t).reduce(+))
//
//// PART 2 --------------------------------------------------------------------------------
//
//var pe = 0
//var ke = 0
//var positions = posMoons
//var velocity = [Point3].init(repeating: Point3(x: 0, y: 0, z: 0), count: 4)
//
//func findGravCycle(_ pts: [Int]) -> Int {
//    var seenPts = [[Int]:Bool]()
//
//    let vel = [Int](repeating: 0, count: pts.count)
//    var pandv = Array(zip(pts, vel))
//
//    let z = pandv.map { $0.0 + $0.1 }
//    seenPts[z] = true
//    while true {
//        pandv = pandv.map { pav in
//            let p1 = pav.0
//            var vel = pav.1
//            pandv.forEach { pav2 in
//                let p2 = pav2.0
//                if(p1 < p2) {
//                    vel += 1
//                } else if(p1 > p2) {
//                    vel -= 1
//                }
//            }
//            return (p1 + vel, vel)
//        }
//
//        let z = pandv.flatMap { [$0.0, $0.1] }
//        if seenPts[z] == true {
//            return seenPts.count - 1
//        } else {
//            seenPts[z] = true
//        }
//    }
//}
//
//let xs = posMoons.map { $0.x } |> findGravCycle
//let ys = posMoons.map { $0.y } |> findGravCycle
//let zs = posMoons.map { $0.z } |> findGravCycle
//let cycle = lcm(xs, lcm(ys, zs))
//print(cycle)
