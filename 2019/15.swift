#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

var input = IO.get()
  .components(separatedBy: ",")
  .compactMap(trim >>> Int.init)

enum PathResult: Int {
  case obstructed = 0
  case ok
  case target
}

enum CellType: Int {
  case wall = 0
  case open
  case target
}

struct PathPoint: Hashable {
  let point: Point
  let dist: Int
  let type: CellType

  func hash(into hasher: inout Hasher) {
    hasher.combine(point)
    hasher.combine(dist)
    hasher.combine(type)
  }
}

func mapUnknownSpace(
  _ machine: Machine,
  _ point: PathPoint,
  _ searched: [Point: PathPoint] = [Point: PathPoint]()
) -> [Point: PathPoint] {
  var searchedM = searched
  Direction.allCases.forEach { dir in
    var robot = machine  // n.b. this makes a copy of the Intcode machine
    robot.addInput(dir.rawValue)
    let output = PathResult(rawValue: robot.runProgram())!
    switch output {
    case .obstructed:
      let newPoint = PathPoint(point: point.point + dir, dist: point.dist + 1, type: .wall)
      searchedM[newPoint.point] = newPoint
    case .ok:
      let newPoint = PathPoint(point: point.point + dir, dist: point.dist + 1, type: .open)
      if searchedM[newPoint.point] == nil {
        searchedM[newPoint.point] = newPoint
        searchedM = mapUnknownSpace(robot, newPoint, searchedM)
      }
    case .target:
      let newPoint = PathPoint(point: point.point + dir, dist: point.dist + 1, type: .target)
      searchedM[newPoint.point] = newPoint
    }
  }

  return searchedM
}

// Part 1

let start = PathPoint(point: Point(x: 0, y: 0), dist: 0, type: .open)
let space = mapUnknownSpace(Machine(input, []), start)
let part1 = space.filter { (key, value) -> Bool in
  value.type == .target
}

print(part1)

// Part 2

func mapSpace(
  _ space: [Point: PathPoint],
  _ point: PathPoint,
  _ searched: [Point: PathPoint] = [Point: PathPoint]()
) -> [Point: PathPoint] {
  var searchedM = searched
  Direction.allCases.forEach { dir in
    let p = point.point + dir
    if let output = space[p]?.type {
      switch output {
      case .wall:
        let newPoint = PathPoint(point: p, dist: point.dist + 1, type: .wall)
        searchedM[newPoint.point] = newPoint
      case .open:
        let newPoint = PathPoint(point: p, dist: point.dist + 1, type: .open)
        if searchedM[newPoint.point] == nil {
          searchedM[newPoint.point] = newPoint
          searchedM = mapSpace(space, newPoint, searchedM)
        }
      case .target:
        let newPoint = PathPoint(point: p, dist: point.dist + 1, type: .target)
        searchedM[newPoint.point] = newPoint
      }
    }
  }

  return searchedM
}

let start2 = PathPoint(point: Point(x: -20, y: 18), dist: 0, type: .target)
let space2 = mapSpace(space, start2)
let part2 = space2.mapValues { $0.dist }.values.max()

print(part2)