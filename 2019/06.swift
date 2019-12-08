#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

///-------------------------------------------------------------------------
/// I went with straight-forward recursive tree algorithms for building,
/// finding paths, and counting nodes. I built an actual tree instead of
/// using a hashmap, etc.
/// ------------------------------------------------------------------------

typealias Orbit = (String, String)

let input = IO.getLines()
  .map { $0.components(separatedBy: ")") }
  .map { ($0[0], $0[1]) }

struct Node {
  var name: String
  var nodes: [Node]
}

func getNode(_ name: String) -> Node {
  let orbit = input.filter { $0.0 == name }
  return expandOrbits(name, orbit)!
}

func expandOrbits(_ name: String, _ orbits: [Orbit]) -> Node? {
  if orbits.count == 0 {
    return Node(name: name, nodes: [])
  } else {
    let nextOrbits = orbits.map { o in
      (o.1, input.filter { $0.0 == o.1 })
    }
    return Node(
      name: name,
      nodes: nextOrbits.compactMap {
        return expandOrbits($0.0, $0.1)
      })
  }
}

///-------------------------------------------------------------------------
/// Part 1
/// ------------------------------------------------------------------------

func countOrbits(_ node: Node, _ depth: Int) -> Int {
  if node.nodes.count == 0 {
    return depth
  } else {
    return depth + node.nodes.map { countOrbits($0, depth + 1) }.reduce(0, +)
  }
}

print(countOrbits(getNode("COM"), 0))

///-------------------------------------------------------------------------
/// Part 2
/// ------------------------------------------------------------------------

func nodePath(_ target: String, _ node: Node, _ path: [Node] = []) -> [Node] {
  if target == node.name {
    return path + [node]
  } else {
    let f = flip
      <| (curry <| nodePath <| target)
      <| (path + [node])
    return node.nodes.flatMap(f)
  }
}

func pathDistanceToEnd(_ path: [Node], from: String) -> Int {
  return path.reduce(0) { acc, node in
    if acc > 0 || node.name == from {
      return acc + 1
    } else {
      return 0
    }
  } - 2
}

let rootNode        = getNode("COM")
let youPath         = nodePath("YOU", rootNode)
let santaPath       = nodePath("SAN", rootNode)
let commonAncestors = zipUntil({ a, b in a.name != b.name }, youPath, santaPath)
let lastCommonAnc   = last(commonAncestors).0.name
let ancToYou        = pathDistanceToEnd(youPath, from: lastCommonAnc)
let ancToSanta      = pathDistanceToEnd(santaPath, from: lastCommonAnc)

print(ancToYou + ancToSanta)
