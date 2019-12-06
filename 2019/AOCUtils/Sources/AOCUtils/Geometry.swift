//
//  Point.swift
//  AOCUtils
//
//  Created by Chris Constable on 12/3/19.
//

import Foundation

public struct Point : Hashable {
    public let x: Int
    public let y: Int
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }
    
    public init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }
}

public func add(_ a: Point, _ b: Point) -> Point {
    return Point(x: a.x + b.x, y: a.y + b.y)
}

public func mannDistance(_ point: Point) -> Int {
  return abs(point.x) + abs(point.y)
}
