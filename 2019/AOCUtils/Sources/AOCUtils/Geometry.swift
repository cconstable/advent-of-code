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

public struct Point3 : Hashable {
    public let x: Int
    public let y: Int
    public let z: Int
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
        hasher.combine(z)
    }
    
    public init(x: Int, y: Int, z: Int) {
        self.x = x
        self.y = y
        self.z = z
    }
}

public enum Direction: Int, CaseIterable {
    case north = 1
    case south
    case west
    case east
    
    public func next() -> Direction {
        switch self {
        case .north:
            return .south
        case .south:
            return .west
        case .west:
            return .east
        case .east:
            return .north
        }
    }
    
    public func opposite() -> Direction {
        switch self {
        case .north:
            return .south
        case .south:
            return .north
        case .west:
            return .east
        case .east:
            return .west
        }
    }
    
    public func isOpposite(_ dir: Direction) -> Bool {
        switch self {
        case .north:
            return dir == .south
        case .south:
            return dir == .north
        case .west:
            return dir == .east
        case .east:
            return dir == .west
        }
    }
}

public func +(_ a: Point, _ b: Direction) -> Point {
    switch b {
    case .north:
        return Point(x: a.x, y: a.y + 1)
    case .south:
        return Point(x: a.x, y: a.y - 1)
    case .east:
        return Point(x: a.x + 1, y: a.y)
    case .west:
        return Point(x: a.x - 1, y: a.y)
    }
}

public func +(_ a: Point, _ b: Point) -> Point {
    return Point(x: a.x + b.x, y: a.y + b.y)
}

public func mannDistance(_ point: Point) -> Int {
  return abs(point.x) + abs(point.y)
}
