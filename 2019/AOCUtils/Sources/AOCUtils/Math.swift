//
//  File.swift
//  
//
//  Created by Chris Constable on 12/3/19.
//

import Foundation

public func sum(_ list: [Int]) -> Int {
    return list.reduce(0, +)
}

public func product(_ list: [Int]) -> Int {
    return list.reduce(1, *)
}

public func product<A: Sequence, B: Sequence>(_ xs: A, _ ys: B) -> AnySequence<(A.Iterator.Element, B.Iterator.Element)> {
    return AnySequence(xs.lazy.flatMap { x in ys.lazy.map { y in return (x, y) } })
}

public func union<A>(_ a: Set<A>, _ b: Set<A>) -> Set<A> {
    return a.union(b)
}

public func intersection<A>(_ a: Set<A>, _ b: Set<A>) -> Set<A> {
    return a.intersection(b)
}

