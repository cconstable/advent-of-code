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

// Heap's algorithm
// https://en.wikipedia.org/wiki/Heap%27s_algorithm
public func perm<T>(_ list: inout [T], _ n: Int = 0) -> [[T]] {
    var output = [[T]]()
    if n == 1 {
        return [list]
    } else {
        for i in 0..<n {
            output += perm(&list, n - 1)
            if n % 2 == 0 {
                list.swapAt(i, n - 1)
            } else {
                list.swapAt(0, n - 1)
            }
        }
        return output
    }
}
