//
//  File.swift
//  
//
//  Created by Chris Constable on 12/5/19.
//

import Foundation

public func padLeft<A>(_ a: A, _ n: Int, _ list: [A]) -> [A] {
    return Array(repeating: a, count: n) + list
}

public func padRight<A>(_ a: A, _ n: Int, _ list: [A]) -> [A] {
    return Array(repeating: a, count: n) + list
}

public func head<A>(_ list: [A]) -> A {
    return list.first!
}

public func last<A>(_ list: [A]) -> A {
    return list.last!
}

public func zipUntil<A, B>(_ pred: (A, B) -> Bool, _ a: [A], _ b: [B]) -> [(A, B)] {
    if (a.count == 0 || b.count == 0) {
        return []
    } else {
        if !pred(head(a), head(b)) {
            return [(head(a), head(b))] + zipUntil(pred, Array(a.dropFirst()), Array(b.dropFirst()))
        } else {
            return []
        }
    }
}

public func chunks<A>(_ list: [A], _ n: Int) -> [[A]] {
    var count = 0
    var chunk = [A]()
    var chunks = [[A]]()
    list.forEach {
        chunk.append($0)
        count += 1
        if (count % n == 0) {
            chunks.append(chunk)
            chunk = [A]()
        }
    }
    
    if chunk.count != 0 {
        chunks.append(chunk)
    }
    
    return chunks
}
