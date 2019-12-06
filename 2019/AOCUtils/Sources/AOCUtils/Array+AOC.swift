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
