//
//  Functional.swift
//  AOCUtils
//
//  Created by Chris Constable on 12/3/19.
//
//  Common functional (FP) utilities

import Foundation

precedencegroup FunctionApplication {
    higherThan: BitwiseShiftPrecedence
    associativity: left
    assignment: false
}

infix operator |>: FunctionApplication
public func |><A, B> (lhs: A, rhs: (A) -> B) -> B {
    return rhs(lhs)
}

infix operator <|: FunctionApplication
public func <|<A, B> (lhs: (A) -> B, rhs: A) -> B {
    return lhs(rhs)
}

infix operator <^>: FunctionApplication
public func <^><A, B> (lhs: A?, rhs: (A) -> B) -> B? {
    return lhs.map(rhs)
}

public func <^><A, B> (lhs: [A], rhs: (A) -> B) -> [B] {
    return lhs.map(rhs)
}

precedencegroup FunctionComposition {
    higherThan: BitwiseShiftPrecedence
    associativity: left
    assignment: false
}

infix operator >>>: FunctionComposition
public func >>><A, B, C> (lhs: @escaping  (A) -> B, rhs: @escaping (B) -> C) -> (A) -> C {
    return { a in
        rhs(lhs(a))
    }
}

infix operator <<<: FunctionComposition
public func <<<<A, B, C> (lhs: @escaping  (B) -> C, rhs: @escaping (A) -> B) -> (A) -> C {
    return { a in
        lhs(rhs(a))
    }
}

public func curry<A, B, C>(_ f: @escaping (A, B) -> C) -> (A) -> (B) -> C {
    return { a in
        return { b in
            return f(a, b)
        }
    }
}

public func curry<A, B, C, D>(_ f: @escaping (A, B, C) -> D) -> (A) -> (B) -> (C) -> D {
    return { a in
        return { b in
            return { c in
                return f(a, b, c)
            }
        }
    }
}

public func flip<A, B, C>(_ f: @escaping (A) -> (B) -> C) -> (B) -> (A) -> C {
    return { b in
        return { a in
            return f(a)(b)
        }
    }
}

public func iterate<A>(_ f: (A) -> A, _ seed: A, _ n: Int) -> [A] {
    var result = seed
    var resultList: [A] = []
    for _ in 0..<n {
        result = f(result)
        resultList.append(result)
    }
    return resultList
}

public func map<A, B>(_ f: @escaping (A) -> B) -> ([A]) -> [B] {
    return { input in
        input.map(f)
    }
}

extension Array {
    /// Reduce with no initial element. Uses elements of the Array. Returns nil or the first element
    /// if there are zero or one elements respectively.
    public func reduce(_ f: (Element, Element) -> Element) -> Element? {
        if self.count == 0 { return nil }
        if self.count == 1 { return self[0] }
        
        var result = self[0]
        for i in 1..<self.count {
            result = f(result, self[i])
        }
        return result
    }
}
