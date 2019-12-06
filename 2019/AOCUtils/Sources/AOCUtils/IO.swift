//
//  IO.swift
//  AOCUtils
//
//  Created by Chris Constable on 12/3/19.
//

import Foundation

public struct IO {
    static public func get() -> String {
        return getLines().joined(separator: "\n")
    }
    
    static public func getLines() -> [String] {
        return Array(sequence(first: nil, next: { _ in readLine() }).compactMap{$0})
    }
    
    static public func printLn(_ printable: Any) {
        print(printable)
    }
}
