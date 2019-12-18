//
//  Int+AOC.swift
//  
//
//  Created by Chris Constable on 12/3/19.
//

import Foundation

extension Int {
    /// Needed because Swift's type inference can't figure out Int.init sometimes
    static public func fromString(_ string: String) -> Int? {
        return Int(string)
    }
}
