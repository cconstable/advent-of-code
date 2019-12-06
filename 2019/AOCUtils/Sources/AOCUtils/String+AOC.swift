//
//  String+AOC.swift
//  AOCUtils
//
//  Created by Chris Constable on 12/3/19.
//

import Foundation

extension String {
    public subscript(safe index: Int) -> String? {
        guard index >= 0 && index < count else { return nil }
        return String(self[self.index(startIndex, offsetBy: index)])
    }

    public subscript(safe range: CountableRange<Int>) -> String? {
        guard let lowerIndex = index(startIndex, offsetBy: max(0, range.lowerBound), limitedBy: endIndex) else { return nil }
        guard let upperIndex = index(lowerIndex, offsetBy: range.upperBound - range.lowerBound, limitedBy: endIndex) else { return nil }
        return String(self[lowerIndex..<upperIndex])
    }
    
    public subscript(safe range: PartialRangeFrom<Int>) -> String? {
        guard let lowerIndex = index(startIndex, offsetBy: max(0, range.lowerBound), limitedBy: endIndex) else { return nil }
        return String(self[lowerIndex..<endIndex])
    }
    
    public subscript(safe range: PartialRangeUpTo<Int>) -> String? {
        guard let upperIndex = index(startIndex, offsetBy: range.upperBound, limitedBy: endIndex) else { return nil }
        return String(self[startIndex..<upperIndex])
    }
    
    public subscript(safe range: PartialRangeThrough<Int>) -> String? {
        guard let upperIndex = index(startIndex, offsetBy: range.upperBound, limitedBy: index(before: endIndex)) else { return nil }
        return String(self[startIndex...upperIndex])
    }
}

public func trim(_ string: String) -> String {
    return string.trimmingCharacters(in: .whitespacesAndNewlines)
}

public func lines(_ string: String) -> [String] {
    return string.components(separatedBy: "\n")
}
