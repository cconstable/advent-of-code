//
//  SparseArray.swift
//  
//
//  Created by Chris Constable on 12/17/19.
//

import Foundation

public struct SparseArray<E>: RandomAccessCollection, MutableCollection {
    let defaultValue: E
    var array: [E] = []
    
    public init(_ defaultValue: E) {
        self.defaultValue = defaultValue
    }
    
    public init(_ defaultValue: E, _ array: [E]) {
        self.defaultValue = defaultValue
        self.array = array
    }
    
    public var startIndex: Int {
        get { array.startIndex }
    }
    
    /// Returns the last populated index
    public var endIndex: Int {
        get { array.endIndex }
    }
    
    public subscript(position: Int) -> E {
        get {
            if position < array.count {
                return array[position]
            } else {
                return defaultValue
            }
        }
        set(newValue) {
            if position >= array.count{
                array += Array(repeating: defaultValue, count: (position + 1) - array.count)
            }
            array[position] = newValue
        }
    }
}
