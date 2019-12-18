//
//  Tree.swift
//  
//
//  Created by Chris Constable on 12/14/19.
//

import Foundation

typealias Tree = Node

public struct Node<A> {
    public let value: A
    public let children: [Node]
    
    public init(_ value: A, _ children: [Node] = []) {
        self.value = value
        self.children = children
    }
}
