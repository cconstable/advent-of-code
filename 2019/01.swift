#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

let input = IO.getLines().compactMap(Int.init)

/// -------------------------------------------------------------------------
/// Part 1
/// -------------------------------------------------------------------------
func calcFuel(_ fuel: Int) -> Int {
  return max(0, fuel / 3 - 2)
}

IO.printLn
  <| input
  .reduce(0) { $0 + calcFuel($1) }

///-------------------------------------------------------------------------
/// Part 2 - Explicit recusion
/// ------------------------------------------------------------------------

do {
  func calcFuel(_ fuel: Int) -> Int {
    switch max(0, fuel / 3 - 2) {
    case 0:
      return 0
    case let x:
      return x + calcFuel(x)
    }
  }

  IO.printLn
    <| input
    .map(calcFuel)
    .reduce(0, +)
}

/**-------------------------------------------------------------------------
 Part 2 - Hylomorphism

 Part two contains an interesting pattern called a hylomorphism. A hylo is
 a essentially a generalization of an unfold (creating a structure from a
 "seed") and a fold (i.e. a reduce). A hylo is a type of recusion scheme.
 Generalizing this recursion scheme is difficult in Swift due to lack of
 higher-kinded types. However the general pattern is still visible.
--------------------------------------------------------------------------*/

do {
  func calcFuel(_ fuel: Int) -> Int {
    return max(0, fuel / 3 - 2)
  }

  func noneIfZero(_ num: Int) -> Int? {
    return num == 0 ? .none : num
  }

  // anamorphisms are "unfolds"
  let ana = { fuel in
    Array(sequence(first: calcFuel(fuel), next: calcFuel >>> noneIfZero))
  }

  // catamorphisms are "folds"
  let cat = sum
  let hylo = ana >>> cat

  IO.printLn
    <| input
    .map(hylo)
    .reduce(0, +)
}
