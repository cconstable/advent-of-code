#!/usr/bin/swift sh

import AOCUtils  // ./AOCUtils
import Foundation

typealias Material = (name: String, quantity: Int)
typealias MaterialMap = [String: Material]
typealias Reaction = (input: [Material], output: Material)
typealias ReactionMap = [String: Reaction]

func parseMaterial(_ input: String) -> Material {
  let components = input |> split(" ")
  return (
    name: components[1],
    quantity: Int(components[0])!
  )
}

func parseReaction(_ input: String) -> Reaction {
  let components = input |> split("=>")
  return (
    input: components[0]
      |> split(",")
      |> map(trim)
      |> map(parseMaterial),
    output: components[1]
      |> trim
      |> parseMaterial
  )
}

let input = rawInput
  |> lines
  |> map(parseReaction)

var reactions = ReactionMap()
input.forEach {
  reactions[$0.output.name] = $0
}

var availableMats = MaterialMap()
var materialMap = MaterialMap()

func useAvailable(_ target: String, _ max: Int) -> Int {
  if let availableMat = availableMats[target] {
    if availableMat.quantity > max {
      availableMats[target] = Material(name: target, quantity: availableMat.quantity - max)
      return max
    } else if availableMat.quantity == max {
      availableMats.removeValue(forKey: target)
      return max
    } else {
      availableMats.removeValue(forKey: target)
      return availableMat.quantity
    }
  } else {
    return 0
  }
}

func calcMaterials(_ target: String, _ amount: Int) {
  if let material = reactions[target] {
    // Try to pull out what we need from the available materials
    let amountToProduce = amount - useAvailable(target, amount)

    // If there isn't enough find out how many more we need and produce just enough
    if amountToProduce > 0 {
      let reactionsNeeded = Int(ceil(Double(amountToProduce) / Double(material.output.quantity)))
      let producedQuantity = reactionsNeeded * material.output.quantity

      // Since we just made some new materials, make all the sub-materials
      material.input.forEach { input in
        calcMaterials(input.name, input.quantity * reactionsNeeded)
      }

      if let availableMat = availableMats[target] {
        availableMats[target] = Material(
          name: target, quantity: availableMat.quantity + producedQuantity)
      } else {
        availableMats[target] = Material(name: target, quantity: producedQuantity)
      }

      if let mat = materialMap[target] {
        materialMap[target] = Material(name: target, quantity: mat.quantity + producedQuantity)
      } else {
        materialMap[target] = Material(name: target, quantity: producedQuantity)
      }

      // Pull out what we need
      let x = useAvailable(target, amountToProduce)
    }
  } else {
    if let mat = materialMap["ORE"] {
      materialMap["ORE"] = Material(name: "ORE", quantity: mat.quantity + amount)
    } else {
      materialMap["ORE"] = Material(name: "ORE", quantity: amount)
    }
  }
}

// ------------------------------------------------------------------------
// Part 1

availableMats = MaterialMap()
materialMap = MaterialMap()
calcMaterials("FUEL", 1)
print(materialMap["ORE"]!.quantity)

// ------------------------------------------------------------------------
// Part 2 - Pen and paper + seeding
// did some math to create an upper and lower bounds. start at lower bounds
// and work up. not a satisfying solution.

var fuelCount = 2_144_650
var oreNeeded = 0
while oreNeeded < 1_000_000_000_000 {
  fuelCount += 1
  availableMats = MaterialMap()
  materialMap = MaterialMap()
  calcMaterials("FUEL", fuelCount)
  oreNeeded = materialMap["ORE"]!.quantity
}

print(fuelCount - 1)