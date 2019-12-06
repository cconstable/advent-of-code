// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "AOCUtils",
    products: [
        .library(
            name: "AOCUtils",
            targets: ["AOCUtils"]),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "AOCUtils",
            dependencies: []),
        .testTarget(
            name: "AOCUtilsTests",
            dependencies: ["AOCUtils"]),
    ]
)
