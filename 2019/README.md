# AoC 2019 Solutions

### Running solutions

#### Swift

**Requirements**
- [Swift compiler](https://swift.org/download/#releases)
- [swift-sh](https://github.com/mxcl/swift-sh) for running scripts

```bash
cat 01-input.txt | ./01.swift
```

#### Haskell

**Requirements**
- Any recent GHC toolchain. I'm using [Stack](https://docs.haskellstack.org/en/stable/README/).

```bash
# For days that need input...
cat 01-input.txt | stack runghc -- 01.hs

# Some days don't have input...
stack runghc -- 04.hs
```

### Notes

- Swift
    - `AOCUtils` is a Swift package that contains some utilties such as String extensions and functional operators.
    - The following operators are used for clarity and flow:
        - `|>` forward invocation `"hi" |> print` == `print("hi")`
        - `<|` backwards invocation `print <| "hi"` == `print("hi")`
