# fxTap Adapter

This is a osu! beatmap parser with a converter to the fxTap format.

## Build

1. Install Cabal and Glasgow Haskell Compiler. Or install GHCup (contains both of them) instead.

2. Run this command:

```sh
cabal build
```

## Run

### Syntax

```sh
cabal run . -- <beatmap-path>
```

### Example

```sh
cabal run . -- my-beatmap.osu
```

This will generate `my-beatmap.f4k`.
