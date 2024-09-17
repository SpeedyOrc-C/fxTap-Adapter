# fxTap Adapter

This is a osu! beatmap parser with a converter to the fxTap format.

## Build

1. Install Cabal and Glasgow Haskell Compiler. Or install GHCup (contains both of them) instead.

2. Run this command:

```sh
cabal build
```

3. And you'll see it inside folder `dist-newstyle/build`

## Run

### Syntax

Run with Cabal:

```sh
cabal run . -- <beatmap-path>
```

Or run the executable directly:

```sh
./fxTap-Adapter <beatmap-path>
```

## Exceptions

Some beatmaps are not following the specification.
Here are all the exceptions so far:

* Allow `None` in `General`'s `SampleSet`.
* Allow more than one space between tags.
* Allow decimal `Events`' `startTime`
