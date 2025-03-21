# fxTap Adapter

This is a osu! and Malody beatmap parser with a converter to the fxTap format.

## Usage

### Build

Install Cabal and Glasgow Haskell Compiler.
Using [GHCup](https://www.haskell.org/ghcup) is recommended.
Then run this command:

```sh
cabal build
```

And you can find the executable in folder [dist-newstyle](./dist-newstyle).

### Install

Run this command:

```
cabal install
```

And the executable named `fxta` will be installed to Cabal's folder.

### Run

This will generate a binary fxTap beatmap from any osu! and Malody beatmaps.

```
fxta <beatmap_path> [<output_path>]
```

If you want a C header that can be imported to any C/C++ program,
use `-ch` flag (stands for "C header"), and add the symbol name.
So that you don't need to worry about number's endianness
in different platforms.

```
fxta [-ch <symbol_name>] <beatmap_path> [<output_path>]
```

## Exceptions

Some beatmaps are not following the specification.
Here are all the exceptions so far:

* Allow `None` in `General`'s `SampleSet`.
* Allow more than one space between tags.
* Allow decimal `Events`' `startTime`
