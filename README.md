# fxTap Adapter

This is an [osu!](https://osu.ppy.sh) and [Malody](https://malody.mugzone.net)
beatmap parser with a converter to [fxTap](https://github.com/SpeedyOrc-C/fxTap)’s format.

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

Convert `a.osu` to `b.fxt` in binary for Casio calculator (big endian):

```sh
fxta -b casio -i a.osu -o b.fxt
```

## Exceptions

Some beatmaps are not following the specification.
Here are all the exceptions so far:

* Allow `None` in `General`'s `SampleSet`.
* Allow more than one space between tags.
* Allow decimal `Events`' `startTime`

## Binary Format

**Warning**: Numbers' endianness can vary depending on the platform.

```ebnf
beatmap ::=
   header,
   overall difficulty,
   song title,
   song artist,
   beatmap version,
   column count,
   columns sizes,
   notes;

header ::= "FXT@2602";

(* Number of notes in columns 1 to 8 *)
column count ::= 1 byte;
columns sizes ::= {column size};
column size ::= 2 bytes;

overall difficulty ::= IEEE double;

song title ::= string;
song artist ::= string;
beatmap version ::= string;

string ::= string size, string content;
string content = {1 byte};
string size ::= 1 byte;

(*
Accumulated start time is used to reduce the memory needed,
since we got a really small RAM.

Examples:

For [100, 100, 200, 100, 300, 100], actual time of notes are
    [100, 200, 400, 500, 800, 900].

For [1, 1, 1, 1, 1], actual times of notes are
    [1, 2, 3, 4, 5].

This helps us keep the time of each note as small as possible.
So that we don't need a large integer to store it.

p.s. Duration doesn't affect the accumulated time.

Notes are listed COLUMN BY COLUMN, not by time,
which means you'll firstly see all notes in
column 1, then column 2, then column 3, and so on.
And the accumulated time in each column is calculated separately.
*)
notes ::= {note};
note ::= note accumulated start time, note duration;
note accumulated start time ::= 2 bytes;
(* Tap has duration of 0, hold has duration > 0 *)
note duration ::= 2 bytes;
```