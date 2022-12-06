# Advent of Code 2022 🎄

My solutions to help the elves on their 🍋 starfruit ⭐ expedition for [Advent of Code
2022 🎄](https://adventofcode.com/2022).

Learning [Clojure](https://clojure.org/) was my own expedition. Don't mind the
wacky clj build setup, I wanted to keep all files at the top level.

## Usage

Run the solutions for a given day:
```shell
$ clj -M -m day-01
$ # ... until ...
$ clj -M -m day-25
```

## Puzzle Inputs

The puzzle inputs are downloaded automatically using an
[adventofcode.com](https://adventofcode.com/) session cookie stored in the
`session_cookie` file in the project root (not checked into git). The input
files are also cached as `input/day-XX.txt` to avoid downloading them again. The
automatic download can therefore be bypassed by creating the cached input file
manually.

## Challenge Structure

Each day's puzzle solution `day-XX.clj` contains a `load-input` function that
loads and parses the day's puzzle input. Additionally it contains a `part-one`
and `part-two` function to solve the two parts of the challenge. Finally, the
`-main` function calls these and prints their results.
