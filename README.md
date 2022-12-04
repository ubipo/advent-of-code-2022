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

## Challenge Structure

Each day's challenge solution `day-XX.clj` contains a `read-input` function that
parses the day's input file `day-XX.txt`. Additionally it contains a `part-one`
and `part-two` function to solve the two parts of the challenge. Finally, the
`-main` function calls these and prints their results.
