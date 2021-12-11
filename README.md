<!-- vim:set tw=80 spell: -->

# Advent of Code 2021

These are my [Advent of Code 2021](https://adventofcode.com/2021) solutions
written in [Scala 3](https://scala-lang.org/), 
           [Haskell](https://www.haskell.org/),
           [Python](https://www.python.org/)
         and [Rust](https://www.rust-lang.org/).

|Day|Title|L1|L2|L3|L4|
|---|---|---|---|---|---|
|[01](https://adventofcode.com/2021/day/1)|Sonar Sweep|[Scala](day01/Day01.scala)|[Haskell](day01/Day01.hs#L5-L9)|[Python](day01/Day01.py)|[Rust](day01/Day01.rs)|
|[02](https://adventofcode.com/2021/day/2)|Dive!|[Scala](day02/Day02.scala)|[Haskell](day02/Day02.hs#L7-L35)|[Python](day02/Day02.py)|[Rust](day02/Day02.rs)|
|[03](https://adventofcode.com/2021/day/3)|Binary Diagnostic|[Scala](day03/Day03.scala)|[Haskell](day03/Day03.hs#L9-L41)|[Python](day03/Day03.py)|[Rust](day03/Day03.rs)|
|[04](https://adventofcode.com/2021/day/4)|Giant Squid|[Scala](day04/Day04.scala)|[Haskell](day04/Day04.hs)|[Python](day04/Day04.py)|[Rust](day04/Day04.rs)|
|[05](https://adventofcode.com/2021/day/5)|Hydrotermal Venture|[Scala](day05/Day05.scala)|[Haskell](day05/Day05.hs#L11-L30)|[Python](day05/Day05.py)|[Rust](day05/Day05.rs)|
|[06](https://adventofcode.com/2021/day/6)|Lanternfish|[Scala](day06/Day06.scala)|[Haskell](day06/Day06.hs#L13-L29)|[Python](day06/Day06.py)|[Rust](day06/Day06.rs)|
|[07](https://adventofcode.com/2021/day/7)|The Treachery of Whales|[Scala](day07/Day07.scala)|[Haskell](day07/Day07.hs#L10-L17)|[Python](day07/Day07.py)|[Rust](day07/Day07.rs)|
|[08](https://adventofcode.com/2021/day/8)|Seven Segment Search|[Scala](day08/Day08.scala)|[Haskell](day08/Day08.hs)|[Python](day08/Day08.py)|[Rust](day08/Day08.rs)|
|[09](https://adventofcode.com/2021/day/9)|Smoke Basin|[Scala](day09/Day09.scala)|[Haskell](day09/Day09.hs)|[Python](day09/Day09.py)|[Rust](day09/Day09.rs)|
|[10](https://adventofcode.com/2021/day/10)|Syntax Scoring|[Scala](day10/Day10.scala)|[Haskell](day10/Day10.hs#L7-L41)|[Python](day10/Day10.py)|[Rust](day10/Day10.rs)|
|[11](https://adventofcode.com/2021/day/11)|Dumbo Octopus||[Haskell](day11/Day11.hs)|[Python](day11/Day11.py)||

## Remarks

- Scala solutions are **short** and **concise**, be sure to check them out.
- The solutions are self contained, they can easily be executed as shown in the
  next [section](#running-the-solutions). This also makes them just a bit cluttered
  because the parsing and the part selection are inside the same file.
- Both parts of each problem are solved in the same file, this requires a bit of
  generalization to solve both parts with almost the same code, and makes the
  code longer than other solutions.
- These are not the shortest or fastest solutions, but they are readable and
  show the key ideas for solving each problem and how to translate them to each
  different language.
- The Python solutions are a bit clunky because these are the ones I code first.
- The solutions in Rust are not so good because this is the first time I use
  Rust for this kind of problems. It gets better day by day, but don't expect amazing
  code.

## Running the solutions

The solutions are self-contained and should run easily using the appropriate
interpreter/compiler for each language, keeping the following considerations in
mind:

  - Input is taken from standard input.
  - Either `1` or `2` must be passed as arguments to choose which part to run.

### Scala

For example, to run the **Scala** solution for the **second part** of the
**first day**, you can use the following command:

```shell
cat day01/test01.txt | scala day01/Day01.scala 2
```

### Haskell

```shell
cat day01/test01.txt | runghc day01/Day01.hs 2
```

### Python

```shell
cat day01/test01.txt | python3 day01/Day01.py 2
```

### Rust

To run a Rust solution you must compile it first and then run the resulting
executable:

```shell
rustc day01/Day01.rs
cat day01/test01.txt | ./Day01 2
```

