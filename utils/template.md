<!-- vim:set tw=80 spell: -->

# Advent of Code 2021

These are my [Advent of Code 2021](https://adventofcode.com/2021) solutions
written in [Scala 3](https://scala-lang.org/), 
           [Haskell](https://www.haskell.org/),
           [Python](https://www.python.org/)
         and [Rust](https://www.rust-lang.org/).

<!--TABLE-->

## Remarks

- These are not the shortest or fastest solutions, but they are readable and
  show the key ideas for solving each problem and how to translate them to each
  different language.
- Both parts of each problem are solved in the same file, this requires a bit of
  generalization to solve both parts with almost the same code.
- The solutions are self contained, they can easily be executed as shown in the
  next [section](#running-the-solutions). This also makes them just a bit cluttered
  because the parsing and the part selection are inside the same file.
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

