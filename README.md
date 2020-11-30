# Advent of Code 2020

## About

[Advent of Code](https://adventofcode.com/) is an annual programming puzzle challenge oragized by [Eric Wastl](http://was.tl/). Between December 1 and December 25 (Christmas), a new programming puzzle is posted daily. This repo contains my solutions for the [2020 puzzles](https://adventofcode.com/2020). I encourage everyone to solve the puzzles on their own before looking at my solutions.

## Running the Code

Each puzzle will have its own folder in this repository, named with the day of the month the puzzle was posted. Each folder will have the input as a text file and the solution as an F# script. To run the solutions, you must have the [.NET 5.0 SDK](https://dotnet.microsoft.com/download/dotnet/5.0) installed. Open the repo on the command line and run:
```sh
dotnet fsi XX/solution.fsx
```
Where `XX` is the date of the puzzle, `01` through `25`.

All of the scripts should be platform-agnostic and run [wherever .NET 5.0 is supported](https://github.com/dotnet/core/blob/master/release-notes/5.0/5.0-supported-os.md) (e.g. Windows 7 SP1+, OSX 10.13+, various Linux distros).
