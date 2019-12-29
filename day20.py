from enum import Enum
from typing import List, Dict, Tuple, Set
import doctest, unittest
import fileinput

class Tile(Enum):
    EMPTY = 0
    WALL = 1
    FLOOR = 2

    def __str__(self) -> str:
        if self is Tile.EMPTY:
            return " "
        elif self is Tile.WALL:
            return "#"
        elif self is Tile.FLOOR:
            return "."
        else:
            assert False

class Dir(Enum):
    """Direction of a portal: in or out."""
    IN = +1
    OUT = -1

Point = Tuple[int, int]

def main():
    input_ = "day20-input"

    grid, portals, start, end = read_grid(input_ + ".txt")

    # Sanity checks
    ##print("---")
    ##print(input_)
    ##print("---")
    ##print_grid(grid, portals, start, end)
    ##print("---")
    ##print(portals)
    ##print(f"len(portals): {len(portals)}")
    ##print(f"start: {start}, end: {end}")

    dist = shortest_path_length(grid, portals, start, end)
    print(f"shortest path length: {dist}")

    ##doctest.testmod()
    ##unittest.main()

class TestMain(unittest.TestCase):
    def test_process_label(self):
        lines = input_str1.split("\n")
        self.assertEqual(process_label(lines, 0, 9), ("AA", 2, 9))
        self.assertEqual(process_label(lines, 1, 9), ("AA", 2, 9))

def shortest_path_length(
    grid: List[List[Tile]],
    portals: Dict[Point, Tuple[Point, Dir]],
    start: Point,
    end: Point,
) -> int:
    start_level = 0
    start_dist = 0
    queue: List[Tuple[Point, int, int]] = [(start, start_level, start_dist)]

    visited: Set[Point] = set()
    print("End:", end)

    while queue:
        current, level, dist = queue.pop(0)
        ##print(current, level, dist)

        if (current, level) in visited:
            continue
        visited.add((current, level))

        if level == 0 and current == end:
            return dist

        if current in portals:
            next_, dir_ = portals[current]
            level_diff = 1 if dir_ is Dir.IN else -1
            next_level = level + level_diff
            if next_level >= 0:
                queue.append((next_, level + level_diff, dist + 1))

        i, j = current
        if grid[i - 1][j] is Tile.FLOOR:
            queue.append(((i - 1, j), level, dist + 1))
        if grid[i + 1][j] is Tile.FLOOR:
            queue.append(((i + 1, j), level, dist + 1))
        if grid[i][j - 1] is Tile.FLOOR:
            queue.append(((i, j - 1), level, dist + 1))
        if grid[i][j + 1] is Tile.FLOOR:
            queue.append(((i, j + 1), level, dist + 1))

    assert False

def portal_dir(lines: List[str], x: int, y: int) -> Dir:
    ##"""
    ##Is this an inner portal or an outer portal?
    ##Implemented via a terrible hack :)

    ##Examples for input_str1
    ##>>> from os import linesep
    ##>>> lines1 = input_str1.rstrip(linesep).split(linesep)
    ##>>> portal_dir(lines1, 2, 9) is Dir.OUT
    ##True
    ##>>> portal_dir(lines1, 16, 13) is Dir.OUT
    ##True
    ##>>> portal_dir(lines1, 8, 2) is Dir.OUT
    ##True
    ##>>> portal_dir(lines1, 6, 9) is Dir.IN
    ##True
    ##>>> portal_dir(lines1, 12, 11) is Dir.IN
    ##True

    ##Examples for input_str2
    ##>>> lines2 = input_str2.rstrip(linesep).split(linesep)
    ##>>> portal_dir(lines2, 2, 19) is Dir.OUT
    ##True
    ##>>> portal_dir(lines2, 17, 32) is Dir.OUT
    ##True
    ##>>> portal_dir(lines2, 17, 2) is Dir.OUT
    ##True
    ##"""
    if x == 2 or y == 2 or x == len(lines) - 3 or y == len(lines[x]) - 3:
        return Dir.OUT
    else:
        return Dir.IN

def read_grid(
    file_name: str
) -> (
    List[List[Tile]],  # grid
    Dict[Point, Tuple[Point, Dir]],  # portals
    int,  # start
    int,  # end
):
    lines = []
    with fileinput.input(file_name) as f:
        for line in f:
            lines.append(line.rstrip('\n'))

    grid    = []
    labels  = {}  # E.g., {"AB": (x1,y1)}
    portals = {}  # E.g., {(x1,y1): (x2,y2), (x2,y2): (x1,y1)}

    for i, line in enumerate(lines):
        row = []
        for j, char in enumerate(line):
            if char == "#":
                row.append(Tile.WALL)
            elif char == ".":
                row.append(Tile.FLOOR)
            elif char.isalpha():
                label, x, y = process_label(lines, i, j)
                if label == "AA":
                    start = (x, y)
                elif label == "ZZ":
                    end = (x, y)
                elif label in labels and (x, y) not in portals:
                    x2, y2 = labels[label]
                    portals[(x, y)] = ((x2, y2), portal_dir(lines, x, y))
                    portals[(x2, y2)] = ((x, y), portal_dir(lines, x2, y2))
                else:
                    labels[label] = (x, y)
                row.append(Tile.EMPTY)
            else:
                row.append(Tile.EMPTY)
        grid.append(row)
    return grid, portals, start, end

def process_label(
    lines: List[str], i: int, j: int
) -> (
    str,  # label
    int,  # portal x
    int,  # portal y
):
## """
## Example:
##   A<-- this one is (i, j)
##   A
## ##.<-- the portal goes here (x, y)
## ##....
## ##.### etc...

## >>> from os import linesep
## >>> lines = input_str1.split(linesep)
## >>> process_label(lines, 0, 9)
## ('AA', 2, 9)
## >>> process_label(lines, 1, 9)
## ('AA', 2, 9)
## """
    if i > 0 and lines[i - 1][j].isalpha():
        label = lines[i - 1][j] + lines[i][j]
        if i - 1 > 0 and lines[i - 2][j] == ".":  # .BA
            x = i - 2
        else:  # A is current location: BA.
            assert lines[i + 1][j] == "."
            x = i + 1
        y = j
    elif i < len(lines) - 1 and lines[i + 1][j].isalpha():
        label = lines[i][j] + lines[i + 1][j]
        if i > 0 and lines[i - 1][j] == ".":
            x = i - 1
        else:
            assert lines[i + 2][j] == "."
            x = i + 2
        y = j
    elif j > 0 and lines[i][j - 1].isalpha():
        label = lines[i][j - 1] + lines[i][j]
        if j - 1 > 0 and lines[i][j - 2] == ".":
            y = j - 2
        else:
            assert lines[i][j + 1] == "."
            y = j + 1
        x = i
    elif j < len(lines[i]) - 1 and lines[i][j + 1].isalpha():
        label = lines[i][j] + lines[i][j + 1]
        if j > 0 and lines[i][j - 1] == ".":
            y = j - 1
        else:
            assert lines[i][j + 2] == "."
            y = j + 2
        x = i
    else:
        assert False
    return label, x, y

def print_grid(
    grid: List[List[Tile]],
    portals: Dict[Point, Tuple[Point, Dir]],
    start: Point,
    end: Point,
):
    for i, row in enumerate(grid):
        for j, tile in enumerate(row):
            if (i, j) == start:
                print("s", end="")
            elif (i, j) == end:
                print("e", end="")
            elif (i, j) in portals:
                _, dir_ = portals[(i, j)]
                if dir_ is Dir.IN:
                    print("i", end="")
                else:
                    print("o", end="")
            else:
                print(tile, end="")
        print()

if __name__ == "__main__":
    main()
    doctest.testmod()
