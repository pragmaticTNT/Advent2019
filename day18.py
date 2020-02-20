lily_input = '''\
#################################################################################
#...........#.......................#...#.........#.....#.....#...Q.............#
###.#######.#################.#.###.###G#.#####.#.#.#.###.#.#.#.#######.#####.###
#...#.......................#.#.#.......#...#...#z#.#...#.#.#.#...#u..#....b#...#
#.#########################.###.#######.###.#.#####.###.#.#.#####.#.#.#####.###.#
#.............#...#l....#.#...#.....#.#.#...#...#...#...#.#.....#.#.#...#...#...#
#.###########.#.###.#.#.#.###.#####.#.#.#######.#.###.#N#.###.###.#.###.#####.#.#
#.#.........#.#.....#.#.....#.#.....#...#.......#.#.#.#.#...#...#.#...#.......#.#
#.#.#######.#.#.#####.#####.#.#.#####.###.#####.#.#.#.#####.###.#.###.###########
#.#.#.D.#...#.#.....#.....#.#.#.....#...#.#.....#...#...#...#...#...#...#.......#
#.#S#.###.###.#####.#####.#.#.#####.###.#.#.#######.###.#.###.#.###.###.#.#####.#
#...#.......#.#...#.#...#.#.#.#...#.#.#.#.#.#.......#.#.....#.#.#.....#...#.#.A.#
#.###########.#.#.#.#.###.#.#.#C#.#.#.#.#.#.#.#######.#######.###.#######.#.#.#.#
#.#.........#.#.#.#.#.#...#.#...#...#.#.#.#.#...#.#.....#.#...#...#...Y.#...#.#.#
#.#.#######.#.#.###.#.#.#############.#.#.#####.#.#.#.#.#.#.###.###.###.###.#.###
#.#.#...#.#...#...#...#.........#.....#.#.....#.#...#.#.#.#.......#.#.#.#...#...#
###.#.#.#.#######.###.#####.###.#.#.###.#.#.#.#.#####.#.#.#########.#.#.#######.#
#...#.#...#.......#.....#...#.#.#.#.#...#.#.#.#.#.....#...#...#m..#...#.#.....#w#
#.###.###.#.#.#####.#####.###.#.#.#.#.###.#.#.#.#.#.#######.#.#.#.###.#.#.###.#.#
#e..#...#...#.#.......#...#.....#.#.#...#.#.#.#...#.#...F...#...#.#...#.#...#...#
#.#.###.#######.#######.#.#####.###.###.#.#.#.#####.#.###########.#.###.#.#.###.#
#.#...#...#.......#...#.#.#...#.......#.#.#.#.#.....#...#i..#.I.#.#.#y#.#.#.#.#.#
#.###.#.#.#.#######.###.###.#.#######.#.#.#.###.#######.#.#.###.#M#.#.#.###.#.#.#
#.#.#.#.#.......#...#.#.....#...#.....#.#.#...#...#.#...#.#.....#.....#x#...#.#.#
#.#.#.#.#########.###.#.#######.###.###.#.###.###.#.#.###.#############.#.###.#.#
#.#.#.#.#.#.....#.#...#...#...#...#...#.#v..#...#...#f..#.#...#..o#.....#.#...#.#
#.#.#.#.#.#.###.#.#.#.###.#.#####.#####.#.###.#.###.###J#.#.#.#.#.#.#####.###.#.#
#...#.#...#.#.#...#.#...#.#.#...#.#...#.#.#...#.#...#...#.#.#...#.#.#...#...#...#
###.#.###.#.#.#####.#####.#.#.#.#.#.#.#.#.#.#####.###.###.#.#####.#.#.###.#.#.###
#...#...#.#...#...#.........#.#...#.#.#.#.#.#.....#...#.#.#.#...#.X.#.....#.#...#
#.#####.#####.#.#.#.#########.#####.#.#.#.#.#.#####.###.#.#O#.#.###########.###.#
#.#...#.....#...#.#.....#...#...#...#.#.#.#...#...#.#..k#...#.#.....#.......#.#.#
#.#B#######.#####.###.###.#.###.#.###.#.#.#####.#.#.#.###########.#.#.#######.#.#
#.#.......#.....#.#...#...#...#...#...#.#...#.#.#..j#.....#.......#.#.#.......#.#
#.#.#####.###.#.#.#####.#####.#.###.#.#.###.#.#.#######.#.#.#######.#.#######.#.#
#.#...#.....#.#.#.......#.....#.#.#.#.#.#.#.#.#.#...#...#.#...#...#.#.....#...#.#
#.#####.###.#.###########.#####.#.#.###.#.#.#.#K#.#.#.###R#.#T###.#.#####.#.###.#
#.#...#...#.#.....#.....#.#...#.#.#...#.#.#.#.#.#.#...#...#.#.#...#...#...#...#.#
#.#.#.###.#.#####.#.#.###.#.#.#.#.###.#.#.#.#.#.#.#####.#####.#.#####.#.###.#.#.#
#...#.....#.....#...#.....#.#.......#.........#...#...........#.......H.#...#..p#
#######################################.@.#######################################
#...........#.....#.....#...#...#.....#.........#...#.......#...#...#...........#
#.###.#####.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.###.#.#.#.#.###.#.#.#.#.###.#######.#
#.#.#.#...#.....#.#.#.#.#.#.#.#...#.#...#.#.#...#.#...#...#...#...#...#.#.......#
#.#.#.#.#########.###.#.#.#.#####.#.###.#.#.#.###.#######.###########.#.#.#######
#.#.#.#.........#...#.#.#.#.....#.#...#.#.#.#...#.......#...#.#...#...#.#.#.....#
#W#.#.#####.###.###.#.#.#.#####.#####.#.###.###.#######.###.#.#.#.#.###.#.#.###.#
#...#.#...#...#...#.#.#.#.#.....#.....#.#...#...#........g#.#...#.#.#..s#.#...#.#
###.#.#.#.###.###.#.#.#.#.#.#####.#####.#.#######.#######.#.#####.#.#.#####.###.#
#...#.#.#.#t..#...#.#.#.#.#...#...#...#.#.#.......#...#...#.....#.#.#.......#...#
#.###.#.#.###.#.###.#.#.#.###.###.#.#.#.#.#.#######.#.#########.#.#.#.#######.#.#
#...#...#...#.#.....#.#.#...#...#.#.#...#...#.......#...#...#...#...#...#.....#.#
###.#######.#######.#.#.###.###.#.#.#####.###.#########.#.#.#.###.#######.#####.#
#.#.#.....#...#...#...#.#r#.#.#...#.....#.#.#...#.....#...#.#.#.#...#.....#.....#
#.#.#.#######.#.#.#####.#.#.#.#########V#.#.###.#.###.#####.#.#.###.#.#####.#####
#...#.#.....#.#.#.#...#.#.#.#.........#.#...#.#.#...#.#...#...#...#...#.#...#...#
#P###.#.###.#.#.#.#.#.#.#.#.#.#####.###.###.#.#.###.#.###.#######.#####.#.#####.#
#.....#.#.....#.#.#.#...#.#.#.....#.....#.#...#...#.#...#.#.............#.......#
###.###.#####.#.#.###.###.#.#####.#######.###.###.#.###.#.#.#.#########.#######.#
#...#...#...#.#.#...#.....#.#.#...#.....#.#.....#.#.#.....#.#.........#.....#...#
#####.###.#.###.###.#######.#.#.###.###.#.#.#####.#.#######.#######.#.#####.#.###
#.....#...#.......#.#.#.....#.#.#...#...#...#...#.#.......#.#.#...#.#.....#.#.#.#
#.###.#.###########.#.#.#####.#.###.#.#######.#.#.#.#####.#.#.#.#.#.#####.###.#.#
#...#.#.#.....#.....#...#.....#...#.#...#...#.#...#...#...#.#...#.#.#...#...#.#d#
#.#.#.#.#.###.#.#########.#.#####.#.#####.#.#.#########.#.#.#####.#.#.#.###.#.#.#
#.#.#.#...#.#.#...#.......#.#...#.#.#...#.#.#.........#.#.#.....#.#...#...#.#.#.#
#.#.#######.#.###.#.#######.#.#.#.#.#.#.#.#.#########.#.#######.#.#########.#.#.#
#.#.........#...#.#...#.L.#...#c#.#...#.#.#...........#....q..#.#.........#.....#
#.#######.#####.#.###.###.#####.#.#####.#.#############.#####.#E#########.#######
#.#.....#.......#...#.....#...#.#.#...#.#.#...#...#.....#...#.#.#.......#.......#
#.#.###############.#####.###.#.#Z#.#.#.#.#.#.#.#.#.#####.###.#.#.#############.#
#.#.......#.....#.#.....#.....#...#.#...#...#.#.#...#...#.U.#...#.....#.........#
#.#.###.#.###.#.#.###.#######.#####.###.#.###.#.#####.#.###.#########.#.#########
#.#...#.#.....#.....#.......#.#.....#.#.#.#...#...#...#.#.........#...#.#.#.....#
#.###.#.###########.#######.#.#.#.###.#.#.#.#####.###.#.#.#######.#.#.#.#.#.#.#.#
#.#...#a#.........#...#.#...#.#.#.#...#.#.#.....#.....#...#.....#...#.#.#...#.#.#
#.#####.#.#####.#####.#.#.###.###.#.#.#.#.#####.#########.#.###.#######.#####.#.#
#.#...#.#.#...#.....#...#..n..#...#.#.#.#...#.#.........#.#.#...#.....#.#...#.#.#
#.#.#.#.#.#.#.#####.###.#######.###.#.#.###.#.#########.###.#.###.###.#.#.#.#.#.#
#...#...#...#....h#.............#...#...#.............#.....#.......#.....#...#.#
#################################################################################
'''

input_0 = '''\
#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######
'''

input_1 = '''\
###############
#d.ABC.#.....a#
######...######
######.@.######
######...######
#b.....#.....c#
###############
'''

input_2 = '''\
#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############
'''

input_3 = '''\
#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba...BcIJ#
#####.@.#####
#nK.L...G...#
#M###N#H###.#
#o#m..#i#jk.#
#############
'''

import string

from collections import deque
from enum import Enum
from typing import List, Dict, Tuple, Set, Deque

def main():
  #for input_ in input_0, input_1, input_2, input_3:
  #  board = Map(input_)
  #  print(board)
  #  #print(board.reachable_keys())
  #  print(board.best_route())
  input_ = lily_input
  board = Map(input_)
  print(board)
  print(board.best_route())

class Tile(Enum):
  wall = 0
  floor = 1
  key = 2
  door = 3

  @classmethod
  def new(cls, c: str): # -> Tile:
    if c == "#":
      return Tile.wall
    elif c == "." or c == "@":
      return Tile.floor
    elif c.islower():
      return Tile.key
    elif c.isupper():
      return Tile.door
    else:
      assert False

  def __str__(self) -> str:
    if self == Tile.wall:
      return "#"
    elif self == Tile.floor:
      return "."
    elif self == Tile.key:
      return "$"
    elif self == Tile.door:
      return ">"
    else:
      assert False

Point = Tuple[int, int]

class Map:
  def __init__(self, input_str: str):
    self.height: int = None
    self.width: int = None
    self.grid: Dict[Point, Tile] = {}
    self.keys: Dict[Point, str] = {}
    self.doors: Dict[Point, str] = {}
    self.current: List[Point] = []
    self.collected_keys: Set[str] = set()

    self.populate_grid(input_str)

    self.memo = {}

  def populate_grid(self, input_str: str) -> None:
    '''Populate self.grid (and self.keys/doors), and set self.current'''
    current = None
    rows = input_str.split()
    self.height = len(rows)
    for i, row in enumerate(rows):
      self.width = len(row)
      for j, char in enumerate(row):
        if char == "@":
          current = (i,j)

        self.grid[(i,j)] = Tile.new(char)
        if self.grid[(i,j)] == Tile.key:
          self.keys[(i,j)] = char
        elif self.grid[(i,j)] == Tile.door:
          self.doors[(i,j)] = char

    row, col = current
    self.grid[(row, col)] = Tile.wall
    self.grid[(row+1, col)] = Tile.wall
    self.grid[(row-1, col)] = Tile.wall
    self.grid[(row, col+1)] = Tile.wall
    self.grid[(row, col-1)] = Tile.wall
    self.current = [(row-1, col-1), (row-1, col+1), (row+1, col-1), (row+1, col+1)]

  @classmethod
  def keys_to_bitmask(cls, keys: Set[str]) -> int:
    '''Convert a set of [a-z] into a bitmask of bitlength <= 26
    Low-order bits are abc... high order bits are ...xyz'''
    bitmask = 0
    for offset, char in enumerate(string.ascii_lowercase):
      if char in keys:
        bitmask |= 1 << offset
    return bitmask

  def best_route(self) -> int:
    '''Return the length of the best path through the maze
    that collects all keys!

    - find reachable keys
    - for each, go pick up that key and then
      recursively find the best route from that state.
    - of all of these, return the best:
      distance to that key PLUS best_route from that key
    - (memoize "best_route", based on set of collected keys
       together with current position)

    todo:
    - convert key-set to bit-mask
    - memoize based on (bitmask, curr_pos)
    '''
    k = (self.keys_to_bitmask(self.collected_keys), tuple(self.current))
    if k in self.memo:
      return self.memo[k]

    if len(self.collected_keys) == len(self.keys):
      return 0

    best_dist = 2**60
    for i, reachable_keys in enumerate(self.reachable_keys()):
      for pos, dist in reachable_keys:
        key = self.keys[pos]
        original_pos = self.current[i]
        self.collected_keys.add(key)

        self.current[i] = pos
        total_dist = dist + self.best_route()
        best_dist = min(best_dist, total_dist)
        #print(key, self.collected_keys)
        #print("dist:", best_dist)

        self.collected_keys.remove(key)
        self.current[i] = original_pos
    self.memo[k] = best_dist
    return best_dist

  def kd(self, key: str) -> str:
    return key.upper()

  def dk(self, door: str) -> str:
    return door.lower()

  def __str__(self) -> None:
    str_rep = ""
    for row in range(self.height):
      for col in range(self.width):
        if (row, col) in self.current:
          str_rep += '@'
        elif (row, col) in self.keys:
          str_rep += self.keys[(row, col)]
        elif (row, col) in self.doors:
          str_rep += self.doors[(row, col)]
        else:
          str_rep += str(self.grid[(row,col)])
      str_rep += '\n'
    return str_rep

  def adjacent(self, cell: Point) -> List[Point]:
    x, y = cell
    adj_pts = []
    if (x, y+1) in self.grid:
      adj_pts.append((x, y+1))
    if (x, y-1) in self.grid:
      adj_pts.append((x, y-1))
    if (x+1, y) in self.grid:
      adj_pts.append((x+1, y))
    if (x-1, y) in self.grid:
      adj_pts.append((x-1, y))
    return adj_pts

  def reachable_keys(self) -> List[List[Tuple[Point, int]]]:
    '''Return the list of reachable uncollected keys, together
    with their shortest-path-distance from the current position.'''
    reachable = []

    for current in self.current:
      # BFS from self.current[i]
      part_list = []
      visited: Set[Point] = set()
      q: Deque[Tuple[Point, int]] = deque()  # position, distance
      q.append((current, 0))
      while q:
        cell, dist = q.popleft()
        visited.add(cell)
        if self.grid[cell] == Tile.key and self.keys[cell] not in self.collected_keys:
          part_list.append((cell, dist))
          continue
        for c in self.adjacent(cell):
          if c not in visited and self.grid[c] != Tile.wall and \
              (self.grid[c] != Tile.door or self.dk(self.doors[c]) in self.collected_keys):
            q.append((c, dist + 1))
      reachable.append(part_list)

    return reachable

if __name__ == '__main__':
  main()
