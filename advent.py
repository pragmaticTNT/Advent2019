import fileinput
import itertools as it
import math

class Node():
    def __init__(self, value):
        self.value      = value
        self.parent     = None
        self.children   = []
        self.orbit      = 0
    def sizeOrbit(self):
        if len(self.children) == 0:
            self.orbit = 0
        else:
            orbit = [c.sizeOrbit()+1 for c in self.children]
            self.orbit = sum(orbit)
        return self.orbit

class Tree():
    def __init__(self, head, edges):
        nodeList = {}
        for u,v in edges:
            if u not in nodeList:
                nodeList[u] = Node(u)
            if v not in nodeList:
                nodeList[v] = Node(v)
            nodeList[v].parent = nodeList[u]
            nodeList[u].children.append(nodeList[v])
        self.head = nodeList[head]
        self.nodeList = nodeList
    def nOrbits(self):
        self.head.sizeOrbit()
        nNodes = 0
        for n in self.nodeList:
            nPtr = self.nodeList[n]
            ##print(n, nPtr.orbit)
            nNodes += nPtr.orbit
        return nNodes
    def sPath(self, src, tar):
        srcPath = [self.nodeList[src]]
        tarPath = [self.nodeList[tar]]
        while srcPath[-1] != self.head:
            srcPath.append(srcPath[-1].parent)
        while tarPath[-1] != self.head:
            tarPath.append(tarPath[-1].parent)
        while srcPath[-1] == tarPath[-1]:
            srcPath.pop(-1)
            tarPath.pop(-1)
        return len(srcPath + tarPath)

def fuelRequired(mass):
    fuel = mass//3 - 2
    totalFuel = fuel
    while fuel//3 - 2 > 0:
        fuel = fuel//3 - 2
        totalFuel += fuel
    return totalFuel

def getAddr(tape, mode, rel, c):
    val = 0
    if mode == 0:
        val = tape[c]
    elif mode == 1:
        val = c
    elif mode == 2:
        val = rel + tape[c]
    return val

def getVal(tape, mode, rel, c):
    return tape[getAddr(tape, mode, rel, c)]

def intMachine(tape, inputs):
    c       = 0
    rel     = 0
    iCounter= 0
    while tape[c] != 99:
        instr = tape[c]
        mode    = instr // 100
        instr   = instr % 100
        if instr == 1:
            t1 = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t2 = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = t1 + t2
            c += 4
        if instr == 2:
            t1 = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t2 = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = t1 * t2
            ##print("Op2:", t1, t2, tape[c+3])
            c += 4
        if instr == 3:
            addr = getAddr(tape, mode, rel, c+1)
            ##print("Get input:", mode, rel, addr)
            tape[addr] = inputs[iCounter]
            iCounter += 1
            c += 2
        if instr == 4:
            s = getVal(tape, mode, rel, c+1)
            c += 2
            print(s)
        if instr == 5:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode, rel, c+2)
            c = t if s else (c + 3)
        if instr == 6:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode, rel, c+2)
            c = t if s == 0 else (c + 3)
        if instr == 7:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = s < t
            ##print("Op7:", s, t)
            c += 4
        if instr == 8:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = s == t
            c += 4
        if instr == 9:
            rel += getVal(tape, mode, rel, c+1)
            c += 2
    return

def formatTape(fileName):
    tape = []
    with fileinput.input(fileName) as f:
        code = f.readline()
        tape = [int(c) for c in code.split(",")]
    return tape

def day1(fileName):
    fuel    = 0
    with fileinput.input(fileName) as f:
        for line in f:
            addFuel = fuelRequired(int(line))
            ##addFuel = int(line)//3 - 2
            ##print("Fuel added: ", addFuel)
            fuel += addFuel
    print("Total Fuel: ", fuel)

## ---> No longer works due to changes to intMachine
##def day2(fileName):
##    intMachine(formatTape(fileName), [5])

def day3(fileName):
    with fileinput.input(fileName) as f:
        wire1 = f.readline()
        wire2 = f.readline()

    w1 = wire1.split(",")
    w1 = [(w[0], int(w[1:])) for w in w1]
    w2 = wire2.split(",")
    w2 = [(w[0], int(w[1:])) for w in w2]
    dim = 9*max(max([b for a,b in w1]), max([b for a,b in w2]))
    grid = [[0]*(2*dim+1) for j in range(2*dim+1)]
    c  = [dim, dim]
    #intersect = []
    steps = 9999999999

    time = 1
    current = c[:]
    for d,l in w1:
        row = current[0]
        col = current[1]
        if d == 'R':
            for i in range(col+1, col+l+1):
                grid[row][i] = time
                time += 1
            current[1] = col+l
        elif d == 'L':
            for i in range(col-1, col-l-1, -1):
                grid[row][i] = time
                time += 1
            current[1] = col-l
        elif d == 'U':
            for i in range(row-1, row-l-1, -1):
                grid[i][col] = time
                time += 1
            current[0] = row-l
        elif d == 'D':
            for i in range(row+1, row+l+1):
                grid[i][col] = time
                time += 1
            current[0] = row+l

    time = 1
    current = c[:]
    for d,l in w2:
        row = current[0]
        col = current[1]
        if d == 'R':
            for i in range(col+1, col+l+1):
                if grid[row][i] != 0:
                    ##intersect.append([row,i])
                    if grid[row][i] + time < steps:
                        steps = grid[row][i] + time
                time += 1
                ##grid[row][i] = 2
            current[1] = col+l
        elif d == 'L':
            for i in range(col-1, col-l-1, -1):
                if grid[row][i] != 0:
                    ##intersect.append([row,i])
                    if grid[row][i] + time < steps:
                        steps = grid[row][i] + time
                time += 1
                ##grid[row][i] = 2
            current[1] = col-l
        elif d == 'U':
            for i in range(row-1, row-l-1, -1):
                if grid[i][col] != 0:
                    ##intersect.append([i,col])
                    if grid[i][col] + time < steps:
                        steps = grid[i][col] + time
                time += 1
                ##grid[i][col] = 2
            current[0] = row-l
        elif d == 'D':
            for i in range(row+1, row+l+1):
                if grid[i][col] != 0:
                    ##intersect.append([i,col])
                    if grid[i][col] + time < steps:
                        steps = grid[i][col] + time
                time += 1
                ##grid[i][col] = 2
            current[0] = row+l
    ##for row in grid:
    ##    for entry in grid:
    ##        print(entry, end="")
    ##    print()
    ##intersect = [abs(c[0]-a) + abs(c[1]-b) for a,b in intersect]
    ##print("Min M-dist intersection:", min(intersect))
    print(steps)

def day4():
    lb = 272091
    ub = 815432
    count = 0
    for i in range(lb, ub+1):
        double = False
        l = [int(c) for c in list(str(i))]
        check = [a-b for a, b in zip(l[:-1], l[1:])]
        exact = False
        if 0 in check and all(i <= 0 for i in check):
            for j in range(1, len(check)-1):
                if check[j-1] < 0 and check[j] == 0 and check[j+1] < 0:
                    exact = True
            if check[0] == 0 and check[1] < 0:
                exact = True
            if check[-1] == 0 and check[-2] < 0:
                exact = True
            if exact:
                ##print("Good:", i)
                count += 1
    print(count)

def day6(fileName):
    system = []
    with fileinput.input(fileName) as f:
        for line in f:
            system.append(line.rstrip().split(")"))
    systemTree = Tree("COM", system)
    print(systemTree.sPath("YOU", "SAN") - 2)

def day7(fileName):
    init    = [i for i in range(5,10)]
    n       = len(init)
    tape    = formatTape(fileName)
    perm    = it.permutations(init)
    maxAmp  = 0

    for inputs in perm:
        amp     = 0
        ptrs    = [0 for i in range(n)]
        tapes   = [tape[:] for i in range(n)]
        c       = 0
        loop    = False
        end     = False
        while not end:
            io = [amp] if loop else [inputs[c], amp]
            amp, ptr = intMachine(tapes[c], io, ptrs[c])
            ptrs[c] = ptr
            c = (c+1) % 5
            if c == 0:
                loop = True
                if tapes[4][ptr] == 99:
                    end = True
        if amp > maxAmp:
            maxAmp = amp
    print(maxAmp)

def day8(fileName):
    s = ""
    with fileinput.input(fileName) as f:
        s = f.readline()
        s = s.strip()
    n = len(s)//150
    l = [s[150*i:150*i+150] for i in range(n)]
    ##zeroCount = [row.count('0') for row in l]
    ##rowIndex = zeroCount.index(min(zeroCount))
    ##print(l[rowIndex].count('1')*l[rowIndex].count('2'))
    img = list(l[0])
    for i in range(150):
        layer = 0
        while img[i] == '2':
            img[i] = l[layer][i]
            layer += 1
    img = ['.' if c == '0' else '1' for c in img]
    for row in [img[25*i:25*i+25] for i in range(6)]:
        print("".join(row))

def day9(fileName):
    tape = formatTape(fileName)
    tape += [0 for i in range(len(tape)*100)]
    intMachine(tape, [2])

def countAstroid(sector, row, col):
    width   = len(sector)
    length  = len(sector[0])
    tempMap = [[True for i in range(length)] for j in range(width)]
    for c in range(col+1, length):
        if sector[row][c] >= 0:
            sector[row][col] += 1
            sector[row][c] += 1
            break
    for r in range(row+1, width):
        for c in range(length):
            if tempMap[r][c] and sector[r][c] >= 0:
                sector[row][col] += 1
                sector[r][c] += 1
                rowGap = r - row
                colGap = c - col
                rcGcd = math.gcd(rowGap, colGap)
                rowGap //= rcGcd
                colGap //= rcGcd
                rr = r
                cc = c
                while 0 <= rr < width and 0 <= cc < length:
                    tempMap[rr][cc] = False
                    rr += rowGap
                    cc += colGap

def pSector(sector):
    for row in sector:
        for cell in row:
            print('.' if cell<0 else cell, end="")
        print()
    print()

def day10(fileName):
    sector = []
    with fileinput.input(fileName) as f:
        for line in f:
            line = line.strip()
            sector.append([-1 if c=='.' else 0 for c in line])
    for row in range(len(sector)):
        for col in range(len(sector[0])):
            if sector[row][col] >= 0:
                countAstroid(sector, row, col)
                ##pSector(sector)
    maxRow  = [max(row) for row in sector]
    rInd    = maxRow.index(max(maxRow))
    cInd    = sector[rInd].index(max(rInd))
    ##print(max(sum(sector, [])))

def day11(fileName):
    return

def main():
    fileName = "day10-input.txt"
    fileName = "test.txt"
    day10(fileName)

if __name__ == "__main__":
    main()
