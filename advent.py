import fileinput
import itertools as it
import math
import heapq
import time as tm
import os
clear = lambda: os.system('clear')

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

def intMachine(tape, fnGetInput, fnSetOutput):
    c       = 0
    rel     = 0
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
            tape[addr] = fnGetInput()
            c += 2
        if instr == 4:
            s = getVal(tape, mode, rel, c+1)
            c += 2
            fnSetOutput(s)
            #print(s)
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
            ##print("Op7:", s, t)
            tape[addr] = s < t
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
    ## May consider making the tape length dynamic
    tape += [0 for i in range(len(tape)*10)]
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

def gcdCoord(r,c):
    rcGcd = math.gcd(r, c)
    r //= rcGcd
    c //= rcGcd
    return (r, c)

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
                (rowGap, colGap) = gcdCoord(rowGap, colGap)
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
    m = len(sector)
    n = len(sector[0])
    for row in range(m):
        for col in range(n):
            if sector[row][col] >= 0:
                countAstroid(sector, row, col)
                ##pSector(sector)
    ##print(max(sum(sector, [])))

    ## ===> PART2: Detecting 200th Astroid
    print("Starting Part2...")
    def fnGetSlope(l):
        eps     = 0.0001
        r,c     = l[0]
        quad = ((c>0)&(r<=0)) + 2*((c<=0)&(r<0)) + 3*((c<0)&(r>=0))
        slope = abs(r)/(abs(c)+eps) if quad%2 else abs(c)/(abs(r)+eps)
        return (quad, slope)

    maxRow  = [max(row) for row in sector]
    rInd    = maxRow.index(max(maxRow))
    cInd    = sector[rInd].index(max(sector[rInd]))
    print("Tower location:", rInd, cInd)
    sector[rInd][cInd] = -1

    count   = 0
    bet     = 200
    grid    = {}
    (row, col) = (0,0)
    for i in range(m):
        for j in range(n):
            if sector[i][j] >= 0:
                (r, c) = (rInd-i, j-cInd)
                (minR, minC) = gcdCoord(r,c)
                if (minR, minC) in grid:
                    grid[(minR, minC)].append((r,c))
                else:
                    grid[(minR, minC)] = [(r,c)]
    grid = list(grid.values())
    grid = [sorted(g, key=lambda p: abs(p[0])+abs(p[1])) for g in grid]
    grid = sorted(grid, key=fnGetSlope)
    i = 0
    while count < bet:
        while len(grid[i]) == 0:
            i = (i + 1) % len(grid)
        (row, col) = grid[i].pop(0)
        count += 1
        i = (i + 1) % len(grid)
        ##print("Astroid", count, ':', rInd-row, cInd+col)
    print((cInd+col)*100 + rInd - row)

def pMap(grid, default, sym):
    coordinates = grid.keys()
    minRow      = min([r for r,c in coordinates])
    minCol      = min([c for r,c in coordinates])
    maxRow      = max([r for r,c in coordinates])
    maxCol      = max([c for r,c in coordinates])
    for r in range(minRow, maxRow+1):
        for c in range(minCol, maxCol+1):
            inMap = (r,c) in grid
            tile = default
            if inMap:
                if sym == None:
                    tile = grid[(r,c)]
                elif grid[(r,c)] in sym:
                    tile  = sym[grid[(r,c)]]
            print(tile, end="")
        print()
    print()

def day11(fileName):
    tape    = formatTape(fileName)
    grid    = {(0,0):1}
    row     = 0
    col     = 0
    rDir    = 0
    outMode = 0

    def fnGetInput():
        return grid[(row, col)] if (row, col) in grid else 0

    def fnSetOutput(val):
        nonlocal outMode
        nonlocal row
        nonlocal col
        nonlocal rDir

        if outMode == 0:
            ## Black = 0, White = 1
            grid[(row, col)] = val
            outMode = True
        else:
            ## CCW = 0, CW = 1
            rDir = (rDir + (-1)**(val+1)) % 4
            if rDir == 0:
                row -= 1
            if rDir == 1:
                col += 1
            if rDir == 2:
                row += 1
            if rDir == 3:
                col -= 1
            outMode = False
        return

    intMachine(tape, fnGetInput, fnSetOutput)
    ##print(len(grid))
    pMap(grid, ' ', {'1':'#'})

def elemwiseSum(list1, list2):
    return [sum(p) for p in zip(list1, list2)]

def velChange(pos):
    larger  = [sum([u > v for u in pos]) for v in pos]
    smaller = [sum([u < v for u in pos]) for v in pos]
    return [l-s for l,s in zip(larger, smaller)]

def simulate(stepLim, pos, vel):
    step = 0
    while step < stepLim:
        newX = velChange(pos['x'])
        newY = velChange(pos['y'])
        newZ = velChange(pos['z'])
        ## Adds new velocity to old
        vel['x'] = elemwiseSum(vel['x'], newX)
        vel['y'] = elemwiseSum(vel['y'], newY)
        vel['z'] = elemwiseSum(vel['z'], newZ)
        pos['x'] = elemwiseSum(pos['x'], vel['x'])
        pos['y'] = elemwiseSum(pos['y'], vel['y'])
        pos['z'] = elemwiseSum(pos['z'], vel['z'])
        step += 1
    return

def previousState(p, v):
    pos = p[:]
    vel = v[:]
    count = 0
    history = set()
    history.add((tuple(pos), tuple(vel)))
    while True:
        ##print("History:", history)
        newVel  = velChange(pos)
        vel     = elemwiseSum(vel, newVel)
        pos     = elemwiseSum(pos, vel)
        cur     = (tuple(pos), tuple(vel))
        count += 1
        if cur in history:
            return count
        history.add(cur)

def lcm(a,b):
    return (a*b)//math.gcd(a,b)

def lcmList(l):
    result = 1
    for elem in l:
        result = lcm(elem, result)
    return result

def day12(fileName):
    pos     = {'x':[], 'y':[], 'z':[]}
    vel     = {}
    nMoons  = 4
    step    = 0
    stepLim = 1000
    with fileinput.input(fileName) as f:
        for line in f:
            line = line.strip('<>\n')
            line = line.split(',')
            pos["x"].append(int(line[0].split('=')[-1]))
            pos["y"].append(int(line[1].split('=')[-1]))
            pos["z"].append(int(line[2].split('=')[-1]))
    vel["x"] = [0 for i in range(4)]
    vel["y"] = [0 for i in range(4)]
    vel["z"] = [0 for i in range(4)]

    ##simulate(stepLim, pos, vel)
    ##energy = 0
    ##for i in range(nMoons):
    ##    iPos = [pos['x'][i], pos['y'][i], pos['z'][i]]
    ##    iVel = [vel['x'][i], vel['y'][i], vel['z'][i]]
    ##    pot = [abs(p) for p in iPos]
    ##    kin = [abs(v) for v in iVel]
    ##    energy += sum(pot)*sum(kin)
    ##print(energy)

    ## ===> PART2: Back Again
    xLoop = previousState(pos['x'], vel['x'])
    yLoop = previousState(pos['y'], vel['y'])
    zLoop = previousState(pos['z'], vel['z'])
    loop = [xLoop, yLoop, zLoop]
    print(loop)
    print(lcmList(loop))

def findCoord(screen, val):
    for cell in screen:
        if screen[cell] == val:
            return cell

def day13(fileName):
    tape    = formatTape(fileName)
    x       = 0
    y       = 0
    count   = 0
    screen  = {}
    tiles   = {0:' ', 1:'#', 2:'.', 3:'_', 4:'o'}

    def fnGetInput():
        clear()
        pMap(screen, ' ', tiles)
        tm.sleep(0.1)

        rBall, cBall        = findCoord(screen, 4)
        rPaddle, cPaddle    = findCoord(screen, 3)
        if cBall < cPaddle:
            return -1
        elif cBall > cPaddle:
            return 1
        return 0

    def fnSetOutput(val):
        nonlocal count
        nonlocal x
        nonlocal y
        if count % 3 == 0:
            x = val
        elif count % 3 == 1:
            y = val
        else:
            if x < 0:
                print("Score:", val)
            screen[(y,x)] = val
        count = (count+1) % 3
        return

    ##intMachine(tape, fnGetInput, fnSetOutput)
    ##nBlocks = sum([arcade[k] == 2 for k in arcade])
    ##print(nBlocks)

    ## ===> PART2: Playing the Game
    tape[0] = 2
    intMachine(tape, fnGetInput, fnSetOutput)

def needOre(reactions, qOre):
    orePerFuel  = 0
    store = {}
    stack = [('FUEL',qOre)]
    while stack:
        tar, quantity = stack.pop(-1)
        #print(tar, quantity)
        #print(store)
        if tar == "ORE":
            orePerFuel += quantity
        else:
            src     = reactions[tar]
            qTar    = src[0]
            if tar in store and store[tar] >= quantity:
                store[tar] -= quantity
            else:
                quantity -= store[tar] if tar in store else 0
                mult        = math.ceil(quantity/qTar)
                store[tar]  = mult*qTar - quantity
                for chem, qSrc in src[1]:
                    stack.append((chem, mult*qSrc))
    return orePerFuel

def day14(fileName):
    reactions   = {}

    with fileinput.input(fileName) as f:
        for line in f:
            line = line.strip()
            line = line.split(" => ")
            src = line[0].split(',')
            src = [chem.split() for chem in src]
            src = [(chem[1], int(chem[0])) for chem in src]
            quant, tar = line[1].split()
            reactions[tar] = [int(quant), src]

    orePerFuel  = needOre(reactions, 1)
    print("Ore Per Fuel:", orePerFuel)

    ## ==> PART2: Producing Fuel
    totalOre    = 1000000000000
    low         = totalOre // orePerFuel
    ## the range is not [low, low+orePerFuel]
    high        = low + totalOre
    while high - low > 1:
        mid = (high+low)//2
        ok  = needOre(reactions, mid) <= totalOre
        if ok:
            low = mid
        else:
            high = mid
    print("Fuel Produced:", low)

def day15(fileName):
    tape    = formatTape(fileName)
    maze    = {(0,0):1}
    direct  = {(1,0):1, (-1,0):2, (0,1):3, (0,-1):4}
    stack   = []
    row     = 0
    col     = 0

    def fnGetInput():
        nonlocal row
        nonlocal col
        if (row-1, col) not in maze:
            stack.append([(row,col), (row-1, col)])
        elif (row+1, col) not in maze:
            stack.append([(row,col), (row+1, col)])
        elif (row, col-1) not in maze:
            stack.append([(row,col), (row, col-1)])
        elif (row, col+1) not in maze:
            stack.append([(row,col), (row, col+1)])
        if stack:
            src, tar = stack[-1]
            if tar in maze:
                stack[-1]   = [tar, src]
                src, tar    = tar, src
            return direct[(src[0]-tar[0], src[1]-tar[1])]

    def fnSetOutput(val):
        nonlocal row
        nonlocal col

        src, tar = stack[-1]
        if val == 0:
            stack.pop()
        else:
            if tar in maze:
                stack.pop()
            row, col = tar
        maze[tar] = val
        ##print("val:{}, ({},{})".format(val, row, col))
        return

    intMachine(tape, fnGetInput, fnSetOutput)
    for cell in maze:
        if maze[cell] == 0:
            maze[cell] = -3
        elif maze[cell] == 1:
            maze[cell] = -1
        elif maze[cell] == 2:
            print("Target:", cell)
            maze[cell] = -2
    ##stack.append([(0,0),-1])
    ##row, col = (0,0)
    ##count = 0
    ##while maze[(row, col)] != -2:
    ##    coord, count = stack.pop(0)
    ##    row, col = coord
    ##    ##print(row, col, count)
    ##    if maze[coord] == -1:
    ##        if coord == (0,0):
    ##            count = 0
    ##        maze[coord] = count
    ##        count += 1
    ##        stack.append([(row-1,col), count])
    ##        stack.append([(row+1,col), count])
    ##        stack.append([(row,col-1), count])
    ##        stack.append([(row,col+1), count])
    ####pMap(maze, ' ', None)
    ##print("Move:", count)

    ## ===> Part2: Flood Fill
    target = (-20, -16)
    maze[target] = -1
    stack.append([target, -1])
    row, col = target
    count = 0
    while stack:
        coord, count = stack.pop(0)
        row, col = coord
        if maze[coord] == -1:
            if coord == target:
                count = 0
            maze[coord] = count
            stack.append([(row-1,col), count+1])
            stack.append([(row+1,col), count+1])
            stack.append([(row,col-1), count+1])
            stack.append([(row,col+1), count+1])
    ## The count is off by one since it explores
    ## all cells adjacent to the last cell
    print("Fill:", count-1)
    return

def ip(v1, v2):
    return sum([a*b for a,b in zip(v1,v2)])

def day16(fileName):
    signal  = []
    pattern = [0,1,0,-1]
    offset  = 0
    with fileinput.input(fileName) as f:
        line = f.readline()
        line = line.strip()
        offset = int(line[0:7])
        signal = [int(c) for c in line]

    n = len(signal)
    print("Length:", n)
    print("Offset:", offset)
    phase = 100
    for i in range(phase):
        newSignal = []
        for i in range(1, n+1):
            mask = []
            mark = 0
            while len(mask) <= n:
                mask += [pattern[mark]]*i
                mark = (mark+1) % 4
            mask    = mask[1:(n+1)]
            bit     = ip(signal, mask)
            bit     = abs(bit) % 10
            newSignal.append(bit)
        signal = newSignal[:]
    print(newSignal[0:8])

def compress(string):
    count = 1
    compStr = ""
    for i in range(len(string)-1):
        if string[i] == string[i+1]:
            count += 1
        else:
            compStr += str(count) if count>1 else string[i]
            compStr += ','
            count = 1
    compStr += str(count) if count>1 else string[-1]
    return compStr

def day17(fileName):
    tape = formatTape(fileName)
    view = {}
    inputs = {}
    inputs[0] = "A,A,B,C,B,C,B,C,A,C"
    inputs[1] = "R,6,L,8,R,8"
    inputs[2] = "R,4,R,6,R,6,R,4,R,4"
    inputs[3] = "L,8,R,6,L,10,L,10"
    inputs[4] = "n"
    row, col = (0,0)
    start   = (row, col)
    step    = 0
    count   = 0

    def fnGetInput():
        nonlocal step
        nonlocal count
        inputChar = '\n'
        if step < len(inputs[count]):
            inputChar = inputs[count][step]
            step += 1
        else:
            count += 1
            step = 0
        inputChar = ord(inputChar)
        return inputChar

    def fnSetOutput(val):
        nonlocal row
        nonlocal col
        nonlocal start
        if val != 10 and val < 1000:
            view[(row, col)] = chr(val)
            if chr(val) != '#' and chr(val) != '.':
                start = (row,col)
            col += 1
        elif val == 10:
            row += 1
            col = 0
        else:
            print("Dirt", val)

    intMachine(tape, fnGetInput, fnSetOutput)
    ##pMap(view, '.', None)
    ##align = 0
    ##for cell in view:
    ##    r, c = cell
    ##    if view[cell] == '#' and (r-1,c) in view and view[(r-1,c)] == '#' and (r+1,c) in view and view[(r+1,c)] == '#' and (r,c-1) in view and view[(r,c-1)] == '#' and (r,c+1) in view and view[(r,c+1)] == '#':
    ##        align += r*c
    ##print("Align:", align)

    ## ===> Part2: Warning
    def move(r, c, f):
        forward = tuple([r+(f==2)-(f==0), c+(f==1)-(f==3)])
        left    = tuple([r-(f==1)+(f==3), c-(f==0)+(f==2)])
        right   = tuple([r-(f==3)+(f==1), c-(f==2)+(f==0)])
        if forward in view and view[forward] == '#':
            return forward[0], forward[1], f, 'F'
        elif left in view and view[left] == '#':
            return r, c, (f-1) % 4, 'L'
        elif right in view and view[right] == '#':
            return r, c, (f+1) % 4, 'R'
        else:
            return r, c, f, ''

    f    = 1 #{0=N,1=E,2=S,3=W}
    r, c = start
    command = 'R'
    commands = "R"
    while command:
        r, c, f, command = move(r, c, f)
        commands += command
    ##pMap(view, '.', None)
    commands = compress(commands)
    ##print(commands)

def day19(fileName):
    tape    = formatTape(fileName)
    view    = {}

    def checkCell(i,j):
        xCoord  = i
        yCoord  = j
        isX     = False
        temp    = tape[:]

        def fnGetInput():
            nonlocal isX
            nonlocal xCoord
            nonlocal yCoord
            isX = not isX
            return xCoord if isX else yCoord

        def fnSetOutput(val):
            nonlocal xCoord
            nonlocal yCoord
            view[(xCoord,yCoord)] = val

        if (i,j) not in view:
            intMachine(temp, fnGetInput, fnSetOutput)
            ##print("({},{}): {}".format(i,j, view[(i,j)]))
        return view[(i,j)]

    ##for i in range(50):
    ##    for j in range(50):
    ##        checkCell(i,j)
    ##pMap(view, ' ', {0:'.', 1:'#'})
    ##print(sum(view.values()))

    ## ==> Part2: Prep Beam
    N       = 50
    col     = 8*(N-1)+4
    row     = 9*(N-1)+5
    rOffset = 0
    rDim    = 100

    while True:
        inRow = row+rOffset
        if  checkCell(col,inRow) and\
            checkCell(col, inRow+rDim-1):
            if col > 1729:
                print(col, row, rOffset)
            if  checkCell(col+rDim-1, inRow) and\
                checkCell(col+rDim-1, inRow+rDim-1):
                print(col*10000+inRow)
                return
            else:
                rOffset += 1
        else:
            row += 1 + (col % 8 == 7)
            col += 1
            if col % 8 == 4:
                N   += 1
                ##rOffset = max(N-2, 0)//2

def main():
    fileName = "day19-input.txt"
    ##fileName = "test.txt"
    day19(fileName)

if __name__ == "__main__":
    main()
