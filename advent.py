import fileinput

def fuelRequired(mass):
    fuel = mass//3 - 2
    totalFuel = fuel
    while fuel//3 - 2 > 0:
        fuel = fuel//3 - 2
        totalFuel += fuel
    return totalFuel

def day1(fileName):
    fuel    = 0
    with fileinput.input(fileName) as f:
        for line in f:
            addFuel = fuelRequired(int(line))
            ##addFuel = int(line)//3 - 2
            ##print("Fuel added: ", addFuel)
            fuel += addFuel
    print("Total Fuel: ", fuel)

def day2(fileName):
    with fileinput.input(fileName) as f:
        code = f.readline()
        tape = [int(c) for c in code.split(",")]

    n = len(tape)
    c = 0
    while tape[c] != 99:
        instr = tape[c]
        mode  = -1
        if instr > 99:
            mode    = instr // 100
            instr   = instr % 100
        if instr == 1:
            if mode < 0:
                tape[tape[c+3]] = tape[tape[c+1]] + tape[tape[c+2]]
            else:
                t1 = tape[c+1] if mode % 10 else tape[tape[c+1]]
                mode = mode // 10
                t2 = tape[c+2] if mode % 10 else tape[tape[c+2]]
                tape[tape[c+3]] = t1 + t2
            c += 4
        if instr == 2:
            if mode < 0:
                tape[tape[c+3]] = tape[tape[c+1]] * tape[tape[c+2]]
            else:
                t1 = tape[c+1] if mode % 10 else tape[tape[c+1]]
                mode = mode // 10
                t2 = tape[c+2] if mode % 10 else tape[tape[c+2]]
                tape[tape[c+3]] = t1 * t2
            c += 4
        if instr == 3:
            tape[tape[c+1]] = 5
            c += 2
        if instr == 4:
            print(tape[tape[c+1]])
            c += 2
        if instr == 5:
            if mode < 0:
                c = tape[tape[c+2]] if tape[tape[c+1]] else c + 3
            else:
                s = tape[c+1] if mode % 10 else tape[tape[c+1]]
                mode = mode // 10
                t = tape[c+2] if mode % 10 else tape[tape[c+2]]
                c = t if s else c + 3
        if instr == 6:
            if mode < 0:
                c = tape[tape[c+2]] if tape[tape[c+1]] == 0 else c + 3
            else:
                s = tape[c+1] if mode % 10 else tape[tape[c+1]]
                mode = mode // 10
                t = tape[c+2] if mode % 10 else tape[tape[c+2]]
                c = t if s == 0 else c + 3
        if instr == 7:
            do
        if instr == 8:
            do

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

def main():
    fileName = "day5-input.txt"
    ##fileName = "test.txt"
    day2(fileName)

if __name__ == "__main__":
    main()
