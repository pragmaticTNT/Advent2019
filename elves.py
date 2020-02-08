def read_program(fileName=None):
    input_str = next(iter(fileinput.input())).split(',')
    program = list(map(int, input_str))
    return program

def main():
    print("Hello! We are the elves, and we are here to help.")

if __name__ == "__main__":
    main()
