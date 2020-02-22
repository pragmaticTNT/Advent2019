import fileinput

# =========================
# ===> File Processing <===
# =========================

def read_program() -> list:
    with fileinput.input() as f:
        input_str = f.readline()
    input_str = input_str.split(',')
    program = list(map(int, input_str))
    return program

# ==============
# ===> Math <===
# ==============

def extended_euclid(a: int, b:int) -> (int, int, int):
    """
    Computes gcd(a,b) and the Bezout coefficients s and t
    where a*s + b*t = gcd(a,b).
    """
    r0, s0, t0 = a, 1, 0
    r1, s1, t1 = b, 0, 1
    while r1:
        #print(f"({r0}, {s0}, {t0}) -> ({r1}, {s1}, {t1})")
        q = r0//r1
        rNew = r0 % r1
        sNew = s0 - s1*q
        tNew = t0 - t1*q
        r0, s0, t0 = r1, s1, t1
        r1, s1, t1 = rNew, sNew, tNew
    return (r0, s0, t0)

# ==============================
# ===> String Comprehension <===
# ==============================

def compress(s: str) -> str:
    """
    Doc String:
    """
    count = 1
    compStr = ""
    for i in range(len(s)-1):
        if s[i] == s[i+1]:
            count += 1
        else:
            compStr += str(count) if count>1 else s[i]
            compStr += ','
            count = 1
    compStr += str(count) if count>1 else s[-1]
    return compStr

def main():
    print("Hello! We are the elves, and we are here to help.")

if __name__ == "__main__":
    main()
