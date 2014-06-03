import sys
import time
import os

def getSize(name):
    statinfo = os.stat(name)
    return statinfo.st_size

def getEnd(name, bytes):
    f = open(name, 'rb')
    f.seek(-bytes, 2)
    ls = f.read(bytes)
    s = str(ls, "utf8")
    f.close()
    return s

def checkCt(ct):
    if ct % 1000 == 0:
        sys.stderr.write(str(ct) + "\n")
        sys.stderr.flush()

def main():
    ct = 0
    t = 0
    filename = sys.argv[1]
    if not os.path.isfile(filename):
        sys.stderr.write('No such file "%s"' % filename)
        sys.exit(1)
    size = getSize(filename)
    repopulate = "-r" in sys.argv
    killeof = "-k" in sys.argv
    if repopulate:
        lines = [line for line in open(filename)]
        for line in lines:
            print(line, end="")
            sys.stdout.flush()
            ct += 1
            checkCt(ct)
        if killeof:
            print("")
            sys.exit(1)
    while True:
        newSize = getSize(filename)
        if newSize > size:
            print(getEnd(filename, newSize - size)[:-1])
            sys.stdout.flush()
            ct += 1
            checkCt(ct)
            size = newSize
        time.sleep(1)



if __name__ == '__main__':
    main()
