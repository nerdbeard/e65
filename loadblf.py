#!/usr/bin/env python2

def load(stream):
    from struct import unpack, error
    def read(): return unpack("<H", stream.read(2))[0]
    magic = read()
    assert magic == 0xffff, "Not a binary load file; magic was %r" % (magic,)
    memory = list('\x00'*0xffff)
    while stream:
        try:
            start, end = read(), read()
        except error:
            break
        length = end - start + 1
        data = list(stream.read(length))
        memory[start:end+1] = data
        print "Read %d bytes to %x->%x" % (length, start, end)
    return memory

def main():
    from sys import argv
    file(argv[2], 'w').write("".join(load(file(argv[1]))))

if __name__ == "__main__":
    main()
