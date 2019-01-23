#!/usr/bin/env python2

def main():
    from sys import stdin
    print("\n".join("%x -> %x @ %x"%seg for seg in parse(stdin)))

def parse(stream):
    from struct import unpack, error
    def read(): return unpack("<H", stream.read(2))[0]
    magic = read()
    assert magic == 0xffff, "Not a binary load file; magic was %r" % (magic,)
    results = []
    while stream:
        try:
            addrs = (read(), read(), stream.tell())
        except error:
            break
        results.append(addrs)
        stream.seek(addrs[1] - addrs[0] + 1)
    return results

if __name__ == "__main__":
    main()
