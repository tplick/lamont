#!/usr/bin/env python3

import hashlib
import sys

md5 = hashlib.md5()

last_line = ""

for line in sys.stdin:
    print(line, end="")
    if ": [" in line:
        md5.update(line.encode('utf8'))
    last_line = line

print(md5.hexdigest())

