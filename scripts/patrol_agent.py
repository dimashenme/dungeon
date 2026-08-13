#!/usr/bin/env python3

"""Horizontal patrol agent for Dungeon's version-1 console protocol.

Usage: patrol_agent.py LOWER_X UPPER_X
"""

import sys


def read_block():
    block = []
    for raw_line in sys.stdin:
        line = raw_line.rstrip("\n")
        if not line:
            return block
        block.append(line)
    return None


def send(command):
    print(command, flush=True)


def fields(line):
    return dict(
        field.split("=", 1)
        for field in line.split()[1:]
        if "=" in field
    )


def fail(message):
    print(f"patrol agent: {message}", file=sys.stderr)
    raise SystemExit(1)


if len(sys.argv) != 3:
    fail("usage: patrol_agent.py LOWER_X UPPER_X")

lower, upper = sorted(map(int, sys.argv[1:]))
ready = read_block()
if not ready or not ready[0].startswith("ready version=1 "):
    fail("expected a version-1 ready block")

send("c")
character = read_block()
if not character or not character[0].startswith("character "):
    fail("expected character data")

direction = "l"
while True:
    send("s")
    area = read_block()
    if area is None:
        break
    if not area or not area[0].startswith("area "):
        fail("expected an area observation")

    try:
        x = int(fields(area[0])["pos"].split(",", 1)[0])
    except (KeyError, ValueError):
        fail("malformed area position")

    if x < lower:
        direction = "l"
    elif x > upper:
        direction = "h"
    elif lower == upper:
        direction = "."
    elif x == lower:
        direction = "l"
    elif x == upper:
        direction = "h"

    send(direction)
    result = read_block()
    if result is None:
        break
    if not result or not (
        result[0].startswith("ok turn=")
        or result[0].startswith("held turn=")
    ):
        fail("expected an admitted or held turn result")
