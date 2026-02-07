# SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
#
# SPDX-License-Identifier: Apache-2.0

import std/[tables, strutils, sets, os]

type
  Graph = Table[string, seq[string]]

proc parseRules(input: string): Graph =
  var g: Graph
  for line in input.splitLines:
    let ln = line.strip
    if ln.len == 0: continue
    let parts = ln.split(":", maxsplit = 1)
    let key = parts[0].strip
    let values = parts[1].strip.splitWhitespace
    g[key] = values
  result = g

var outCount = 0

proc dfs(g: Graph, node: string, onStack: var HashSet[string]) =
  if node == "out":
    inc outCount
    return
  if node in onStack:
    return

  onStack.incl node
  for nxt in g.getOrDefault(node, @[]):
    dfs(g, nxt, onStack)
  onStack.excl node

when isMainModule:
  let path = if paramCount() >= 1: paramStr(1) else: "input.txt"
  let content = readFile(path)

  let graph = parseRules(content)
  var stack = initHashSet[string]()
  dfs(graph, "you", stack)

  echo "out reached: ", outCount
