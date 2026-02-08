# SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
#
# SPDX-License-Identifier: Apache-2.0

import std/[tables, strutils, sets, os]

type
  Graph = Table[string, seq[string]]
  RevGraph = Table[string, seq[string]] # child -> parents

# -------------------------
# Parsers (same now; you can modify Part2 later)
# -------------------------
proc parseRulesPart1(input: string): Graph =
  var g: Graph
  for line in input.splitLines:
    let ln = line.strip
    if ln.len == 0: continue
    if ln.startsWith("#"): continue
    let parts = ln.split(":", maxsplit = 1)
    if parts.len != 2: continue
    let key = parts[0].strip
    let values = parts[1].strip.splitWhitespace
    g[key] = values
  result = g

proc parseRulesPart2(input: string): Graph =
  var g: Graph
  for line in input.splitLines:
    let ln = line.strip
    if ln.len == 0: continue
    if ln.startsWith("#"): continue
    let parts = ln.split(":", maxsplit = 1)
    if parts.len != 2: continue
    let key = parts[0].strip
    let values = parts[1].strip.splitWhitespace
    g[key] = values
  result = g

# -------------------------
# Reverse graph + reachability sets (for pruning)
# -------------------------
proc buildReverse(g: Graph): RevGraph =
  var rev: RevGraph
  for parent, kids in g.pairs:
    for child in kids:
      rev.mgetOrPut(child, @[]).add(parent)
  result = rev

proc canReach(rev: RevGraph, target: string): HashSet[string] =
  var seen = initHashSet[string]()
  var q: seq[string] = @[target]
  seen.incl(target)
  var i = 0
  while i < q.len:
    let cur = q[i]
    inc i
    for p in rev.getOrDefault(cur, @[]):
      if p notin seen:
        seen.incl(p)
        q.add(p)
  result = seen

# -------------------------
# Cycle detection (needed to know if DP is valid)
# -------------------------
proc hasCycle(g: Graph): bool =
  var color = initTable[string, uint8]() # 0=unseen, 1=visiting, 2=done

  proc visit(u: string): bool =
    let c = color.getOrDefault(u, 0'u8)
    if c == 1'u8: return true
    if c == 2'u8: return false
    color[u] = 1'u8
    for v in g.getOrDefault(u, @[]):
      if visit(v): return true
    color[u] = 2'u8
    false

  for k in g.keys:
    if color.getOrDefault(k, 0'u8) == 0'u8:
      if visit(k): return true
  false

# -------------------------
# Part 1: DP count (DAG only) OR DFS (fallback)
# -------------------------
proc countAllDP(
  g: Graph,
  node: string,
  memo: var Table[string, int64],
  canOut: HashSet[string]
): int64 =
  if node == "out": return 1
  if node notin canOut: return 0
  if memo.hasKey(node): return memo[node]
  var acc: int64 = 0
  for nxt in g.getOrDefault(node, @[]):
    acc += countAllDP(g, nxt, memo, canOut)
  memo[node] = acc
  acc

proc dfsCountAll(g: Graph, node: string, onStack: var HashSet[string],
    outCount: var int64) =
  if node == "out":
    inc outCount
    return
  if node in onStack:
    return
  onStack.incl node
  for nxt in g.getOrDefault(node, @[]):
    dfsCountAll(g, nxt, onStack, outCount)
  onStack.excl node

# -------------------------
# Part 2: DP with 2-bit mask (DAG only)
# mask bit0 = seen fft, bit1 = seen dac
# -------------------------
proc countMaskDP(
  g: Graph,
  node: string,
  maskIn: int,
  memo: var Table[(string, int), int64],
  canOut: HashSet[string],
  canFft: HashSet[string],
  canDac: HashSet[string]
): int64 =
  # pruning: if can't reach out from here, no paths
  if node != "out" and (node notin canOut):
    return 0

  # memo key is (node, maskIn) BEFORE consuming current node
  let key = (node, maskIn)
  if memo.hasKey(key): return memo[key]

  var mask = maskIn
  if node == "fft": mask = mask or 1
  if node == "dac": mask = mask or 2

  # pruning: if we still need fft/dac but can't reach them from here, stop
  if ((mask and 1) == 0) and (node notin canFft): return 0
  if ((mask and 2) == 0) and (node notin canDac): return 0

  if node == "out":
    return (if (mask and 3) == 3: 1 else: 0)

  var acc: int64 = 0
  for nxt in g.getOrDefault(node, @[]):
    acc += countMaskDP(g, nxt, mask, memo, canOut, canFft, canDac)

  memo[key] = acc
  acc

# -------------------------
# Part 2 fallback: pruned DFS for cyclic graphs (still no storing)
# -------------------------
proc dfsCountWithFlagsPruned(
  g: Graph,
  node: string,
  onStack: var HashSet[string],
  seenFft: bool,
  seenDac: bool,
  finalCount: var int64,
  canOut: HashSet[string],
  canFft: HashSet[string],
  canDac: HashSet[string]
) =
  if node != "out" and (node notin canOut):
    return

  let nowSeenFft = seenFft or (node == "fft")
  let nowSeenDac = seenDac or (node == "dac")

  if (not nowSeenFft) and (node notin canFft): return
  if (not nowSeenDac) and (node notin canDac): return

  if node == "out":
    if nowSeenFft and nowSeenDac:
      inc finalCount
    return

  if node in onStack:
    return

  onStack.incl node
  for nxt in g.getOrDefault(node, @[]):
    dfsCountWithFlagsPruned(g, nxt, onStack, nowSeenFft, nowSeenDac, finalCount,
        canOut, canFft, canDac)
  onStack.excl node

# -------------------------
# Runs
# -------------------------
proc runPart1(content: string, startNode: string): int64 =
  let g1 = parseRulesPart1(content)
  let rev1 = buildReverse(g1)
  let canOut1 = canReach(rev1, "out")

  if not hasCycle(g1):
    var memo = initTable[string, int64]()
    return countAllDP(g1, startNode, memo, canOut1)
  else:
    var stack = initHashSet[string]()
    var cnt: int64 = 0
    dfsCountAll(g1, startNode, stack, cnt)
    return cnt

proc runPart2(content: string, startNode: string): int64 =
  let g2 = parseRulesPart2(content)
  let rev2 = buildReverse(g2)
  let canOut = canReach(rev2, "out")
  let canFft = canReach(rev2, "fft")
  let canDac = canReach(rev2, "dac")

  if not hasCycle(g2):
    var memo = initTable[(string, int), int64]()
    return countMaskDP(g2, startNode, 0, memo, canOut, canFft, canDac)
  else:
    var stack = initHashSet[string]()
    var cnt: int64 = 0
    dfsCountWithFlagsPruned(g2, startNode, stack, false, false, cnt, canOut,
        canFft, canDac)
    return cnt

when isMainModule:
  let filePath = if paramCount() >= 1: paramStr(1) else: "input.txt"
  let content = readFile(filePath)

  let part1Count = runPart1(content, "you")
  echo "part1 out reached: ", part1Count

  let part2Count = runPart2(content, "svr")
  echo "part2 out reached: ", part2Count
