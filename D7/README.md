<!--
SPDX-FileCopyrightText: 2025 Germán Caggianese <german.caggianese@pm.me>
SPDX-License-Identifier: Apache-2.0
-->
# Puzzle [[https://adventofcode.com/2025/day/7][Day 7: Laboratories]]
## Part One
Process top-to-bottom. For each cell `a_{i,j}`:
- `.` → do nothing
- `S` or `|` → look at $a_{i+1,j}$ (cell below):
  - if `.` → change it to `|`
  - if `^` → $a_{i+1,j-1}$ and $a_{i+1,j+1}$ become `|`
    - **Important**: mark `^` as used
Count how many `^` splitters are marked as used.

## Part Two
Count all possible timelines when `^` splits go **one direction at a time**.

For each cell `a_{i,j}`:
- `col >= cols` → return 1 (completed path)
- `row >= rows` → return 1 (reached bottom)
- `^` at $a_{r,col}$ → recurse:
  - `countPaths(r+1, col-1)` + `countPaths(r+1, col+1)`
  - if `col == 0` → left branch returns 1 (off-grid = completed)

### Example
```
.S.        .S.   .S.
...   →    .|.   .|.
.^.        |^.   .^|
...        |..   ..|
..^        |.^   .|^
```
Left path: 1 (reaches bottom)
Right path: hits `^` at (4,2) → splits again → 2
**Total: 3 timelines**

### Optimization
Memoization via `HashMap(State, usize)` where `State = {row, col}`.
Avoids recalculating paths from the same position.
