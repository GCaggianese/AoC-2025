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
