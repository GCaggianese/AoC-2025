<!--
SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
SPDX-License-Identifier: Apache-2.0
-->
# Puzzle [Day 10: Factory](https://adventofcode.com/2025/day/10)

## Part One

Find the minimum number of button presses required to configure each machine's indicator lights to match the target state.

* **Input**: List of machines, each containing:
  * Indicator light diagram in `[brackets]`
  * Button wiring schematics in `(parentheses)`
  * Joltage requirements in `{curly braces}` (ignored for Part One)

### Parsing Rules

#### Target State (Indicator Lights)
The indicator light diagram represents the desired binary state:
* `.` → bit is `0` (off)
* `#` → bit is `1` (on)

**Example**: `[.##.]` → `0110`

#### Button Mappings
Each button schematic lists which indicator lights it toggles (XOR operation). The positions are zero-indexed:
* `(3)` → toggles position 3 → binary mask where only bit 3 is set
* `(0,2)` → toggles positions 0 and 2 → binary mask where bits 0 and 2 are set

The binary length is determined by the number of indicator lights.

**Example for 4-bit target** `[.##.]`:
```
(3)     → 0001  (position 3 set)
(1,3)   → 0101  (positions 1, 3 set)
(2)     → 0010  (position 2 set)
(2,3)   → 0011  (positions 2, 3 set)
(0,2)   → 1010  (positions 0, 2 set)
(0,1)   → 1100  (positions 0, 1 set)
```

**Example for 6-bit target** `[.###.#]`:
```
(0,1)   → 110000  (positions 0, 1 set)
```

### Problem Formulation

Given:
* Initial state: all lights off → `0000...0`
* Target state: parsed from indicator diagram
* Available operations: XOR with any button mask

Find the minimum number of button presses (subset of buttons) such that:

$$\bigoplus_{b \in S} b = \text{target}$$

where $S$ is the subset of buttons used, and each button can be pressed 0 or 1 times (pressing twice cancels out).

### Algorithm

Use dynamic programming to explore all reachable XOR states:

1. Initialize `reachable[0] = 0` (zero presses to reach state 0)
2. For each button mask $b$:
   * For each state $s$ in `reachable`:
     * Compute new state: $s' = s \oplus b$
     * Update: `reachable[s'] = min(reachable[s'] , reachable[s] + 1)`
3. Return `reachable[target]`

**Complexity**: $O(n \times 2^k)$ where $n$ is the number of buttons and $k$ is the bit length.

### Example Solutions

**Machine 1**: `[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1)`
* Target: `0110`
* Buttons: `{0001, 0101, 0010, 0011, 1010, 1100}`
* Solution: `(0,2) ⊕ (0,1) = 1010 ⊕ 1100 = 0110`
* **Minimum presses**: `2`

**Machine 2**: `[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4)`
* Target: `00010`
* **Minimum presses**: `3`

**Machine 3**: `[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2)`
* Target: `011101`
* **Minimum presses**: `2`

### Answer
Sum the minimum presses across all machines:
$$2 + 3 + 2 = 7$$

