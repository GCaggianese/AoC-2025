# SPDX-FileCopyrightText: 2026 Germán Caggianese（康青旭）<german.caggianese@pm>
#
# SPDX-License-Identifier: Apache-2.0

import re
from typing import Dict, List, Tuple, Set

# =========================
# Section splitting + parse
# =========================

REQ_RE = re.compile(r"^\s*(\d+)\s*x\s*(\d+)\s*:\s*(.+?)\s*$")
HDR_RE = re.compile(r"^\d+\s*:$")
ROW_RE = re.compile(r"^[.#]+$")


def split_sections(text: str) -> Tuple[str, str]:
    """
    Returns (piece_section_text, req_section_text).
    Assumes the requirements section starts at the first line like '4x4: ...'.
    """
    lines = text.splitlines()
    for i, ln in enumerate(lines):
        if REQ_RE.match(ln.strip()):
            return "\n".join(lines[:i]), "\n".join(lines[i:])
    return text, ""


def parse_matrices(piece_text: str) -> List[List[List[int]]]:
    """
    Parses blocks like:
      0:
      ###
      ##.
      ##.

    Returns [matrix0, matrix1, ...] where #->1 and .->0
    Matrices are listed in the order they appear.
    """
    matrices: List[List[List[int]]] = []
    current_grid: List[List[int]] = []

    for ln in piece_text.splitlines():
        s = ln.strip()
        if not s:
            continue

        if HDR_RE.match(s):  # header like "0:"
            if current_grid:
                matrices.append(current_grid)
                current_grid = []
            continue

        if ROW_RE.match(s):  # row like "##."
            current_grid.append([1 if ch == "#" else 0 for ch in s])
            continue

    if current_grid:
        matrices.append(current_grid)

    return matrices


def parse_requirements(req_text: str, num_piece_types: int) -> Dict[Tuple[int, int], List[List[int]]]:
    """
    Parses lines like:
      4x4: 0 0 0 0 2 0
      12x5: 1 0 1 0 2 2
      12x5: 1 0 1 0 3 2

    Returns dict: {(h,w): [[c0..c5], [c0..c5], ...]}.
    Multiple lines with the same (h,w) are multiple scenarios (NOT an error).
    """
    reqs: Dict[Tuple[int, int], List[List[int]]] = {}

    for ln in req_text.splitlines():
        ln = ln.strip()
        if not ln:
            continue

        m = REQ_RE.match(ln)
        if not m:
            continue  # or raise if you want strictness

        h = int(m.group(1))
        w = int(m.group(2))
        counts = [int(x) for x in m.group(3).split()]

        if len(counts) != num_piece_types:
            raise ValueError(
                f"Expected {num_piece_types} counts for {h}x{w}, got {len(counts)}: {ln}"
            )

        reqs.setdefault((h, w), []).append(counts)

    return reqs


# =========================
# Piece rotations + placements (bitmasks)
# =========================

def mat3_to_coords(mat3: List[List[int]]) -> Set[Tuple[int, int]]:
    """Return {(r,c), ...} where mat3[r][c] == 1 (expects 3x3)."""
    coords: Set[Tuple[int, int]] = set()
    for r in range(3):
        for c in range(3):
            if mat3[r][c]:
                coords.add((r, c))
    return coords


def rot90_coords(coords: Set[Tuple[int, int]], k: int = 3) -> Set[Tuple[int, int]]:
    """Rotate coords 90° clockwise inside a kxk frame."""
    return {(c, k - 1 - r) for (r, c) in coords}


def coords_key(coords: Set[Tuple[int, int]]) -> Tuple[Tuple[int, int], ...]:
    return tuple(sorted(coords))


def all_orientations_3x3(mat3: List[List[int]]) -> List[Set[Tuple[int, int]]]:
    """
    Return unique rotations (0,90,180,270) as coord-sets.
    Dedupes in case the piece is rotationally symmetric.
    """
    seen = set()
    out: List[Set[Tuple[int, int]]] = []
    cur = mat3_to_coords(mat3)
    for _ in range(4):
        key = coords_key(cur)
        if key not in seen:
            seen.add(key)
            out.append(cur)
        cur = rot90_coords(cur, 3)
    return out


def coords_to_board_mask(coords: Set[Tuple[int, int]], board_h: int, board_w: int, dr: int, dc: int) -> int:
    """
    Place piece coords onto an HxW board with top-left offset (dr,dc),
    returning an integer bitmask with bit (r*board_w + c) set if occupied.
    """
    m = 0
    for r, c in coords:
        rr, cc = r + dr, c + dc
        bit = rr * board_w + cc
        m |= 1 << bit
    return m


def all_placements_for_piece_3x3(mat3: List[List[int]], board_h: int, board_w: int) -> List[int]:
    """
    All legal placements (as board bitmasks) of this 3x3 piece on an HxW board,
    allowing rotations, no reflections.
    """
    if board_h < 3 or board_w < 3:
        return []

    placements: List[int] = []
    for orient in all_orientations_3x3(mat3):
        for dr in range(board_h - 3 + 1):
            for dc in range(board_w - 3 + 1):
                placements.append(coords_to_board_mask(orient, board_h, board_w, dr, dc))
    return placements


# =========================
# Solver: is requirement possible?
# =========================

def build_piece_list(counts: List[int]) -> List[int]:
    """Expand [c0,c1,...] into a list like [0,0,4,4,4,...]"""
    out: List[int] = []
    for pid, c in enumerate(counts):
        out.extend([pid] * c)
    return out


def is_case_possible(
    board_h: int,
    board_w: int,
    counts: List[int],
    mats: List[List[List[int]]],
    placements_cache: Dict[Tuple[int, int, int], List[int]],
) -> bool:
    """
    Returns True iff we can place exactly the required multiset of pieces
    on an empty board without overlaps (rotations allowed).
    """
    piece_ids = build_piece_list(counts)
    if not piece_ids:
        return True

    # Quick necessary condition: total occupied cells must fit
    # Each piece has popcount equal to number of 1s in its 3x3.
    piece_cells = [sum(sum(row) for row in mats[i]) for i in range(len(mats))]
    total_cells = sum(piece_cells[pid] for pid in piece_ids)
    if total_cells > board_h * board_w:
        return False

    # Build placements list per piece id (cached)
    per_piece_placements: Dict[int, List[int]] = {}
    for pid in set(piece_ids):
        key = (board_h, board_w, pid)
        if key not in placements_cache:
            placements_cache[key] = all_placements_for_piece_3x3(mats[pid], board_h, board_w)
        per_piece_placements[pid] = placements_cache[key]
        if not per_piece_placements[pid]:
            return False  # cannot place this piece on this board

    # Order the expanded piece list to prune: place the "hardest" pieces first
    piece_ids.sort(key=lambda pid: len(per_piece_placements[pid]))

    # Memoize by (index, occupied_mask)
    memo: Set[Tuple[int, int]] = set()

    def dfs(i: int, occ: int) -> bool:
        if i == len(piece_ids):
            return True

        state = (i, occ)
        if state in memo:
            return False

        pid = piece_ids[i]
        for pmask in per_piece_placements[pid]:
            if (pmask & occ) == 0:
                if dfs(i + 1, occ | pmask):
                    return True

        memo.add(state)
        return False

    return dfs(0, 0)


def count_possible_cases(
    reqs: Dict[Tuple[int, int], List[List[int]]],
    mats: List[List[List[int]]],
) -> Tuple[int, int, List[Tuple[int, int, int]]]:
    """
    Returns:
      (possible_count, total_count, list_of_possible_cases)

    list_of_possible_cases contains tuples: (board_h, board_w, case_index)
    """
    placements_cache: Dict[Tuple[int, int, int], List[int]] = {}
    possible = 0
    total = 0
    possible_cases: List[Tuple[int, int, int]] = []

    for (h, w), scenarios in sorted(reqs.items()):
        for case_idx, counts in enumerate(scenarios):
            total += 1
            ok = is_case_possible(h, w, counts, mats, placements_cache)
            if ok:
                possible += 1
                possible_cases.append((h, w, case_idx))

    return possible, total, possible_cases


# =========================
# Debug printing
# =========================

def show_piece(mat: List[List[int]]) -> None:
    for row in mat:
        print("".join("#" if v else "." for v in row))


# =========================
# Main
# =========================

def main() -> None:
    # path = "../../input_test.txt"
    path = "input.txt"
    with open(path, "r", encoding="utf-8") as f:
        text = f.read()

    piece_text, req_text = split_sections(text)
    mats = parse_matrices(piece_text)

    num_piece_types = len(mats)
    reqs = parse_requirements(req_text, num_piece_types) if req_text.strip() else {}

    print("How many piece matrices:", len(mats))
    print()

    for i, mat in enumerate(mats):
        print(f"Piece {i}:")
        show_piece(mat)
        print()

    if reqs:
        print("Requirements:")
        for (h, w), scenarios in sorted(reqs.items()):
            for s_idx, counts in enumerate(scenarios):
                print(f"{h}x{w} (case {s_idx}): {counts}")
        print()

        possible, total, possible_cases = count_possible_cases(reqs, mats)
        print(f"Possible cases: {possible}/{total}")
        print("These are possible (board_h, board_w, case_index):")
        for t in possible_cases:
            print(" ", t)


if __name__ == "__main__":
    main()
