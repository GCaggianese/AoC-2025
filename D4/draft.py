# SPDX-FileCopyrightText: 2025 Germán Caggianese <german.caggianese@pm.me>
#
# SPDX-License-Identifier: Apache-2.0

def invert(mat):
    return [[1 - mat[i][j]
             for j in range(len(mat[0]))]
            for i in range(len(mat))]

def add_neighbors(mat):
    rows = len(mat)
    cols = len(mat[0])
    result = [[0] * cols for _ in range(rows)]

    directions = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1)
    ]

    for i in range(rows):
        for j in range(cols):
            neighbor_sum = 0
            for di, dj in directions:
                ni, nj = i + di, j + dj
                if 0 <= ni < rows and 0 <= nj < cols:
                    neighbor_sum += mat[ni][nj]
            result[i][j] = mat[i][j] + neighbor_sum

    return result

def elementwise_multiply(mat_a, mat_b):
    return [[mat_a[i][j] * mat_b[i][j]
             for j in range(len(mat_a[0]))]
            for i in range(len(mat_a))]

def correction(mat):
    rows = len(mat)
    cols = len(mat[0])

    # Top and bottom rows
    for j in range(cols):
        mat[0][j] += 3
        mat[rows-1][j] += 3

    # Left and right columns
    for i in range(1, rows-1):
        mat[i][0] += 3
        mat[i][cols-1] += 3

    mat[0][0] += 2
    mat[rows-1][0] += 2
    mat[0][cols-1] += 2
    mat[rows-1][cols-1] += 2

    return mat

def counter(filtered_mat):
    counter = 0
    rows = len(filtered_mat)
    cols = len(filtered_mat[0])
    for i in range(rows):
        for j in range(cols):
            if filtered_mat[i][j] >= 5:
                counter += 1
    return counter

def parse_grid(filename):
    with open(filename, 'r') as f:
        return [[1 if c == '.' else 0 for c in line.strip()]
                for line in f]

in_mat = parse_grid('input.txt')
out_mat = add_neighbors(in_mat)
out_mat = correction(out_mat)
filtered_mat = elementwise_multiply(out_mat, invert(in_mat))

# print("Out_Mat")
# print(out_mat)

# print("Filtered")
# print(filtered_mat)


print("Counter P1:", counter(filtered_mat))
# print(len(filtered_mat))

def copy_matrix(mat):
    return [[mat[i][j] for j in range(len(mat[0]))]
            for i in range(len(mat))]

# ### ### #
# Part 2
counter_p2 = 0
current_mat = copy_matrix(in_mat)

while True:
    out_p2 = add_neighbors(current_mat)
    out_p2 = correction(out_p2)
    filtered_p2 = elementwise_multiply(out_p2, invert(current_mat))

    new_activations = 0
    rows = len(filtered_p2)
    cols = len(filtered_p2[0])

    for i in range(rows):
        for j in range(cols):
            if filtered_p2[i][j] >= 5:
                new_activations += 1
                current_mat[i][j] = 1

    counter_p2 += new_activations

    if new_activations == 0:
        break

print("Counter P2:", counter_p2)
