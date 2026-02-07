// SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
//
// SPDX-License-Identifier: Apache-2.0

use regex::Regex;
use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone)]
struct Column {
    vector: Vec<f64>,
    value: u32,
}

// Use a slightly larger epsilon for stability, but strict for integer checks
const EPSILON: f64 = 1e-4;

fn solve_xor(values: &[u32], target: u32) -> u32 {
    if target == 0 { return 0; }
    let mut queue = VecDeque::from([(0u32, 0u32)]);
    let mut visited = HashSet::new();
    visited.insert(0);

    while let Some((curr, dist)) = queue.pop_front() {
        for &v in values {
            let next = curr ^ v;
            if next == target { return dist + 1; }
            if !visited.contains(&next) {
                visited.insert(next);
                queue.push_back((next, dist + 1));
            }
        }
    }
    0
}

fn is_integer(val: f64) -> bool {
    (val.round() - val).abs() < EPSILON
}

// Returns Option: None if impossible, Some(count) if solvable
fn solve_counter(columns: Vec<Column>, targets: Vec<f64>) -> Option<u64> {
    let num_rows = targets.len();
    let num_cols = columns.len();

    let mut matrix = vec![vec![0.0; num_cols + 1]; num_rows];
    
    // Fill Matrix
    for (col_idx, col) in columns.iter().enumerate() {
        for (row_idx, &val) in col.vector.iter().enumerate() {
            matrix[row_idx][col_idx] = val;
        }
    }
    for (row_idx, &t) in targets.iter().enumerate() {
        matrix[row_idx][num_cols] = t;
    }

    // Gaussian Elimination
    let mut pivot_row = 0;
    let mut pivot_indices = vec![usize::MAX; num_rows]; 
    let mut free_vars = Vec::new();
    let mut col = 0;

    // Track which columns are pivots to identify free variables later
    let mut is_pivot_col = vec![false; num_cols];

    while pivot_row < num_rows && col < num_cols {
        // Find pivot
        let mut max_row = pivot_row;
        for r in pivot_row + 1..num_rows {
            if matrix[r][col].abs() > matrix[max_row][col].abs() {
                max_row = r;
            }
        }

        if matrix[max_row][col].abs() < EPSILON {
            // Column is zero, skip it
            col += 1;
            continue;
        }

        // Swap
        matrix.swap(pivot_row, max_row);
        pivot_indices[pivot_row] = col;
        is_pivot_col[col] = true;

        // Normalize
        let pivot_val = matrix[pivot_row][col];
        for c_iter in col..=num_cols {
            matrix[pivot_row][c_iter] /= pivot_val;
        }

        // Eliminate
        for r in 0..num_rows {
            if r != pivot_row {
                let factor = matrix[r][col];
                if factor.abs() > EPSILON {
                    for c_iter in col..=num_cols {
                        matrix[r][c_iter] -= factor * matrix[pivot_row][c_iter];
                    }
                }
            }
        }

        pivot_row += 1;
        col += 1;
    }

    // Identify Free Variables
    for c in 0..num_cols {
        if !is_pivot_col[c] {
            free_vars.push(c);
        }
    }

    // Check for inconsistency (0 = something)
    for r in pivot_row..num_rows {
        if matrix[r][num_cols].abs() > EPSILON {
            // System is inconsistent
            return None; 
        }
    }

    let mut best_total = u64::MAX;
    let mut current_free_vals = vec![0.0; free_vars.len()];
    
    solve_recursive(
        0,
        &free_vars,
        &matrix,
        &pivot_indices,
        num_cols,
        &mut current_free_vals,
        &mut best_total,
    );

    if best_total == u64::MAX { None } else { Some(best_total) }
}

fn solve_recursive(
    idx: usize,
    free_vars: &[usize],
    matrix: &Vec<Vec<f64>>,
    pivot_indices: &[usize],
    num_cols: usize,
    current_free_vals: &mut Vec<f64>,
    best_total: &mut u64,
) {
    if idx == free_vars.len() {
        let mut current_sum = 0.0;
        
        // Sum free variables
        for &v in current_free_vals.iter() {
            current_sum += v;
        }
        
        // Pruning: if free vars alone exceed best, stop
        if current_sum >= *best_total as f64 { return; }

        // Calculate Pivot Variables
        for r in 0..pivot_indices.len() {
            let p_col = pivot_indices[r];
            if p_col == usize::MAX { break; }

            // P = Target - Sum(Coeff * Free)
            let mut val = matrix[r][num_cols];
            for (i, &f_col) in free_vars.iter().enumerate() {
                val -= matrix[r][f_col] * current_free_vals[i];
            }

            // Strict Integer and Non-Negative Check
            // We allow a tiny tolerance for float errors, then round.
            if val < -0.1 { return; } // Definitely negative
            if !is_integer(val) { return; } // Not an integer

            let rounded = val.round();
            if rounded < 0.0 { return; } // Negative after rounding (e.g. -0.0001 -> 0 is fine, -0.9 -> -1 is bad)
            
            current_sum += rounded;
        }

        let total_int = current_sum as u64;
        if total_int < *best_total {
            *best_total = total_int;
        }
        return;
    }

    // Simplified Search Strategy
    // We just iterate 0..limit. 
    // The coefficients are usually positive in these problems, so valid solutions are clustered near 0.
    // We stop if `val` makes us exceed `best_total`.
    
    // LIMIT: If we don't find a solution in 1000 iterations of a free variable, 
    // it's likely this path is bad or the problem is huge.
    // AoC inputs typically resolve with small coefficients.
    let search_limit = 200; 

    for val in 0..=search_limit {
        let val_f = val as f64;
        
        // Pruning
        let mut partial_sum = val_f;
        for k in 0..idx { partial_sum += current_free_vals[k]; }
        if partial_sum >= *best_total as f64 { break; }

        current_free_vals[idx] = val_f;
        solve_recursive(idx + 1, free_vars, matrix, pivot_indices, num_cols, current_free_vals, best_total);
    }
}

fn main() {
    let test_file = "input.txt";
    if !Path::new(test_file).exists() {
        println!("File {} not found.", test_file);
        return;
    }

    let content = fs::read_to_string(test_file).expect("Failed to read file");
    let re_pattern = Regex::new(r"\[([.#]+)\]").unwrap();
    let re_target = Regex::new(r"\{([0-9,]+)\}").unwrap();
    let re_vector = Regex::new(r"\(([0-9,]+)\)").unwrap();

    let mut grand_total: u64 = 0;

    for (i, line) in content.lines().enumerate() {
        if line.starts_with("[source") || line.is_empty() { continue; }

        let caps_p = match re_pattern.captures(line) {
            Some(c) => c,
            None => continue,
        };
        let caps_t = match re_target.captures(line) {
            Some(c) => c,
            None => continue,
        };

        let pattern_str = &caps_p[1];
        let len = pattern_str.len();
        let target_xor = u32::from_str_radix(&pattern_str.replace('.', "0").replace('#', "1"), 2).unwrap_or(0);
        let targets: Vec<f64> = caps_t[1].split(',').map(|s| s.trim().parse().unwrap_or(0.0)).collect();

        let mut columns = Vec::new();
        for cap in re_vector.captures_iter(line) {
            let mut vec = vec![0.0; len];
            let idxs: Vec<usize> = cap[1].split(',').map(|s| s.trim().parse().unwrap_or(999)).collect();
            let mut b_val = 0u32;
            for &v in &idxs {
                if v < len {
                    vec[v] = 1.0;
                    b_val |= 1 << (len - 1 - v);
                }
            }
            columns.push(Column { vector: vec, value: b_val });
        }

        let xor_result = solve_xor(&columns.iter().map(|c| c.value).collect::<Vec<_>>(), target_xor);
        
        match solve_counter(columns, targets) {
            Some(count) => {
                println!("Line {}: XOR: {} | Counter: {}", i + 1, xor_result, count);
                grand_total += count;
            },
            None => {
                // Problem with logic or impossible inputs.
                println!("Line {}: IMPOSSIBLE to solve!", i + 1);
            }
        }
    }
    
    println!("\nGRAND TOTAL: {}", grand_total);
}
