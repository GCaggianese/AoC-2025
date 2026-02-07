// SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
//
// SPDX-License-Identifier: Apache-2.0

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

interface XorResult {
  minUses: number;
  valuesUsed: number[];
}

function findMinimumXorSubset(
  values: number[],
  target: number
): XorResult | null {
  // Map of xor_result -> {minUses, valuesUsed}
  const reachable = new Map<number, { uses: number; values: number[] }>();
  
  // Base case: 0 XOR uses gives you 0
  reachable.set(0, { uses: 0, values: [] });
  
  // For each value in our set
  for (const value of values) {
    // Create a snapshot of current reachable states (to avoid modifying while iterating)
    const currentStates = Array.from(reachable.entries());
    
    for (const [xorResult, state] of currentStates) {
      const newXor = xorResult ^ value;
      const newUses = state.uses + 1;
      const newValues = [...state.values, value];
      
      // Update if we found a better path or this is the first path
      if (!reachable.has(newXor) || newUses < reachable.get(newXor)!.uses) {
        reachable.set(newXor, { uses: newUses, values: newValues });
      }
    }
  }
  
  // Check if target is reachable
  if (!reachable.has(target)) {
    return null;
  }
  
  const result = reachable.get(target)!;
  return {
    minUses: result.uses,
    valuesUsed: result.values
  };
}

// Helper function to display binary
function toBinary(num: number, bits: number = 4): string {
  return num.toString(2).padStart(bits, '0');
}

// const filePath = path.join(__dirname, '../input_test.txt');
const filePath = path.join(__dirname, '../input.txt');
const content = fs.readFileSync(filePath, 'utf-8');
const lines = content.split('\n').filter(line => line.trim());

let totalMinUses = 0;

lines.forEach((line, index) => {
  console.log(`\n======= Line ${index + 1} =======`);
  console.log(line);
  console.log('');
  
  // Extract the part between [ ]
  const bracketMatch = line.match(/\[([.#]+)\]/);
  if (bracketMatch && bracketMatch[1]) {
    const binaryString = bracketMatch[1];
    const length = binaryString.length;
    
    // Convert . to 0 and # to 1
    const binary = binaryString.replace(/\./g, '0').replace(/#/g, '1');
    const target = parseInt(binary, 2);
    
    console.log(`Target: [${binaryString}] -> ${toBinary(target, length)} (${target})`);
    
    // Extract all (...) groups
    const parenMatches = Array.from(line.matchAll(/\(([0-9,]+)\)/g));
    const values: number[] = [];
    
    console.log('\nAvailable values:');
    for (const match of parenMatches) {
      if (match[1]) {
        // Parse the positions
        const positions = match[1].split(',').map(n => parseInt(n.trim()));
        
        // Create binary number with 1s at specified positions
        const bits = new Array(length).fill('0');
        for (const pos of positions) {
          if (pos >= 0 && pos < length) {
            bits[pos] = '1';
          }
        }
        
        const resultBinary = bits.join('');
        const resultDecimal = parseInt(resultBinary, 2);
        values.push(resultDecimal);
        
        console.log(`  (${match[1]}) -> ${resultBinary} (${resultDecimal})`);
      }
    }
    
    console.log('\nFinding minimum XOR subset...');
    const result = findMinimumXorSubset(values, target);
    
    if (result) {
      console.log('✓ Solution found!');
      console.log(`Minimum uses: ${result.minUses}`);
      console.log(`Values to XOR: ${result.valuesUsed.map(v => toBinary(v, length)).join(' ⊕ ')}`);
      
      // Verify the result
      const verification = result.valuesUsed.reduce((acc, val) => acc ^ val, 0);
      console.log(`Verification: ${toBinary(verification, length)} ${verification === target ? '✓' : '✗'}`);
      
      totalMinUses += result.minUses;
    } else {
      console.log('✗ No solution found - target is unreachable');
    }
  }
});

console.log('\n=============================');
console.log(`TOTAL MINIMUM USES: ${totalMinUses}`);
console.log('=============================');
