/*
 * SPDX-FileCopyrightText: 2025 Germán Caggianese <german.caggianese@pm.me>
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

long long sum = 0;
long long sum_p2 = 0;

int is_divisor_valid(long long value, int digits, int p) {
    // Calculate M = (10^digits - 1) / (10^p - 1)
    long long numerator = (long long)pow(10, digits) - 1;
    long long denominator = (long long)pow(10, p) - 1;
    long long M = numerator / denominator;

    if (value % M != 0)
        return 0;

    // Check pattern has exactly p digits (no leading zeros)
    long long pattern = value / M;
    int pattern_digits = (pattern == 0) ? 1 : (int)floor(log10(pattern)) + 1;

    return (pattern_digits == p);
}

int is_invalid(long long value) {
    int digits = (int)floor(log10(value)) + 1;

    // Try all divisors of digits
    for (int p = 1; p < digits; p++) {
        if (digits % p == 0) { // p is a divisor
            if (is_divisor_valid(value, digits, p) == 1) {
                return 1; // Found a valid repetition
            }
        }
    }

    return 0;
}

void checker_p2(long long blimit, long long tlimit) {
    printf("Checking range: %lld-%lld\n", blimit, tlimit);

    for (long long value = blimit; value <= tlimit; value++) {
        if (is_invalid(value) == 1) {
            printf("  Invalid ID: %lld\n", value);
            sum_p2 += value;
        }
    }
}

void checker(long long blimit, long long tlimit) {
    printf("Range: %lld-%lld\n", blimit, tlimit);

    for (long long value = blimit; value <= tlimit; value++) {
        int digits = (int)floor(log10((double)value)) + 1;
        if (digits % 2 != 0)
            continue; // odd digits, always valid

        long long order = (long long)pow(10, digits / 2);
        long long multiplier = order + 1;

        if (value % multiplier == 0) {
            printf("  Invalid ID: %lld\n", value);
            sum += value;
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t size = 0;
    ssize_t length;

    while ((length = getline(&line, &size, file)) != -1) {
        printf("Processing line: %s", line);

        char *token = strtok(line, ",");
        while (token != NULL) {
            long long blimit, tlimit;
            if (sscanf(token, "%lld-%lld", &blimit, &tlimit) == 2) {
                checker(blimit, tlimit);
            }
            token = strtok(NULL, ",");
        }
    }

    free(line);
    fclose(file);

    FILE *file_p2 = fopen("input_p2.txt", "r");
    if (!file_p2) {
        perror("Error opening file");
        return 1;
    }

    char *line_p2 = NULL;
    size_t size_p2 = 0;
    ssize_t length_p2;

    while ((length_p2 = getline(&line_p2, &size_p2, file_p2)) != -1) {
        printf("Processing line: %s", line_p2);

        char *token = strtok(line_p2, ",");
        while (token != NULL) {
            long long blimit, tlimit;
            if (sscanf(token, "%lld-%lld", &blimit, &tlimit) == 2) {
                checker_p2(blimit, tlimit);
            }
            token = strtok(NULL, ",");
        }
    }

    free(line_p2);
    fclose(file_p2);

    printf("\n------------ p1 ------------\n");
    printf("Sum of invalid IDs: %lld\n", sum);
    printf("\n");
    printf("\n------------ p2 ------------\n");
    printf("Sum of invalid IDs: %lld\n", sum_p2);
    return 0;
}
