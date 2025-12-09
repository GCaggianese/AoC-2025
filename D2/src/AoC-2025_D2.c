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

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
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

    printf("\n---------------------\n");
    printf("Sum of invalid IDs: %lld\n", sum);

    return 0;
}
