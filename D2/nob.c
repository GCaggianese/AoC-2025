/*
 * Copyright (C) 2024 Alexey Kutepov
 * SPDX-FileCopyrightText: 2024 Alexey Kutepov
 *
 * SPDX-License-Identifier: Unlicense
 */
#define NOB_IMPLEMENTATION
#include "nob.h"

#define BUILD_FOLDER "build/"
#define SRC_FOLDER "src/"

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);

    if (!nob_mkdir_if_not_exists(BUILD_FOLDER))
        return 1;

    Nob_Cmd cmd = {0};

    nob_cmd_append(&cmd, "gcc", "-Wall", "-Wextra", "-o",
                   BUILD_FOLDER "AoC-2025_D2", SRC_FOLDER "AoC-2025_D2.c");
    if (!nob_cmd_run(&cmd))
        return 1;

    return 0;
}
