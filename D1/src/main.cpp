// SPDX-FileCopyrightText: 2025 Germán Caggianese <german.caggianese@pm.me>
//
// SPDX-License-Identifier: GPL-3.0-or-later

#include <charconv>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>

int dial = 50;
int clicker = 0;
int new_clicker = 0;

template <typename T> T floor_mod(T a, T b) {
    return a - b * std::floor(static_cast<double>(a) / static_cast<double>(b));
}

void fright(int r) { dial = floor_mod((dial + r), 100); }

void fleft(int l) { dial = floor_mod((dial - l), 100); }

void click() {
    if (dial == 0) {
        clicker += 1;
    }
}

void new_protocol_click_l(int rotation) {
    int from_zero = rotation / 100;
    new_clicker = new_clicker + from_zero;

    int next_dial = floor_mod((dial - rotation), 100);
    if (next_dial > dial && dial != 0 && next_dial != 0) {
        new_clicker += 1;
    }
    std::cout << new_clicker << "\n";
}

void new_protocol_click_r(int rotation) {
    int from_zero = rotation / 100;
    new_clicker = new_clicker + from_zero;

    int next_dial = floor_mod((dial + rotation), 100);
    if (next_dial < dial && dial != 0 && next_dial != 0) {
        new_clicker += 1;
    }
    std::cout << new_clicker << "\n";
}

void parse_file(std::string_view filepath) {
    std::ifstream file(filepath.data());

    for (std::string line; std::getline(file, line);) {
        if (line.empty())
            continue;

        char direction = line[0];

        int value = 0;
        auto [ptr, ec] =
            std::from_chars(line.data() + 1, line.data() + line.size(), value);

        if (ec == std::errc{}) { // Success
            std::cout << line << '\n';

            if (direction == 'L') {
                new_protocol_click_l(value);
                fleft(value);
                click();
                std::cout << "Dial position: " << dial << "\n";
            } else if (direction == 'R') {
                new_protocol_click_r(value);
                fright(value);
                click();
                std::cout << "Dial position: " << dial << "\n";
            }
        }
        if (ec != std::errc{}) { // Fail
            std::cerr << "Parse error on line: " << line << '\n';
            continue;
        }
    }
}

auto main(int argc, char *argv[]) -> int {

    std::cout << "Hey AoC 2025!\n";

    parse_file(argv[1]);
    std::cout << "Clicks: " << clicker << "\n";
    std::cout << "Clicks New Protocol: " << new_clicker << "\n";

    std::cout << "Clicks answer Part Two: " << (new_clicker + clicker) << "\n";

    return 0;
}
