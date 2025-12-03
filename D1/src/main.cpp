#include <charconv>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>

int dial = 50;

template <typename T> T floor_mod(T a, T b) {
    return a - b * std::floor(static_cast<double>(a) / static_cast<double>(b));
}

void fright(int r) { dial = floor_mod((dial + r), 99); }

void fleft(int l) { dial = floor_mod((dial - l), 99); }

int click(int clicker) {
    int counter = 0;
    if (dial == 0) {
        counter += 1;
    }
    return clicker + counter;
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
                fleft(value);
                std::cout << "Dial position: " << dial << "\n";
            } else if (direction == 'R') {
                fright(value);
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

    return 0;
}
