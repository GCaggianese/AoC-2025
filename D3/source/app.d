// SPDX-FileCopyrightText: 2025 Germán Caggianese <german.caggianese@pm.me>
//
// SPDX-License-Identifier: Apache-2.0

import std.stdio;
import std.algorithm;
import std.range;
import std.conv;
import std.math : pow;
import std.algorithm : maxElement, countUntil;
import std.array : array;


int jolts = 0;
long jolts_p2 = 0;

void outputJoltage(int[] og){
    int[] aux1 = og.dropBack(1);
    auto maxIndex = aux1.maxIndex();

    jolts += aux1.maxElement()*10;

    int[] aux2 = og.drop(maxIndex + 1);
    jolts += aux2.maxElement();
    writeln("Plus: ", aux1.maxElement()*10 + aux2.maxElement());
    writeln("Jolts = ", jolts, "\n");
}


void outputJoltage_p2(long[] og){
    long multiplier = cast(long) pow(10.0, 11);
    string result = "";

    for(int i = 0; i < 12; i++){
        int remaining_needed = 12 - i;
        auto window_size = og.length - remaining_needed + 1;
        long[] window = og[0..window_size].array;
        long maxVal = window.maxElement;
        auto maxIdx = window.countUntil(maxVal);
        result ~= to!string(maxVal);
        jolts_p2 += maxVal * multiplier;
        og = og[maxIdx + 1 .. $];
        multiplier /= 10;
    }

    writeln("Plus: ", result);
    writeln("Jolts = ", jolts_p2);
}

int[] parseLine(string input){
    int[] ret;
    string[] aux = input.split("");
    foreach (i;aux){
        ret ~= parse!int(i);
    }
    writeln(ret);
    return ret;
}

long[] parseLine_p2(string input){
    long[] ret;
    string[] aux = input.split("");
    foreach (i;aux){
        ret ~= parse!long(i);
    }
    writeln(ret);
    return ret;
}

void main()
{

    auto file = File("input.txt", "r");
    foreach (line; file.byLine()) {
        outputJoltage(parseLine(line.idup));
    }
    file.close();

    writeln("--------- p1 result ---------");
    writeln("Jolts: ", jolts, "\n\n");

    auto file_p2 = File("input_2.txt", "r");
    foreach (line; file_p2.byLine()) {
        outputJoltage_p2(parseLine_p2(line.idup));
    }
    file_p2.close();

    writeln("--------- p2 result ---------");
    writeln("Jolts p2: ", jolts_p2);

}
