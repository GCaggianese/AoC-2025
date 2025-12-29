// SPDX-FileCopyrightText: 2025 Germán Caggianese <german.caggianese@pm.me>
//
// SPDX-License-Identifier: Apache-2.0

import std.stdio;
import std.algorithm;
import std.range;
import std.conv;

int jolts = 0;

void outputJoltage(int[] og){
    int[] aux1 = og.dropBack(1);
    auto maxIndex = aux1.maxIndex();

    jolts += aux1.maxElement()*10;

    int[] aux2 = og.drop(maxIndex + 1);
    jolts += aux2.maxElement();
    writeln("Plus: ", aux1.maxElement()*10 + aux2.maxElement());
    writeln("Jolts = ", jolts, "\n");
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

void main()
{

    auto file = File("input.txt", "r");
    foreach (line; file.byLine()) {
        outputJoltage(parseLine(line.idup));
    }
    file.close();

    writeln("--------- p1 result ---------");
    writeln("Jolts: ", jolts);
}
