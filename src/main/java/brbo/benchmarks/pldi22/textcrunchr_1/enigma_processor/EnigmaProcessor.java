package brbo.benchmarks.pldi22.textcrunchr_1.enigma_processor;

import brbo.benchmarks.Common;

public abstract class EnigmaProcessor extends Common {
    void f(int inps) {
        if (inps < 0) {
            return;
        }

        int R = 0;
        int sb = 0;
        int it = inps;
        while (it > 0) {
            int line = ndInt2(1, it);
            it -= line;
            sb += line;
            R += line;
        }

        int theString = sb;
        R += sb;

        // EnigmaMachine machine = EnigmaFactory.buildEnigmaMachine();
        R += 52;
        R += 52;
        R += 52;
        R += 26;

        int sb2 = 0;
        for (int i = 0; i < theString; i++) {
            if (ndBool()) {
                ;
            }
            sb2 += 1;
            R += 1;
        }

        int name = 32; // String name = "Enigma transformation (5, 9, 14)";
        int value = sb2; // String value = encodedString;

        // Deallocations
        R -= sb;
        R -= theString;
    }
}