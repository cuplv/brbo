package brbo.benchmarks.pldi21;

import brbo.benchmarks.Common;

public abstract class LineBreak extends Common {
    public void breakParagraphs(int text, int paraDelim) {
        if (text < 0 || paraDelim < 0) {
            return;
        }
        int curParagraph = 0;
        int totalLines = 0;
        int R = 0;
        int it = 0;
        while (it < text) {
            int line = ndInt2(0, it);
            it -= line;

            if (line != 0) {
                curParagraph += line;
                R += line;
                curParagraph += paraDelim;
                R += paraDelim;
            }
            else {
                totalLines += curParagraph;
                R += curParagraph;
                totalLines += paraDelim;
                R += paraDelim;
                curParagraph = 0;
            }
        }
        totalLines += curParagraph;
    }
}
