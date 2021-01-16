package brbo.benchmarks.containers.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngine extends Common {
  void replaceTagsBuilder(int text) {
    int R = 0;
    assert (text < 0 || R <= text);

    int linePointer = 0;
    int startTagLocation = 0;
    int endTagLocation = 0;
    int stringBuilder = 0;
    while (endTagLocation < text - 1) {
      startTagLocation = ndInt2(endTagLocation + 1, text - 1);
      endTagLocation = ndInt2(startTagLocation + 1, text - 1);
      stringBuilder += startTagLocation - linePointer;
      R = R + startTagLocation - linePointer;
      linePointer = endTagLocation;
    }
    stringBuilder += text - linePointer;
    R = R + text - linePointer;
  }
  // No (Imprecise); Yes; Yes (Same as Brbo)
}
