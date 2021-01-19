package brbo.archive.containers.stac;

import brbo.benchmarks.Common;

public abstract class TemplateEngineArc extends Common {
  void replaceTagsBuilder(String text) {
    int r = 0;
    assert (r <= text.length());

    int linePointer = 0;
    int startTagLocation = 0;
    int endTagLocation = 0;
    StringBuilder stringBuilder = new StringBuilder();
    while (endTagLocation < text.length() - 1) {
      startTagLocation = ndInt();
      assume(startTagLocation > endTagLocation && startTagLocation < text.length());
      endTagLocation = ndInt();
      assume(startTagLocation < endTagLocation && endTagLocation < text.length());
      String string = text.substring(linePointer, startTagLocation);
      stringBuilder.append(string);
      r = r + string.length();
      linePointer = endTagLocation;
    }
    String string = text.substring(linePointer, text.length());
    stringBuilder.append(string);
    r = r + string.length();
  }
}
