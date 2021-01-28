package brbo.benchmarks.containers.digikam;

import brbo.benchmarks.Common;

abstract public class CompressionDetector1 extends Common {
  void f(int height, int width) {
    if (height <= 0 || width <= 0)
      return;
    int sum = 0;
    int average_top = 0;
    int average_middle = 0;
    int average_bottom = 0;
    int R = 0;
    for (int i = 0; i < height; i++) {
      for (int j = 0; j < width; j += 8) {
        sum = 0;
        for (int k = j; k < 8; k++) {
          sum += ndInt();
        }
        average_top++;
        R = R + 1;
      }
      for (int j2 = 0; j2 < width; j2 += 8) {
        sum = 0;
        for (int k2 = j2; k2 < 8; k2++) {
          sum += ndInt();
        }
        average_middle++;
      }
      for (int j3 = 0; j3 < width; j3 += 8) {
        sum = 0;
        for (int k3 = j3; k3 < 8; k3++) {
          sum += ndInt();
        }
        average_bottom++;
      }
    }
    average_bottom = 0;
    average_middle = 0;
    R = R + (-average_top);
    average_top = 0;
    for (int j4 = 0; j4 < width; j4++) {
      for (int i2 = 0; i2 < height; i2 += 8) {
        sum = 0;
        for (int k4 = i2; k4 < 8; k4++) {
          sum += ndInt();
        }
        average_top++;
        R = R + 1;
      }
      for (int i3 = 0; i3 < height; i3 += 8) {
        sum = 0;
        for (int k5 = i3; k5 < 8; k5++) {
          sum += ndInt();
        }
        average_middle++;
      }
      for (int i4 = 0; i4 < height; i4 += 8) {
        sum = 0;
        for (int k6 = i4; k6 < 8; k6++) {
          sum += ndInt();
        }
        average_bottom++;
      }
    }
  }
}
