package brbo.benchmarks;

public abstract class Common {
  public int INT_SIZE = 4;
  public int BOOL_SIZE = 1;

  /**
   *
   * @return Non-deterministic integer
   */
  public abstract int ndInt();

  /**
   *
   * @return Non-deterministic boolean
   */
  public abstract boolean ndBool();

  public abstract void assume(boolean expression);

  /**
   *
   * @param args All heap locations (represented by variable names) that
   *             can reach the heap location pointed to by x, via accessing
   *             elements inside containers
   */
  public abstract void reach(int x, int... args);
}
