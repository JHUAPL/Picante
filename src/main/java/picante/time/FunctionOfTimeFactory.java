package picante.time;

import picante.math.functions.UnivariateFunction;

/**
 * factory methods for creating FunctionOfTime objects; (the traditional name for this class would
 * have been FuntionOfTimes, but that seemed pretty awkward)
 */
public class FunctionOfTimeFactory {
  /**
   * adapt a univariate function that takes ET to a FunctionOfTime
   */
  public static FunctionOfTime wrap(final UnivariateFunction univarFunc) {
    return new FunctionOfTime() {
      private final TimeAdapter ta = TimeAdapter.getInstance();

      @Override
      public double evaluate(TSEpoch tsEpoch) {
        return univarFunc.evaluate(ta.getET(tsEpoch));
      }
    };
  }
}
