package picante.data.list;

import com.google.common.base.Function;

/**
 * Static utility methods pertaining to {@link Gaugeable}s.
 * 
 */
public final class Gaugeables {

  private Gaugeables() {}

  /**
   * Adapts a standard Guava function to a {@link Gaugeable}.
   * 
   * @param converter the function to adapt
   * 
   * @return a newly created {@link Gaugeable} that just invokes the apply method on the supplied
   *         function.
   */
  public static <T> Gaugeable<T> from(final Function<T, Double> converter) {
    return new Gaugeable<T>() {

      @Override
      public double gauge(T t) {
        return converter.apply(t);
      }
    };
  }


  /**
   * The identity gauge is for an Indexable<Double> that is its own gauge. (Note that this method
   * deals only with type Double - its not generic!)
   * 
   * @return a new {@link Gaugeable} that just returns the value
   */
  public static Gaugeable<Double> identity() {
    return new Gaugeable<Double>() {
      @Override
      public double gauge(Double t) {
        return t;
      }
    };
  }


}
