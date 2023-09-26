package picante.math.intervals;

import static picante.math.PicanteMath.sqrt;

/**
 * an interval that also has a predetermined value associated with it; usually, this is some kind of
 * custom specified midpoint; for log intervals, this will often be the geometric mean, and for
 * linear intervals, it will be the average; for intervals based on an indexing scheme, the value
 * could be the index (so on the interval [0 to 1) (closed on 0, on on 1), the "mid point" value is
 * 0)
 * 
 * @author vandejd1
 */
public class IntervalWithValue {
  private final double midValue;
  private final UnwritableInterval interval;

  public IntervalWithValue(IntervalWithValue other) {
    this(other.interval.begin, other.interval.end, other.midValue);
  }

  public IntervalWithValue(double begin, double end, double midValue) {
    this.interval = new UnwritableInterval(begin, end);
    this.midValue = midValue;
  }

  /**
   * the underlying interval
   * 
   * @return
   */
  public UnwritableInterval getInterval() {
    return interval;
  }

  /**
   * convenience method for getting the beginning of the underlying interval
   * 
   * @return
   */
  public double getBegin() {
    return interval.getBegin();
  }

  /**
   * convenience method for getting the end of the underlying interval
   * 
   * @return
   */
  public double getEnd() {
    return interval.getEnd();
  }

  /**
   * the value to associated with this interval, usually a midpoint of some kind, like a geometric
   * mean, a linear mean, or (for histograms built on indexes) the index to associate with the
   * interval
   * 
   * @return
   */
  public double getMidValue() {
    return midValue;
  }

  @Override
  public String toString() {
    return "[" + interval.begin + ", (" + midValue + "), " + interval.end + "]";
  }

  public static IntervalWithValue createFromInterval(Interval interval,
      boolean createGeometricMeanValue) {
    if (createGeometricMeanValue) {
      double mean = sqrt(interval.getBegin() * interval.getEnd());
      return new IntervalWithValue(interval.getBegin(), interval.getEnd(), mean);
    } else {
      double mean = (interval.getBegin() + interval.getEnd()) / 2.0;
      return new IntervalWithValue(interval.getBegin(), interval.getEnd(), mean);
    }

  }

}
