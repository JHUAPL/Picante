package picante.math.intervals;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.Comparator;

import com.google.common.base.Function;
import picante.designpatterns.Writable;

/**
 * Modifiable child component of the weak-immutability design pattern capturing a single interval on
 * the finite double precision line.
 * <p>
 * Methods for adjusting the contents of an interval are included here, as well as general
 * functionality applicable to intervals in general.
 * </p>
 * 
 * @see UnwritableInterval
 */
public class Interval extends UnwritableInterval
    implements Writable.ImplementationInterface<UnwritableInterval, Interval> {

  /**
   * Comparator used to sort a list of {@link UnwritableInterval} by their beginnings.
   */
  public static final Comparator<UnwritableInterval> BEGIN_COMPARATOR =
      new Comparator<UnwritableInterval>() {
        @Override
        public int compare(UnwritableInterval o1, UnwritableInterval o2) {
          return Double.compare(o1.begin, o2.begin);
        }
      };

  /**
   * Comparator used to sort a list of {@link UnwritableInterval} by their endings.
   */
  public static final Comparator<UnwritableInterval> END_COMPARATOR =
      new Comparator<UnwritableInterval>() {
        @Override
        public int compare(UnwritableInterval o1, UnwritableInterval o2) {
          return Double.compare(o1.end, o2.end);
        }

      };

  /**
   * Comparator used to sort a list of {@link UnwritableInterval} by their lengths.
   */
  public static final Comparator<UnwritableInterval> LENGTH_COMPARATOR =
      new Comparator<UnwritableInterval>() {

        @Override
        public int compare(UnwritableInterval o1, UnwritableInterval o2) {
          return Double.compare(o1.getLength(), o2.getLength());
        }
      };

  /**
   * Function used to transform an interval into its begin point.
   */
  public static final Function<UnwritableInterval, Double> BEGIN_EXTRACTOR =
      new Function<UnwritableInterval, Double>() {
        @Override
        public Double apply(UnwritableInterval input) {
          return input.begin;
        }
      };

  /**
   * Function used to transform an interval into its end point.
   */
  public static final Function<UnwritableInterval, Double> END_EXTRACTOR =
      new Function<UnwritableInterval, Double>() {
        @Override
        public Double apply(UnwritableInterval input) {
          return input.end;
        }
      };

  /**
   * Function used to transform an interval into its length.
   */
  public static final Function<UnwritableInterval, Double> LENGTH_EXTRACTOR =
      new Function<UnwritableInterval, Double>() {

        @Override
        public Double apply(UnwritableInterval input) {
          return input.getLength();
        }
      };

  /**
   * Interval capturing all representable doubles on the line.
   */
  public static final UnwritableInterval ALL_DOUBLES =
      new UnwritableInterval(-Double.MAX_VALUE, Double.MAX_VALUE);

  /**
   * Creates an interval that covers the entire range of valid double precision values.
   */
  public Interval() {
    super();
  }

  /**
   * Copy constructor.
   * 
   * @param interval the interval to copy the contents of
   */
  public Interval(UnwritableInterval interval) {
    super(interval);
  }

  /**
   * Creates an interval with the specified begin and end.
   * 
   * @param begin the start of the interval
   * @param end the end of the interval
   * 
   * @throws IllegalArgumentException if begin &gt; end
   */
  public Interval(double begin, double end) {
    super(begin, end);
  }

  /**
   * Sets the interval to the supplied values.
   * 
   * @param begin the new start of the interval
   * @param end the new end of the interval
   * 
   * @throws IllegalArgumentException if begin &gt; end.
   */
  public void set(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    this.begin = begin;
    this.end = end;
  }

  /**
   * Convenience set method that mutates only the beginning of the interval.
   * <p>
   * Note: this is equivalent to calling this.set(begin,this.getEnd()).
   * </p>
   * 
   * @param begin the new start of the interval
   * 
   * @throws IllegalArgumentException if begin &gt; this.getEnd().
   */
  public void setBegin(double begin) {
    set(begin, getEnd());
  }

  /**
   * Convenience set method that mutates only the end of the interval.
   * <p>
   * Note: this is equivalent to calling this.set(this.getBegin(), end).
   * </p>
   * 
   * @param end the new end of the interval
   * 
   * @throws IllegalArgumentException if this.getBegin() &gt; end.
   */
  public void setEnd(double end) {
    set(getBegin(), end);
  }

  /**
   * Sets the contents of this interval to the specified interval
   */
  @Override
  public Interval setTo(UnwritableInterval interval) {
    this.begin = interval.begin;
    this.end = interval.end;
    return this;
  }

}
