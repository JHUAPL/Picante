package picante.math.intervals;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.min;
import picante.math.PicanteMath;

/**
 * Class utilizing unmodifiable parent component of the weak-immutability design pattern to capture
 * a single interval on the finite, double precision line.
 * <p>
 * A common notation utilized in the documentation of this class is the standard mathematical usage
 * of open and closed interval notation. For example, &quot;(&quot; and &quot;)&quot; indicate that
 * the begin and end of an interval does not include the end point. Similarly &quot;[&quot; and
 * &quot;]&quot; indicate the interval does include its ends. They may be mixed, like so: (a,b]
 * which indicates the interval from a to b, excluding a and including b.
 * </p>
 * <p>
 * The variety of containment operations included on this class may, at first appear confusing. They
 * were largely included for completeness, and all of the interval methods assume that the argument
 * to the method is a closed interval. {@link PicanteMath#ulp(double)} is useful for manipulating
 * these situations, if you do wish to exclude the boundary. TODO: Consider adding
 * createUlpContracted and createUlpExpanded methods?
 * </p>
 * <p>
 * As with other weakly immutable classes, the unwritable parent is the less utilized of the two
 * classes. {@link Interval} contains many useful static features applicable to both elements of the
 * design pattern.
 * </p>
 */
public class UnwritableInterval {

  /**
   * The data fields are protected, per the weak-immutability pattern approach to capturing data.
   * This enables the sub-class to utilize the same storage as the parent.
   */
  protected double begin;
  protected double end;

  /**
   * Creates the default unwritable interval, covering the entire range of double precision values.
   */
  public UnwritableInterval() {
    this.begin = -Double.MAX_VALUE;
    this.end = Double.MAX_VALUE;
  }

  /**
   * Creates an unwritable copy of the supplied interval
   * 
   * @param interval the interval to copy
   */
  public UnwritableInterval(UnwritableInterval interval) {
    super();
    this.begin = interval.begin;
    this.end = interval.end;
  }

  /**
   * Creates an unwritable interval from the supplied beginning and ending.
   * 
   * @param begin the start of the interval
   * @param end the end of the interval
   * 
   * @throws IllegalArgumentException if begin &gt; end
   */
  public UnwritableInterval(double begin, double end) {
    super();
    checkArgument(begin <= end, "Interval end precedes begin.");
    this.begin = begin;
    this.end = end;
  }

  /**
   * Retrieves the begin value of the interval
   * 
   * @return the beginning of the interval
   */
  public double getBegin() {
    return begin;
  }

  /**
   * Retrieves the end value of the interval
   * 
   * @return the end of the interval
   */
  public double getEnd() {
    return end;
  }

  /**
   * Is the interval a singleton?
   * 
   * @return true if begin == end; false otherwise
   */
  public boolean isSingleton() {
    return (begin == end);
  }

  /**
   * Retrieves the length of the interval
   * 
   * @return the length, (end - begin) or {@link Double#POSITIVE_INFINITY} if the interval length is
   *         too large to contain in a double precision value. (In general this should only be a
   *         problem if the length of interval exceeds {@link Double#MAX_VALUE}, which should be
   *         unlikely.).
   */
  public double getLength() {
    return end - begin;
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(begin);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(end);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof UnwritableInterval)) {
      return false;
    }
    final UnwritableInterval other = (UnwritableInterval) obj;
    if (Double.doubleToLongBits(begin) != Double.doubleToLongBits(other.begin)) {
      return false;
    }
    if (Double.doubleToLongBits(end) != Double.doubleToLongBits(other.end)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "[" + begin + "," + end + "]";
  }

  /**
   * Is the supplied value contained within the interval in a (,) fashion?
   * 
   * @param value the value to consider for containment
   * 
   * @return true if value lies in (this.begin,this.end); false otherwise
   */
  public boolean openContains(double value) {
    return (value > begin) && (value < end);
  }

  /**
   * Is the supplied value contained within the interval in a [,] fashion?
   * 
   * @param value the value to consider for containment
   * 
   * @return true if value lies in [this.begin,this.end]; false otherwise
   */
  public boolean closedContains(double value) {
    return (value >= begin) && (value <= end);
  }

  /**
   * Is the supplied value contained within the interval in a (,] fashion?
   * 
   * @param value the value to consider for containment
   * 
   * @return true if value lies in (this.begin,this.end]; false otherwise
   */
  public boolean beginOpenContains(double value) {
    return (value > begin) && (value <= end);
  }

  /**
   * Is the supplied value contained within the interval in a [,) fashion?
   * 
   * @param value the value to consider for containment
   * 
   * @return true if value lies in [this.begin,this.end); false otherwise
   */
  public boolean endOpenContains(double value) {
    return (value >= begin) && (value < end);
  }

  /**
   * Retrieves the midpoint of the interval.
   * 
   * @return the middle point in the interval: (this.begin + this.end)/2.0
   */
  public double getMiddle() {
    return (end + begin) / 2.0;
  }

  /**
   * Clamps the supplied value into the range supported by the interval. This function returns:
   * <ul>
   * <li>( value <= begin ) -> begin</li>
   * <li>( value >= end ) -> end</li>
   * <li>value, otherwise</li>
   * </ul>
   * 
   * @param value the value to clamp
   * 
   * @return a value in the range [this.begin, this.end]
   */
  public double clamp(double value) {
    return min(max(begin, value), end);
  }

  /**
   * Clamps the supplied interval into the range supported by this interval. This function
   * individually clamps the 'begin' and 'end' of the supplied interval; see
   * {@link UnwritableInterval#clamp(double value)}.
   * <ul>
   * <li>If interval.end <= this.begin then returns singleton interval this.begin</li>
   * <li>If interval.begin >= this.end then returns singleton interval this.end</li>
   * </ul>
   * 
   * @param interval the interval to clamp
   * 
   * @return an interval in the range [this.begin, this.end]
   */
  public UnwritableInterval clamp(UnwritableInterval interval) {
    return new UnwritableInterval(clamp(interval.getBegin()), clamp(interval.getEnd()));
  }

  /**
   * Does the interval contain the specified interval in a [,] fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] is completely contained within the instance interpreted as
   *         [this.begin,this.end]; false otherwise
   */
  public boolean closedContains(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return (begin >= this.begin) && (end <= this.end);
  }

  /**
   * Does the interval contain the specified interval in a [,] fashion?
   * 
   * @param interval the closed interval to consider for containment
   * 
   * @return true if interval is completely contained within the instance interpreted as
   *         [this.begin, this.end]; false otherwise
   */
  public boolean closedContains(UnwritableInterval interval) {
    return (interval.begin >= this.begin) && (interval.end <= this.end);
  }

  /**
   * Does the interval contain the specified interval in a (,) fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] is completely contained within the instance interpreted as
   *         (this.begin,this.end); false otherwise
   */
  public boolean openContains(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return (begin > this.begin) && (end < this.end);
  }

  /**
   * Does the interval contain the specified interval in a (,) fashion?
   * 
   * @param interval the closed interval to consider for containment
   * 
   * @return true if interval is completely contained within the instance interpreted as
   *         (this.begin, this.end); false otherwise
   */
  public boolean openContains(UnwritableInterval interval) {
    return (interval.begin > this.begin) && (interval.end < this.end);
  }

  /**
   * Does the interval contain the specified interval in a (,] fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] is completely contained within the instance interpreted as
   *         (this.begin,this.end]; false otherwise
   */
  public boolean beginOpenContains(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return (begin > this.begin) && (end <= this.end);
  }

  /**
   * Does the interval contain the specified interval in a (,] fashion?
   * 
   * @param interval the closed interval to consider for containment
   * 
   * @return true if interval is completely contained within the instance interpreted as
   *         (this.begin, this.end]; false otherwise
   */
  public boolean beginOpenContains(UnwritableInterval interval) {
    return (interval.begin > this.begin) && (interval.end <= this.end);
  }

  /**
   * Does the interval contain the specified interval in a [,) fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] is completely contained within the instance interpreted as
   *         [this.begin,this.end); false otherwise
   */
  public boolean endOpenContains(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return (begin >= this.begin) && (end < this.end);
  }

  /**
   * Does the interval contain the specified interval in a [,) fashion?
   * 
   * @param interval the closed interval to consider for containment
   * 
   * @return true if interval is completely contained within the instance interpreted as
   *         [this.begin,this.end); false otherwise
   */
  public boolean endOpenContains(UnwritableInterval interval) {
    return (interval.begin >= this.begin) && (interval.end < this.end);
  }

  /**
   * Does the interval intersect with the specified interval in a [,] fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] intersects with the instance interpreted as [this.begin,this.end];
   *         false otherwise
   */
  public boolean closedIntersects(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return this.begin == begin || (this.begin < begin ? this.end >= begin : end >= this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a [,] fashion?
   * 
   * @param interval the closed interval to consider for intersection
   * 
   * @return true if interval intersects with the instance interpreted as [this.begin,this.end];
   *         false otherwise
   */
  public boolean closedIntersects(UnwritableInterval interval) {
    return this.begin == interval.begin
        || (this.begin < interval.begin ? this.end >= interval.begin : interval.end >= this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a (,) fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] intersects with the instance interpreted as (this.begin,this.end);
   *         false otherwise
   */
  public boolean openIntersects(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return isSingleton() ? false
        : (this.begin == begin && begin != end)
            || (this.begin < begin ? this.end > begin : end > this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a (,) fashion?
   * 
   * @param interval the closed interval to consider for intersection
   * 
   * @return true if interval intersects with the instance interpreted as (this.begin,this.end);
   *         false otherwise
   */
  public boolean openIntersects(UnwritableInterval interval) {
    return isSingleton() ? false
        : (this.begin == interval.begin && interval.begin != interval.end)
            || (this.begin < interval.begin ? this.end > interval.begin
                : interval.end > this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a (,] fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] intersects with the instance interpreted as (this.begin,this.end];
   *         false otherwise
   */
  public boolean beginOpenIntersects(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return isSingleton() ? false
        : (this.begin == begin && begin != end)
            || (this.begin < begin ? this.end >= begin : end > this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a (,] fashion?
   * 
   * @param interval the closed interval to consider for intersection
   * 
   * @return true if interval intersects with the instance interpreted as (this.begin,this.end];
   *         false otherwise
   */
  public boolean beginOpenIntersects(UnwritableInterval interval) {
    return isSingleton() ? false
        : (this.begin == interval.begin && interval.begin != interval.end)
            || (this.begin < interval.begin ? this.end >= interval.begin
                : interval.end > this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a [,) fashion?
   * 
   * @param begin the start of the closed interval to consider
   * @param end the end of the closed interval to consider
   * 
   * @return true if [begin,end] intersects with the instance interpreted as [this.begin,this.end);
   *         false otherwise
   */
  public boolean endOpenIntersects(double begin, double end) {
    checkArgument(begin <= end, "End precedes begin.");
    return isSingleton() ? false
        : this.begin == begin || (this.begin < begin ? this.end > begin : end >= this.begin);
  }

  /**
   * Does the interval intersect with the specified interval in a [,) fashion?
   * 
   * @param interval the closed interval to consider for intersection
   * 
   * @return true if interval intersects with the instance interpreted as [this.begin,this.end);
   *         false otherwise
   */
  public boolean endOpenIntersects(UnwritableInterval interval) {
    return isSingleton() ? false
        : this.begin == interval.begin || (this.begin < interval.begin ? this.end > interval.begin
            : interval.end >= this.begin);
  }

  /**
   * Evaluates the signed distance from a supplied value to the interval. If the value is
   * closed-contained in the interval the distance is zero. If the value is before the interval the
   * distance is positive, if it is after the distance is negative.
   * 
   * @param value the value to evaluate the signed distance to the interval
   * 
   * @return the signed distance from the supplied value to the interval, zero if the value is
   *         closed-contained in the interval, positive if before, negative if after
   */
  public double signedDistanceTo(double value) {
    if (closedContains(value)) {
      return 0.0;
    }
    return value < begin ? begin - value : end - value;
    // return min(abs(begin - value), abs(end - value));
  }

  /**
   * Makes an unwritable copy of the supplied interval.
   * <p>
   * This method makes an unwritable copy only if necessary. It tries to avoid making a copy
   * wherever possible.
   * </p>
   * 
   * @param interval an interval to copy.
   * 
   * @return either a reference to interval (if interval is already only an instance of
   *         {@link UnwritableInterval}, otherwise an unwritable copy of interval's contents
   */
  public static UnwritableInterval copyOf(UnwritableInterval interval) {
    if (interval.getClass().equals(UnwritableInterval.class)) {
      return interval;
    }
    return new UnwritableInterval(interval);
  }
}
