package picante.mechanics;

import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;

/**
 * Trivial implementation of the <code>Coverage</code> interface that captures a single interval in
 * time.
 * <p>
 * This class is package private, as consumers of it should not care that the underlying
 * implementation is backed by a single interval. If they do, the method:
 * {@link Coverages#isSingleInterval(Coverage)} exists for that purpose.
 * </p>
 */
final class IntervalCoverage extends AbstractCoverage {

  private final UnwritableInterval interval;

  /**
   * Creates a new interval coverage instance.
   * 
   * @param begin the start of the interval
   * @param end the end of the interval
   * 
   * @throws IllegalArgumentException if begin &gt; end
   */
  public IntervalCoverage(double begin, double end) {
    this.interval = new UnwritableInterval(begin, end);
  }

  /**
   * Creates a new interval coverage instance by copying the supplied interval
   * 
   * @param interval the interval to copy
   */
  public IntervalCoverage(UnwritableInterval interval) {
    this.interval = UnwritableInterval.copyOf(interval);
  }

  /**
   * Method to retrieve a reference to the backing interval.
   * 
   * @return a truly unwritable interval
   */
  UnwritableInterval getInterval() {
    return interval;
  }

  @Override
  public boolean contains(double time) {
    return interval.closedContains(time);
  }

  @Override
  public Interval getBoundingInterval(Interval buffer) {
    return buffer.setTo(interval);
  }

  @Override
  public Interval getBracketingInterval(double time, Interval buffer) {
    if (contains(time)) {
      buffer.setTo(interval);
      return buffer;
    }
    throw new TimeOutOfBoundsException(time);
  }

  @Override
  public boolean hasNextInterval(double time) {
    return time < interval.getBegin();
  }

  @Override
  public Interval getNextInterval(double time, Interval buffer) {
    if (time < interval.getBegin()) {
      return buffer.setTo(interval);
    }
    throw new TimeOutOfBoundsException(
        "There is no coverage interval that occurs after " + "the supplied time,  " + time + ".");
  }

}
