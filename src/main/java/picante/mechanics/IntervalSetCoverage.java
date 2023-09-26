package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import picante.collections.CollectionUtilities;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

/**
 * Package private class that adapts {@link IntervalSet} to {@link Coverage}.
 * <p>
 * This class is package private, as consumers of it should not care that the underlying
 * implementation is backed by an interval set.
 * </p>
 */
final class IntervalSetCoverage extends AbstractCoverage {

  private final IntervalSet set;

  /**
   * Creates a new interval set coverage instance.
   * 
   * @param set the set to back the coverage
   */
  public IntervalSetCoverage(IntervalSet set) {
    checkArgument(set.size() > 0, "Supplied set must have at least one interval.");
    this.set = set;
  }

  /**
   * Package private method used to retrieve the interval set backing the coverage.
   * 
   * @return the reference to the set
   */
  IntervalSet getSet() {
    return set;
  }

  @Override
  public boolean contains(double time) {
    return set.contains(time);
  }

  @Override
  public Interval getBoundingInterval(Interval buffer) {
    buffer.set(set.get(0).getBegin(), set.get(set.size() - 1).getEnd());
    return buffer;
  }

  @Override
  public Interval getBracketingInterval(double time, Interval buffer) {

    /*
     * Locate the interval whose start occurs just prior to time.
     */
    int index = CollectionUtilities.lastLessThanOrEqualTo(set.getIntervalBegins(), time);

    if (index == -1) {
      throw new TimeOutOfBoundsException(time);
    }

    UnwritableInterval interval = set.get(index);

    if (!interval.closedContains(time)) {
      throw new TimeOutOfBoundsException(time);
    }

    return buffer.setTo(interval);
  }

  @Override
  public boolean hasNextInterval(double time) {

    /*
     * getNextInterval will always succeed, unless time is greater than or equal to the start of the
     * last interval in the set.
     */
    return (time < set.get(set.size() - 1).getBegin());
  }

  @Override
  public Interval getNextInterval(double time, Interval buffer) {

    /*
     * Locate the interval that begins just after the specified time.
     */
    int index = CollectionUtilities.firstGreaterThan(set.getIntervalBegins(), time);

    /*
     * If there is no interval that starts after the specified time, then throw the appropriate
     * exception.
     */
    if (index == set.size()) {
      throw new TimeOutOfBoundsException(time);
    }

    return buffer.setTo(set.get(index));
  }

}
