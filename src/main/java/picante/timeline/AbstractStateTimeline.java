package picante.timeline;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;

/**
 * This package private class consolidates functionality that would be common to both the
 * {@link StateTimeline} and the {@link StateTimeline.Builder} class.
 * 
 * @param <S> this class must implement {@link Object#equals(Object)} and {@link Object#hashCode()}
 *        properly.
 * 
 */
abstract class AbstractStateTimeline<S> {

  final NavigableMap<UnwritableInterval, S> intervalMap;

  AbstractStateTimeline(NavigableMap<UnwritableInterval, S> intervalMap) {
    super();
    this.intervalMap = intervalMap;
  }

  /**
   * Retrieves the supporting domain of the timeline.
   * <p>
   * This is the interval over which the various methods involving &quot;time&quot; from which
   * allowed values are drawn.
   * </p>
   * 
   * @return an interval containing the domain
   */
  public abstract UnwritableInterval getDomain();

  /**
   * Retrieves the state associated with an element of the domain.
   * <p>
   * If the element happens to lie on a transition point, then it chooses the state of the
   * subsequent interval. This method is equivalent to:
   * {@link StateTimeline#getState(double, Direction)} with {@link Direction#NEXT} as the direction
   * argument.
   * </p>
   * 
   * @param t the value from the domain to find the state
   * 
   * @return the state associated with t
   * 
   * @throws IllegalArgumentException if t is not contained within {@link StateTimeline#getDomain()}
   */
  public S getState(double t) {
    return getState(t, Direction.NEXT);
  }

  /**
   * Retrieves the state associated with an element of the domain, resolving states associated with
   * a transition time according to a specified direction.
   * 
   * @param t the value from the domain to find the state
   * @param direction the choice when t is at a transition boundary between two states
   * 
   * @return the value associated with t, or the requested value in the event t is a transition
   *         point
   * 
   * @throws IllegalArgumentException if t is not contained within {@link StateTimeline#getDomain()}
   */
  public S getState(double t, Direction direction) {

    UnwritableInterval domain = getDomain();

    checkArgument(domain.closedContains(t), "Requested time: %s is outside supported domain: .", t,
        domain);

    /*
     * Construct an interval that contains just t itself. This may seem counter-intuitive, but we
     * will then locate the key on the map that is the greatest key less than or equal to this
     * singleton interval. And since the intervals stored in the map intersect only on their end
     * points, cover the domain, and are sorted on their start times it will be the interval that
     * contains S.
     */
    UnwritableInterval query = new UnwritableInterval(t, t);

    UnwritableInterval mapKey = intervalMap.floorKey(query);

    /*
     * There are some cases worth discussing. First off, mapKey should never be null as we have
     * already determined that the requested t lives within the domain covered by the union of
     * intervalMap's key space. Further, we can never have the case where t "falls off" the end of
     * the domain and we attribute it to the last interval.
     * 
     * Second, if t is either end of the domain, then direction does not matter because by
     * definition this is not a transition from one state to another.
     * 
     * So we only "care" about direction in the event that t is the beginning of the located
     * interval, but not the beginning of the first interval in intervalMap.
     */
    if (mapKey.getBegin() != t) {
      return intervalMap.get(mapKey);
    }

    /*
     * If we reach here then t is the beginning of an interval, and the direction specified may
     * matter. First check to see if we're the first interval.
     */
    if (mapKey.equals(intervalMap.firstKey())) {
      return intervalMap.get(mapKey);
    }

    /*
     * At this point mapKey is not the first interval of intervalMap. If the specified direction is
     * NEXT, then simply return the value associated with mapKey (as t is the beginning of the
     * interval and pointing into the next state at the transition).
     */
    if (direction.equals(Direction.NEXT)) {
      return intervalMap.get(mapKey);
    }

    /*
     * Otherwise, we need to return the state associated with the previous interval.
     */
    return intervalMap.get(intervalMap.lowerKey(mapKey));

  }

  /**
   * Creates a view of the underlying {@link StateTimeline#getState(double)} method as a
   * {@link Function}.
   * 
   * @return a newly created {@link Function} that invokes {@link StateTimeline#getState(double)} to
   *         create the return value.
   */
  public Function<Double, S> function() {
    return new Function<Double, S>() {

      @Override
      public S apply(Double input) {
        return getState(input);
      }
    };
  }

  /**
   * Creates a view of the underlying {@link StateTimeline#getState(double, Direction)} method as a
   * {@link Function}.
   * 
   * @param direction the direction to supply to the
   *        {@link StateTimeline#getState(double, Direction)} method.
   * 
   * @return a newly created {@link Function} that invokes
   *         {@link StateTimeline#getState(double, Direction)} to create the return value.
   */
  public Function<Double, S> function(final Direction direction) {
    return new Function<Double, S>() {

      @Override
      public S apply(Double input) {
        return getState(input, direction);
      }

    };
  }

  public Iterable<Double> getTransitionTimes() {
    return getTransitionTimes(intervalMap);
  }

  static final <S> Iterable<Double> getTransitionTimes(
      NavigableMap<UnwritableInterval, S> intervalMap) {
    /*
     * The transition times are the beginnings of all the intervals in the keySet of the
     * intervalMap, except the first.
     */
    return Iterables.skip(Iterables.transform(Collections.unmodifiableSet(intervalMap.keySet()),
        Interval.BEGIN_EXTRACTOR), 1);
  }

  public Iterable<S> getStates() {
    return Collections.unmodifiableCollection(intervalMap.values());
  }

  public Set<Entry<UnwritableInterval, S>> getEntries() {
    return Collections.unmodifiableSet(intervalMap.entrySet());
  }

  @Override
  public final int hashCode() {
    /*
     * Even though hashCode here is as Eclipse would have implemented it, and we have altered the
     * definition of equals to account for a more detailed definition of key equality, these changes
     * are more restrictive and thus hashCode should still be valid.
     */
    final int prime = 31;
    int result = 1;
    result = prime * result + ((intervalMap == null) ? 0 : intervalMap.hashCode());
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
    if (!(obj instanceof AbstractStateTimeline)) {
      return false;
    }
    AbstractStateTimeline<?> other = (AbstractStateTimeline<?>) obj;
    if (intervalMap == null) {
      if (other.intervalMap != null) {
        return false;
      }
    } else if (!intervalMap.equals(other.intervalMap)) {
      return false;
    }

    /*
     * If we reach here, then the maps are likely the same; however, we need to iterate over the
     * intervals as the equals method on TreeMap uses the compareTo method from the comparator. So
     * intervals that have the same start time will match, and we want to guard against that.
     */
    Iterator<UnwritableInterval> thisIterator = intervalMap.keySet().iterator();
    Iterator<UnwritableInterval> otherIterator = other.intervalMap.keySet().iterator();

    while (thisIterator.hasNext()) {
      if (!thisIterator.next().equals(otherIterator.next())) {
        return false;
      }
    }

    return true;
  }

  @Override
  public final String toString() {
    return intervalMap.toString();
  }

  /**
   * Simple method that computes the subset of an interval map that covers, as best as possible, the
   * supplied interval.
   * <p>
   * This returns a subset of the supplied intervalMap with the intervals from the keySet that
   * intersect the supplied interval.
   * </p>
   * 
   * @param interval
   * @param intervalMap
   * 
   * @return a subMap view of intervalMap or null if no entries are valid in the covering
   */
  static <S> NavigableMap<UnwritableInterval, S> coveringSubsetMap(UnwritableInterval interval,
      NavigableMap<UnwritableInterval, S> intervalMap) {

    /*
     * Handle the easy cases first. If intervalMap is empty, interval is of zero length, or if
     * interval does not overlap any of the intervals in intervalMap then return null.
     */
    if (intervalMap.isEmpty() || interval.getLength() == 0
        || (interval.getEnd() <= intervalMap.firstKey().getBegin())
        || (interval.getBegin() >= intervalMap.lastKey().getEnd())) {
      return null;
    }

    /*
     * There is overlap and intervalMap has at least one interval, compute the bounding keys. Start
     * with the beginning of the map. Look up the key that contains interval.getBegin().
     */
    UnwritableInterval beginKey =
        intervalMap.floorKey(new UnwritableInterval(interval.getBegin(), interval.getBegin()));

    /*
     * If beginKey is null, then interval has a begin that is less than the start of the first
     * interval in intervalMap. Just assign beginKey to be the first interval in the map.
     */
    if (beginKey == null) {
      beginKey = intervalMap.firstKey();
    }

    UnwritableInterval endKey =
        intervalMap.floorKey(new UnwritableInterval(interval.getEnd(), interval.getEnd()));

    /*
     * Since we know there is overlap, endKey is always going to be some key. We just need to check
     * whether or not we have selected the beginning of an interval. If this happens, then we just
     * need to take the preceding interval. This interval must exist, as the overlap condition
     * requires it.
     */
    if (endKey.getBegin() == interval.getEnd()) {
      endKey = intervalMap.lowerKey(endKey);
    }

    /*
     * Create the map to return, include both startKey and endKey.
     */
    return intervalMap.subMap(beginKey, true, endKey, true);

  }

}
