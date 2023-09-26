package picante.math.intervals;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkElementIndex;
import static com.google.common.base.Preconditions.checkState;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.min;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.primitives.Doubles;
import picante.collections.ArrayUtilities;
import picante.collections.CollectionUtilities;
import picante.data.list.Retrievable;
import picante.math.Statistics;
import picante.math.functions.UnivariateFunction;

/**
 * Immutable class that captures a minimal covering of a closed set of intervals on the real line.
 * Sets of intervals on the real line are useful for solving scheduling problems amongst other
 * mathematical root finding problems. This class encapsulates the definition of a closed set of
 * intervals over the finite double precision numbers.
 * <p>
 * There are several convenience static methods on this class for creating and manipulating the
 * immutable interval sets. However, if you are chasing efficiency, it may be better to utilize the
 * corresponding {@link Builder} and its methods. It will require far less intermediate allocation
 * of memory, much like {@link StringBuilder} does.
 * </p>
 * <p>
 * In general, methods involving statistics of the interval lengths on an instance will throw a
 * runtime exception if they are invoked on the empty set.
 * </p>
 * <p>
 * The iteration order is defined from the first interval that occurs in time to the last. This is
 * also followed by the {@link Retrievable} implementation. This class and its builder both
 * implement {@link Retrievable} as a convenience mechanism to allow one to be passed in lieu of the
 * other in applications where efficient utilization of memory and system resources are required.
 * </p>
 */
public final class IntervalSet implements Iterable<UnwritableInterval>, Retrievable<Interval> {

  /**
   * The empty set of intervals.
   */
  public static final IntervalSet EMPTY =
      new IntervalSet(new ImmutableList.Builder<UnwritableInterval>().build());

  /**
   * The entire, finite precision double real line.
   */
  public static final IntervalSet LINE =
      new IntervalSet(new ImmutableList.Builder<UnwritableInterval>()
          .add(new UnwritableInterval(-Double.MAX_VALUE, Double.MAX_VALUE)).build());

  /**
   * Immutable list of disjoint, closed intervals sorted by {@link IntervalSet#BEGIN_COMPARATOR}.
   */
  private final ImmutableList<UnwritableInterval> intervals;

  /**
   * Transformed view of intervals providing only the beginning points.
   */
  private final List<Double> beginEndPoints;

  /**
   * Transformed view of intervals providing only the ending points.
   */
  private final List<Double> endEndPoints;

  /**
   * Interval statistics--lazily initialized. Initially set to null, but each of the retrieval
   * methods populates the field.
   */
  private Statistics statistics = null;

  /**
   * Bounding interval--lazily initialized. Initially set to null, but the retrieval method
   * populates the field on demand.
   */
  private UnwritableInterval boundingInterval = null;

  /**
   * Constructs a new set of intervals from the supplied list--a reference is retained internally.
   * 
   * @param intervals a list of disjoint, closed intervals sorted by
   *        {@link IntervalSet#BEGIN_COMPARATOR}
   */
  private IntervalSet(ImmutableList<UnwritableInterval> intervals) {
    this.intervals = intervals;
    this.beginEndPoints = Lists.transform(this.intervals, Interval.BEGIN_EXTRACTOR);
    this.endEndPoints = Lists.transform(this.intervals, Interval.END_EXTRACTOR);
  }

  /**
   * Retrieves the number of individual disjoint intervals in the set.
   * 
   * @return the number of intervals
   */
  @Override
  public int size() {
    return intervals.size();
  }

  /**
   * Is this set empty?
   * 
   * @return true if the set contains no intervals; false otherwise
   */
  public boolean isEmpty() {
    return intervals.size() == 0;
  }

  /**
   * Returns a list of doubles containing the start of each interval in the set.
   * 
   * @return an unmodifiable list of doubles in increasing order
   */
  public List<Double> getIntervalBegins() {
    return this.beginEndPoints;
  }

  /**
   * Returns a list of doubles containing the end of each interval in the set.
   * 
   * @return an unmodifiable list of doubles in increasing order
   */
  public List<Double> getIntervalEnds() {
    return this.endEndPoints;
  }

  /**
   * Determines if the supplied value is contained within the interval set.
   * 
   * @param value value of interest
   * 
   * @return true if value is an element of the set, false otherwise.
   */
  public boolean contains(double value) {
    int index = CollectionUtilities.lastLessThanOrEqualTo(beginEndPoints, value);

    if (index == -1) {
      return false;
    }

    return intervals.get(index).closedContains(value);
  }

  /**
   * Retrieves the index of the interval in the set that contains the supplied value. Use
   * {@link IntervalSet#contains(double)} to test whether this method will throw an
   * {@link IllegalArgumentException} for the supplied value.
   * 
   * @param value the value of interest
   * 
   * @return the index of the interval containing value
   *         {@link UnwritableInterval#closedContains(double)}
   * 
   * @throws IllegalArgumentException if value is not contained within an interval in the set
   */
  public int getEnclosingIndex(double value) {

    int index = CollectionUtilities.lastLessThanOrEqualTo(beginEndPoints, value);

    checkArgument(index > -1, "No such interval. %s occurs prior to all intervals in the set.",
        value);

    checkArgument(intervals.get(index).closedContains(value), "No enclosing interval for value: %s",
        value);

    return index;
  }

  /**
   * Determines if the supplied value lies on on the boundary of the intervals in the set.
   * 
   * @param value the value of interest
   * 
   * @return true if value is an endpoint of one of the disjoint, closed intervals in the set.
   */
  public boolean onBoundary(double value) {
    int index = CollectionUtilities.lastLessThanOrEqualTo(beginEndPoints, value);

    /*
     * value precedes all of the intervals in the set, so it clearly can not be on the boundary.
     */
    if (index == -1) {
      return false;
    }

    /*
     * The supplied value must agree with either endpoint of the interval at position index.
     */
    UnwritableInterval interval = intervals.get(index);

    return (interval.getBegin() == value) || (interval.getEnd() == value);

  }

  /**
   * Determines if the supplied value lies strictly in the interior of the intervals in the set.
   * 
   * @param value the value of interest
   * 
   * @return true, if value is contained within the set and not an endpoint of an interval.
   */
  public boolean inInterior(double value) {
    return contains(value) && (!onBoundary(value));
  }

  /**
   * Determines if the supplied interval is contained within in the set of intervals.
   * 
   * @param begin the start of the interval of interest
   * @param end the end of the interval of interest
   * 
   * @return true if the interval set contains the entire closed interval, false otherwise
   * 
   * @throws IllegalArgumentException if end < begin
   */
  public boolean contains(double begin, double end) {
    int index = CollectionUtilities.lastLessThanOrEqualTo(beginEndPoints, begin);

    if (index == -1) {
      return false;
    }

    /*
     * closedContains throws IllegalArgumentException if begin > end.
     */
    return intervals.get(index).closedContains(begin, end);
  }

  /**
   * Retrieves the index of the interval in the set that contains the supplied interval. Use
   * {@link IntervalSet#contains(double, double)} to test whether this method will throw an
   * {@link IllegalArgumentException} for the supplied interval.
   * 
   * @param begin the start of the interval of interest
   * @param end the end of the interval of interest
   * 
   * @return the index of the interval containing [begin,end]
   *         {@link UnwritableInterval#closedContains(double, double)}
   * 
   * @throws IllegalArgumentException if value is not contained within an interval in the set, or if
   *         begin > end
   */
  public int getEnclosingIndex(double begin, double end) {

    int index = CollectionUtilities.lastLessThanOrEqualTo(beginEndPoints, begin);

    checkArgument(index > -1, "No such interval. [%s,%s] starts prior to all intervals in the set.",
        begin, end);

    /*
     * closedContains throws IllegalArgumentException if begin > end.
     */
    checkArgument(intervals.get(index).closedContains(begin, end),
        "Interval: [%s,%s] is not enclosed in any interval in the set.", begin, end);

    return index;

  }

  /**
   * Determines if the supplied interval is contained within the set of intervals.
   * 
   * @param interval the closed interval to test containment
   * 
   * @return true if the interval set contains the entire closed interval
   */
  public boolean contains(UnwritableInterval interval) {
    int index =
        CollectionUtilities.lastLessThanOrEqualTo(intervals, interval, Interval.BEGIN_COMPARATOR);

    if (index == -1) {
      return false;
    }

    return intervals.get(index).closedContains(interval);
  }

  /**
   * Retrieves the index of the interval in the set that contains the supplied interval. Use
   * {@link IntervalSet#contains(double, double)} to test whether this method will throw an
   * {@link IllegalArgumentException} for the supplied interval.
   * 
   * @param interval the closed interval to test containment
   * 
   * @return the index of the interval containing interval
   *         {@link UnwritableInterval#closedContains(double, double)}
   * 
   * @throws IllegalArgumentException if value is not contained within an interval in the set, or if
   *         begin > end
   */
  public int getEnclosingIndex(UnwritableInterval interval) {
    int index =
        CollectionUtilities.lastLessThanOrEqualTo(intervals, interval, Interval.BEGIN_COMPARATOR);

    checkArgument(index > -1, "No such interval. %s starts prior to all intervals in the set.",
        interval);

    checkArgument(intervals.get(index).closedContains(interval),
        "Interval: %s is not enclosed in any interval in the set.", interval);

    return index;

  }

  /**
   * {@inheritDoc}
   * <p>
   * Iterates over the list of unique intervals contained within this set.
   * </p>
   * 
   * @return an unmodifiable iterator that iterates through the list of intervals in increasing
   *         order
   */
  @Override
  public Iterator<UnwritableInterval> iterator() {
    return intervals.iterator();
  }

  /**
   * Retrieves an interval from the set
   * 
   * @return the interval of interest
   * 
   * @throws IndexOutOfBoundsException if index lies outside of the range [0, this.size()-1]
   */
  public UnwritableInterval get(int index) {
    return intervals.get(index);
  }

  /**
   * Determines if the supplied interval set is completely contained within the instance.
   * <p>
   * This boolean is a simply subset test that allows all subsets.
   * </p>
   * 
   * @param set the set on which to test containment in the instance
   * 
   * @return true if the instance completely contains set
   */
  public boolean contains(IntervalSet set) {
    for (UnwritableInterval interval : set) {
      if (!contains(interval)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines if the supplied interval set is a proper subset of the instance.
   * 
   * @param set the set on which to test if it is a proper subset.
   * 
   * @return true if the instance contains the set, but not the entire set
   */
  public boolean properContains(IntervalSet set) {
    /*
     * Verify that the two sets are not equal to one another first. If they are, then this clearly
     * is not proper containment.
     */
    if (this.equals(set)) {
      return false;
    }

    /*
     * Simply determine if set is contained entirely within this instance.
     */
    return contains(set);
  }

  /**
   * Simple method to defer statistics computation until they are required.
   * 
   * @return statistics instance covering the population.
   * 
   * @throws IllegalStateException if the instance is the empty set.
   */
  private Statistics fetchStatistics() {
    checkState(intervals.size() > 0, "Can not compute statistics for the empty set.");
    return (statistics == null)
        ? statistics = Statistics
            .createPopulationStatistics(Lists.transform(this.intervals, Interval.LENGTH_EXTRACTOR))
        : this.statistics;
  }

  /**
   * Simple method to defer creation of the bounding interval until it is required.
   * 
   * @return the interval bounding the set
   * 
   * @throws IllegalStateException if the instance is the empty set
   */
  private UnwritableInterval fetchBound() {
    checkState(intervals.size() > 0, "Bounding interval does not exist for the empty set.");
    return (boundingInterval == null)
        ? boundingInterval = new UnwritableInterval(intervals.get(0).getBegin(),
            intervals.get(intervals.size() - 1).getEnd())
        : this.boundingInterval;
  }

  /**
   * Retrieves the sum of the measure of all intervals contained in the set.
   * 
   * @return the total set length
   */
  public double getLengthTotal() {
    if (isEmpty()) {
      return 0.0;
    }
    return fetchStatistics().getSum();
  }

  /**
   * Retrieves the average measure of all intervals contained in the set.
   * 
   * @return the average interval length
   * 
   * @throws IllegalStateException if the set is empty
   */
  public double getLengthAverage() {
    return fetchStatistics().getMean();
  }

  /**
   * Retrieves the standard deviation of the measure of all intervals contained in the set.
   * 
   * @return the standard deviation of the interval length
   * 
   * @throws IllegalStateException if the set is empty
   */
  public double getLengthStandardDeviation() {
    return fetchStatistics().getStandardDeviation();
  }

  /**
   * Retrieves the index of the first interval of the minimum measure.
   * 
   * @return the index of the first interval of minimum length
   * 
   * @throws IllegalStateException if the set is empty
   */
  public int getLengthMinimumIndex() {
    return fetchStatistics().getMinimumIndices().get(0);
  }

  /**
   * Retrieves the index of the first interval of the maximum measure.
   * 
   * @return the index of the first interval of maximum length
   * 
   * @throws IllegalStateException if the set is empty
   */
  public int getLengthMaximumIndex() {
    return fetchStatistics().getMaximumIndices().get(0);
  }

  /**
   * Retrieves the measure of the minimum interval
   * 
   * @return the length of the smallest interval in the set
   */
  public double getLengthMinimum() {
    if (isEmpty()) {
      return 0.0;
    }
    return fetchStatistics().getMinimumValue();
  }

  /**
   * Retrieves the measure of the maximum interval
   * 
   * @return the length of the largest interval in the set
   */
  public double getLengthMaximum() {
    if (isEmpty()) {
      return 0.0;
    }
    return fetchStatistics().getMaximumValue();
  }

  /**
   * Retrieves the interval that bounds the entire interval set
   * 
   * @return the tightly bound interval that brackets the set
   * 
   * @throws IllegalStateException if the set is empty
   */
  public UnwritableInterval getBoundingInterval() {
    return fetchBound();
  }

  @Override
  public Interval get(int index, Interval buffer) {
    return buffer.setTo(intervals.get(index));
  }

  @Override
  public String toString() {
    return intervals.toString();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((intervals == null) ? 0 : intervals.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    IntervalSet other = (IntervalSet) obj;
    if (intervals == null) {
      if (other.intervals != null) {
        return false;
      }
    } else if (!intervals.equals(other.intervals)) {
      return false;
    }
    return true;
  }

  /**
   * Creates a builder instance for usage.
   * 
   * @return a newly constructed, empty builder.
   */
  public static Builder builder() {
    return new Builder();
  }

  /**
   * Creates a builder instance for usage with the supplied initial capacity. The only reason to
   * utilize this method is if you are planning on inserting a known or well bounded number of
   * intervals into the builder, it will improve performance slightly.
   * 
   * @param initialCapacity the initial capacity of the builder
   * 
   * @return a newly constructed, empty builder with capacity for initialCapacity intervals
   */
  public static Builder builder(int initialCapacity) {
    return new Builder(initialCapacity);
  }

  /**
   * Creates a builder instance and initializes it to the supplied initial state.
   * 
   * @param initialState the initial state to load into the builder.
   * 
   * @return a newly constructed builder populated with the contents of initialState
   */
  public static Builder builder(IntervalSet initialState) {
    return builder(initialState.size()).addAll(initialState);
  }

  /**
   * The {@link IntervalSet} builder used to create interval sets.
   * <p>
   * For convenience, the builder implements {@link Retrievable} and {@link Indexable}; however,
   * each invocation of {@link Builder#get(int)} creates a new {@link UnwritableInterval}, unlike
   * {@link IntervalSet#get(int)}.
   * </p>
   */
  public static final class Builder implements
      picante.designpatterns.Builder<IntervalSet, RuntimeException>, Retrievable<Interval> {

    /**
     * The state of the currently captured interval set contents. This is a sorted list that
     * contains the interval begin values (at indices 0,2,4,...) and the interval end values (at
     * indices 1,3,5,7...). It must have a multiple of two elements at all times.
     */
    @VisibleForTesting
    static class Content {
      private final static int INITIAL_CAPACITY = 20;

      double[] array;
      int committed;

      public Content() {
        array = new double[INITIAL_CAPACITY];
        committed = 0;
      }

      public Content(int initialCapacity) {
        array = new double[initialCapacity * 2];
        committed = 0;
      }

      public void ensureCapacity(int capacity) {
        int newPadding = (3 * array.length) / 2 + 1 - capacity;
        array = Doubles.ensureCapacity(array, capacity, newPadding > 0 ? newPadding : 0);
      }

      public void clear() {
        committed = 0;
      }

      public void add(double begin, double end) {
        ensureCapacity(committed + 2);
        array[committed++] = begin;
        array[committed++] = end;
      }
    }

    private Content state;
    private Content buffer;
    private final Interval interval = new Interval();

    /**
     * Swaps the state and buffer contents.
     */
    private void swapStateAndBuffer() {
      Content content = state;
      state = buffer;
      buffer = content;
    }

    /**
     * Constructs a default builder.
     */
    public Builder() {
      state = new Content();
      buffer = new Content();
    }

    /**
     * Constructs a builder with the supplied initial capacity for its internal buffers.
     * 
     * @param initialCapacity the number of intervals to allocate space for in the internal buffers
     *        initially.
     */
    public Builder(int initialCapacity) {
      state = new Content(initialCapacity);
      buffer = new Content(initialCapacity);
    }

    /**
     * {@inheritDoc}
     * 
     * @return a newly constructed interval set capturing the current state of the builder
     */
    @Override
    public IntervalSet build() {

      /*
       * Simple check, if the number of intervals committed to the state field is 0, then the set is
       * empty. Just return the empty instance.
       */
      if (state.committed == 0) {
        return IntervalSet.EMPTY;
      }

      ImmutableList.Builder<UnwritableInterval> builder = ImmutableList.builder();

      for (int i = 0; i < state.committed; i += 2) {
        builder.add(new UnwritableInterval(state.array[i], state.array[i + 1]));
      }

      return new IntervalSet(builder.build());
    }

    /**
     * Replaces the current state of the builder with the supplied interval set.
     * 
     * @param set the set to replace the state with
     * 
     * @return a reference to the builder instance for convenience
     */
    public Builder setTo(IntervalSet set) {
      empty();

      state.ensureCapacity(2 * set.size());

      int i = 0;
      for (UnwritableInterval interval : set) {
        state.array[i++] = interval.getBegin();
        state.array[i++] = interval.getEnd();
      }

      state.committed = i;

      return this;
    }

    /**
     * Replaces the current state of the builder with the union of the supplied intervals.
     * 
     * @param intervals the intervals to use to build up the set
     * 
     * @return reference to the builder instance for convenience
     */
    public Builder setTo(Iterable<? extends UnwritableInterval> intervals) {
      empty();
      return addAll(intervals);
    }

    /**
     * Clears the existing state to the equivalent of {@link IntervalSet#EMPTY}
     * 
     * @return a reference to the builder instance for convenience
     */
    public Builder empty() {
      state.clear();
      return this;
    }

    /**
     * Unions all the intervals in the iterable to the current state of the builder.
     * 
     * @param intervals the iterable of intervals to add
     * 
     * @return a reference to the builder for convenience
     */
    public Builder addAll(Iterable<? extends UnwritableInterval> intervals) {
      for (UnwritableInterval interval : intervals) {
        add(interval);
      }
      return this;
    }

    /**
     * Unions all the intervals in the record list to the current state of the builder.
     * 
     * @param intervals the record list of intervals to add.
     * 
     * @return a reference to the builder for convenience
     */
    public Builder addList(Retrievable<Interval> intervals) {
      for (int i = 0; i < intervals.size(); i++) {
        add(intervals.get(i, interval));
      }
      return this;
    }

    /**
     * Unions a set with the builder state
     * 
     * @param set the set to union
     * 
     * @return a reference to the builder for convenience
     */
    public Builder union(IntervalSet set) {

      /*
       * Two trivial cases, set is empty or state is empty. In either case, we are to return the
       * non-empty set.
       */
      if (set.size() == 0) {
        return this;
      }
      if (state.committed == 0) {
        return setTo(set);
      }

      /*
       * No choice, perform the appropriate merging of the two sets.
       */
      buffer.clear();

      int stateIndex = 0;
      int setIndex = 0;
      UnwritableInterval interval = set.get(0);

      /*
       * Set end to be negative infinity so that any actual double value will not compare less than
       * it.
       */
      double end = Double.NEGATIVE_INFINITY;

      while ((stateIndex < state.committed - 1) || setIndex < set.size()) {

        boolean useState = false;

        /*
         * Determine whether the next interval is to come from state or interval.
         */
        if (stateIndex > state.committed - 1) {
          useState = false;
        } else if (setIndex > set.size() - 1) {
          useState = true;
        } else if (state.array[stateIndex] < interval.getBegin()) {
          useState = true;
        } else if (interval.getBegin() <= state.array[stateIndex]) {
          useState = false;
        }

        if (useState) {

          if (state.array[stateIndex] <= end) {
            buffer.array[buffer.committed - 1] =
                max(buffer.array[buffer.committed - 1], state.array[stateIndex + 1]);
          } else {
            buffer.add(state.array[stateIndex], state.array[stateIndex + 1]);
          }
          end = buffer.array[buffer.committed - 1];
          stateIndex += 2;

        } else {

          if (interval.getBegin() <= end) {
            buffer.array[buffer.committed - 1] =
                max(buffer.array[buffer.committed - 1], interval.getEnd());
          } else {
            buffer.add(interval.getBegin(), interval.getEnd());
          }
          end = buffer.array[buffer.committed - 1];

          /*
           * Only retrieve the next interval if we haven't reached the end of the list.
           */
          interval = set.get(min(++setIndex, set.size() - 1));

        }

      }

      swapStateAndBuffer();

      return this;
    }

    /**
     * Unions the supplied interval with the state of the builder.
     * 
     * @param interval the interval to union
     * 
     * @return reference to the builder instance for convenience of method chaining
     */
    public Builder union(UnwritableInterval interval) {
      return this.union(IntervalSet.create(interval));
    }

    /**
     * Intersect the builder with a set
     * 
     * @param set the set to intersect with the builder
     * 
     * @return a reference to the builder for convenience
     */
    public Builder intersect(IntervalSet set) {

      /*
       * Two trivial cases, set is empty or state is empty. In either case clear the current state.
       */
      if (set.size() == 0) {
        state.clear();
        return this;
      }
      if (state.committed == 0) {
        return this;
      }

      /*
       * No choice, perform the appropriate intersection of the two sets.
       */
      buffer.clear();

      int stateIndex = 0;
      int setIndex = 0;
      UnwritableInterval interval = set.get(0);

      while ((stateIndex < state.committed - 1) && setIndex < set.size()) {

        boolean useState = false;

        /*
         * Determine whether the next interval is to come from state or interval.
         */
        if (state.array[stateIndex + 1] < interval.getEnd()) {
          useState = true;
        } else if (interval.getEnd() <= state.array[stateIndex + 1]) {
          useState = false;
        }

        if (useState) {

          if (state.array[stateIndex + 1] >= interval.getBegin()) {
            buffer.add(max(interval.getBegin(), state.array[stateIndex]),
                state.array[stateIndex + 1]);
          }

          stateIndex += 2;

        } else {

          if (interval.getEnd() >= state.array[stateIndex]) {
            buffer.add(max(interval.getBegin(), state.array[stateIndex]), interval.getEnd());
          }

          /*
           * Only retrieve the next interval if we haven't reached the end of the list.
           */
          interval = set.get(min(++setIndex, set.size() - 1));

        }

      }

      swapStateAndBuffer();

      return this;
    }

    /**
     * Intersects the supplied interval with the state of the builder.
     * 
     * @param interval the interval to intersect
     * 
     * @return reference to the builder instance for convenience of method chaining
     */
    public Builder intersect(UnwritableInterval interval) {
      return this.intersect(IntervalSet.create(interval));
    }

    /**
     * Intersects an {@link Iterable} of interval sets with the contents of the builder.
     * 
     * @param sets
     * @return
     */
    public Builder intersectAll(Iterable<IntervalSet> sets) {
      for (IntervalSet set : sets) {
        this.intersect(set);
      }
      return this;
    }

    /**
     * Differences a supplied set from the current state of the builder.
     * 
     * @param set the set to difference from the builder
     * 
     * @return a reference to the builder with state: builder = ( builder \ set )
     */
    public Builder difference(IntervalSet set) {

      /*
       * Handle the trivial cases, if either of the sets are empty. If the state is empty, return.
       * If the set is empty, leave state alone and return.
       */
      if (state.committed == 0) {
        return this;
      }
      if (set.size() == 0) {
        return this;
      }

      /*
       * Loop through the intervals of state. We are removing intervals from set from it.
       */
      buffer.clear();

      int stateIndex = 0;
      int setIndex = 0;

      double first;
      double last;
      UnwritableInterval interval = set.get(0);

      while (stateIndex < state.committed) {

        /*
         * Work with the first interval which is the next interval present in state.
         */
        first = state.array[stateIndex];
        last = state.array[stateIndex + 1];

        boolean unresolved = setIndex < set.size();
        boolean keep = true;

        while (unresolved) {

          if (last < interval.getBegin()) {

            /*
             * The interval [first,last] precedes the next interval of set. Retain it.
             */

            unresolved = false;
          } else if (first > interval.getEnd()) {

            /*
             * [first,last] is after the end of the current interval in set, pull the next interval.
             */
            interval = set.get(min(++setIndex, set.size() - 1));
            unresolved = setIndex < set.size();
          } else {

            /*
             * The current interval from set overlaps the current interval pulled from state. There
             * are several possibilities:
             * 
             * 1. The current interval from state is contained in the current interval of set. This
             * includes singleton intervals. Just mark [first, last] so that it won't be kept.
             * 
             * 2. The interval from set overlaps at the beginning of the interval from state.
             * 
             * 3. The interval from set falls inside the current interval.
             * 
             * 4. The interval from set overlaps at the end of the current interval.
             */
            if ((interval.getBegin() <= first) && (last <= interval.getEnd())) {
              /*
               * Case #1
               */
              keep = false;
              unresolved = false;
            } else if (interval.getBegin() <= first) {
              /*
               * Case #2
               */
              first = interval.getEnd();
              interval = set.get(min(++setIndex, set.size() - 1));
              unresolved = setIndex < set.size();
            } else if ((first <= interval.getBegin()) && (last >= interval.getEnd())
                && (!interval.isSingleton())) {
              /*
               * Case #3 (non-singleton interval from set)
               */
              buffer.add(first, interval.getBegin());
              first = interval.getEnd();

              if (first == last) {
                keep = false;
                unresolved = false;
              }

              interval = set.get(min(++setIndex, set.size() - 1));
              unresolved = unresolved && (setIndex < set.size());
            } else if ((first <= interval.getBegin()) && (last >= interval.getEnd())
                && (interval.isSingleton())) {
              /*
               * Case #3 (singleton interval from set)
               */
              interval = set.get(min(++setIndex, set.size() - 1));
              unresolved = setIndex < set.size();
            } else {
              /*
               * Case #4
               */
              last = interval.getBegin();
              unresolved = false;
            }

          }

        }

        if (keep) {
          buffer.add(first, last);
        }

        stateIndex += 2;

      }

      swapStateAndBuffer();

      return this;
    }

    /**
     * Subtracts the supplied interval from the state of the builder.
     * 
     * @param interval the interval to subtract
     * 
     * @return reference to the builder instance for convenience of method chaining
     */
    public Builder difference(UnwritableInterval interval) {
      return this.difference(IntervalSet.create(interval));
    }

    /**
     * Remove the state of a builder from an interval set.
     * 
     * @param set the set from which to difference the builder's state
     * 
     * @return a reference to the builder which is: builder = ( set \ builder )
     */
    public Builder removeFrom(IntervalSet set) {

      /*
       * Handle the trivial cases, if either of the sets are empty. If the state is empty, set state
       * to set's contents. If the set is empty, clear state.
       */
      if (state.committed == 0) {
        return setTo(set);
      }
      if (set.size() == 0) {
        state.clear();
        return this;
      }

      /*
       * Loop through the intervals of state. We are removing intervals from set from it.
       */
      buffer.clear();

      int stateIndex = 0;
      int setIndex = 0;

      double first;
      double last;
      UnwritableInterval interval = set.get(0);

      while (setIndex < set.size()) {

        /*
         * Work with the first interval which is the next interval present in state.
         */
        first = interval.getBegin();
        last = interval.getEnd();

        boolean unresolved = stateIndex < state.committed;
        boolean keep = true;

        while (unresolved) {

          if (last < state.array[stateIndex]) {

            /*
             * The interval [first,last] precedes the next interval of set. Retain it.
             */

            unresolved = false;
          } else if (first > state.array[stateIndex + 1]) {

            /*
             * [first,last] is after the end of the current interval in set, pull the next interval.
             */
            stateIndex += 2;
            unresolved = stateIndex < state.committed;
          } else {

            /*
             * The current interval from set overlaps the current interval pulled from state. There
             * are several possibilities:
             * 
             * 1. The current interval from state is contained in the current interval of set. This
             * includes singleton intervals. Just mark [first, last] so that it won't be kept.
             * 
             * 2. The interval from set overlaps at the beginning of the interval from state.
             * 
             * 3. The interval from set falls inside the current interval.
             * 
             * 4. The interval from set overlaps at the end of the current interval.
             */
            if ((state.array[stateIndex] <= first) && (last <= state.array[stateIndex + 1])) {
              /*
               * Case #1
               */
              keep = false;
              unresolved = false;
            } else if (state.array[stateIndex] <= first) {
              /*
               * Case #2
               */
              first = state.array[stateIndex + 1];
              stateIndex += 2;
              unresolved = stateIndex < state.committed;
            } else if ((first <= state.array[stateIndex]) && (last >= state.array[stateIndex + 1])
                && (state.array[stateIndex] != state.array[stateIndex + 1])) {
              /*
               * Case #3 (non-singleton interval from set)
               */
              buffer.add(first, state.array[stateIndex]);
              first = state.array[stateIndex + 1];

              if (first == last) {
                keep = false;
                unresolved = false;
              }
              stateIndex += 2;
              unresolved = unresolved && (stateIndex < state.committed);
            } else if ((first <= state.array[stateIndex]) && (last >= state.array[stateIndex + 1])
                && (state.array[stateIndex] == state.array[stateIndex + 1])) {
              /*
               * Case #3 (singleton interval from set)
               */
              stateIndex += 2;
              unresolved = stateIndex < state.committed;
            } else {
              /*
               * Case #4
               */
              last = state.array[stateIndex];
              unresolved = false;
            }

          }

        }

        if (keep) {
          buffer.add(first, last);
        }

        interval = set.get(min(++setIndex, set.size() - 1));

      }

      swapStateAndBuffer();

      return this;
    }

    /**
     * Applies the supplied filter to the current state captured by the builder.
     * 
     * @param filter the filter to apply to the intervals present. Intervals for which apply returns
     *        true are kept, otherwise are removed
     * 
     * @return a reference to builder for convenience
     */
    public Builder filter(Predicate<UnwritableInterval> filter) {

      int i = 0;
      int j = 0;

      Interval interval = new Interval();

      while (i < state.committed) {

        interval.set(state.array[i], state.array[i + 1]);

        if (filter.apply(interval)) {
          state.array[j] = state.array[i];
          state.array[j + 1] = state.array[i + 1];
          j += 2;
        }

        i += 2;
      }

      state.committed = j;

      return this;
    }

    /**
     * Produces the complement of the current state relative to the values available on the double
     * precision line.
     * <p>
     * This is analogus to calling:
     * <code>builder.complementAgainst(-Double.MAX_VALUE, Double.MAX_VALUE);</code>
     * </p>
     * 
     * @return a reference to the builder instance for convenience
     */
    public Builder complement() {
      return complementAgainst(-Double.MAX_VALUE, Double.MAX_VALUE);
    }

    /**
     * Produces the complement of the current state relative to the supplied interval. This is
     * analogous to performing the standard set complement operation, but intersecting the result
     * with the supplied interval.
     * 
     * @param begin the beginning of the interval of applicability for the complement
     * @param end the end of the interval of applicability for the complement
     * 
     * @return a reference to the builder for convenience
     * 
     * @throws IllegalArgumentException if begin > end
     */
    public Builder complementAgainst(double begin, double end) {
      checkArgument(begin <= end);

      /*
       * Handle the trivial cases first, the current state is empty or the supplied window does not
       * intersect the current state. In this case the complement is the entire interval against
       * which it is to be applied.
       */
      if (state.committed == 0 || state.array[0] >= end
          || state.array[state.committed - 1] <= begin) {
        state.ensureCapacity(2);
        state.clear();
        state.array[state.committed++] = begin;
        state.array[state.committed++] = end;
        return this;
      }

      /*
       * We are going to use the buffer, so dump its contents and ensure it has sufficient capacity
       * to store the resultant complement. Note, absolute worst case buffer will require the number
       * of intervals in state plus an additional interval.
       */
      buffer.clear();

      /*
       * Now, this may seem a bit unusual, but the complement operator is simply a shift (within
       * some acceptable range) of all the elements in this list. Consider:
       * 
       * [b0,e0][b1,e1],[b2,e2],....,[bn,en]
       * 
       * the closure of its complement against the real line is:
       * 
       * (-infinity,b0],[e0,b1],[e1,b2],....,[en,infinity)
       * 
       * So it's a transfer of elements from the beginning to the end of intervals. Start by
       * locating the first end that is not less than the beginning of the input interval.
       */
      int index = indexOfIntervalEndJustAfterOrEqualTo(state, begin);

      /*
       * If the interval whose end was selected above does not split an interval in the input
       * window, the the complement begins with begin.
       */
      if ((index <= state.committed - 1) && (state.array[index - 1] > begin)) {
        /*
         * It's possible that the start of the interval just after begin occurs after the requested
         * end of the complement period. Correct for that.
         */
        buffer.add(begin, min(state.array[index - 1], end));
      }

      /*
       * Move intervals into the output buffer until we reach the end of the list or the start of
       * the next interval exceeds end.
       */
      while ((index < state.committed - 1) && (state.array[index + 1] < end)) {
        buffer.add(state.array[index], state.array[index + 1]);
        index += 2;
      }

      /*
       * Lastly, if the end of the input interval does not split an interval in the set, then end
       * must be the right most point in the complement.
       */
      if ((index <= state.committed - 1) && (state.array[index] < end)) {
        buffer.add(state.array[index], end);
      }

      /*
       * Transfer the contents of buffer to state.
       */
      swapStateAndBuffer();

      return this;

    }

    /**
     * Produces the complement of the current state relative to the supplied interval. This is
     * analogous to performing the standard set complement operation, but intersecting the result
     * with the supplied interval.
     * 
     * @param interval the beginning of the interval of applicability for the complement
     * 
     * @return a reference to the builder for convenience
     * 
     * @throws IllegalArgumentException if begin > end
     */
    public Builder complementAgainst(UnwritableInterval interval) {
      return complementAgainst(interval.begin, interval.end);
    }

    /**
     * Expands each interval present in the builder by the supplied delta maintaining the central
     * point as the fixed point.
     * <p>
     * This is analogous to calling: <code>builder.expand(delta/2.0,delta/2.0);</code>
     * </p>
     * 
     * @param delta the delta to contract the total length of the interval.
     * 
     * @return a reference to the builder for convenience
     */
    public Builder expand(double delta) {
      return expand(delta / 2.0, delta / 2.0);
    }

    /**
     * Expands each interval present in the builder by the supplied deltas.
     * 
     * @param beginDelta amount to subtract from the beginning of each interval, may be negative
     *        which contracts
     * @param endDelta amount to add to the end of each interval, may be negative which contracts
     * 
     * @return a reference to the builder for convenience
     */
    public Builder expand(double beginDelta, double endDelta) {

      /*
       * Nothing to do if the state array is empty.
       */
      if (state.committed == 0) {
        return this;
      }

      /*
       * Expand the intervals individually, as the over laps will be managed later.
       */
      int gone = 0;

      for (int i = 0; i <= state.committed - 1; i += 2) {
        state.array[i - gone] = state.array[i] - beginDelta;
        state.array[i - gone + 1] = state.array[i + 1] + endDelta;

        if (state.array[i - gone] > state.array[i - gone + 1]) {
          gone += 2;
        }
      }

      /*
       * Continue if at least one interval survived the expansion.
       */
      state.committed = state.committed - gone;

      if (state.committed == 0) {
        return this;
      }

      /*
       * None of the intervals can have extended to completely contain any of the other intervals.
       * (They were all expanded by the same amount.) So the first endpoint is still the first
       * endpoint.
       * 
       * Step through the state array, looking for the next endpoint less than the following begin
       * point. This marks the end of the new first interval and the beginning of the new second
       * interval. Keep this up until the last endpoint has been reached. This remains the last
       * endpoint.
       */
      int i = 1;
      int j = 1;

      while (j < state.committed - 1) {
        if (state.array[j] < state.array[j + 1]) {
          state.array[i] = state.array[j];
          state.array[i + 1] = state.array[j + 1];
          i += 2;
        }
        j += 2;
      }

      state.array[i] = state.array[j];
      state.committed = i + 1;

      return this;
    }

    /**
     * Contracts each interval present in the builder by the supplied delta maintaining the central
     * point as the fixed point.
     * <p>
     * This is analogous to calling: <code>builder.contract(delta/2.0,delta/2.0);</code>
     * </p>
     * 
     * @param delta the delta to contract the total length of the interval.
     * 
     * @return a reference to the builder for convenience
     */
    public Builder contract(double delta) {
      return contract(delta / 2.0, delta / 2.0);
    }

    /**
     * Contracts each interval currently present in the builder by the supplied deltas, removing
     * intervals whose measure becomes negative.
     * 
     * @param beginDelta amount to add to the beginning of each interval, may be negative which
     *        expands
     * @param endDelta amount to subtract from the end of each interval, may be negative which
     *        expands
     * 
     * @return a reference to the builder for convenience
     */
    public Builder contract(double beginDelta, double endDelta) {
      return expand(-beginDelta, -endDelta);
    }

    /**
     * Fills gaps in the current state of the builder of a particular size.
     * 
     * @param gapSize the gaps less than or equal to this are filled in
     * 
     * @return a reference to the builder for convenience
     * 
     * @throws IllegalArgumentException if gapSize is &lt; 0
     */
    public Builder fillGaps(double gapSize) {

      checkArgument(gapSize >= 0, "Gap size parameter must not be negative.");

      /*
       * Nothing to do if the current state is empty.
       */
      if (state.committed == 0) {
        return this;
      }

      int i = 1;
      int j = 1;

      while (j < state.committed - 1) {

        if ((state.array[j] + gapSize) < state.array[j + 1]) {
          state.array[i] = state.array[j];
          state.array[i + 1] = state.array[j + 1];
          i += 2;
        }

        j += 2;

      }

      state.array[i] = state.array[j];

      state.committed = i + 1;

      return this;
    }

    /**
     * Filters out any intervals in the builder less than or equal to a certain length
     * 
     * @param lengthToRemove the measure of intervals less than or equal to which to remove from the
     *        builder
     * 
     * @return a reference to the instance for convenience
     * 
     * @throws IllegalArgumentException if lengthToRemove &lt; 0
     */
    public Builder removeIntervals(double lengthToRemove) {

      checkArgument(lengthToRemove >= 0, "Length to remove must not be negative");

      int i = -1;
      int j = 1;

      while (j <= state.committed - 1) {

        if ((state.array[j] - state.array[j - 1]) > lengthToRemove) {
          i += 2;
          state.array[i - 1] = state.array[j - 1];
          state.array[i] = state.array[j];
        }

        j += 2;

      }

      state.committed = i + 1;

      return this;
    }

    public Builder add(UnwritableInterval interval) {
      return this.add(interval.begin, interval.end);
    }

    /**
     * Adds an interval to the current state of the builder. This is analogous to creating a single
     * interval inteval set and unioning it with the current state of the builder.
     * 
     * @param begin the start of the interval to add
     * @param end the end of the interval to add
     * 
     * @return a reference to the builder instance for convenience
     * 
     * @throws IllegalArgumentException if begin > end
     */
    public Builder add(double begin, double end) {
      checkArgument(begin <= end);

      /*
       * We might have to add an interval to the state array. Just make sure it has the necessary
       * capacity to do so.
       */
      state.ensureCapacity(state.committed + 2);

      /*
       * Start out with the simple cases, state is empty, input interval comes after all existing
       * intervals, input interval precedes all existing intervals.
       */
      if ((state.committed == 0) || (begin > state.array[state.committed - 1])) {
        state.array[state.committed++] = begin;
        state.array[state.committed++] = end;
        return this;
      }

      /*
       * Ignore the intervals that lie completely before the start of the interval to insert.
       */
      int index = indexOfIntervalEndJustAfterOrEqualTo(state, begin);

      /*
       * There are three cases that remain at this point:
       * 
       * [begin,end] lies entirely between two existing intervals
       * 
       * [begin,end] overlaps the next interval only
       * 
       * [begin,end] overlaps multiple intervals
       * 
       * Handle the first case first, as it is simplest. Recall we have already ensured that there
       * is enough space in the state array to absorb an additional interval.
       */
      if (end < state.array[index - 1]) {

        /*
         * Make room for the newly inserted interval.
         */
        for (int j = state.committed - 1; j >= index - 1; j--) {
          state.array[j + 2] = state.array[j];
        }

        state.array[index - 1] = begin;
        state.array[index] = end;
        state.committed += 2;
        return this;
      }

      /*
       * Now address the more complicated of cases. Locate the interval whose end is just past the
       * end of the interval we are preparing to add.
       */
      double newEnd = max(end, state.array[index]);
      int after = indexOfIntervalEndJustAfter(state, newEnd);

      /*
       * The begin and end of the requested addition may or may not replace the ends of the current
       * interval. Configure the interval in the state appropriately.
       */
      state.array[index - 1] = min(begin, state.array[index - 1]);
      state.array[index] = newEnd;

      /*
       * Check to see if the modified extends into the next interval, merge the two.
       */
      if ((after <= state.committed - 1) && (state.array[index] >= state.array[after - 1])) {
        state.array[index] = state.array[after];
        after += 2;
      }

      /*
       * Move the rest of the intervals forward to take up spaces left by the absorbed intervals.
       */
      while (after <= state.committed - 1) {
        index += 2;
        state.array[index - 1] = state.array[after - 1];
        state.array[index] = state.array[after];
        after += 2;
      }

      /*
       * The length of the contents of the state array is one more than the index.
       */
      state.committed = index + 1;

      return this;
    }

    /**
     * Returns the index of the endpoint of the interval just after the specified value.
     * 
     * @param intervalList the ordered list of doubles forming a set of intervals.
     * 
     * @param value the value of interest
     * 
     * @return an index that is odd (interval end point) in the range: [1, intervalList.size()+1]
     */
    @VisibleForTesting
    static int indexOfIntervalEndJustAfterOrEqualTo(Content content, double value) {
      int index =
          ArrayUtilities.firstGreaterThanOrEqualTo(content.array, 0, content.committed, value);

      /*
       * Adjust the index to move to the end of the first interval whose end is just past begin.
       * Note: index must be pointing to the
       */
      if (index % 2 == 0) {
        index++;
      }
      return index;
    }

    /**
     * Returns the index of the endpoint of the interval just after or equivalent to the specified
     * value.
     * 
     * @param intervalList the order list of doubles forming a set of intervals
     * 
     * @param value the value of interest
     * 
     * @return an index that is odd (interval end point) in the range: [1, intervaList.size()+1]
     */
    @VisibleForTesting
    static int indexOfIntervalEndJustAfter(Content content, double value) {
      int index = ArrayUtilities.firstGreaterThan(content.array, 0, content.committed, value);

      /*
       * Adjust the index to move to the end of the first interval whose end is just past begin.
       * Note: index must be pointing to the
       */
      if (index % 2 == 0) {
        index++;
      }
      return index;
    }

    @Override
    public Interval get(int index, Interval buffer) {
      checkElementIndex(index, state.committed / 2);
      buffer.set(state.array[2 * index], state.array[2 * index + 1]);
      return buffer;
    }

    public UnwritableInterval get(int index) {
      checkElementIndex(index, state.committed / 2);
      return new UnwritableInterval(state.array[2 * index], state.array[2 * index + 1]);
    }

    @Override
    public int size() {
      return state.committed / 2;
    }

  }

  /**
   * Convenience method to create an interval set from the supplied interval.
   * <p>
   * Note: this is not a wrapper, but rather a snapshot of the contents of the supplied interval
   * argument is created and converted into an interval set. Mutation of the supplied interval
   * argument after creating the {@link IntervalSet} has no impact on the state of the returned
   * object.
   * </p>
   * 
   * @param interval the interval to convert
   * 
   * @return a newly created {@link IntervalSet} containing the single interval provided.
   */
  public static IntervalSet create(UnwritableInterval interval) {
    return builder().add(interval).build();
  }

  /**
   * Convenience method to create an interval set from the supplied intervals.
   * <p>
   * Intervals may overlap, the resultant {@link IntervalSet} will be the union of the supplied
   * intervals.
   * </p>
   * 
   * @param intervals intervals to use to create a new set
   * 
   * @return a newly created interval set containing the union of the supplied intervals.
   */
  public static IntervalSet create(Iterable<? extends UnwritableInterval> intervals) {

    /*
     * Skip the work if possible. Technically this isn't creating, but IntervalSet is truly
     * immutable, so it's safe.
     */
    if (intervals instanceof IntervalSet) {
      return (IntervalSet) intervals;
    }

    return builder().addAll(intervals).build();
  }

  /**
   * Convenience method to create an interval set from the supplied list of doubles.
   * <p>
   * Turns a list of doubles: {1,2,3,4,5,6} into an interval set: [1,2] [3,4] [5,6]
   * </p>
   * 
   * @param intervals a list of doubles, alternating begin and end of a series of intervals. Even
   *        indexed elements mark the beginning of intervals, odd the end.
   * 
   * @return a newly created interval set containing the union of the intervals
   * 
   * @throws IllegalArgumentException if intervals is not of even length, or if the beginning of a
   *         specified interval strictly exceeds its specified end
   */
  public static IntervalSet create(double... intervals) {
    checkArgument(intervals.length % 2 == 0, "intervals array must be even");
    List<UnwritableInterval> results = Lists.newArrayList();
    for (int i = 0; i < intervals.length; i += 2) {
      results.add(new UnwritableInterval(intervals[i], intervals[i + 1]));
    }
    return builder(intervals.length / 2).setTo(results).build();
  }

  /**
   * Convenience method to create an interval set from the supplied list of intervals.
   * 
   * @param intervals a sequence of intervals to union together to create the interval set.
   * 
   * @return a newly created interval set containing the union of the intervals
   */
  public static IntervalSet create(UnwritableInterval... intervals) {
    Builder builder = builder();
    for (UnwritableInterval interval : intervals) {
      builder.add(interval);
    }
    return builder.build();
  }

  /**
   * Creates a new builder and installs the state of the instance into it.
   * 
   * @return a newly constructed builder with its state configured as the instance.
   */
  private Builder createBuilder() {
    return builder(this.size()).addAll(this);
  }

  /**
   * Convenience method that creates the union of this set with another.
   * 
   * @param set the set to union with the instance.
   * 
   * @return a newly constructed set containing the union of the instance and set.
   */
  public IntervalSet union(IntervalSet set) {
    if (this.equals(LINE) || set.equals(LINE)) {
      return LINE;
    }
    if (this.isEmpty()) {
      return set;
    }
    if (set.isEmpty()) {
      return this;
    }
    return createBuilder().union(set).build();
  }

  /**
   * Convenience method that creates the union of this set with an interval.
   * 
   * @param interval the interval to union with the instance
   * 
   * @return a newly constructed set containing the union of the instance and interval.
   */
  public IntervalSet union(UnwritableInterval interval) {
    return union(IntervalSet.create(interval));
  }

  /**
   * Convenience method that creates the intersection of this set with another.
   * 
   * @param set the set to intersect with the instance.
   * 
   * @return a newly constructed set containing the intersection of the instance and set.
   */
  public IntervalSet intersect(IntervalSet set) {
    if (this.isEmpty() || set.isEmpty()) {
      return EMPTY;
    }
    if (this.equals(LINE)) {
      return set;
    }
    if (set.equals(LINE)) {
      return this;
    }
    return createBuilder().intersect(set).build();
  }

  /**
   * Convenience method that creates the intersection of this set with an interval.
   * 
   * @param interval the interval to intersect with the instance.
   * 
   * @return a newly constructed set containing the intersection of the instance and interval.
   */
  public IntervalSet intersect(UnwritableInterval interval) {
    return intersect(IntervalSet.create(interval));
  }

  /**
   * Convenience method that creates the difference of this set with another.
   * 
   * @param set the set to remove from the instance
   * 
   * @return a newly constructed set containing the difference of the instance and set (instance /
   *         set)
   */
  public IntervalSet difference(IntervalSet set) {
    return createBuilder().difference(set).build();
  }

  /**
   * Convenience method that creates the difference of this set with an interval.
   * 
   * @param interval the interval to remove from the instance
   * 
   * @return a newly constructed set containing the difference of the instance and interval
   *         (instance / interval)
   */
  public IntervalSet difference(UnwritableInterval interval) {
    return createBuilder().difference(create(interval)).build();

  }

  /**
   * Convenience method that creates a new set by applying the supplied filter to the instance.
   * 
   * @param filter the filter to apply to the instance
   * 
   * @return a newly constructed set containing the result of the filter's application to the
   *         instance
   */
  public IntervalSet filter(Predicate<UnwritableInterval> filter) {
    return createBuilder().filter(filter).build();
  }

  /**
   * Convenience method that creates the complement of the instance.
   * 
   * @return a newly constructed set containing the complement of the instance
   */
  public IntervalSet complement() {
    return createBuilder().complement().build();
  }

  /**
   * Convenience method that creates the complement of the instance against an interval
   * 
   * @param begin the start of the interval to complement against
   * @param end the end of the interval to complement against
   * 
   * @return a newly constructed set containing the complement of the instance intersected with the
   *         interval [begin,end]
   */
  public IntervalSet complementAgainst(double begin, double end) {
    return createBuilder().complementAgainst(begin, end).build();
  }

  /**
   * Convenience method that creates the complement of the instance against the supplied interval
   * 
   * @param interval the interval against which to complement the instance
   * 
   * @return a newly constructed set containing the complement of the instance intersected with
   *         interval
   */
  public IntervalSet complementAgainst(UnwritableInterval interval) {
    return createBuilder().complementAgainst(interval).build();
  }

  /**
   * Convenience method that creates the expansion of intervals in the set
   * 
   * @param delta the total amount about which to expand each interval, may be negative which
   *        implies contraction
   * 
   * @return a newly constructed set containing the expansion of the instance by delta about the
   *         midpoint of each existing interval
   */
  public IntervalSet expand(double delta) {
    return createBuilder().expand(delta).build();
  }

  /**
   * Convenience method that creates the expansion of the intervals in the set by applying deltas to
   * each end of the intervals separately.
   * 
   * @param beginDelta the amount to subtract from the start of each interval, may be negative which
   *        implies contraction
   * @param endDelta the amount to add to the end of each interval, may be negative which implies
   *        contraction
   * 
   * @return a newly constructed set containing the expansion of the instance by the specified
   *         deltas
   */
  public IntervalSet expand(double beginDelta, double endDelta) {
    return createBuilder().expand(beginDelta, endDelta).build();
  }

  /**
   * Convenience method that creates the contraction of intervals in the set
   * 
   * @param delta the total amount about which to contract each interval, may be negative which
   *        implies expansion
   * 
   * @return a newly constructed set containing the contraction of the instance by delta about the
   *         midpoint of each existing interval
   */
  public IntervalSet contract(double delta) {
    return createBuilder().contract(delta).build();
  }

  /**
   * Convenience method that creates the contraction of the intervals in the set by applying deltas
   * to each end of the intervals separately.
   * 
   * @param beginDelta the amount to add to the start of each interval, may be negative which
   *        implies expansion
   * @param endDelta the amount to subtract from the end of each interval, may be negative which
   *        implies expansion
   * 
   * @return a newly constructed set containing the contraction of the instance by the specified
   *         deltas
   */
  public IntervalSet contract(double beginDelta, double endDelta) {
    return createBuilder().contract(beginDelta, endDelta).build();
  }

  /**
   * Convenience method that creates a gap filled copy of the instance.
   * 
   * @param gapSize the gaps less than or equal to this are filled in
   * 
   * @return a newly constructed set whose gaps have been filled
   * 
   * @throws IllegalArgumentException if gapSize is &lt; 0
   */
  public IntervalSet fillGaps(double gapSize) {
    return createBuilder().fillGaps(gapSize).build();
  }

  /**
   * Convenience method that creates a filtered version of the instance with intervals removed
   * 
   * @param lengthToRemove the measure of intervals less than or equal to which to remove from the
   *        builder
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if lengthToRemove &lt; 0
   */
  public IntervalSet removeIntervals(double lengthToRemove) {
    return createBuilder().removeIntervals(lengthToRemove).build();
  }

  /**
   * Convenience method that adds the interval to the instance. Note: this is an expensive way to
   * build up a set of intervals, consider using the {@link Builder} as this is precisely for what
   * it was designed.
   * 
   * @param interval the interval to add to the instance
   * 
   * @return a newly constructed set containing the union of the instance with interval
   */
  public IntervalSet add(UnwritableInterval interval) {
    return createBuilder().add(interval).build();
  }

  /**
   * Convenience method that adds the specified interval to the instance. Note: this is an expensive
   * way to build up a set of intervals, consider using the {@link Builder} as this is precisely for
   * what it was designed.
   * 
   * @param begin the start of an interval
   * @param end the end of an interval
   * 
   * @return a newly constructed set containing the union of the instance with [begin,end]
   * 
   * @throws IllegalArgumentException if end &lt; begin
   */
  public IntervalSet add(double begin, double end) {
    return createBuilder().add(begin, end).build();
  }

  /**
   * Applies a {@link UnivariateFunction} to the end points of intervals within the interval set.
   * <p>
   * If the supplied function is not monotonically increasing over the domain covered by this
   * interval set, then the results are likely <b>not</b> to be what is expected. This method simply
   * invokes the supplied function on the end points and accumulates the results in a newly created
   * interval set.
   * </p>
   * 
   * @param function a monotonically increasing univariate function
   * 
   * @return a newly created interval set with the function applied to each intervals end points
   */
  public IntervalSet applyToEndPoints(UnivariateFunction function) {
    IntervalSet.Builder resultBuilder = new IntervalSet.Builder(this.size());

    for (UnwritableInterval interval : this) {
      resultBuilder.add(function.evaluate(interval.getBegin()),
          function.evaluate(interval.getEnd()));
    }

    return resultBuilder.build();
  }

}
