package picante.timeline;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TreeMap;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMultiset;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.Maps;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

/**
 * Class that captures the evolving state of a system on a covered timeline.
 * <p>
 * This class is immutable, and can only be created by use of {@link StateTimeline.Builder}, which
 * are itself available from static utility methods available on the class.
 * </p>
 *
 * @param <S> the state class, which must implement {@link Object#equals(Object)} and
 *        {@link Object#hashCode()} properly.
 */
public final class StateTimeline<S> {

  /**
   * Domain of the StateTimeline
   */
  private final UnwritableInterval domain;

  /**
   * Monotonically increasing transition times, which are located on the interior of the domain;
   * will be empty when the timeline contains a single state
   */
  private final ImmutableList<Double> transitionTimes;

  /**
   * Sorted states of the StateTimeline; will contain at least one State
   */
  private final ImmutableList<S> states;

  /**
   * Creates a new StateTimeline
   * <p>
   * This constructor is package private, and assumes the necessary checks for consistency on the
   * map have been performed by the caller.
   * </p>
   * 
   * @param intervalMap a map of intervals to state S, sorted by the begin values of the
   *        UnwritableInterval key. Note: these intervals should only overlap on the boundary, and
   *        should completely cover the beginning of the first interval through the end of the last
   *        interval.
   */
  StateTimeline(NavigableMap<UnwritableInterval, S> intervalMap) {
    this.domain =
        new UnwritableInterval(intervalMap.firstKey().getBegin(), intervalMap.lastKey().getEnd());
    this.transitionTimes =
        ImmutableList.copyOf(AbstractStateTimeline.getTransitionTimes(intervalMap));
    this.states = ImmutableList.copyOf(intervalMap.values());
  }

  private StateTimeline(UnwritableInterval domain, ImmutableList<Double> transitionTimes,
      ImmutableList<S> states) {
    this.domain = domain;
    this.transitionTimes = transitionTimes;
    this.states = states;
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
  public UnwritableInterval getDomain() {
    return domain;
  }

  /**
   * Retrieves a list of times where states transition from one to another.
   * 
   * @return the list of times where states are transition from one to another
   */
  public ImmutableList<Double> getTransitionTimes() {
    return transitionTimes;
  }

  /**
   * Retrieves an {@link ImmutableList} of all the elements in ascending interval order within the
   * timeline.
   * 
   * @return an immutable list that contains the states in ascending order
   */
  public ImmutableList<S> getStates() {
    return states;
  }

  /**
   * Retrieves the {@link IntervalSet} supporting the specified state on the timeline.
   * <p>
   * This function retrieves the intervals in the domain of the timeline that would map to the
   * supplied state of interest.
   * </p>
   * 
   * @param state the state to find its support
   * 
   * @return an {@link IntervalSet} containing the intervals that
   *         {@link StateTimeline#getState(double)} would return state for in their interior.
   */
  public IntervalSet getSupport(S state) {
    return getSupport(Predicates.equalTo(state));
  }

  /**
   * Retrieves the {@link ImmutableList} of supporting states that match a {@link Predicate}.
   * <p>
   * This analog of the {@link StateTimeline#getSupport(Predicate)} method that returns the actual
   * intervals, not coalesced into a single interval as with {@link IntervalSet}. The reason that
   * there is no analog of {@link StateTimeline#getSupport(Object)} is that it would be equivalent
   * that method due to how intervals are coalesced with adjacent states, S, being
   * {@link Object#equals(Object)} to one another.
   * </p>
   * 
   * @param filter the filter to apply to states for inclusion in the support
   * 
   * @return an {@link ImmutableList} containing the intervals that
   *         {@link StateTimeline#getState(double)} would return for a state that
   *         {@link Predicate#apply(Object)} would return true.
   */
  public ImmutableList<UnwritableInterval> getSupportIntervals(Predicate<? super S> filter) {
    ImmutableList.Builder<UnwritableInterval> resultBuilder = ImmutableList.builder();
    if (getTransitionTimes().size() == 0) {
      if (filter.apply(getStates().get(0))) {
        resultBuilder.add(getDomain());
      }
      return resultBuilder.build();
    }
    if (filter.apply(getStates().get(0))) {
      resultBuilder
          .add(new UnwritableInterval(getDomain().getBegin(), getTransitionTimes().get(0)));
    }
    for (int j = 1; j < getTransitionTimes().size(); j++) {
      if (filter.apply(getStates().get(j))) {
        resultBuilder.add(
            new UnwritableInterval(getTransitionTimes().get(j - 1), getTransitionTimes().get(j)));
      }
    }
    if (filter.apply(getStates().get(getStates().size() - 1))) {
      resultBuilder.add(new UnwritableInterval(
          getTransitionTimes().get(getTransitionTimes().size() - 1), getDomain().getEnd()));
    }
    return resultBuilder.build();
  }

  /**
   * Retrieves the {@link IntervalSet} supporting states that match a {@link Predicate}.
   * 
   * @param filter the filter to apply to states for inclusion in the support.
   * 
   * @return an {@link IntervalSet} containing the intervals that
   *         {@link StateTimeline#getState(double)} would return for a state that
   *         {@link Predicate#apply(Object)} would return true.
   */
  public IntervalSet getSupport(Predicate<? super S> filter) {
    IntervalSet.Builder builder = IntervalSet.builder();
    if (getTransitionTimes().size() == 0) {
      if (filter.apply(getStates().get(0))) {
        builder.add(getDomain());
      }
      return builder.build();
    }
    if (filter.apply(getStates().get(0))) {
      builder.add(new UnwritableInterval(getDomain().getBegin(), getTransitionTimes().get(0)));
    }
    for (int j = 1; j < getTransitionTimes().size(); j++) {
      if (filter.apply(getStates().get(j))) {
        builder.add(
            new UnwritableInterval(getTransitionTimes().get(j - 1), getTransitionTimes().get(j)));
      }
    }
    if (filter.apply(getStates().get(getStates().size() - 1))) {
      builder.add(new UnwritableInterval(getTransitionTimes().get(getTransitionTimes().size() - 1),
          getDomain().getEnd()));
    }
    return builder.build();
  }

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

    /*
     * Check if 't' falls in the domain
     */
    checkArgument(getDomain().closedContains(t),
        "Requested time: %s is outside supported domain: .", t, getDomain());

    return getStates().get(getIndexOfState(t, direction));
  }

  /**
   * Retrieves the interval associated with an element of the domain.
   * <p>
   * If the element happens to lie on a transition point, then it chooses the interval of the
   * subsequent interval. This method is equivalent to:
   * {@link StateTimeline#getInterval(double, Direction)} with {@link Direction#NEXT} as the
   * direction argument.
   * </p>
   * 
   * @param t the value from the domain to find the interval
   * 
   * @return the state associated with t
   * 
   * @throws IllegalArgumentException if t is not contained within {@link StateTimeline#getDomain()}
   */
  public UnwritableInterval getInterval(double t) {
    return getInterval(t, Direction.NEXT);
  }

  /**
   * Retrieves the interval associated with an element of the domain, resolving intervals associated
   * with a transition time according to a specified direction.
   * 
   * @param t the value from the domain to find the interval
   * @param direction the choice when t is at a transition boundary between two intervals
   * 
   * @return the value associated with t, or the requested value in the event t is a transition
   *         point
   * 
   * @throws IllegalArgumentException if t is not contained within {@link StateTimeline#getDomain()}
   */
  public UnwritableInterval getInterval(double t, Direction direction) {

    /*
     * Check if 't' falls in the domain
     */
    checkArgument(getDomain().closedContains(t),
        "Requested time: %s is outside supported domain: .", t, getDomain());

    /*
     * Simple case of one state, meaning no transition times
     */
    if (getStates().size() == 1) {
      return getDomain();
    }

    /*
     * Find the location in the state list
     */
    int i = getIndexOfState(t, direction);

    /*
     * Form and return the interval, taking care of boundary cases
     */
    if (i == 0) {
      return new UnwritableInterval(getDomain().getBegin(), getTransitionTimes().get(0));
    }
    if (i == getStates().size() - 1) {
      return new UnwritableInterval(getTransitionTimes().get(i - 1), getDomain().getEnd());
    }
    return new UnwritableInterval(getTransitionTimes().get(i - 1), getTransitionTimes().get(i));

  }

  private int getIndexOfState(double t, Direction direction) {
    return getIndexOfState(getIndexOfBin(getTransitionTimes(), t), t, direction);
  }

  private int getIndexOfState(int i, double t, Direction direction) {

    /*
     * If left of the transition times, it must be the first state
     */
    if (i == -1) {
      return 0;
    }

    /**
     * <pre>
     * This if statement needs explanation. First, the setup:
     *      
     *       0     1     2     3     4     5         index, i, of transitionTimes
     *       
     * [     |     |     |     |     |     |     ]   state and domain bounds
     * 
     *    0     1     2     3     4     5     6      index, j,  of states list
     *    
     * At this point in the execution of 'getTransitionTimes,' we must have
     * 0 <= i < transitionTimes.size(), which means i is in the index range of states list.
     * 
     * If 't' does not fall exactly on a transition boundary, then the state index is j = i+1.
     * 
     * If 't' falls on a boundary, then there are two choices: use the previous state or
     * or use the next state. If PREVIOUS, j = i; if NEXT, j = i+1.
     * </pre>
     */
    if (t != getTransitionTimes().get(i) || direction == Direction.NEXT) {
      return i + 1;
    }

    return i;
  }

  /**
   * <p>
   * Input list 'div' of size N represents the locations of the dividers of a battery of N-1 bins;
   * the locations of the dividers must be monotonically increasing. Repeated values are allowed,
   * but only the bin of last of the repeated values will ever be returned. Each bin is a
   * closed-open interval, meaning a value falling exactly on a divider is considered as residing in
   * the bin to the right of that divider. Behaves identically to IDL's VALUE_LOCATE function.
   * <p>
   * If j = -1, val < div.get(0) and is to the left of the battery of bins
   * <p>
   * If 0 <= j < div.size()-1, div.get(j) <= val < div.get(j+1) and is located within the jth bin
   * <p>
   * If j = div.size()-1, val >= div.get(div.size()-1) and is to the right of the battery of bins
   * (or located exactly on the last divider)
   * 
   * @param div list of locations of bin dividers; must be monotonically increasing
   * @param val value whose bin is being located
   * @return integer index j of the bin value is located; -1 <= j <= div.size()-1.
   */
  private static final int getIndexOfBin(List<Double> div, double val) {
    int ju = div.size(), jm, jl = -1;
    while (ju - jl > 1) {
      jm = (ju + jl) >> 1;
      if (val >= div.get(jm)) {
        jl = jm;
      } else {
        ju = jm;
      }
    }
    return jl;
  }

  /**
   * Retrieves a {@link ImmutableMultiset} of all the elements contained within the set.
   * <p>
   * If you are interested in only the set of distinct elements, simply invoke:
   * {@link ImmutableMultiset#entrySet()} on the return value.
   * </p>
   * 
   * @return a multi-set that tracks the number of occurrences and their ordering.
   */
  public ImmutableMultiset<S> getStatesMultiset() {
    return ImmutableMultiset.<S>builder().addAll(getStates()).build();
  }

  /**
   * Retrieves the pairing of intervals to states for the entire timeline.
   */
  public Collection<Entry<UnwritableInterval, S>> getEntries() {
    return new StateTimelineCollection<>(this);
  }

  /**
   * Creates a view of the underlying {@link StateTimeline#getState(double)} methodas a
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

  /**
   * Creates a subset of the timeline covering the requested interval.
   * 
   * @param interval the interval that defines the domain of the subset
   * 
   * @return a {@link StateTimeline} that is the subset of this instance
   * 
   * @throws IllegalArgumentException if interval is not completely contained within
   *         {@link StateTimeline#getDomain()} of the instance on which this method is invoked, and
   *         further the intersection must have non-zero length.
   */
  public StateTimeline<S> subset(UnwritableInterval interval) {

    /*
     * Handle the simple case, if interval is the domain.
     */
    if (interval.equals(getDomain())) {
      return this;
    }

    checkArgument(interval.getLength() > 0,
        "Interval must be of length strictly greater than zero: %s", interval);

    /*
     * Check to see that interval is completely covered in the domain.
     */
    checkArgument(getDomain().closedContains(interval),
        "Interval %s must be completely contained within domain %s", interval, domain);

    /*
     * Get the indices (inclusive) of the transition times within the subset interval
     */
    int iTimeBeg = getIndexOfBin(getTransitionTimes(), interval.getBegin());
    int iTimeEnd = getIndexOfBin(getTransitionTimes(), interval.getEnd());

    /*
     * Get indices (inclusive) of the states that cover the subset interval
     */
    int iStateBeg = getIndexOfState(iTimeBeg, interval.getBegin(), Direction.NEXT);
    int iStateEnd = getIndexOfState(iTimeEnd, interval.getEnd(), Direction.PREVIOUS);

    /*
     * Update the transitionTimes indices to accurately reflect transition subset
     */
    iTimeBeg++;
    if (iTimeEnd >= 0 && interval.getEnd() == getTransitionTimes().get(iTimeEnd)) {
      iTimeEnd--;
    }

    /*
     * Grab the sublists from 'transitionTimes' and 'states,' and return new StateTimeline
     */
    return new StateTimeline<>(UnwritableInterval.copyOf(interval),
        /*
         * This condition prevents out-of-range iTimeBeg and iTimeEnd from being used
         */
        iTimeBeg <= iTimeEnd ? getTransitionTimes().subList(iTimeBeg, iTimeEnd + 1)
            : ImmutableList.of(),
        /*
         * iStateBeg and iStateEnd are never out-of-range
         */
        getStates().subList(iStateBeg, iStateEnd + 1));
  }

  /**
   * Creates a subset of the timeline covering the requested interval.
   * 
   * @param begin the start of the interval
   * @param end the end of the interval
   * 
   * @return a {@link StateTimeline} that is the subset of this instance
   * 
   * @throws IllegalArgumentException if end &lt; begin, or if [begin, end] is not completely
   *         contained within {@link StateTimeline#getDomain()} of the instance on which this method
   *         is invoked, and further the intersection must have non-zero length.
   * 
   */
  public StateTimeline<S> subset(double begin, double end) {
    return subset(new UnwritableInterval(begin, end));
  }

  /**
   * Creates a subset of the timeline for values less than a particular value in the domain.
   * 
   * @param t the value defining the end of the subset timeline.
   * 
   * @return a {@link StateTimeline} that covers from {@link StateTimeline#getDomain()}'s beginning
   *         to t.
   * 
   * @throws IllegalArgumentException if t &le; the beginning of {@link StateTimeline#getDomain()}
   */
  public StateTimeline<S> subsetLessThan(double t) {
    return subset(new UnwritableInterval(getDomain().getBegin(), t));
  }

  /**
   * Creates a subset of the timeline for values greater than a particular value in the domain.
   * 
   * @param t the value defining the end of the subset timeline.
   * 
   * @return a {@link StateTimeline} that covers from {@link StateTimeline#getDomain()}'s beginning
   *         to t.
   * 
   * @throws IllegalArgumentException if t &ge; the end of {@link StateTimeline#getDomain()}
   */
  public StateTimeline<S> subsetGreaterThan(double t) {
    return subset(new UnwritableInterval(t, getDomain().getEnd()));
  }

  /**
   * Transforms the states of the timeline according to the input function.
   * <p>
   * If the function results in back-to-back intervals with identical {@code T}, the intervals are
   * merged.
   * </p>
   * 
   * @param function {@link Function} for converting states from {@code S} to {@code T}
   * 
   * @return a {@link StateTimeline} whose States have been transformed from {@code S} to {@code T}
   */
  public <T> StateTimeline<T> transform(Function<? super S, T> function) {

    /*
     * Transformed states list builder
     */
    ImmutableList.Builder<T> transformedStatesB = ImmutableList.builder();

    /*
     * Transition times of transformed states list builder
     */
    ImmutableList.Builder<Double> transformedTimesB = ImmutableList.builder();

    /*
     * Previous state (last element in 'transformedStatesB' list builder
     */
    T prevState = function.apply(getStates().get(0));

    /*
     * Add first transformed state to builder
     */
    transformedStatesB.add(prevState);

    /*
     * Loop over states, transforming and adding if the state is different than the previous state
     */
    for (int j = 1; j < getStates().size(); j++) {
      T nextState = function.apply(getStates().get(j));
      if (!nextState.equals(prevState)) {
        transformedStatesB.add(nextState);
        transformedTimesB.add(getTransitionTimes().get(j - 1));
        prevState = nextState;
      }
    }

    /*
     * Build and return new timeline
     */
    return new StateTimeline<>(getDomain(), transformedTimesB.build(), transformedStatesB.build());
  }

  /**
   * Overlays onto this StateTimeline the input state with the given support.
   * <p>
   * The support of the input state is restricted to the domain of this StateTimeline.
   * 
   * @param stateSupport support of the state to overlay
   * @param state the state to overlay and replaces states in the current timeline
   * 
   * @return new StateTimeline with the input state overlaid onto it
   */
  public StateTimeline<S> overlay(IntervalSet stateSupport, S state) {

    /*
     * Take the intersection of this domain and the support of state
     */
    IntervalSet support = stateSupport.intersect(getDomain());

    /*
     * Handle the simple case, if the support is empty
     */
    if (support.isEmpty()) {
      return this;
    }

    /*
     * Create a builder with the current timeline
     */
    Builder<S> b = create(this);

    /*
     * Add the intervals and state
     */
    b.add(support, state);

    /*
     * Build and return
     */
    return b.build();

  }

  /**
   * Overlays onto this StateTimeline the input state with the given support.
   * <p>
   * If the support of the input state extends beyond the domain of this StateTimeline, the
   * resulting StateTimeline is expanded using the input default state for the gaps in the expanded
   * regions.
   * 
   * @param stateSupport support of the state to overlay
   * @param state the state to overlay and replaces states in the current timeline
   * @param defaultState the default state used to fill any gaps that result from expansion
   * 
   * @return new StateTimeline with the input state overlaid onto it
   */
  public StateTimeline<S> overlayExpanding(IntervalSet stateSupport, S state, S defaultState) {

    /*
     * Handle the simple case, if the support is empty
     */
    if (stateSupport.isEmpty()) {
      return this;
    }

    /*
     * Create a builder with the current timeline
     */
    Builder<S> b = createExpanding(this, defaultState);

    /*
     * Add the intervals and state
     */
    b.add(stateSupport, state);

    /*
     * Build and return
     */
    return b.build();

  }

  /**
   * Creates a new builder covering the requested domain with the supplied default state.
   * <p>
   * This builder only allows assignments to intervals within the supplied domain.
   * </p>
   * 
   * @param domain the domain over which the time line is to be defined
   * @param defaultState the initial state of the system covering domain
   * 
   * @return a newly created builder constrained to the initial domain
   */
  public static <S> StateTimeline.Builder<S> create(UnwritableInterval domain, S defaultState) {
    return new ConstrainedBuilder<S>(domain, defaultState) {};
  }

  /**
   * Creates a new builder populating the timeline with an existing one.
   * <p>
   * This builder only allows assignments to the domain of the supplied initial timeline.
   * </p>
   * 
   * @param timeline a timeline to &quot;copy&quot;
   * 
   * @return a newly created builder populated with the supplied timeline and constrained to its
   *         domainF
   */
  public static <S> StateTimeline.Builder<S> create(StateTimeline<? extends S> timeline) {
    return new ConstrainedBuilder<S>(timeline);
  }

  /**
   * Creates a new builder initially covering the supplied domain with the default state.
   * <p>
   * This builder allows expansion, i.e. assignments to intervals outside of its currently defined
   * domain. When an interval that is not entirely contained within the existing domain is added,
   * the domain is appropriately expanded. Any gaps are filled with the default state.
   * </p>
   * 
   * @param initialDomain the domain over which the timeline is to be defined
   * @param defaultState the initial state of the system covering domain
   * 
   * @return a newly created builder that expands on demand
   */
  public static <S> StateTimeline.Builder<S> createExpanding(UnwritableInterval initialDomain,
      S defaultState) {
    return new Builder<S>(initialDomain, defaultState);
  }

  /**
   * Creates a new builder initially populating its contents with the supplied timeline.
   * <p>
   * This builder allows expansion, i.e. assignments to intervals outside of its currently defined
   * domain. When an interval that is not entirely contained within the existing domain is added,
   * the domain is appropriately expanded. Any gaps are filled with the default state.
   * </p>
   * 
   * @param timeline the initial timeline to &quot;copy&quot;
   * @param defaultState the default state used to fill any gaps that result from expansion.
   * 
   * @return a newly created builder that expands on demand
   */
  public static <S> StateTimeline.Builder<S> createExpanding(StateTimeline<? extends S> timeline,
      S defaultState) {
    return new Builder<S>(timeline, defaultState);
  }


  /**
   * Builder for {@link StateTimeline}, that provides rudimentary &quot;view-based&quot; accessor
   * methods for querying state.
   * <p>
   * For more sophisticated queries, simply {@link Builder#build()} a {@link StateTimeline}
   * instance. Note: while this class is following the {@link picante.designpatterns.Builder}
   * pattern, it does not subscribe to the method chaining philosophy often utilized. This was
   * intentional given the complexity of some of the methods involved and the richer query methods
   * provided to interrogate the state of the builder. In general a builder instance can act as a
   * stand-in for a mutable variant of the {@link StateTimeline} class.
   * </p>
   *
   * @param <S> the state class captured by the builder and its generated timelines. This class must
   *        implement {@link Object#equals(Object)} and {@link Object#hashCode()} properly.
   */
  public static class Builder<S> extends AbstractStateTimeline<S>
      implements picante.designpatterns.Builder<StateTimeline<S>, RuntimeException> {

    /**
     * The mutable view of the {@link NavigableMap}
     */
    final TreeMap<UnwritableInterval, S> intervalMap;
    final Interval domain;

    /**
     * The default state to be utilized during any expansion efforts
     */
    private final S defaultState;

    /**
     * Static utility method to create the initial map containing a single domain interval and the
     * corresponding default state.
     * <p>
     * This method exists to supply the super constructor with the {@link NavigableMap}.
     * </p>
     */
    static <S> TreeMap<UnwritableInterval, S> createMap(UnwritableInterval domain, S defaultState) {
      TreeMap<UnwritableInterval, S> result = Maps.newTreeMap(Interval.BEGIN_COMPARATOR);
      result.put(UnwritableInterval.copyOf(domain), defaultState);
      return result;
    }

    /**
     * Static utility method to create the initial map from a copy of the input timeline.
     * <p>
     * This method exists to supply the super constructor with the {@link NavigableMap}.
     * </p>
     */
    static <S> TreeMap<UnwritableInterval, S> createMap(StateTimeline<? extends S> timeline) {
      TreeMap<UnwritableInterval, S> result = Maps.newTreeMap(Interval.BEGIN_COMPARATOR);
      for (Entry<UnwritableInterval, ? extends S> entry : timeline.getEntries()) {
        result.put(UnwritableInterval.copyOf(entry.getKey()), entry.getValue());
      }
      return result;
    }

    /**
     * Creates a new builder from the initialDomain with a default state
     * 
     * @param initialDomain the initial domain
     * @param defaultState the default state
     */
    Builder(UnwritableInterval initialDomain, S defaultState) {
      super(createMap(initialDomain, defaultState));
      this.intervalMap = (TreeMap<UnwritableInterval, S>) super.intervalMap;
      this.defaultState = defaultState;
      this.domain = new Interval(initialDomain);
    }

    /**
     * Creates a new builder, copying the supplied timeline's state.
     * 
     * @param timeline the timeline with the state to copy
     * @param defaultState the default state used for expansion
     */
    Builder(StateTimeline<? extends S> timeline, S defaultState) {
      super(createMap(timeline));
      this.intervalMap = (TreeMap<UnwritableInterval, S>) super.intervalMap;
      this.defaultState = defaultState;
      this.domain = new Interval(timeline.getDomain());
    }

    /**
     * {@inheritDoc}
     * 
     * The value returned here will always present the domain of the builder's current state. This
     * is accomplished by returning a reference to an internally updated {@link Interval} that has
     * its values updated whenever the domain changes.
     */
    @Override
    public UnwritableInterval getDomain() {
      return domain;
    }

    /**
     * Retrieves the default state used to construct the builder.
     * 
     * @return the state used to fill gaps or define the baseline timeline in this builder
     */
    public S getDefaultState() {
      return defaultState;
    }

    /**
     * Method that checks to see if interval is acceptable for adding to the existing timeline.
     * <p>
     * Any additional constraints beyond the zero-length interval should be added by subclassing.
     * </p>
     * 
     * @param interval interval to check
     * 
     * @throws IllegalArgumentException if interval is unacceptable.
     */
    void checkAdditionInterval(UnwritableInterval interval) {
      checkArgument(interval.getLength() > 0, "Interval for addition must have non-zero length.");
    }

    void updateDomain() {
      double domainStart = intervalMap.firstKey().getBegin();
      double domainEnd = intervalMap.lastKey().getEnd();

      if ((domainStart < domain.getBegin()) || (domainEnd > domain.getEnd())) {
        domain.set(domainStart, domainEnd);
      }
    }

    /**
     * Adds a necessary preceding interval to the candidates map, if required by adding interval to
     * the timeline
     * 
     * @param interval the new interval to add
     * @param toRemove the list of intervals that cover interval from intervalMap
     * @param candidates a map to receive the preceding interval, if added
     */
    void addPrecedingInterval(UnwritableInterval interval,
        NavigableMap<UnwritableInterval, S> toRemove,
        NavigableMap<UnwritableInterval, S> candidates) {

      /*
       * If interval's start exceeds the last interval in intervalMap, then there is a gap that
       * needs to be filled.
       */
      UnwritableInterval last = intervalMap.lastKey();
      if (interval.getBegin() > last.getEnd()) {
        candidates.put(new UnwritableInterval(last.getEnd(), interval.getBegin()), defaultState);
        return;
      }

      /*
       * If interval's start splits an interval in toRemove, then we need to add the split interval
       * into candidates.
       */
      Entry<UnwritableInterval, S> firstToRemove = toRemove.firstEntry();
      if (firstToRemove == null) {
        /*
         * No intervals in the set of stuff to remove, just return.
         */
        return;
      }
      if (firstToRemove.getKey().getBegin() < interval.getBegin()) {
        candidates.put(
            new UnwritableInterval(firstToRemove.getKey().getBegin(), interval.getBegin()),
            firstToRemove.getValue());
      }

    }

    /**
     * Adds a necessary following interval to the candidates map, if required by adding interval to
     * the timeline
     * 
     * @param interval the new interval to add
     * @param toRemove the list of intervals that cover interval from intervalMap
     * @param candidates a map to receive the following interval, if added
     */
    void addFollowingInterval(UnwritableInterval interval,
        NavigableMap<UnwritableInterval, S> toRemove,
        NavigableMap<UnwritableInterval, S> candidates) {

      /*
       * If interval's end precedes the first interval in intervalMap, then there is a gap that
       * needs to be filled.
       */
      UnwritableInterval first = intervalMap.firstKey();
      if (interval.getEnd() < first.getBegin()) {
        candidates.put(new UnwritableInterval(interval.getEnd(), first.getBegin()), defaultState);
        return;
      }

      /*
       * If interval's end splits an interval in toRemove, then we need to add the split interval
       * into candidates.
       */
      Entry<UnwritableInterval, S> lastToRemove = toRemove.lastEntry();
      if (lastToRemove == null) {
        /*
         * No intervals in the set of stuff to remove, just return.
         */
        return;
      }
      if (lastToRemove.getKey().getEnd() > interval.getEnd()) {
        candidates.put(new UnwritableInterval(interval.getEnd(), lastToRemove.getKey().getEnd()),
            lastToRemove.getValue());
      }

    }

    /**
     * Merges the list of mappings in candidates to the underlying intervalMap.
     * <p>
     * This process merges intervals together when adjacent intervals in the map would have the same
     * state as far as {@link Object#equals(Object)} is concerned.
     * </p>
     * 
     * @param candidates
     */
    void mergeAndAddToIntervalMap(NavigableMap<UnwritableInterval, S> candidates) {

      for (Entry<UnwritableInterval, S> entry : candidates.entrySet()) {

        /*
         * Find the entry in intervalMap that precedes the candidate entry.
         */
        Entry<UnwritableInterval, S> previous = intervalMap.lowerEntry(entry.getKey());

        if ((previous != null) && (previous.getValue().equals(entry.getValue()))) {
          /*
           * Remove the previous entry from the map. This is necessary because an insert of two
           * equivalent keys (as far as the comparator is concerned) does not necessary for an
           * update to the stored key.
           */
          intervalMap.remove(previous.getKey());
          intervalMap.put(
              new UnwritableInterval(previous.getKey().getBegin(), entry.getKey().getEnd()),
              entry.getValue());
        } else {
          intervalMap.put(entry.getKey(), entry.getValue());
        }

      }

      /*
       * Now check to see if the entry just following the last entry in candidates has the same
       * state and requires merging.
       */
      Entry<UnwritableInterval, S> lastEntryAdded = intervalMap.floorEntry(candidates.lastKey());
      Entry<UnwritableInterval, S> following = intervalMap.higherEntry(lastEntryAdded.getKey());

      if ((following != null) && (following.getValue().equals(lastEntryAdded.getValue()))) {

        /*
         * Remove the last entry added to the map, and the following entry as we are going to merge
         * them. Updating the mapping with a new interval that has the same start as an existing
         * interval in the keySet won't necessarily update the key.
         */
        intervalMap.remove(lastEntryAdded.getKey());
        intervalMap.remove(following.getKey());
        intervalMap.put(
            new UnwritableInterval(lastEntryAdded.getKey().getBegin(), following.getKey().getEnd()),
            lastEntryAdded.getValue());
      }

    }

    /**
     * Adds an interval to the timeline assuming the value of a supplied state.
     * 
     * @param interval an interval to add
     * @param state the state to attribute to it
     * 
     * @throws IllegalArgumentException if interval is of zero length, or if a non-expanding builder
     *         if interval is not completely contained within the domain of the timeline
     */
    public void add(UnwritableInterval interval, S state) {

      checkAdditionInterval(interval);

      /*
       * Make a defensive copy of interval before starting any of this effort.
       */
      interval = UnwritableInterval.copyOf(interval);

      /*
       * Compute a view of the entries in intervalMap that will need to be altered or removed as a
       * result of this addition. This is a bit messy given all the conditional logic for handling
       * intervals that lie outside of the current domain of intervalMap.
       */
      NavigableMap<UnwritableInterval, S> toRemove =
          AbstractStateTimeline.coveringSubsetMap(interval, this.intervalMap);

      if (toRemove == null) {
        toRemove = Maps.newTreeMap(Interval.BEGIN_COMPARATOR);
      }

      /*
       * Create and populate a map of the candidate intervals to add to intervalMap.
       */
      NavigableMap<UnwritableInterval, S> candidates = Maps.newTreeMap(Interval.BEGIN_COMPARATOR);

      addPrecedingInterval(interval, toRemove, candidates);
      candidates.put(interval, state);
      addFollowingInterval(interval, toRemove, candidates);

      /*
       * Remove the elements from toRemove from the map. They are to be replaced with intervals from
       * candidates and are no longer required.
       */
      toRemove.clear();

      /*
       * Now add the candidate intervals, merging as necessary.
       */
      mergeAndAddToIntervalMap(candidates);

      /*
       * If necessary update the domain.
       */
      updateDomain();

    }

    /**
     * Adds an interval to the timeline with the supplied state.
     * 
     * @param begin the start of the interval
     * @param end the end of the interval
     * @param state the state to attribute to [begin,end]
     * 
     * @throws IllegalArgumentException if end >= begin, or if a non-expanding builder if interval
     *         is not completely contained within the domain of the timeline
     */
    public void add(double begin, double end, S state) {
      add(new UnwritableInterval(begin, end), state);
    }

    /**
     * Adds a series of interval with the same state to the timeline.
     * <p>
     * If supplying an {@link IntervalSet}, it may make sense to
     * {@link IntervalSet#removeIntervals(double)} to remove zero length intervals first.
     * </p>
     * 
     * @param intervals an iterable of non-zero length intervals, may be {@link IntervalSet}
     * @param state the state to attribute to the intervals
     * 
     * @throws IllegalArgumentException if intervals contains a zero-length interval, or if a
     *         non-expanding builder if all intervals are not completely contained within the
     *         timeline's domain
     */
    public void add(Iterable<? extends UnwritableInterval> intervals, S state) {
      for (UnwritableInterval interval : intervals) {
        checkAdditionInterval(interval);
      }

      for (UnwritableInterval interval : intervals) {
        add(interval, state);
      }
    }

    /**
     * Adds an interval to the timeline with the supplied state, only if the states that are being
     * replaced do not match a supplied filter.
     * <p>
     * This method can be used to implement rudimentary rules about what states can replace others.
     * For example, suppose we have a builder of a timeline that captures integer states. If the
     * addition of any new state to the timeline is required to be an integer higher than those that
     * it is replacing, the following code demonstrates how to accomplish this:
     * </p>
     * 
     * <pre>
     * {@code
     *   StateTimeline.Builder<Integer> builder = StateTimeline.create(new UnwritableInterval(0,10), 0);
     *   
     *   Integer newState = -1;
     *   UnwritableInterval intervalForNewState = new UnwritableInterval(5,7);
     * 
     *   boolean wasAdded = builder.add(interval, newState, (s) -> newState <= s);
     * 
     *   if ( !wasAdded ) { 
     *      System.out.println("Addition of state: " + newState + " was rejected."); 
     *   }
     * </pre>
     * 
     * @param interval the interval to add
     * 
     * @param state the state to associate
     * @param protectFilter the filter to be supplied states being replaced, if true, aborts the
     *        addition attempt
     * 
     * @return true if the add was successful, false otherwise
     * 
     * @throws IllegalArgumentException if interval is of zero length, or if a non-expanding builder
     *         if interval is not completely contained within the domain of the timeline
     */
    public boolean add(UnwritableInterval interval, S state, Predicate<? super S> protectFilter) {

      checkAdditionInterval(interval);

      NavigableMap<UnwritableInterval, S> toRemove =
          AbstractStateTimeline.coveringSubsetMap(interval, this.intervalMap);

      for (S stateToReplace : toRemove.values()) {
        if (protectFilter.apply(stateToReplace)) {
          return false;
        }
      }

      add(interval, state);

      return true;
    }

    /**
     * Adds an interval to the timeline with the supplied state, only if the states that are being
     * replaced do not match a supplied filter
     * <p>
     * This method can be used to implement rudimentary rules about what states can replace others.
     * For example, suppose we have a builder of a timeline that captures integer states. If the
     * addition of any new state to the timeline is required to be an integer higher than those that
     * it is replacing, the following code demonstrates how to accomplish this:
     * </p>
     * 
     * <pre>
     * {@code
     *   StateTimeline.Builder<Integer> builder = StateTimeline.create(new UnwritableInterval(0,10), 0);
     *   
     *   Integer newState = -1;
     * 
     *   boolean wasAdded = builder.add(5, 7, newState, (s) -> newState <= s);
     * 
     *   if ( !wasAdded ) { 
     *      System.out.println("Addition of state: " + newState + " was rejected."); 
     *   }
     * </pre>
     * 
     * @param begin the start of the interval to add
     * 
     * @param end the end of the interval to add
     * @param state the state to associate with [begin,end]
     * @param protectFilter the filter to be supplied states being replaced, if true, aborts the
     *        addition attempt
     * 
     * @return true if the add was successful, false otherwise
     * 
     * @throws IllegalArgumentException if end >= begin, or if a non-expanding builder if interval
     *         is not completely contained within the domain of the timeline
     */
    public boolean add(double begin, double end, S state, Predicate<? super S> protectFilter) {
      return add(new UnwritableInterval(begin, end), state, protectFilter);
    }

    /**
     * Adds an iterable of intervals to the timeline associated with the supplied state, only if
     * states that are being replaced do not match a supplied filter.
     * 
     * <p>
     * This method can be used to implement rudimentary rules about what states can replace others.
     * For example, suppose we have a builder of a timeline that captures integer states. If the
     * addition of any new state to the timeline is required to be an integer higher than those that
     * it is replacing, the following code demonstrates how to accomplish this:
     * </p>
     * 
     * <pre>
     * {@code
     *   StateTimeline.Builder<Integer> builder = StateTimeline.create(new UnwritableInterval(0,10), 0);
     *   
     *   Integer newState = -1;
     *   IntervalSet intervalSet = IntervalSet.create(1,4,5,8);
     * 
     *   boolean wasAdded = builder.add(intervalSet, newState, (s) -> newState <= s);
     * 
     *   if ( !wasAdded ) { 
     *      System.out.println("Addition of state: " + newState + " was rejected."); 
     *   }
     * </pre>
     * 
     * @param intervals an iterable of non-zero length intervals, may be {@link IntervalSet}
     * 
     * @param state the state to associate
     * @param protectFilter the filter to be supplied states being replaced, if true, aborts the
     *        addition attempt
     * 
     * @return true if the add was successful, false otherwise
     * 
     * @throws IllegalArgumentException if intervals contains a zero-length interval, or if a
     *         non-expanding builder if all intervals are not completely contained within the
     *         timeline's domain
     * 
     */
    public boolean add(Iterable<? extends UnwritableInterval> intervals, S state,
        Predicate<? super S> protectFilter) {

      for (UnwritableInterval interval : intervals) {
        checkAdditionInterval(interval);
        NavigableMap<UnwritableInterval, S> toRemove =
            AbstractStateTimeline.coveringSubsetMap(interval, intervalMap);
        for (S stateToReplace : toRemove.values()) {
          if (protectFilter.apply(stateToReplace)) {
            return false;
          }
        }
      }

      for (UnwritableInterval interval : intervals) {
        add(interval, state);
      }

      return true;
    }

    /**
     * Build a subset of the timeline currently captured by the builder.
     * 
     * @param begin specifies the start of the interval of interest
     * @param end specifies the end of the interval of interest
     * 
     * @return if the builder is expanding, any valid interval will be accepted and regions outside
     *         the current domain of the builder will be filled with
     *         {@link StateTimeline.Builder#getDefaultState()}
     */
    public StateTimeline<S> buildSubset(double begin, double end) {
      return buildSubset(new UnwritableInterval(begin, end));
    }

    /**
     * Build a subset of the timeline currently captured by the builder.
     * 
     * @param subset specifies the interval of interest
     * 
     * @return if the builder is expanding, any valid interval will be accepted and regions outside
     *         the current domain of the builder will be filled with
     *         {@link StateTimeline.Builder#getDefaultState()}
     */
    public StateTimeline<S> buildSubset(UnwritableInterval subset) {

      /*
       * The implementation of this method is a bit of a mess, because what's happening here is
       * potentially so complicated. There are many side cases to consider.
       */

      /*
       * Verify that the requested subset is "safe" for addition. This is a bit of a hack, but it
       * works. If it is an unconstrained builder it will validate the interval isn't length zero.
       * If it's a constrained builder it will block an interval that isn't a subset of the domain.
       */
      checkAdditionInterval(subset);

      UnwritableInterval domain = getDomain();

      /*
       * Handle the simple case first.
       */
      if (domain.equals(subset)) {
        return build();
      }

      /*
       * Covering subset can only handle a subset of domain, including the actual domain. If subset
       * lies outside the domain or has bounds that cover it, then the query needs to be cut to only
       * cover the domain.
       */
      IntervalSet query = IntervalSet.create(domain).intersect(subset);

      if (query.isEmpty() || query.size() == 1 && query.get(0).getLength() == 0) {
        /*
         * If query is empty, or if the size of query is length one and the interval is of length
         * zero then it only intersects on the boundary. Just return a timeline with the default
         * state and subset.
         */
        return new StateTimeline<>(
            ImmutableSortedMap.<UnwritableInterval, S>orderedBy(Interval.BEGIN_COMPARATOR)
                .put(UnwritableInterval.copyOf(subset), getDefaultState()).build());
      }

      /*
       * Determine the subset from the timeline builder that covers query. Note: we can just use the
       * bounding interval, because query was created as the intersection of two intervals so the
       * result must be an interval (if we reach this point).
       */
      NavigableMap<UnwritableInterval, S> coveringSubset =
          AbstractStateTimeline.coveringSubsetMap(query.getBoundingInterval(), intervalMap);

      /*
       * Now we have the parts necessary to assemble. Start by creating the builder to capture the
       * map contents.
       */
      ImmutableSortedMap.Builder<UnwritableInterval, S> builder =
          ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);

      /*
       * Handle the case when the covering subset only has a single entry, as this is a special
       * case. If we reach this point, it must have at least one entry.
       */
      if (coveringSubset.size() == 1) {

        Entry<UnwritableInterval, S> entry = coveringSubset.firstEntry();

        /*
         * There are a couple of cases to consider: if entry's state is the default state and either
         * endpoint of subset exceeds the domain boundary expand the interval. Otherwise add new
         * intervals. Or if not, contract interval.
         */
        if (entry.getValue().equals(getDefaultState())) {
          /*
           * Just return subset with default state.
           */
          builder.put(UnwritableInterval.copyOf(subset), getDefaultState());
          return new StateTimeline<>(builder.build());
        }

        /*
         * If necessary insert intervals that precede or follow domain with the default state.
         */
        double start = entry.getKey().getBegin();
        if (subset.getBegin() < start) {
          builder.put(new UnwritableInterval(subset.getBegin(), start), getDefaultState());
        } else {
          start = subset.getBegin();
        }

        double end = entry.getKey().getEnd();
        if (subset.getEnd() > end) {
          builder.put(new UnwritableInterval(end, subset.getEnd()), getDefaultState());
        } else {
          end = subset.getEnd();
        }

        builder.put(new UnwritableInterval(start, end), entry.getValue());
        return new StateTimeline<>(builder.build());
      }

      /*
       * If we reach here, then we have multiple entries in the covering subset which requires
       * different handling than the single entry. Extract the first and last entries.
       */
      Entry<UnwritableInterval, S> first = coveringSubset.firstEntry();
      Entry<UnwritableInterval, S> last = coveringSubset.lastEntry();

      for (Entry<UnwritableInterval, S> entry : coveringSubset.entrySet()) {
        if (entry.equals(first)) {

          /*
           * Check to see if the start of subset precedes the entry's interval.
           */
          if (subset.getBegin() < entry.getKey().getBegin()) {
            if (entry.getValue().equals(getDefaultState())) {
              builder.put(new UnwritableInterval(subset.getBegin(), entry.getKey().getEnd()),
                  getDefaultState());
            } else {
              builder.put(new UnwritableInterval(subset.getBegin(), entry.getKey().getBegin()),
                  getDefaultState());
              builder.put(entry);
            }
          } else {
            builder.put(new UnwritableInterval(subset.getBegin(), entry.getKey().getEnd()),
                entry.getValue());
          }

        } else if (entry.equals(last)) {

          /*
           * Check to see if the end of subset follows the entry's interval.
           */
          if (subset.getEnd() > entry.getKey().getEnd()) {
            if (entry.getValue().equals(getDefaultState())) {
              builder.put(new UnwritableInterval(entry.getKey().getBegin(), subset.getEnd()),
                  getDefaultState());
            } else {
              builder.put(entry);
              builder.put(new UnwritableInterval(entry.getKey().getEnd(), subset.getEnd()),
                  getDefaultState());
            }
          } else {
            builder.put(new UnwritableInterval(entry.getKey().getBegin(), subset.getEnd()),
                entry.getValue());
          }

        } else {
          builder.put(entry);
        }
      }

      return new StateTimeline<>(builder.build());
    }

    /**
     * Builds the timeline
     * 
     * @return a newly created {@link StateTimeline} capturing the contents of the builder
     */
    @Override
    public StateTimeline<S> build() {
      return new StateTimeline<>(intervalMap);
    }

  }

  /**
   * Simple extension of {@link Builder} that changes the interval constraint checking to prevent
   * additions of intervals that lie outside the initial domain
   * 
   * @param <S> the state class captured by the builder and its generated timelines. This class must
   *        implement {@link Object#equals(Object)} and {@link Object#hashCode()} properly.
   */
  static class ConstrainedBuilder<S> extends Builder<S> {

    /**
     * Creates a new builder from the initialDomain
     * 
     * @param initialDomain
     * @param defaultState
     */
    ConstrainedBuilder(UnwritableInterval initialDomain, S defaultState) {
      super(initialDomain, defaultState);
    }

    ConstrainedBuilder(StateTimeline<? extends S> timeline) {
      /*
       * It's fine to supply null for the default state in this instance, as expansion is not
       * permitted and so will never happen.
       */
      super(timeline, null);
    }

    @Override
    void checkAdditionInterval(UnwritableInterval interval) {
      super.checkAdditionInterval(interval);
      checkArgument(
          interval.getBegin() >= intervalMap.firstKey().getBegin()
              && interval.getEnd() <= intervalMap.lastKey().getEnd(),
          "Interval must be contained completely within the domain.");
    }

    @Override
    void updateDomain() {
      /*
       * Do nothing, as the domain can't change.
       */
    }

  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((domain == null) ? 0 : domain.hashCode());
    result = prime * result + ((states == null) ? 0 : states.hashCode());
    result = prime * result + ((transitionTimes == null) ? 0 : transitionTimes.hashCode());
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
    StateTimeline<?> other = (StateTimeline<?>) obj;
    if (domain == null) {
      if (other.domain != null) {
        return false;
      }
    } else if (!domain.equals(other.domain)) {
      return false;
    }
    if (states == null) {
      if (other.states != null) {
        return false;
      }
    } else if (!states.equals(other.states)) {
      return false;
    }
    if (transitionTimes == null) {
      if (other.transitionTimes != null) {
        return false;
      }
    } else if (!transitionTimes.equals(other.transitionTimes)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "StateTimeline [" + getEntries() + "]";
  }

}
