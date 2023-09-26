/**
 * Filename: TimeRange.java Author : vandejd1 Created : Mar 26, 2009
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import java.util.Comparator;

import com.google.common.collect.ComparisonChain;


/**
 * an immutable time window representation
 * 
 * @author vandejd1
 */
public class TSRange implements Cloneable, Comparable<TSRange> {

  public static final Comparator<TSRange> T0_ONLY = new Comparator<TSRange>() {
    @Override
    public int compare(TSRange o1, TSRange o2) {
      return o1.t0.compareTo(o2.t0);
    }
  };

  public static final Comparator<TSRange> T1_ONLY = new Comparator<TSRange>() {
    @Override
    public int compare(TSRange o1, TSRange o2) {
      return o1.t1.compareTo(o2.t1);
    }
  };

  public static final Comparator<TSRange> T0_THEN_T1 = new Comparator<TSRange>() {
    @Override
    public int compare(TSRange o1, TSRange o2) {
      return o1.compareTo(o2);
    }
  };

  public static final Comparator<TSRange> createMidpointComparator(final TimeSystem<?> timesystem) {
    return new Comparator<TSRange>() {
      @Override
      public int compare(TSRange o1, TSRange o2) {
        return o1.getMidpoint(timesystem).compareTo(o2.getMidpoint(timesystem));
      }
    };
  }

  public static final TSRange ALL_TIME = new TSRange(TSEpoch.MIN_TIME, TSEpoch.MAX_TIME);
  /**
   * the start of the time range
   */
  protected TSEpoch t0;

  /**
   * the end of the time range
   */
  protected TSEpoch t1;

  /**
   * an immutable time window representation, with the given start (t0) and stop (t1) times
   * 
   * @param t0
   * @param t1
   */
  public TSRange(TSEpoch t0, TSEpoch t1) {

    this.t0 = t0;
    this.t1 = t1;
  }

  public TSRange(TSRange otherRange) {
    this.t0 = otherRange.t0;
    this.t1 = otherRange.t1;
  }

  public TSEpoch getT0() {
    return t0;
  }

  public TSEpoch getT1() {
    return t1;
  }


  /**
   * subtracts the two time values (using the given time system) and adds half the difference to the
   * start time to find the midpoint
   * 
   * @param anyTimeSys
   * @return
   */
  public TSEpoch getMidpoint(TimeSystem<?> anyTimeSys) {
    double duration = getDurationInSeconds(anyTimeSys);
    return anyTimeSys.add(t0, duration / 2.0);
  }


  /**
   * returns the elapsed seconds in the given time system
   * 
   * @return
   */
  public final double getDurationInSeconds(TimeSystem<?> anyTimeSys) {
    return anyTimeSys.difference(t1, t0);
  }

  /**
   * Does this range contain the given time, where the given time can be greater than or equal to
   * T0, but must be strictly less than T1.
   * 
   * @param time
   * @return
   */
  public final boolean containsInclusiveLow(TSEpoch time) {
    // logicallly, the simplest way to answer this question
    // is to rule out the regions outside the range of this class

    if ((time.compareTo(t0) < 0) // Is the given epoch is before T0?
        || (time.compareTo(t1) >= 0)) { // Is the given epoch is at or beyond T1?
      // The given epoch is either before T0
      // or its at-or-beyond T1, so return false
      return false;
    } else {
      // The given epoch must be at or beyond the start
      // and before the endpoint of this time range.
      return true;
    }
  }


  /**
   * Does this range, including the endpoints as part of the range, contain the given epoch?
   * 
   * So, if the given time is within or at the boundaries of this time range, then true is returned.
   * Otherwise, false.
   * 
   * @param time
   * @return
   */
  public final boolean containsInclusive(TSEpoch time) {
    // logicallly, the simplest way to answer this question
    // is to rule out the regions outside the range of this class

    if ((time.compareTo(t0) < 0) || (time.compareTo(t1) > 0)) {
      // The given epoch is either less than the start time
      // or greater than the stop time, so this time range
      // does NOT contain the given epoch.
      return false;
    } else {
      // The given epoch must be within or touching the
      // endpoints of this time range.
      return true;
    }
  }

  /**
   * Does this range, exclusive of the endpoints, contain the given epoch?
   * 
   * If the given time is fully within the boundaries (and not touching any boundaries of this time
   * range) true is returned, otherwise, false.
   * 
   * @param tm
   * @return
   */
  public final boolean containsExclusive(TSEpoch time) {
    if ((time.compareTo(t0) > 0) && (time.compareTo(t1) < 0)) {
      return true;
    }
    return false;
  }

  /**
   * Does this range, exclusive of the endpoints, contain all the times between the other T0 and T1
   * supplied as parameters? The return true, otherwise false.
   * 
   * It is assumed that T0 >= T1, so if this is not true, the results are not guaranteed.
   * 
   * @param otherT0
   * @param otherT1
   * @return
   */
  public final boolean containsExclusive(TSEpoch otherT0, TSEpoch otherT1) {
    return (t0.compareTo(otherT0) < 0) && (otherT1.compareTo(t1) < 0);
  }

  /**
   * Does this range, including the endpoints, contain all the times between the other T0 and T1
   * supplied as parameters? Then return true, otherwise false.
   * 
   * It is assumed that T0 >= T1, so if this is not true, the results are not guaranteed.
   * 
   * @param otherT0
   * @param otherT1
   * @return
   */
  public final boolean containsInclusive(TSEpoch otherT0, TSEpoch otherT1) {
    return (t0.compareTo(otherT0) <= 0) && (otherT1.compareTo(t1) <= 0);
  }

  /**
   * Does this range, including the endpoints, contain all the times between the other T0 and T1 of
   * the given other range? Then return true, otherwise false.
   * 
   * It is assumed that T0 >= T1, so if this is not true, the results are not guaranteed.
   * 
   * @param otherT0
   * @param otherT1
   * @return
   */
  public final boolean containsInclusive(TSRange otherRange) {
    return containsInclusive(otherRange.t0, otherRange.t1);
  }

  /**
   * Does this range, exclusive of the endpoints, contain all the times between the other T0 and T1
   * of the given other range? The return true, otherwise false.
   * 
   * It is assumed that T0 >= T1, so if this is not true, the results are not guaranteed.
   * 
   * @param otherT0
   * @param otherT1
   * @return
   */
  public final boolean containsExclusive(TSRange otherRange) {
    return containsExclusive(otherRange.t0, otherRange.t1);
  }

  /**
   * if the overlap of this range with the other range is greater than zero, return true, otherwise
   * false
   * 
   * @param otherRange
   * @return
   */
  public final boolean overlapsExclusive(TSRange otherRange) {
    return overlapsExclusive(otherRange.t0, otherRange.t1);
  }

  /**
   * if the overlap of this range with the other range (given by the otherT1 and otherT2 parameters)
   * is greater than zero, return true, otherwise false
   * 
   * @param otherRange
   * @return
   */
  public final boolean overlapsExclusive(TSEpoch otherT0, TSEpoch otherT1) {
    if (otherT0.compareTo(t0) < 0) {
      return otherT1.compareTo(t0) > 0;
    }
    return otherT0.compareTo(t1) < 0;
  }


  /**
   * Computes the degree of overlap (as a value from 0 meaning no overlap to 1.0 meaning full
   * overlap) between this range and another range defined from otherT0 to otherT1. All duration
   * amounts are computed in the time system provided.
   * 
   * @param otherT0
   * @param otherT1
   * @param anyTimeSys
   * @return
   */
  public final double overlapFraction(TSEpoch otherT0, TSEpoch otherT1, TimeSystem<?> anyTimeSys) {
    if (!overlapsExclusive(otherT0, otherT1)) {
      return 0;
    }
    // Get the largest start time.
    TSEpoch largestStartTime;
    if (otherT0.compareTo(t0) > 0) {
      largestStartTime = t0;
    } else {
      largestStartTime = otherT0;
    }
    // Get the smallest end time:
    TSEpoch smallestEndTime;
    if (otherT1.compareTo(t1) < 0) {
      smallestEndTime = otherT1;
    } else {
      smallestEndTime = t1;
    }
    return new TSRange(largestStartTime, smallestEndTime).getDurationInSeconds(anyTimeSys)
        / getDurationInSeconds(anyTimeSys);
  }


  /**
   * Computes the degree of overlap (as a value from 0 meaning no overlap to 1.0 meaning full
   * overlap) between this range and the other range. All duration amounts are computed in the time
   * system provided.
   * 
   * @param otherRange
   * @param anyTimeSys
   * @return
   */
  public final double overlapFraction(TSRange otherRange, TimeSystem<?> anyTimeSys) {
    return overlapFraction(otherRange.t0, otherRange.t1, anyTimeSys);
  }


  /**
   * performs a deep copy of all immutable fields; throws no checked exceptions (because it knows
   * that this object implements Cloneable.)
   */
  @Override
  public Object clone() {
    try {
      // after super.clone() returns, clone all mutable fields:
      TSRange copy = (TSRange) super.clone();
      copy.t0 = (TSEpoch) t0.clone();
      copy.t1 = (TSEpoch) t1.clone();
      // you can't change the preferred time system, so that does
      // not need to be cloned.
      // If the preferred time system is made non-final, then it will
      // have to be cloned here too.
      return copy;
    } catch (CloneNotSupportedException e) {
      // This should never happen because TSEpoch implements Clonable.
      throw new InternalError(e.toString());
    }
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((t0 == null) ? 0 : t0.hashCode());
    result = prime * result + ((t1 == null) ? 0 : t1.hashCode());
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
    TSRange other = (TSRange) obj;
    if (t0 == null) {
      if (other.t0 != null) {
        return false;
      }
    } else if (!t0.equals(other.t0)) {
      return false;
    }
    if (t1 == null) {
      if (other.t1 != null) {
        return false;
      }
    } else if (!t1.equals(other.t1)) {
      return false;
    }
    return true;
  }

  @Override
  public int compareTo(TSRange o) {
    return ComparisonChain.start().compare(this.t0, o.t0).compare(this.t1, o.t1).result();
  }

  @Override
  public String toString() {
    return t0.toString() + "-" + t1.toString();
  }

}
