package picante.spice.kernel.ck;

import picante.data.list.GaugedRetrievableLLE;
import picante.data.list.GaugedRetrievableLLT;
import picante.exceptions.BugException;
import picante.math.intervals.Interval;
import picante.mechanics.Coverages;
import picante.mechanics.TimeOutOfBoundsException;

/**
 * Implementation of the {@link CKCoverage} interface that assumes segment content is organized as a
 * time series of records and a time series of interpolation interval starts.
 * <p>
 * Interpolation is allowed between the start of each interval and the last record in the record
 * time series that occurs just prior to the next interpolation interval start.
 * </p>
 * <p>
 * Assumptions that must be met, in order to utilize this class to implement the CK coverage
 * interface:
 * <ul>
 * <li>Both the record table and interval table must contain records that are sorted in strictly
 * increasing time order.</li>
 * <li>Each time entry in the interpolation interval starts table <b>must</b> have an equivalent
 * entry in the records table.</li>
 * <li>segmentStart is less than or equal to the first interpolation interval (or equivalently the
 * first record time). segmentEnd is greater than or equal to the last record provided in the record
 * table.</li>
 * </ul>
 * </p>
 */
class IntervalStartsCKCoverage implements CKCoverage {

  /**
   * The table of records whose times are to be used to end interpolation intervals.
   */
  private final GaugedRetrievableLLT<?> recordTable;

  /**
   * The table of records whose times are to be used to start interpolation intervals.
   */
  private final GaugedRetrievableLLE<?> intervalTable;

  /**
   * The start of the coverage bounding interval.
   */
  private final double segmentStart;

  /**
   * The end of the coverage bounding interval.
   */
  private final double segmentEnd;

  /**
   * Local buffer to capture results.
   */
  private final Interval interval = new Interval();

  /**
   * Create an interval start record oriented implementation of the CK coverage interface.
   * 
   * @param segmentStart the start of the bounding interval
   * @param segmentEnd the end of the bounding interval
   * @param recordTable the table of data records, enabled for last less than searching
   * @param intervalTable the table of interval starts, enabled for last less than or equal to
   *        searching
   */
  public IntervalStartsCKCoverage(double segmentStart, double segmentEnd,
      GaugedRetrievableLLT<?> recordTable, GaugedRetrievableLLE<?> intervalTable) {
    this.segmentStart = segmentStart;
    this.segmentEnd = segmentEnd;
    this.recordTable = recordTable;
    this.intervalTable = intervalTable;
  }

  /**
   * Is the supplied time contained within the bounding interval
   * 
   * @param encodedSCLK the time of interest
   * 
   * @return true, if encodedSCLK lies in the bounding interval, false otherwise
   */
  private boolean isWithinBounds(double encodedSCLK) {
    return (encodedSCLK >= segmentStart) && (encodedSCLK <= segmentEnd);
  }

  /**
   * Retrieve the time associated with the last applicable record in the coverage.
   * 
   * @return the last relevant record time
   */
  private double getLastRecordTime() {
    return recordTable.getGauge(recordTable.size() - 1);
  }

  /**
   * Retrieve the time associated with the start of the last interpolation interval.
   * 
   * @return the last interpolation interval start
   */
  private double getLastIntervalTime() {
    return intervalTable.getGauge(intervalTable.size() - 1);
  }

  /**
   * Determine the end time of a particular interpolation interval at a specific index in the
   * interval table.
   * 
   * @param index the index into the interval table for a particular start
   * 
   * @return the actual end of the interval
   */
  private double getIntervalEnd(int index) {

    /*
     * Simple exceptional case check, is this the last interval?
     */
    if (index == intervalTable.size() - 1) {
      return getLastRecordTime();
    }

    /*
     * If not, then we have to locate the last record in the recordTable that occurs just prior to
     * the start of the next interval.
     */
    double start = intervalTable.getGauge(++index);

    index = recordTable.indexLastLessThan(start);

    return recordTable.getGauge(index);
  }

  /**
   * Populate buffer with the interpolation interval of a particular index.
   * 
   * @param index the index of the interval of interest.
   * 
   * @param buffer an interval buffer to capture the results.
   */
  private void getInterpolationInterval(int index, Interval buffer) {
    buffer.set(intervalTable.getGauge(index), getIntervalEnd(index));
  }

  @Override
  public boolean contains(double encodedSCLK) {

    if (!isWithinBounds(encodedSCLK)) {
      return false;
    }

    /*
     * Locate the start of the interpolation interval applicable for this particular encoded SCLK.
     */
    int index = intervalTable.indexLastLessThanOrEqualTo(encodedSCLK);

    if (index == -1) {
      return false;
    }

    getInterpolationInterval(index, interval);

    return (interval.getBegin() <= encodedSCLK) && (encodedSCLK <= interval.getEnd());

  }

  @Override
  public Interval getBoundingInterval(Interval buffer) {
    buffer.set(segmentStart, segmentEnd);
    return buffer;
  }

  @Override
  public Interval getBracketingInterval(double encodedSCLK, Interval buffer) {

    /*
     * If the requested time lies outside the bounding interval, then clearly there is no bracketing
     * interval.
     */
    if (!isWithinBounds(encodedSCLK)) {
      throw new TimeOutOfBoundsException(encodedSCLK);
    }

    /*
     * Locate the start of the interpolation interval that contains encodedSCLK.
     */
    int index = intervalTable.indexLastLessThanOrEqualTo(encodedSCLK);

    /*
     * Check to see if the time lies before the start of the first interpolation interval. If it
     * does, then throw a time out of bounds exception.
     */
    if (index == -1) {
      throw new TimeOutOfBoundsException(encodedSCLK);
    }

    getInterpolationInterval(index, buffer);

    /*
     * Determine if the interval contains the requested encoded SCLK.
     */
    if ((encodedSCLK < buffer.getBegin()) || (encodedSCLK > buffer.getEnd())) {
      throw new TimeOutOfBoundsException(encodedSCLK);
    }

    return buffer;
  }

  @Override
  public boolean hasNextInterval(double encodedSCLK) {
    return encodedSCLK < getLastIntervalTime();
  }

  @Override
  public Interval getNextInterval(double encodedSCLK, Interval buffer) {

    /*
     * Handle the two special cases first. If the encodedSCLK supplied occurs prior to the start of
     * the bounding interval, then buffer must be the first interpolation interval.
     */
    if (encodedSCLK < segmentStart) {
      getInterpolationInterval(0, buffer);
      return buffer;
    }

    /*
     * And the second special case, when encodedSCLK exceeds the time associated with the last
     * interpolation interval.
     */
    if (encodedSCLK >= getLastIntervalTime()) {
      throw new TimeOutOfBoundsException(encodedSCLK);
    }

    /*
     * Now handle the nominal case. Locate the interval start that occurs just prior to the supplied
     * encoded SCLK.
     */
    int index = intervalTable.indexLastLessThanOrEqualTo(encodedSCLK);

    /*
     * This should never happen, just perform a check to make certain that the code fails here
     * rather than in some unexpected fashion.
     */
    if (index == intervalTable.size() - 1) {
      throw new BugException("If the code is functioning as designed this "
          + "should never happen. It has already been determined that " + "the supplied time, "
          + encodedSCLK + ", is smaller than the start of the last " + "interpolation interval.");
    }

    getInterpolationInterval(++index, buffer);
    return buffer;
  }

  @Override
  public int hashCode() {
    return Coverages.hashCodeImplementation(this);
  }

  @Override
  public boolean equals(Object obj) {
    return Coverages.equalsImplementation(this, obj);
  }

}
