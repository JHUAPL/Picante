package picante.spice.kernel.ck;

import picante.data.list.GaugedRetrievable;
import picante.data.list.GaugedRetrievableLLE;
import picante.data.list.Retrievable;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;

/**
 * Implementation of the type-2 C-kernel segment, constant rate, extrapolation off quaternion
 * records.
 * <p>
 * Assumptions about the correctness of segment structure:
 * <ul>
 * <li>The records, interval starts, and interval stops are sorted properly in time</li>
 * <li>There are no duplicate records in the segment, with regards to timing</li>
 * <li>InitialEncodedSCLK <= First Record Encoded SCLK <= Last Record Encoded SCLK <=
 * FinalEncodedSCLK</li>
 * <li>[intervalStart(i), intervalStop(i)] does not overlap any other intervals in the stop/start
 * except possibly on the end points.</li>
 * </ul>
 * </p>
 */
public class CKType2 extends AbstractCKSegment implements CKSegmentWithAV {

  private final Retrievable<CKType2Record> records;
  private final GaugedRetrievableLLE<?> intervalStarts;
  private final GaugedRetrievable<?> intervalStops;
  private final boolean hasAngularVelocity;

  private final CKCoverage coverage;
  private final Type2Interpolator interpolator = new Type2Interpolator();

  /**
   * Contains the encodedSCLK of the left end of the currently cached interpolation interval.
   */
  private double leftSCLK;

  /**
   * Contains the encodedSCLK of the right end of the currently cached interpolation interval.
   */
  private double rightSCLK;

  /**
   * Contains the encodedSCLK of the start (left end) of the next cached interpolation interval, or
   * if it's the last record in the file the end of the current interval.
   */
  private double nextLeftSCLK;
  private final CKType2Record record = new CKType2Record();
  private final RotationMatrixIJK matrix = new RotationMatrixIJK();
  private final VectorIJK rate = new VectorIJK();

  public CKType2(String name, int instrumentID, int referenceID, double initialEncodedSCLK,
      double finalEncodedSCLK, Retrievable<CKType2Record> records,
      GaugedRetrievableLLE<?> intervalStarts, GaugedRetrievable<?> intervalEnds,
      boolean hasAngularVelocity) {
    super(name, instrumentID, referenceID, initialEncodedSCLK, finalEncodedSCLK);
    this.records = records;
    this.intervalStarts = intervalStarts;
    this.intervalStops = intervalEnds;
    this.hasAngularVelocity = hasAngularVelocity;
    this.coverage = createCoverage(intervalStarts, intervalEnds);

    /*
     * Configure the cache so that it will force an update when any query is performed.
     */
    this.leftSCLK = Double.MAX_VALUE;
    this.rightSCLK = -Double.MAX_VALUE;
    this.nextLeftSCLK = -Double.MAX_VALUE;
  }

  public Retrievable<CKType2Record> getRecords() {
    return this.records;
  }

  public GaugedRetrievableLLE<?> getIntervalStarts() {
    return this.intervalStarts;
  }

  public GaugedRetrievable<?> getIntervalStops() {
    return this.intervalStops;
  }

  /**
   * Creates a simple implementation of the coverage object backed by an {@link IntervalSet}.
   * <p>
   * The decision to do this lies with the fact that the type 2 segment stores individual
   * interpolation intervals as two separate arrays in the DAF. So linear search is the only way to
   * resolve the interval methods on the {@link Coverage} interface. So caching isn't a bad per se,
   * though it does fly in the face of the fast construction/low memory utilization optimizations
   * typically present in the SPICE implementations.
   * </p>
   * 
   * @param intervalStarts
   * @param intervalStops
   * 
   * @return
   */
  CKCoverage createCoverage(GaugedRetrievableLLE<?> intervalStarts,
      GaugedRetrievable<?> intervalStops) {
    final IntervalSet.Builder builder = IntervalSet.builder();
    for (int i = 0; i < intervalStarts.size(); i++) {
      builder.add(intervalStarts.getGauge(i), intervalStops.getGauge(i));
    }

    return new CKCoverage() {

      private Coverage delegate = Coverages.create(builder.build());

      @Override
      public boolean contains(double time) {
        updateCache(time);
        return isCacheEvaluatable(time);
      }

      @Override
      public Interval getBoundingInterval(Interval buffer) {
        return delegate.getBoundingInterval(buffer);
      }

      @Override
      public Interval getBracketingInterval(double time, Interval buffer) {
        return delegate.getBracketingInterval(time, buffer);
      }

      @Override
      public boolean hasNextInterval(double time) {
        return delegate.hasNextInterval(time);
      }

      @Override
      public Interval getNextInterval(double time, Interval buffer) {
        return delegate.getNextInterval(time, buffer);
      }

      @Override
      public boolean equals(Object object) {
        return delegate.equals(object);
      }

      @Override
      public int hashCode() {
        return delegate.hashCode();
      }

    };
  }

  @Override
  public boolean hasAngularVelocity() {
    return hasAngularVelocity;
  }

  @Override
  public int getType() {
    return 2;
  }

  @Override
  public CKCoverage getCoverage() {
    return this.coverage;
  }

  @Override
  public RotationMatrixIJK getTransform(double encodedSCLK, RotationMatrixIJK buffer) {

    /*
     * Update the internal cache if necessary.
     */
    updateCache(encodedSCLK);

    /*
     * Check to see if the cache can be evaluated at the specified encoded SCLK.
     */
    if (!isCacheEvaluatable(encodedSCLK)) {
      throw new CKEvaluationException(encodedSCLK);
    }

    /*
     * All that remains at this point is to compute the desired rotation from the cache content.
     * Simple check, see if the encodedSCLK value matches that of the leftSCLK.
     */
    if (encodedSCLK == leftSCLK) {
      return buffer.setTo(matrix);
    }

    return interpolator.interpolate(leftSCLK, record.getSecondsPerTickRate(), matrix, rate,
        encodedSCLK, buffer);
  }

  @Override
  public VectorIJK getAngularRate(double encodedSCLK, VectorIJK buffer) {
    /*
     * Update the internal cache if necessary.
     */
    updateCache(encodedSCLK);

    /*
     * Check to see if the cache can be evaluated at the specified encoded SCLK.
     */
    if (!isCacheEvaluatable(encodedSCLK)) {
      throw new CKEvaluationException(encodedSCLK);
    }

    return buffer.setTo(rate);
  }

  /**
   * Updates the cache to cover the supplied encoded SCLK.
   * 
   * @param encodedSCLK
   */
  private void updateCache(double encodedSCLK) {

    /*
     * If the cache covers the supplied time, there is nothing to do. The comparison here considers
     * the start of the next record's coverage interval, because there may be gaps.
     */
    if ((leftSCLK <= encodedSCLK) && (nextLeftSCLK > encodedSCLK)) {
      return;
    }

    /*
     * Locate the record just prior or equal to the supplied encoded SCLK.
     */
    int index = intervalStarts.indexLastLessThanOrEqualTo(encodedSCLK);

    /*
     * If all the records in the segment follow the supplied encoded SCLK, skip updating the cache.
     */
    if (index == -1) {
      return;
    }

    /*
     * Read the record from the table to populate the cache.
     */
    records.get(index, record);
    record.getRotation(matrix);
    record.getAngularRate(rate);

    leftSCLK = intervalStarts.getGauge(index);
    rightSCLK = intervalStops.getGauge(index);

    /*
     * If the index points to the last entry in intervalStarts, then assign nextLeftSCLK to
     * rightSCLK. Otherwise assign it to the next element.
     */
    if (index == intervalStarts.size() - 1) {
      nextLeftSCLK = rightSCLK;
    } else {
      nextLeftSCLK = intervalStarts.getGauge(index + 1);
    }

  }

  /**
   * Answers the question: is this particular encodedSCLK evaluatable from the cache? By that it
   * means that the time is implicitly valid for the cache, and returns true if interpolation is
   * valid for between the records in the cache.
   * <p>
   * Assumes that {@link #updateCache(double)} has already been invoked so the cache is valid, but
   * should return false otherwise.
   * </p>
   * 
   * @param encodedSCLK the time of interest
   * 
   * @return true if the records in the cache may be used to evaluate the rotation at the specified
   *         encoded SCLK.
   */
  private boolean isCacheEvaluatable(double encodedSCLK) {

    /*
     * Answering this question is simple, does encodedSCLK lie in the interval [leftSCLK,
     * rightSCLK]. updateCache *must* be called before attempting to determine if the cache is
     * evaluatable, otherwise it may opt to use the extrapolated rightSCLK instead of the value
     * potentially from the next record.
     */
    return (encodedSCLK >= leftSCLK) && (encodedSCLK <= rightSCLK);
  }

}
