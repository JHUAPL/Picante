package picante.spice.kernel.ck;

import picante.data.list.GaugedRetrievableLLE;
import picante.data.list.GaugedRetrievableLLET;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.rotations.Quaternion;

/**
 * Implementation of the type-3 C-kernel segment, constant rate, linear interpolation between
 * quaternion records.
 * <p>
 * Assumptions about the correctness of segment structure:
 * <ul>
 * <li>The records and interval starts are sorted properly in time.</li>
 * <li>There are no duplicate records in the segment, with regards to timing.</li>
 * <li>InitialEncodedSCLK <= First Record Encoded SCLK <= Last Record Encoded SCLK <=
 * FinalEncodedSCLK</li>
 * <li>Interpolation interval starts are elements of the actual record times.</li>
 * </ul>
 * </p>
 */
public class CKType3 extends AbstractCKSegment {

  private final GaugedRetrievableLLET<Quaternion> records;
  private final GaugedRetrievableLLE<?> intervals;
  private final Coverage coverage;
  private final Quaternion qBuffer = new Quaternion();
  private final Type3Interpolator interpolator = new Type3Interpolator();

  /*
   * The following fields are utilized for the internal caching mechanism built into the class. This
   * prevents unnecessary, and potentially costly lookups from the supporting record tables.
   */
  private final RotationMatrixIJK leftMatrix = new RotationMatrixIJK();
  private final RotationMatrixIJK rightMatrix = new RotationMatrixIJK();
  private double leftSCLK;
  private double rightSCLK;
  private boolean isInterpolationAllowed;

  /**
   * Creates a type 3 C-kernel segment without angular velocity.
   * 
   * @param name the segment name
   * @param instrumentID the integer ID code of the frame the segment transforms vectors from
   * @param referenceID the integer ID code of the frame the segment transforms vectors to
   * @param initialEncodedSCLK the bracketing, initial SCLK must be less than or equal to the
   *        encoded SCLK of the first record in <code>records</code>
   * @param finalEncodedSCLK the bracketing, final SCLK must be greater than or equal to the encoded
   *        SCLK of the last record in <code>records</code>
   * @param records a record table of quaternions. This table must be properly sorted on time and
   *        contain no duplicate records. The quaternions are such that when converted to a rotation
   *        matrix, they represent a matrix that rotates vectors from the frame associated with
   *        referenceID to the frame associated with instrumentID.
   * @param intervals a record table of interval start times, which must be coincident with encoded
   *        SCLK times present in <code>records</code>
   */
  public CKType3(String name, int instrumentID, int referenceID, double initialEncodedSCLK,
      double finalEncodedSCLK, GaugedRetrievableLLET<Quaternion> records,
      GaugedRetrievableLLE<?> intervals) {
    super(name, instrumentID, referenceID, initialEncodedSCLK, finalEncodedSCLK);
    this.records = records;
    this.intervals = intervals;
    this.coverage = new Coverage();

    /*
     * Configure the cache so that it will force an update when any query is performed.
     */
    this.leftSCLK = Double.MAX_VALUE;
    this.rightSCLK = -Double.MAX_VALUE;

  }

  public GaugedRetrievableLLET<Quaternion> getRecords() {
    return records;
  }

  public GaugedRetrievableLLE<?> getIntervals() {
    return intervals;
  }

  @Override
  public CKCoverage getCoverage() {
    return coverage;
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
     * Check to see if the encoded SCLK value matches that of the left or right record first.
     */
    if (encodedSCLK == leftSCLK) {
      return buffer.setTo(leftMatrix);
    }

    if (encodedSCLK == rightSCLK) {
      return buffer.setTo(rightMatrix);
    }

    /*
     * At this point interpolation is required.
     */
    return interpolator.interpolate(leftSCLK, leftMatrix, rightSCLK, rightMatrix, encodedSCLK,
        buffer);
  }

  @Override
  public int getType() {
    return 3;
  }

  @Override
  public boolean hasAngularVelocity() {
    return false;
  }

  /**
   * Updates the cache to cover the supplied encoded SCLK.
   * 
   * @param encodedSCLK
   */
  private void updateCache(double encodedSCLK) {

    /*
     * If the cache covers the supplied time, there is nothing to do.
     */
    if ((leftSCLK <= encodedSCLK) && (rightSCLK > encodedSCLK)) {
      return;
    }

    /*
     * Locate the record just prior or equal to the supplied encoded SCLK.
     */
    int index = records.indexLastLessThanOrEqualTo(encodedSCLK);

    /*
     * If all the records in the segment follow the supplied encoded SCLK, skip updating the cache.
     */
    if (index == -1) {
      return;
    }

    /*
     * Set the left cache elements to the record from index.
     */
    leftSCLK = records.getGauge(index);
    records.get(index, qBuffer);
    qBuffer.getRotation(leftMatrix);

    /*
     * If this is the last record in the C-kernel, simply copy the same value into the right record.
     */
    if (index == records.size() - 1) {
      rightSCLK = leftSCLK;
      rightMatrix.setTo(leftMatrix);
      return;
    }

    rightSCLK = records.getGauge(++index);
    records.get(index, qBuffer);
    qBuffer.getRotation(rightMatrix);

    /*
     * Determine if the left and right records lie in the same interpolation interval. This is
     * straightforward: does the start of the interpolation interval associated with the right
     * record precede the encoded SCLK for the left record? If so, interpolation is allowed.
     */
    index = intervals.indexLastLessThanOrEqualTo(rightSCLK);

    /*
     * This should never happen, but if it does, it indicates an improperly constructed type 3
     * segment.
     */
    if (index == -1) {
      throw new CKEvaluationException(
          "Invalid type 3 C-kernel content, " + "the segment contains records that precede the "
              + "start of the first interpolation interval.");
    }

    isInterpolationAllowed = (intervals.getGauge(index) <= leftSCLK);

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
     * First check to see if the times lie inside the cache's coverage. Then see if either of the
     * times are equal to the record times.
     */
    if ((encodedSCLK < leftSCLK) || (encodedSCLK > rightSCLK)) {
      return false;
    }
    if ((encodedSCLK == leftSCLK) || (encodedSCLK == rightSCLK)) {
      return true;
    }
    return isInterpolationAllowed;
  }

  /**
   * Inner subclass of the package private implementation of the CK coverage interface. This allows
   * caching functionality to be bolted on when the contains method is invoked.
   */
  class Coverage extends IntervalStartsCKCoverage {

    /**
     * Simple constructor that falls directly through to the constructor of the super class.
     */
    public Coverage() {
      super(initialEncodedSCLK, finalEncodedSCLK, records, intervals);
    }

    /**
     * {@inheritDoc}
     * 
     * Forces the internal cache of the instance to be updated when invoked.
     */
    @Override
    public boolean contains(double encodedSCLK) {

      /*
       * Check gross, out of bounds errors.
       */
      if ((encodedSCLK < initialEncodedSCLK) || (encodedSCLK > finalEncodedSCLK)) {
        return false;
      }

      /*
       * Update the cache and determine if we can evaluate the resultant supplied time.
       */
      updateCache(encodedSCLK);
      return isCacheEvaluatable(encodedSCLK);
    }

  }

}
