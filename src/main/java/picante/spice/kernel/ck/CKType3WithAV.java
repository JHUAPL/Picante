package picante.spice.kernel.ck;

import picante.data.list.GaugedRetrievableLLE;
import picante.data.list.GaugedRetrievableLLET;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.DifferentiatedRotation;
import picante.mechanics.rotations.Quaternion;
import picante.mechanics.rotations.WrapperWithRate;

/**
 * Implementation of the type-3 C-kernel segment including angular velocity, with standard
 * quaternion records. The assumptions about the viability of the segment data are the same as the
 * type 3 segment.
 * 
 * @see CKType3
 */
public class CKType3WithAV extends AbstractCKSegment implements CKSegmentWithAV {

  private final GaugedRetrievableLLET<WrapperWithRate<Quaternion>> records;
  private final GaugedRetrievableLLE<?> intervals;
  private final Coverage coverage;
  private final WrapperWithRate<Quaternion> rBuffer =
      new WrapperWithRate<Quaternion>(new Quaternion());
  private final Type3Interpolator interpolator = new Type3Interpolator();

  /*
   * The following fields are utilized for the internal caching mechanism built into the class. This
   * prevents unnecessary, and potentially costly lookups from the supporting record tables.
   */
  private final RotationMatrixIJK leftMatrix = new RotationMatrixIJK();
  private final RotationMatrixIJK rightMatrix = new RotationMatrixIJK();
  private final VectorIJK leftAV = new VectorIJK();
  private final VectorIJK rightAV = new VectorIJK();
  private double leftSCLK;
  private double rightSCLK;
  private boolean isInterpolationAllowed;

  /**
   * Constructs a type 3 C-kernel. Conforms to the NAIF standard, but since it is through an
   * interface it is easily adaptable with the {@link DifferentiatedRotation} API to accept any type
   * of differentiable rotation.
   * 
   * @param name
   * @param instrumentID
   * @param referenceID
   * @param initialEncodedSCLK
   * @param finalEncodedSCLK
   * @param records
   * @param intervals
   */
  public CKType3WithAV(String name, int instrumentID, int referenceID, double initialEncodedSCLK,
      double finalEncodedSCLK, GaugedRetrievableLLET<WrapperWithRate<Quaternion>> records,
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

  public GaugedRetrievableLLET<WrapperWithRate<Quaternion>> getRecords() {
    return records;
  }

  public GaugedRetrievableLLE<?> getIntervals() {
    return intervals;
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

    /*
     * All that remains at this point is to compute the desired rotation from the cache content.
     * Check to see if the encoded SCLK value matches that of the left or right record first.
     */
    if (encodedSCLK == leftSCLK) {
      return buffer.setTo(leftAV);
    }

    if (encodedSCLK == rightSCLK) {
      return buffer.setTo(rightAV);
    }

    /*
     * At this point interpolation is required.
     */
    return interpolator.interpolate(leftSCLK, leftAV, rightSCLK, rightAV, encodedSCLK, buffer);
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
    return true;
  }

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
    records.get(index, rBuffer);
    rBuffer.getRotation(leftMatrix);
    rBuffer.getRate(leftAV);

    /*
     * If this is the last record in the C-kernel, simply copy the same value into the right record.
     */
    if (index == records.size() - 1) {
      rightSCLK = leftSCLK;
      rightMatrix.setTo(leftMatrix);
      rightAV.setTo(leftAV);
      return;
    }

    rightSCLK = records.getGauge(++index);
    records.get(index, rBuffer);
    rBuffer.getRotation(rightMatrix);
    rBuffer.getRate(rightAV);

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

    public Coverage() {
      super(initialEncodedSCLK, finalEncodedSCLK, records, intervals);
    }

    @Override
    public boolean contains(double encodedSCLK) {
      return super.contains(encodedSCLK);
    }

  }

}
