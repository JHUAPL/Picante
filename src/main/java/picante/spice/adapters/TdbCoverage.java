package picante.spice.adapters;

import com.google.common.annotations.VisibleForTesting;
import picante.math.intervals.Interval;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.TimeOutOfBoundsException;
import picante.spice.kernel.ck.CKSegment;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

/**
 * Adapter class that adapts the SCLK based coverage returned from the
 * {@link CKSegment#getCoverage()} method to the TDB based coverage required by the higher level
 * crucible mechanics API.
 */
class TdbCoverage implements Coverage {

  /**
   * The SCLK based coverage to adapt.
   */
  private final Coverage delegate;

  /**
   * The converter between encoded SCLK and TDB appropriate for this coverage adaptation.
   */
  private final EncodedSCLKConverter converter;

  private final Interval interval = new Interval();

  /**
   * Creates a new adapter from the supplied coverage and SCLK converter.
   * 
   * @param coverage the CK, SCLK based coverage object to adapt, this adapter retains a reference.
   * 
   * @param converter the encoded SCLK converter to utilize in adapting the SCLK coverage to TDB.
   */
  public TdbCoverage(Coverage coverage, EncodedSCLKConverter converter) {
    this.delegate = coverage;
    this.converter = converter;
  }

  @Override
  public boolean contains(double tdbTime) {

    /*
     * First check to see that the supplied tdbTime is valid for the converter.
     */
    if (!converter.getTDBRange(interval).closedContains(tdbTime)) {
      return false;
    }

    return delegate.contains(converter.convertToEncodedSclk(tdbTime));
  }

  @Override
  public Interval getBoundingInterval(Interval buffer) {
    return convertToTDB(delegate.getBoundingInterval(buffer));
  }

  @Override
  public Interval getBracketingInterval(double tdbTime, Interval buffer) {

    /*
     * Check to see if the supplied tdbTime lies inside the range supported by the converter. If it
     * does not, throw the time out of bounds exception, as there can be no bracketing interval.
     */
    converter.getTDBRange(interval);
    if (!interval.closedContains(tdbTime)) {
      throw new TimeOutOfBoundsException("The supplied time: " + tdbTime
          + " lies outside the range of the " + "supporting SCLK converter.");
    }

    return convertToTDB(
        delegate.getBracketingInterval(converter.convertToEncodedSclk(tdbTime), buffer));
  }

  @Override
  public boolean hasNextInterval(double tdbTime) {

    /*
     * TODO: There is a potential round-off issue hanging in the air here. If the range returned by
     * converter.getTDBRange() is not able to be converted to encoded SCLK, then this may result in
     * unusual or unexpected behavior at the boundary cases.
     * 
     * Handle the boundary cases for the converter. If the supplied time lies outside the range
     * supported by the converter, process the result appropriately.
     */
    converter.getTDBRange(interval);

    if (tdbTime < interval.getBegin()) {
      /*
       * The supplied tdbTime occurs prior to the supported range of the encoded SCLK converter,
       * this must be before any intervals contained in the coverage.
       */
      return true;
    }
    if (tdbTime > interval.getEnd()) {
      /*
       * The supplied tdbTime occurs after the supported range of the encoded SCLK converter, this
       * must be after any intervals contained in the coverage.
       */
      return false;
    }

    /*
     * If we reach here, then it's safe to attempt conversion to encoded SCLK.
     */
    return delegate.hasNextInterval(converter.convertToEncodedSclk(tdbTime));
  }

  @Override
  public Interval getNextInterval(double tdbTime, Interval buffer) {

    /*
     * Handle the boundary cases for the converter, if the supplied time lies outside the range
     * handled by it; process the result properly.
     */
    converter.getTDBRange(interval);

    if (tdbTime < interval.getBegin()) {
      /*
       * The supplied tdbTime occurs prior to the supported range of the encoded SCLK converter.
       * This must be before any of the intervals contained in the coverage. Hand back the first
       * interval.
       */
      return convertToTDB(Coverages.getFirstInterval(delegate, buffer));
    }

    if (tdbTime > interval.getEnd()) {
      /*
       * The supplied tdbTime occurs after the supported range of the encoded SCLK converter. This
       * must be after any of the intervals contained in the coverage. Throw the appropriate runtime
       * exception.
       */
      throw new TimeOutOfBoundsException("The supplied time: " + tdbTime
          + " lies outside the range of the " + "supporting SCLK converter.");

    }

    return convertToTDB(delegate.getNextInterval(converter.convertToEncodedSclk(tdbTime), buffer));
  }

  @VisibleForTesting
  Interval convertToTDB(Interval sclkInterval) {
    sclkInterval.set(converter.convertToTDB(sclkInterval.getBegin()),
        converter.convertToTDB(sclkInterval.getEnd()));
    return sclkInterval;
  }

  @Override
  public boolean equals(Object object) {
    return Coverages.equalsImplementation(this, object);
  }

  @Override
  public int hashCode() {
    return Coverages.hashCodeImplementation(this);
  }

}
