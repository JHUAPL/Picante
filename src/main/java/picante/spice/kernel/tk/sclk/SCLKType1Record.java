package picante.spice.kernel.tk.sclk;

import picante.data.list.GaugedRetrievable;

/**
 * An implementation of the SCLK type 1 clock correlation coefficient record.
 * <p>
 * These records tie encoded SCLK to a parallel time system, typically TDB or TDT. The API provided
 * here is parallel time system agnostic, as it merely provides software that connects the two.
 * Higher level, i.e. more directly useful code, should adapt the parallel time system values
 * appropriately.
 * </p>
 * <p>
 * This object is mutable, as it is designed to be utilized within the {@link GaugedRetrievable}
 * interface.
 * </p>
 */
public class SCLKType1Record {

  /**
   * The encoded SCLK value, typically expressed in units of least significant field of the clock.
   */
  private double encodedSCLK;

  /**
   * Parallel time, typically expressed in units of seconds past J2000.0 in the appropriate time
   * system.
   */
  private double parallelTime;

  /**
   * Clock drift rate, expressed in units of parallel time system seconds per most significant count
   * of the clock.
   */
  private double rate;

  /**
   * Number of least significant clock field counts in one most significant count of the clock.
   */
  private double ticksPerMostSignificantCount;

  /**
   * Creates an empty record with each data field assigned to 0. Note: Usage of this constructor
   * will result in a record whose extrapolate methods may result in floating point errors.
   */
  public SCLKType1Record() {}

  /**
   * Creates an SCLK type 1 coefficient record.
   * 
   * @param encodedSCLK the encoded SCLK component of the record
   * @param parallelTime the parallel time system component of the record
   * @param rate the clock drift rate in units of parallel time system seconds per most significant
   *        count of the SCLK
   * @param ticksPerMostSignificantCount the number of least significant ticks per most significant
   *        count of the clock
   */
  public SCLKType1Record(double encodedSCLK, double parallelTime, double rate,
      double ticksPerMostSignificantCount) {
    this.encodedSCLK = encodedSCLK;
    this.parallelTime = parallelTime;
    this.rate = rate;
    this.ticksPerMostSignificantCount = ticksPerMostSignificantCount;
  }

  public double getEncodedSCLK() {
    return encodedSCLK;
  }

  public double getParallelTime() {
    return parallelTime;
  }

  public double getRate() {
    return rate;
  }

  public double getTicksPerMostSignificantCount() {
    return ticksPerMostSignificantCount;
  }

  /**
   * Sets the content of a record to the supplied values.
   * 
   * @param encodedSCLK
   * @param parallelTime
   * @param rate
   * @param ticksPerMostSignificantCount
   * @return
   */
  public SCLKType1Record setRecord(double encodedSCLK, double parallelTime, double rate,
      double ticksPerMostSignificantCount) {
    this.encodedSCLK = encodedSCLK;
    this.parallelTime = parallelTime;
    this.rate = rate;
    this.ticksPerMostSignificantCount = ticksPerMostSignificantCount;
    return this;
  }

  /**
   * Extrapolate off the record contents to produce a parallel time system time entry from the
   * supplied encoded SCLK.
   * 
   * @param encodedSCLK the encoded SCLK to convert to parallel time system
   * 
   * @return the parallel time system value provided by extrapolating off the record contents
   */
  public double extrapolateToParallelTime(double encodedSCLK) {
    return parallelTime + rate / ticksPerMostSignificantCount * (encodedSCLK - this.encodedSCLK);
  }

  /**
   * Extrapolate off the record contents to produce an encoded SCLK entry from the supplied parallel
   * time system value.
   * 
   * @param parallelTime the parallel time value to convert to encoded SCLK.
   * 
   * @return the corresponding encoded SCLK value provided by extrapolating off the record contents
   */
  public double extrapolateToEncodedSCLK(double parallelTime) {
    return encodedSCLK
        + 1.0 / (rate / ticksPerMostSignificantCount) * (parallelTime - this.parallelTime);
  }

}
