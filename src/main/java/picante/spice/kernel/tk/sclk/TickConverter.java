package picante.spice.kernel.tk.sclk;

/**
 * Class that captures the arithmetic to convert a clock field into units of ticks, that are
 * ultimately used to build encoded SCLK. This conversion moves SCLK fields into ticks, however,
 * these values are NOT encoded SCLK. The partition is ignored in this process.
 */
interface TickConverter {

  /**
   * Determines whether a particular clock value can be safely converted to ticks by this converter.
   * This inherently ignores the partition structure and just determines if the clock fields are
   * reasonable.
   * 
   * @param sclk the SCLK value to check
   * 
   * @return true if the fields for SCLK are convertible to a tick count without error, false
   *         otherwise
   */
  public boolean isValidClock(SCLK sclk);

  /**
   * Converts the fields of the clock to a raw count of ticks, ignoring the partition
   * 
   * @param sclk the SCLK value to convert the clock fields
   * 
   * @return a count of the least significant units of the clock for the fields
   * 
   * @throws SCLKEvaluationException if the supplied SCLK value is not convertible
   */
  public double convertToTicks(SCLK sclk);

  /**
   * Converts a raw count of ticks of the clock to its canonical field values ignoring the partition
   * 
   * @param ticks the number of least significant field counts to convert
   * @param buffer the SCLK value to receive the field breakdown, the partition is left alone
   * 
   * @return a reference to SCLK for convenience
   * 
   * @throws SCLKEvaluationException if anything goes wrong with the conversion from ticks to SCLK
   */
  public SCLK convertToSCLK(double ticks, SCLK buffer);

}
