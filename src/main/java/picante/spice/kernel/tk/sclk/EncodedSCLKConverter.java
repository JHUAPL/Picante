package picante.spice.kernel.tk.sclk;

import picante.math.intervals.Interval;

/**
 * Interface that describes methods necessary to convert between encoded SCLK and TDB seconds past
 * J2000.
 */
public interface EncodedSCLKConverter {

  /**
   * Retrieves the time range, TDB, over which conversions to encoded SCLK are supported.
   * 
   * @param buffer the interval to receive the bounding interval
   * 
   * @return a reference to buffer, for convenience.
   */
  public Interval getTDBRange(Interval buffer);

  /**
   * Retrieves the time range, encoded SCLK, over which conversions to TDB are supported. Note: Due
   * to the way SCLK kernels are constructed this may be a subset of the range over which SCLK to
   * encoded SCLK conversions are permitted.
   * 
   * @param buffer the interval to receive the bounding interval
   * 
   * @return a reference to buffer, for convenience.s
   */
  public Interval getEncodedSclkRange(Interval buffer);

  /**
   * Convert the supplied encoded SCLK value into TDB seconds past J2000.
   * 
   * @param encodedSCLK the encoded SCLK value to convert
   * 
   * @return the TDB seconds past J2000 that corresponds to encodedSCLK
   * 
   * @throws SCLKEvaluationException if the supplied encoded SCLK is out of range supported by the
   *         converter.
   */
  public double convertToTDB(double encodedSCLK);

  /**
   * Convert the supplied TDB seconds past J2000 into encoded SCLK.
   * 
   * @param tdb the TDB seconds past J2000 to convert
   * 
   * @return the corresponding encoded SCLK
   * 
   * @throws SCLKEvaluationException if the supplied TDB lies out of the range supported by the
   *         converter.
   */
  public double convertToEncodedSclk(double tdb);

}
