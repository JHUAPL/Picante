package picante.spice.kernel.tk.lsk;

/**
 * Interface describing the conversion between TDB and another uniform time system.
 * <p>
 * TODO: Work out what exceptions, if any this interface should throw in exceptional cases.
 * 
 * TODO: Document what is meant by uniform time system
 * </p>
 */
public interface UniformTimeProvider {

  /**
   * Converts the a uniform time system value into TDB seconds past J2000.
   * 
   * @param uniformTime the uniform time to convert
   * 
   * @return the corresponding time in TDB seconds past J2000
   */
  public double convertToTDB(double uniformTime);

  /**
   * Converts TDB seconds past J2000 into the parallel time system.
   * 
   * @param tdb the TDB seconds past J2000 to convert
   * 
   * @return the corresponding time in the parallel time system.
   */
  public double convertToUniformTime(double tdb);

}
