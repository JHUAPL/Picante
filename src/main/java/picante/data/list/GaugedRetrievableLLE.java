package picante.data.list;

/**
 * Extension of the {@link GaugedRetrievable} interface that provides a method for performing last
 * less than or equal to searches on the list of gauged (typically temporally) non-decreasing
 * records.
 * <p>
 * The methods on the {@link GaugedRetrievable} interface, specifically:
 * {@link GaugedRetrievable#getGauge(int)} allow generic binary searching to be performed on the
 * contents of any sequence of records against the gauge. However, this particular interface leaves
 * it entirely up to the implementation as to how that search should be performed. It may be able to
 * exploit additional information not captured by the generic interface, but known to the
 * implementation to greatly improve performance.
 * </p>
 * 
 * @param <R> the type of record required by the implementation
 */
public interface GaugedRetrievableLLE<R> extends GaugedRetrievable<R> {

  /**
   * Obtain the index of the record that is last less than or equal to the supplied gauge value.
   * 
   * @param gauge double containing the gauge value of interest
   * 
   * @return an integer in the range [-1, {@link Retrievable#size()}-1]. -1 indicates the supplied
   *         gauge occurs prior to the gauge values associated with all the records
   */
  public int indexLastLessThanOrEqualTo(double gauge);

}
