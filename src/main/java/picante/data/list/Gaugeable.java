package picante.data.list;

/**
 * Interface defining a gauge used to measure the distance between records.
 * <p>
 * Typically the gauge is measuring separation in time between records in a time series, but this is
 * entirely abstract in that it could be temperatures or some other measurable quantity. This
 * interface exists primarily to specify how the conversion from one object to the double gauge will
 * occur.
 * </p>
 * 
 * @param <T> the object to convert to a gauge
 */
public interface Gaugeable<T> {

  /**
   * Compute the position of the supplied value against this gauge.
   * 
   * @param t the value to gauge
   * 
   * @return the gauge associated with the value
   */
  double gauge(T t);

}
