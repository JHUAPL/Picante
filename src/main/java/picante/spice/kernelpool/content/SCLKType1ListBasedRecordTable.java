package picante.spice.kernelpool.content;

import java.util.ArrayList;
import java.util.List;
import java.util.RandomAccess;
import picante.data.list.AbstractGaugedRetrievable;
import picante.spice.kernel.tk.sclk.SCLKType1Record;

/**
 * Class that implements the SclkType1Record table interface by wrapping an array of doubles and a
 * ticks per most significant count value.
 */
class SCLKType1ListBasedRecordTable extends AbstractGaugedRetrievable<SCLKType1Record> {

  private final List<Double> coeffs;
  private final double ticksPerMostSignificantCount;

  /**
   * Creates an SclkType1RecordTable implementation around the single dimensional double array of
   * correlation coefficients commonly extracted from type 1 SCLK kernels.
   * 
   * @param coeffs the list of coefficients of correlation coefficients. They occur in triples,
   *        encoded SCLK, parallel time in seconds, and rate in seconds per most significant tick.
   *        The class retains a reference to the supplied memory, only if the supplied list
   *        implements {@link RandomAccess} otherwise a copy is made for performance reasons. Note:
   *        it is clear this behavior is a bit confusing, but the lookups required by the
   *        implementation of this class benefit greatly from random access to the list. As the fact
   *        this class <b>can</b> wrap this array is purely an implementation detail, it should not
   *        be relied upon in general.
   * 
   * @param ticksPerMostSignificantCount the number of clock ticks per most significant clock count
   * 
   * @throws IllegalArgumentException if coeffs, as supplied, has length not divisible by 3.
   */
  public SCLKType1ListBasedRecordTable(List<Double> coeffs, double ticksPerMostSignificantCount) {

    if (coeffs.size() % 3 != 0) {
      throw new IllegalArgumentException(
          "SCLK coefficient array supplied to the constructor must be "
              + "of length divisible by 3.");
    }

    if (!(coeffs instanceof RandomAccess)) {
      this.coeffs = new ArrayList<Double>(coeffs);
    } else {
      this.coeffs = coeffs;
    }
    this.ticksPerMostSignificantCount = ticksPerMostSignificantCount;
  }

  @Override
  public double getGauge(int index) {
    return coeffs.get(3 * index);
  }

  @Override
  public int size() {
    return coeffs.size() / 3;
  }

  @Override
  public SCLKType1Record get(int index, SCLKType1Record buffer) {
    return buffer.setRecord(coeffs.get(3 * index), coeffs.get(3 * index + 1),
        coeffs.get(3 * index + 2), ticksPerMostSignificantCount);
  }

}
