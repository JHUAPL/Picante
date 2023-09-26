package picante.spice.kernelpool.content;

import java.util.List;

import com.google.common.collect.ImmutableSortedSet;
import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernel.tk.lsk.LSKInstantiationException;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.time.LeapsecondEntry;

public class LSKFactory {

  /**
   * Keyword in the kernel pool that contains the DUT and leapsecond activation table. TODO: This
   * will be needed at some point.
   */
  // private static final String LSK_KEY = "DELTET/DELTA_AT";

  /**
   * Keyword in the kernel pool that contains the time shift between TDT and TAI.
   */
  private static final String DTA_KEY = "DELTET/DELTA_T_A";

  /**
   * Keyword in the kernel pool that contains the amplitude of the oscillatory term in the expansion
   * of the difference between ET and TDT.
   */
  private static final String K_KEY = "DELTET/K";

  /**
   * Keyword in the kernel pool that contains the amplitude of the first term in the trigonometric
   * expansion of the eccentric anomaly.
   */
  private static final String EB_KEY = "DELTET/EB";

  /**
   * Keyword in the kernel pool that contains the mean anomaly parameters.
   */
  private static final String M_KEY = "DELTET/M";

  private static final String LEAP_KEY = "DELTET/DELTA_AT";

  /**
   * Creates a leapseconds kernel from the supplied kernel pool.
   * 
   * @param pool the kernel pool containing the leapseconds content
   * 
   * @return
   * @throws LSKInstantiationException
   */
  public LSK createLSK(UnwritableKernelPool pool) throws LSKInstantiationException {

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    try {

      double[] m = new double[2];
      List<Double> values = retriever.getDoublesExpectedLength(M_KEY, 2);
      m[0] = values.get(0);
      m[1] = values.get(1);

      List<Double> leapsecondValues = retriever.getDoublesLengthModulo(LEAP_KEY, 2);

      ImmutableSortedSet.Builder<LeapsecondEntry> lskBuilder = ImmutableSortedSet.naturalOrder();

      for (int i = 0; i < leapsecondValues.size(); i += 2) {
        lskBuilder.add(new LeapsecondEntry(leapsecondValues.get(i), leapsecondValues.get(i + 1)));
      }

      return new LSK(retriever.getDouble(DTA_KEY), retriever.getDouble(K_KEY),
          retriever.getDouble(EB_KEY), m, lskBuilder.build());
    } catch (KernelPoolValidationException e) {
      throw new LSKInstantiationException("Unable to instantiate LSK kernel.", e);
    }
  }

}
