package picante.spice.kernelpool.content;

import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.ImmutableMap;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;
import picante.spice.kernel.tk.sclk.SCLKInstantiationException;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.spice.kernelpool.BasicKernelPool;

/**
 * Factory class that generates encoded SCLK converters from kernel pool contents.
 */
public class SCLKFactory {

  /**
   * Defines a trivial implementation of the {@link UniformTimeProvider} interface which simply
   * returns the values provided. This is to be used whenever the SCLK is tied directly to TDB.
   */
  private static final UniformTimeProvider TDB_CONVERTER = new UniformTimeProvider() {

    @Override
    public double convertToTDB(double parallelTime) {
      return parallelTime;
    }

    @Override
    public double convertToUniformTime(double tdb) {
      return tdb;
    }
  };

  /**
   * Matcher used to identify the SCLK data type keyword in the kernel pool. This <b>must</b> be
   * present with any definition of an SCLK.
   */
  private final Matcher sclkMatcher = Pattern.compile("SCLK_DATA_TYPE_(-*\\d+)").matcher("");

  /**
   * Creates a type 1 SCLK from the necessary kernel pool based inputs.
   */
  private final SCLKType1Creator type1Creator = new SCLKType1Creator();

  /**
   * Field containing the TDT parallel time converter to supply whenever an SCLK correlates to TDT,
   * rather than TDB.
   */
  private final UniformTimeProvider tdtConverter;

  public SCLKFactory(UniformTimeProvider tdtConverter) {
    this.tdtConverter = tdtConverter;
  }

  /**
   * Creates a map of SPICE SCLK ID codes to their corresponding encoded SCLK providers
   * 
   * @param pool a kernel pool containing the SCLK content to extract
   * 
   * @return a newly created map of newly created encoded SCLK converter implementations
   * 
   * @throws SCLKInstantiationException if creating any of the converters fails for any reason
   */
  public ImmutableMap<Integer, SCLKKernel> createConverters(BasicKernelPool pool)
      throws SCLKInstantiationException {
    ImmutableMap.Builder<Integer, SCLKKernel> builder = ImmutableMap.builder();

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    /*
     * Loop over all the candidate keywords in the kernel pool that match the expected
     * SCLK_DATA_TYPE_<INTEGER> pattern.
     */
    Set<String> keywords = pool.getKeywords();
    for (String keyword : keywords) {

      sclkMatcher.reset(keyword);

      if (sclkMatcher.matches()) {

        /*
         * Note, the number format exception should not be generated if the regular expression that
         * was used to build the matcher is working properly.
         */
        int id = Integer.parseInt(sclkMatcher.group(1));
        builder.put(-id, createConverter(id, retriever));
      }
    }

    return builder.build();
  }

  /**
   * Creates an encoded SCLK converter for a particular ID from the supplied retriever.
   * 
   * @param id the ID code of interest, note this is the negative of the SPICE ID for the SCLK
   * @param retriever a validating kernel pool variable retriever that holds the contents of the
   *        pool from which the SCLK is to be derived
   * 
   * @return a newly created encoded SCLK converter based off the data contents supplied in
   *         retriever
   * 
   * @throws SCLKInstantiationException if an error occurs in creating the converter in question
   */
  private SCLKKernel createConverter(int id, KernelPoolValidatingRetriever retriever)
      throws SCLKInstantiationException {

    try {

      /*
       * Start by retrieving the data type keyword from the pool.
       */
      int type = retriever.getInteger("SCLK_DATA_TYPE_" + id);

      switch (type) {
        case 1:
          return type1Creator.create(id, retriever, TDB_CONVERTER, tdtConverter);

        default:
          throw new UnsupportedOperationException("SCLK of type: " + type + " is unsupported.");
      }

    } catch (Exception e) {
      throw new SCLKInstantiationException(-id, e);
    }

  }

}
