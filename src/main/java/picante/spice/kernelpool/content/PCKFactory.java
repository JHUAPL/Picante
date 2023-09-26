package picante.spice.kernelpool.content;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.ImmutableListMultimap;
import picante.spice.kernel.tk.pck.JEDOffsetConverter;
import picante.spice.kernel.tk.pck.PCKFrameFunction;
import picante.spice.kernel.tk.pck.PCKWithNutationFrameFunction;
import picante.spice.kernel.tk.pck.TextPCK;
import picante.spice.kernel.tk.pck.TextPCKInstantiationException;
import picante.spice.kernel.tk.pck.TimeConverter;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.provided.InertialFrames;

public class PCKFactory {

  /**
   * This is implicitly defined in {@link InertialFrames}.
   */
  private static final int J2000_FRAME_CODE = 1;

  /**
   * Creates a list of PCKFrameFunctions specified by the keywords present in the supplied pool.
   * 
   * @param pool a kernel pool containing PCK frame definitions.
   * 
   * @return a list of newly created PCK frame functions, possibly empty if no such definitions are
   *         present.
   * 
   * @throws TextPCKInstantiationException if the data supplied in the kernel pool is incomplete or
   *         otherwise invalid for any of the bodies of interest
   */
  public TextPCK createTextPCK(BasicKernelPool pool) throws TextPCKInstantiationException {

    List<PCKFrameFunction> result = new ArrayList<PCKFrameFunction>();

    /*
     * Create the validating retrieval mechanism.
     */
    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    /*
     * Start by accumulating all of the PCK frames that are present in the kernel pool. NAIF keys
     * off BODY#_PM, so we'll do the same.
     */
    for (int bodyCode : identifyBodyCodesForFrames(pool.getKeywords())) {
      result.add(createFrameFunction(bodyCode, retriever));
    }

    ImmutableListMultimap.Builder<Integer, Double> radiiResult = ImmutableListMultimap.builder();

    for (int bodyCode : identifyBodyCodesForEllipsoidalShapes(pool.getKeywords())) {
      radiiResult.putAll(bodyCode, fetchRadii(bodyCode, retriever));
    }

    return new TextPCK(result, radiiResult.build());

  }

  List<Double> fetchRadii(int bodyCode, KernelPoolValidatingRetriever pool)
      throws TextPCKInstantiationException {
    try {
      return pool.getDoublesExpectedLength(createKeyword(bodyCode, "RADII"), 3);
    } catch (KernelPoolValidationException e) {
      throw new TextPCKInstantiationException(
          "Unable to retrieve body radii for PCK body: " + bodyCode, e);
    }

  }

  /**
   * Creates the PCK frame function associated with the supplied code.
   * 
   * @param bodyCode the integer code for the body of interest
   * @param pool the pool containing the data defining the frame
   * 
   * @return a newly constructed PCKFrameFunction that captures the rotation for the requested body
   *         from the kernel pool
   * 
   * @throws TextPCKInstantiationException if the data supplied in the kernel pool is incomplete or
   *         otherwise invalid
   */
  PCKFrameFunction createFrameFunction(int bodyCode, KernelPoolValidatingRetriever pool)
      throws TextPCKInstantiationException {

    /*
     * Locate the ID code for the corresponding barycenter.
     */
    int barycenterCode = computeBarycenter(bodyCode);

    try {

      /*
       * Start by obtaining the appropriate time converter.
       */
      TimeConverter converter = createConverter(barycenterCode, pool);

      /*
       * Next determine the reference frame code to which the body fixed frame is relative.
       */
      int referenceCode = getReferenceFrame(barycenterCode, pool);

      /*
       * Pull the appropriate arrays out of the kernel pool for the basic angle evaluations.
       */
      double[] pm =
          createArrayLengthThree(pool.getDoublesLengthNoMoreThan(createKeyword(bodyCode, "PM"), 3));
      double[] poleRA = createArrayLengthThree(
          pool.getDoublesLengthNoMoreThan(createKeyword(bodyCode, "POLE_RA"), 3));
      double[] poleDEC = createArrayLengthThree(
          pool.getDoublesLengthNoMoreThan(createKeyword(bodyCode, "POLE_DEC"), 3));

      /*
       * Now extract any of the nutation and precession coefficients. If there are any terms at all,
       * then the NUT_PREC_ANGLES keyword must exist.
       */
      String nutPrecKeyword = createKeyword(barycenterCode, "NUT_PREC_ANGLES");

      if (pool.containsKeyword(nutPrecKeyword)) {

        /*
         * Retrieve the nutation and precession angles content.
         */
        double[][] nutPrecAngles = create2DArray(pool.getDoublesLengthModulo(nutPrecKeyword, 2));

        int maximumLength = nutPrecAngles[0].length;

        /*
         * And all of the other data arrays, if they exist. If they do not an exception will be
         * generated.
         */
        double[] nutPrecRA =
            fetchAndCreate(createKeyword(bodyCode, "NUT_PREC_RA"), maximumLength, pool);
        double[] nutPrecDEC =
            fetchAndCreate(createKeyword(bodyCode, "NUT_PREC_DEC"), maximumLength, pool);
        double[] nutPrecPM =
            fetchAndCreate(createKeyword(bodyCode, "NUT_PREC_PM"), maximumLength, pool);

        return new PCKWithNutationFrameFunction(converter, referenceCode, bodyCode, poleRA, poleDEC,
            pm, nutPrecAngles, nutPrecRA, nutPrecDEC, nutPrecPM);
      }

      /*
       * No nutation and precession terms were present, return the simpler implementation.
       */
      return new PCKFrameFunction(converter, referenceCode, bodyCode, poleRA, poleDEC, pm);

    } catch (KernelPoolValidationException e) {
      throw new TextPCKInstantiationException(
          "Unable to create PCK frame transformation function for body frame code: " + bodyCode, e);
    }
  }

  /**
   * Fetch values from the kernel pool and create an array to capture them.
   * 
   * @param keyword the keyword to retrieve
   * @param maximumLength the maximum acceptable length of the array
   * @param pool the pool from which to retrieve the values
   * 
   * @return a newly created double[] of length no more than maximumLength.
   * 
   * @throws KernelPoolValidationException if the length of the data array in the pool exceeds
   *         maximumLength, the requested keyword is absent or of a non-numeric type
   */
  double[] fetchAndCreate(String keyword, int maximumLength, KernelPoolValidatingRetriever pool)
      throws KernelPoolValidationException {

    if (pool.containsKeyword(keyword)) {
      return createArray(pool.getDoublesMaximumLength(keyword, maximumLength));
    }
    return new double[0];

  }

  /**
   * Create a 2D array from the supplied list of double values.
   * 
   * @param values a list of 2N values to split into a 2D array of doubles
   * 
   * @return a double[2][N] array containing the 2N values supplied
   */
  double[][] create2DArray(List<Double> values) {
    double[][] result = new double[2][values.size() / 2];
    for (int i = 0; i < values.size() / 2; i++) {
      result[0][i] = values.get(2 * i);
      result[1][i] = values.get(2 * i + 1);
    }
    return result;
  }

  /**
   * Create an array of length three from the supplied list of double values.
   * 
   * @param values a list of double values possibly less than length 3.
   * 
   * @return a zero padded array of length 3, with the contents of values starting at index 0.
   */
  double[] createArrayLengthThree(List<Double> values) {
    double[] result = new double[3];
    for (int i = 0; i < 3; i++) {
      result[i] = (i < values.size()) ? values.get(i) : 0;
    }
    return result;
  }

  /**
   * Creates an array from the supplied list of double values.
   * 
   * @param values a list of N values to adapt into an array.
   * 
   * @return a double[N] array containing the list of values supplied
   */
  double[] createArray(List<Double> values) {
    double[] result = new double[values.size()];
    for (int i = 0; i < values.size(); i++) {
      result[i] = values.get(i);
    }
    return result;
  }

  /**
   * Identifies any body codes for which the kernel pool keyword list contains PCK frame
   * definitions.
   * 
   * @param keywords a list of keywords from a kernel pool
   * 
   * @return a list of integer codes for each body that should contain a PCK frame definition
   */
  Set<Integer> identifyBodyCodesForFrames(Set<String> keywords) {

    Set<Integer> result = new HashSet<Integer>();
    Pattern pattern = Pattern.compile("BODY(-*\\d+)_PM");
    Matcher matcher = pattern.matcher("");

    for (String s : keywords) {
      matcher.reset(s);

      if (matcher.matches()) {
        String code = matcher.group(1);
        result.add(Integer.parseInt(code));
      }

    }

    return result;
  }

  Set<Integer> identifyBodyCodesForEllipsoidalShapes(Set<String> keywords) {

    Set<Integer> result = new HashSet<>();
    Pattern pattern = Pattern.compile("BODY(-*\\d+)_RADII");
    Matcher matcher = pattern.matcher("");

    for (String s : keywords) {
      matcher.reset(s);

      if (matcher.matches()) {
        String code = matcher.group(1);
        result.add(Integer.parseInt(code));
      }
    }

    return result;
  }

  /**
   * Creates the appropriate time converter for the supplied barycenter code.
   * 
   * @param barycenterCode
   * @param pool
   * @return
   * @throws KernelPoolValidationException
   */
  TimeConverter createConverter(int barycenterCode, KernelPoolValidatingRetriever pool)
      throws KernelPoolValidationException {

    /*
     * First check to see if the keyword defining the offset epoch is present. If not then simply
     * return the standard implementation that does not adjust the input time at all.
     */
    String keyword = createKeyword(barycenterCode, "CONSTANTS_JED_EPOCH");

    if (pool.containsKeyword(keyword)) {
      return new JEDOffsetConverter(pool.getDouble(keyword));
    }

    return TimeConverter.NO_CONVERSION;
  }

  /**
   * Determine the reference frame to which the body fixed rotation is relative.
   * 
   * @param barycenterCode
   * @param pool
   * @return
   * 
   * @throws KernelPoolValidationException
   */
  int getReferenceFrame(int barycenterCode, KernelPoolValidatingRetriever pool)
      throws KernelPoolValidationException {

    /*
     * First check to see if the keyword defining the relative reference frame is present. If not
     * then simply return the code for J2000.
     */
    String keyword = createKeyword(barycenterCode, "CONSTANTS_REF_FRAME");

    if (pool.containsKeyword(keyword)) {
      return pool.getInteger(keyword);
    }

    return J2000_FRAME_CODE;

  }

  /**
   * Computes the appropriate barycenter for the identified body code.
   * <p>
   * This was taken directly from the logic in ZZBODBRY, the private SPICE routine of the same
   * purpose.
   * </p>
   * 
   * @param bodyCode a code of interest
   * 
   * @return the integer code of the corresponding barycenter
   */
  int computeBarycenter(int bodyCode) {

    if ((bodyCode >= 100) && (bodyCode <= 999)) {
      return bodyCode / 100;
    }
    if ((bodyCode >= 10000) && (bodyCode <= 99999)) {
      return bodyCode / 10000;
    }
    return bodyCode;

  }

  /**
   * Creates the appropriate PCK kernel pool keyword from the supplied code and stem.
   * 
   * @param idCode the id code of interest
   * @param keyword the string keyword to follow the &quot;BODY#_&quot; value.
   * 
   * @return the keyword associated with idCode for the value of interest.
   */
  String createKeyword(int idCode, String keyword) {
    return "BODY" + idCode + "_" + keyword;
  }

}
