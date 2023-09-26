package picante.spice.kernelpool.content;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Function;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.rotations.EulerAngles;
import picante.mechanics.rotations.Quaternion;
import picante.spice.Utilities;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.fk.TKFrameFunction;

/**
 * Class used to define the various specifications for {@link FrameType#TK} frames.
 */
enum TKFrameProviders {

  /**
   * Defines the code to instantiate the matrix based fixed offset frame definition from the kernel
   * pool.
   * <p>
   * This code deviates from the NAIF standard specification in two specific ways:
   * <ul>
   * <li>If the matrix supplied in the kernel pool deviates sufficiently from a rotation, this will
   * fail to bootstrap the instance.</li>
   * <li>If the matrix supplied in the pool is left-handed, this will also fail.</li>
   * </ul>
   */
  MATRIX {

    @Override
    RotationMatrixIJK getMatrix(int id, String name, KernelPoolValidatingRetriever retriever)
        throws KernelPoolValidationException {

      /*
       * Prefer the name version of the keyword first.
       */
      String keyword = getFrameKeyword("TKFRAME_%s_MATRIX", id, name, retriever);
      List<Double> matrix = retriever.getDoublesExpectedLength(keyword, 9);

      /*
       * Create and sharpen the matrix. If the input components are a left-handed matrix an
       * IllegalArgumentException will be generated.
       */
      RotationMatrixIJK result;

      try {
        result = RotationMatrixIJK.createSharpened(matrix.get(0), matrix.get(1), matrix.get(2),
            matrix.get(3), matrix.get(4), matrix.get(5), matrix.get(6), matrix.get(7),
            matrix.get(8));
      } catch (IllegalArgumentException e) {
        /*
         * If this happens, then the matrix was likely left-handed or not close enough to a rotation
         * to pass for one. At the moment we do not support left-handed matrices in our TKFRAME
         * implementation. This is a deviation from the NAIF standard.
         */
        throw new KernelPoolValidationException(keyword,
            "Contents are not sufficiently close to a rotation after sharpening: "
                + e.getMessage());
      }

      return result.sharpen();
    }

  },

  /**
   * Defines the code to instantiate the Euler angle decomposition definition of a fixed offset
   * frame.
   */
  ANGLES {

    @Override
    RotationMatrixIJK getMatrix(int id, String name, KernelPoolValidatingRetriever retriever)
        throws KernelPoolValidationException {

      /*
       * Two of the three keywords must be present, TKFRAME_#_ANGLES and TKFRAME_#_AXES. If absent,
       * the inferred units are radians, but a TKFRAME_#_UNITS keyword may be present. As with the
       * other keywords, prefer the named version of the keyword to the ID code one.
       */
      List<Double> angles = retriever
          .getDoublesExpectedLength(getFrameKeyword("TKFRAME_%s_ANGLES", id, name, retriever), 3);
      String axesKeyword = getFrameKeyword("TKFRAME_%s_AXES", id, name, retriever);
      List<Integer> axes = retriever.getIntegersExpectedLength(axesKeyword, ACCEPTABLE_AXES, 3);

      /*
       * Check to see if the units on angles need to be converted. This only happens if the UNITS
       * keyword is defined as a string value.
       */
      String unitsKeyword = getFrameKeyword("TKFRAME_%s_UNITS", id, name, retriever);
      String units = "RADIANS";
      if (retriever.getPool().isStringValued(unitsKeyword)) {
        units = retriever.getPool().getStrings(unitsKeyword).get(0);
      }

      /*
       * Verify that units refers to a supported converter.
       */
      String canonicalUnits = units.toUpperCase();
      if (!AngleUnits.ACCEPTABLE_ANGLE_UNITS.contains(canonicalUnits)) {
        throw new KernelPoolValidationException(unitsKeyword,
            "The angle units specified in the first element of the values: " + units
                + " is not in the list of supported units.");
      }

      AngleUnits converter = AngleUnits.valueOf(units.toUpperCase());

      /*
       * Work around the fact that EUL2M handles cases where the neighboring axes in the Euler
       * sequence are equal. Copy the lists from the kernel pool, as we are going to modify them if
       * the axes are equal.
       */
      axes = Lists.newArrayList(axes);
      angles = Lists.newArrayList(angles);

      /*
       * If any of the axes are equal, then we have to make some simple adjustments to accomodate
       * this situation. To emulate what EUL2M does in SPICELIB, we're just going to collapse the
       * equal angles to the middle axes and set the offending (equal) outer axis to a valid value
       * with a 0 angle.
       */
      if (axes.get(0).equals(axes.get(1))) {
        axes.set(0, EULER_AXIS_CYCLE[axes.get(0)]);
        angles.set(1, angles.get(1) + angles.get(0));
        angles.set(0, 0.0);
      }

      if (axes.get(1).equals(axes.get(2))) {
        axes.set(2, EULER_AXIS_CYCLE[axes.get(2)]);
        angles.set(1, angles.get(1) + angles.get(2));
        angles.set(2, 0.0);
      }

      /*
       * Retrieve the appropriate Euler angle decomposition.
       */
      EulerAngles euler = EulerAnglesFactory.INSTANCE.createAngles(axesKeyword, axes.get(0),
          axes.get(1), axes.get(2));

      euler.set(converter.convertToRadians(angles.get(0)),
          converter.convertToRadians(angles.get(1)), converter.convertToRadians(angles.get(2)));

      return euler.getRotation(new RotationMatrixIJK());
    }

  },

  /**
   * Defines the code to instantiate the quaternion (SPICE style) based fixed offset frame.
   */
  QUATERNION {

    @Override
    RotationMatrixIJK getMatrix(int id, String name, KernelPoolValidatingRetriever retriever)
        throws KernelPoolValidationException {

      /*
       * Prefer the name version of the keyword first.
       */
      List<Double> quaternion = retriever
          .getDoublesExpectedLength(getFrameKeyword("TKFRAME_%s_Q", id, name, retriever), 4);

      Quaternion q = new Quaternion(quaternion.get(0), quaternion.get(1), quaternion.get(2),
          quaternion.get(3));

      return q.getRotation(new RotationMatrixIJK());
    }

  };

  /**
   * Set used to capture all of the string based names for each element of the enumeration.
   */
  static final ImmutableSet<String> ACCEPTABLE_PROVIDERS = ImmutableSet.copyOf(Iterators
      .transform(Arrays.asList(values()).iterator(), new Function<TKFrameProviders, String>() {
        @Override
        public String apply(TKFrameProviders input) {
          return input.name();
        }
      }));

  /**
   * Simple array for Euler angles to select a &quot;safe&quot; axis in the event two neighboring
   * axes are equal. This is used to move the outer axes (if they are equal to the middle one) to
   * valid values that will be assigned 0 angles.
   */
  private final static int[] EULER_AXIS_CYCLE = {-1, 2, 3, 1};

  /**
   * The set of acceptable TKFRAME_#_AXES values.
   */
  private static final ImmutableSet<Integer> ACCEPTABLE_AXES = ImmutableSet.of(1, 2, 3);

  /**
   * Abstract method provided by each instance of the the enum used to create the desired rotation
   * matrix.
   * 
   * @param id the integer code for the TKFRAME
   * @param name the name of the TKFRAME (from FRAME_#_NAME, not canonicalized).
   * @param retriever the kernel pool retriever from which to derive content
   * 
   * @return a newly created rotation matrix capturing the rotation
   * 
   * @throws KernelPoolValidationException if extracting any of the contents of from the pool fails
   *         for any reason.
   */
  abstract RotationMatrixIJK getMatrix(int id, String name, KernelPoolValidatingRetriever retriever)
      throws KernelPoolValidationException;

  /**
   * Retrieves the name of the keyword that should hold the data content of interest.
   * <p>
   * This method prefers the name based specification of the keyword to the ID code one.
   * </p>
   * 
   * @param keywordTemplate a template, with a single %s used to indicate the placement of the ID or
   *        name of the frame
   * @param id the integer ID for the frame
   * @param name the non-canonical form of the frame name from FRAME_#_NAME
   * @param retriever the retriever to derive content from
   * 
   * @return a String capturing the keyword where the data content should be stored.
   */
  @VisibleForTesting
  static String getFrameKeyword(String keywordTemplate, int id, String name,
      KernelPoolValidatingRetriever retriever) {

    /*
     * Prefer the name version of the keyword to the ID code one.
     */
    String result = String.format(keywordTemplate, name);
    if (!retriever.containsKeyword(result)) {
      keywordTemplate = String.format(keywordTemplate, "%s");
      result = String.format(keywordTemplate, id);
    }

    return result;
  }

  /**
   * Creates a frame function for the fixed offset frame of interest.
   * <p>
   * This method <b>does not</b> perform any checks to validate that the supplied {@link FrameInfo}
   * argument, info, meets its expectations as that is handled elsewhere in the package.
   * </p>
   * 
   * @param info the FrameInfo describing the fixed offset frame of type {@link FrameType#TK}.
   * 
   * @param retriever the kernel pool retriever to derive content
   * 
   * @param frameCodeMap a mapping from frame names to their corresponding integer codes (actual
   *        frame codes, not class specific codes)
   * 
   * @return a newly created TKFrameFunction
   * 
   * @throws KernelPoolValidationException if any of the requirements on the keywords in the frame
   *         definition are not met
   * 
   * @throws FKInstantiationException if the relative frame defined in the TKFRAME_#_RELATIVE
   *         keyword has a value that is not present in the supplied frameCodeMap or if the
   *         specification defined in the TKFRAME_#_SPEC does not map to an element of this
   *         enumeration
   */
  static TKFrameFunction createFunction(FrameInfo info, KernelPoolValidatingRetriever retriever,
      Map<String, Integer> frameCodeMap)
      throws KernelPoolValidationException, FKInstantiationException {

    /*
     * Begin by retrieving the name recorded in the kernel pool. This is necessary as the name
     * referenced in the info instance has been canonicalized.
     */
    String name = retriever.getString(String.format("FRAME_%d_NAME", info.getCode()));

    int relative = getRelativeFrameCode(info.getCode(), name, retriever, frameCodeMap);

    TKFrameProviders provider = getProvider(info.getCode(), name, retriever);

    return new TKFrameFunction(info.getCode(), relative,
        provider.getMatrix(info.getCode(), name, retriever));
  }

  static TKFrameFunction createBuiltInFunction(FrameInfo info,
      KernelPoolValidatingRetriever retriever, Map<String, Integer> frameCodeMap)
      throws KernelPoolValidationException, FKInstantiationException {

    String name = info.getName();

    int relative = getRelativeFrameCode(info.getCode(), name, retriever, frameCodeMap);

    TKFrameProviders provider = getProvider(info.getCode(), name, retriever);

    return new TKFrameFunction(info.getCode(), relative,
        provider.getMatrix(info.getCode(), name, retriever));
  }


  static boolean isFunctionPresent(FrameInfo info, KernelPoolValidatingRetriever retriever) {

    String keyword =
        getFrameKeyword("TKFRAME_%s_RELATIVE", info.getClassID(), info.getName(), retriever);

    return retriever.containsKeyword(keyword);

  }

  /**
   * Retrieves the integer ID code for the relative frame portion of the TKFRAME definition.
   * 
   * @param id the TKFRAME id
   * @param name the name of the TKFRAME, not canonicalized from FRAME_#_NAME
   * @param retriever the retriever with the content
   * @param frameCodeMap a mapping from frame names to their integer codes
   * 
   * @return the integer code for the relative frame
   * 
   * @throws KernelPoolValidationException if the kernel pool content is not as expected
   * 
   * @throws FKInstantiationException if the name of the frame to which this is defined relative is
   *         not present in frameCodeMap
   */
  @VisibleForTesting
  static int getRelativeFrameCode(int id, String name, KernelPoolValidatingRetriever retriever,
      Map<String, Integer> frameCodeMap)
      throws KernelPoolValidationException, FKInstantiationException {

    String keyword = getFrameKeyword("TKFRAME_%s_RELATIVE", id, name, retriever);

    String value = retriever.getString(keyword);
    value = Utilities.canonicalizeSpiceName(value);
    if (!frameCodeMap.containsKey(value)) {
      throw new FKInstantiationException("Unable to locate frame for which " + id + ", " + name
          + " is defined relative to: " + value);
    }

    return frameCodeMap.get(value);
  }

  /**
   * Retrieves the appropriate provider as described in the TKFRAME_#_SPEC value.
   * 
   * @param id the TKFRAME id
   * @param name the name of the TKFRAME, not canonicalized from FRAME_#_NAME
   * @param retriever the retriever with the content
   * 
   * @return the provider for the specification
   * 
   * @throws KernelPoolValidationException if the kernel pool content is not as expected
   * 
   * @throws FKInstantiationException if the specification does not refer to one of the entries in
   *         this enum
   */
  @VisibleForTesting
  static TKFrameProviders getProvider(int id, String name, KernelPoolValidatingRetriever retriever)
      throws KernelPoolValidationException, FKInstantiationException {

    /*
     * Prefer the name based version, this is the opposite of the general frame meta-data.
     */
    String keyword = getFrameKeyword("TKFRAME_%s_SPEC", id, name, retriever);
    String value = retriever.getString(keyword);
    String canonicalValue = value.toUpperCase();

    /*
     * Check to see if value lies in the supported list of providers.
     */
    if (!ACCEPTABLE_PROVIDERS.contains(canonicalValue)) {
      throw new FKInstantiationException("Unsupported TKFRAME specification: " + value);
    }
    return TKFrameProviders.valueOf(canonicalValue);
  }

}
