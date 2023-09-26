package picante.spice.kernelpool.content;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import picante.spice.Utilities;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameKernel;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.fk.TKFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrameFunction;
import picante.spice.kernelpool.UnwritableKernelPool;

/**
 * Class used to instantiate frame kernel content.
 * <p>
 * <b>Note:</b>This class does not, <i>yet</i>, support frames defined with keywords using the frame
 * name in the definition, rather than the frame ID code.
 * </p>
 */
public class FKFactory {

  private final KeywordFormatter formatter = new KeywordFormatter();

  /**
   * Matcher used to identify keywords in the kernel pool that define the name of a frame with a
   * particular ID code. NAIF uses this keyword to locate frames defined for a specific ID code, so
   * this is a reasonable place to start.
   */
  private final Matcher frameNameMatcher = Pattern.compile("FRAME_(-*\\d+)_NAME").matcher("");

  private final Matcher frameMatcher = Pattern.compile("FRAME_(.*)").matcher("");

  private final Matcher sclkIDMatcher = Pattern.compile("CK_(-*\\d+)_SCLK").matcher("");

  /**
   * Creates a frame kernel from the supplied kernel pool. Since this system is reading keyword
   * value assignments from the kernel pool, it is not possible for there to be a conflict where a
   * frame ID code is "re-used" in the {@link FrameInfo} class stored as values in the frame
   * definitions map supplied to the {@link FrameKernel} constructor.
   * 
   * @param pool the pool containing frame kernel content.
   * 
   * @param ephemerisCodeMap the mapping of all known ephemeris names to their corresponding integer
   *        ID codes
   * @param builtInFrames the mapping of all built in frame ID codes to their corresponding frame
   *        information
   * 
   * @return a newly created frame kernel.
   * 
   * @throws FKInstantiationException if the contents of pool violate any of the frame kernel
   *         content conventions defined by SPICE.
   */
  public FrameKernel create(UnwritableKernelPool pool, Map<String, Integer> ephemerisCodeMap,
      Map<Integer, FrameInfo> builtInFrames) throws FKInstantiationException {
    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);
    ImmutableBiMap<Integer, FrameInfo> frames = createFrameDefinitions(retriever, ephemerisCodeMap);
    ImmutableMap<String, Integer> frameCodeMap = createNameBindings(retriever);
    /*
     * Loop over the builtInFrames. Locate any TK class frames, determine if the kernel pool has any
     * definitions for them, and add those to the list.
     */
    BiMap<Integer, FrameInfo> builtIns = HashBiMap.create();
    for (Integer code : builtInFrames.keySet()) {
      FrameInfo info = builtInFrames.get(code);
      if (TKFrameProviders.isFunctionPresent(builtInFrames.get(code), retriever)) {
        builtIns.put(code, info);
      }
    }

    /*
     * Create a frameCodeMap that contains the built-in frame name codes. This is necessary so that
     * any of the built-in name to integer bindings are properly accounted for in the system. Note:
     * the built-in bindings take precedence over those supplied in the frame kernel content.
     */
    Map<String, Integer> codes = Maps.newHashMap();

    /*
     * Load any kernel defined bindings first, then follow up with the built-in ones, as they take
     * precedence.
     */
    codes.putAll(frameCodeMap);
    for (int i : builtInFrames.keySet()) {
      codes.put(builtInFrames.get(i).getName(), i);
    }
    /*
     * TODO: This could result in the same TKFRAM definition ending up as two distinct functions, if
     * a built-in frame has meta-data loaded for it.
     */
    ImmutableList<TKFrameFunction> kernelProvided =
        createTKFrameFunctions(frames, ImmutableMap.copyOf(codes), retriever);
    ImmutableList<TKFrameFunction> builtInData =
        createBuiltInTKFrameFunctions(builtIns, ImmutableMap.copyOf(codes), retriever);

    /*
     * 1) created an immutableList of DynamicFrameFunctions, passed it into FrameKernel constructor
     * 
     * 2) SpiceInfoHolder is simply a holder with the frame and ephemeris map holders required for
     * dynamic frame construction
     * 
     */
    SpiceInfoHolder infoHolder = new SpiceInfoHolder(frames, retriever, ImmutableMap.copyOf(codes),
        ImmutableMap.copyOf(ephemerisCodeMap));
    ImmutableList<DynamicFrameFunction> dynamicFramesFunctions =
        createDynamicFrameFunctions(infoHolder);
    return new FrameKernel(frames, frameCodeMap, createSCLKMap(retriever),
        ImmutableList.copyOf(Iterables.concat(builtInData, kernelProvided)),
        dynamicFramesFunctions);
  }


  ImmutableList<DynamicFrameFunction> createDynamicFrameFunctions(SpiceInfoHolder infoHolder)
      throws FKInstantiationException {
    ImmutableList.Builder<DynamicFrameFunction> builder = ImmutableList.builder();
    try {
      for (FrameInfo info : infoHolder.getFrames().values()) {
        if (info.getType().equals(FrameType.DYNAMIC)) {
          DynamicFrameFunction dynFrameFunc = DynamicFrameProvider.createFunction(info, infoHolder);
          if (dynFrameFunc != null) {
            builder.add(dynFrameFunc);
          }
        }
      }
      return builder.build();

    } catch (KernelPoolValidationException e) {
      throw new FKInstantiationException("Unable to instantiate frame.", e);
    }
  }


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
   * Creates {@link TKFrameFunction}s for each of the {@link FrameType#TK} frame types present in
   * the supplied frames map.
   * <p>
   * This method deviates from NAIF's standard implementation of TKFRAM. If a frame is defined with
   * a non-canonicalized SPICE frame name, then SPICE will likely allow it in the case of evaluating
   * these frames. However, this implementation does not permit it.
   * </p>
   * 
   * 
   * @param frames
   * @param retriever
   * @return
   * 
   * @throws FKInstantiationException if any of the frames defined in the supplied map have missing
   *         or incomplete content.
   */
  @VisibleForTesting
  ImmutableList<TKFrameFunction> createTKFrameFunctions(BiMap<Integer, FrameInfo> frames,
      Map<String, Integer> frameCodeMap, KernelPoolValidatingRetriever retriever)
      throws FKInstantiationException {

    ImmutableList.Builder<TKFrameFunction> builder = ImmutableList.builder();

    try {

      /*
       * Due to the way TKFRAMEs are defined, there must be a frame entry in the frames map for each
       * frame.
       */
      for (FrameInfo info : frames.values()) {

        if (info.getType().equals(FrameType.TK)) {
          builder.add(TKFrameProviders.createFunction(info, retriever, frameCodeMap));
        }

      }

      return builder.build();

    } catch (KernelPoolValidationException e) {
      throw new FKInstantiationException("Unable to instantiate frame.", e);
    }
  }

  @VisibleForTesting
  ImmutableList<TKFrameFunction> createBuiltInTKFrameFunctions(BiMap<Integer, FrameInfo> frames,
      Map<String, Integer> frameCodeMap, KernelPoolValidatingRetriever retriever)
      throws FKInstantiationException {

    ImmutableList.Builder<TKFrameFunction> builder = ImmutableList.builder();

    try {

      /*
       * Due to the way TKFRAMEs are defined, there must be a frame entry in the frames map for each
       * frame.
       */
      for (FrameInfo info : frames.values()) {

        if (info.getType().equals(FrameType.TK)) {
          builder.add(TKFrameProviders.createBuiltInFunction(info, retriever, frameCodeMap));
        }

      }

      return builder.build();

    } catch (KernelPoolValidationException e) {
      throw new FKInstantiationException("Unable to instantiate frame.", e);
    }
  }

  /**
   * Creates a mapping from CK integer ID codes to the SCLK integer ID codes specified by the frame
   * kernel content in the supplied pool.
   * 
   * @param pool a kernel pool with potential frame kernel content loaded
   * 
   * @return a newly created map of CK integer ID codes (key) to SCLK integer ID codes (values)
   * 
   * @throws FKInstantiationException if anything goes wrong with the retrieval of the kernel pool
   *         content describing the binding.
   */
  @VisibleForTesting
  ImmutableMap<Integer, Integer> createSCLKMap(KernelPoolValidatingRetriever retriever)
      throws FKInstantiationException {

    ImmutableMap.Builder<Integer, Integer> builder = ImmutableMap.builder();

    try {

      for (String keyword : retriever.getPool().getKeywords()) {

        sclkIDMatcher.reset(keyword);

        if (sclkIDMatcher.matches()) {
          /*
           * Note, the number format exception should not be generated if the regular expression
           * that was used to build this matcher is working properly.
           */
          builder.put(Integer.parseInt(sclkIDMatcher.group(1)), retriever.getInteger(keyword));
        }

      }
    } catch (KernelPoolValidationException e) {
      throw new FKInstantiationException("Unable to create proper CK binding to SCLK content.", e);
    }

    return builder.build();
  }

  /**
   * Creates a map from integer's in the frame code space to the frame meta data for all frames
   * defined in the pool associated with the retriever.
   * 
   * @param retriever
   * @return
   * @throws FKInstantiationException if the contents of the frame meta data supplied in the kernel
   *         pool are not all present.
   */
  @VisibleForTesting
  ImmutableBiMap<Integer, FrameInfo> createFrameDefinitions(KernelPoolValidatingRetriever retriever,
      Map<String, Integer> ephemerisCodeMap) throws FKInstantiationException {

    /*
     * We are using BiMap because the mapping between integers and frame information is necessarily
     * 1:1 and onto.
     */
    ImmutableBiMap.Builder<Integer, FrameInfo> builder = ImmutableBiMap.builder();

    for (String keyword : retriever.getPool().getKeywords()) {

      if (frameNameMatcher.reset(keyword).matches()) {
        int frameCode = Integer.parseInt(frameNameMatcher.group(1));
        FrameInfo info = createFrameInfo(frameCode, retriever, ephemerisCodeMap);
        builder.put(frameCode, info);
      }

    }

    return builder.build();

  }

  @VisibleForTesting
  ImmutableMap<String, Integer> createNameBindings(KernelPoolValidatingRetriever retriever) {

    ImmutableMap.Builder<String, Integer> builder = ImmutableMap.builder();

    for (String keyword : retriever.getPool().getKeywords()) {

      if (frameMatcher.reset(keyword).matches()) {

        /*
         * SPICE searches for keywords of the form described by frameMatcher (FRAME_#), where #
         * needs to be upper case only. If it were lower case it gets converted to upper case prior
         * to the query by NAMFRM. So we don't have to worry about any keywords with lower case
         * values.
         * 
         * If the keyword is all upper case and associated with a single integer value, then add its
         * canonicalized name to the map.
         */
        String name = frameMatcher.group(1);
        if (retriever.getPool().isIntegerValued(keyword) && name.toUpperCase().equals(name)) {
          List<Integer> values = retriever.getPool().getIntegers(keyword);
          if (values.size() == 1) {
            builder.put(Utilities.canonicalizeSpiceName(frameMatcher.group(1)), values.get(0));
          }
        }
      }
    }

    return builder.build();

  }

  /**
   * Creates a name
   * 
   * @param id
   * @param retriever
   * @return
   * @throws FKInstantiationException
   */
  @VisibleForTesting
  FrameInfo createFrameInfo(int id, KernelPoolValidatingRetriever retriever,
      Map<String, Integer> ephemerisCodeMap) throws FKInstantiationException {

    /*
     * Each frame requires four separate keywords to be defined in the kernel pool. Check to make
     * certain they are all present, and extract the required information from them. Note: We are
     * explicitly ignoring the FRAME_%s keywords where %s maps to the name stored in the
     * FRAME_%d_NAME keyword's value.
     * 
     * Note: this process is further complicated by the fact that keywords maybe associated with the
     * reported name of the frame.
     */
    try {
      String name = retriever.getString(formatter.format("FRAME_%d_NAME", id));

      int frameClass = getSingleIntegerFrameValue(id, name, "CLASS", retriever);
      int frameClassID = getSingleIntegerFrameValue(id, name, "CLASS_ID", retriever);
      int center = getCenterCode(id, name, retriever, ephemerisCodeMap);

      return new FrameInfo(name, id, FrameType.getTypeForClassInteger(frameClass), frameClassID,
          center);

    } catch (KernelPoolValidationException e) {
      throw new FKInstantiationException("Unable to instantiate frame associated with ID: " + id,
          e);
    } catch (NumberFormatException e) {
      throw new FKInstantiationException("Unable to instantiate frame associated with ID: " + id
          + ". There has been a parse failure.", e);
    }

  }

  @VisibleForTesting
  int getCenterCode(int id, String name, KernelPoolValidatingRetriever retriever,
      Map<String, Integer> ephemerisCodeMap) throws KernelPoolValidationException {

    /*
     * There are two possible keywords that can retain the central body for the frame definition.
     * Prefer the ID code based one first. There are two possible data value scenarios: one is
     * integer based the other is a string.
     */
    String keyword = formatter.format("FRAME_%d_CENTER", id);

    /*
     * If the integer ID code version, preferred, is not present, then assume the name version.
     */
    if (!retriever.getPool().hasKeyword(keyword)) {
      keyword = formatter.format("FRAME_%s_CENTER", name);
    }

    if (retriever.getPool().isIntegerValued(keyword)) {
      return retriever.getInteger(keyword);
    }
    if (retriever.getPool().isStringValued(keyword)) {
      return getBodyCode(ephemerisCodeMap, retriever.getString(keyword));
    }

    throw new KernelPoolValidationException(
        "Unable to locate FRAME_#_CENTER code keyword for frame with: " + id + " or " + name);
  }

  /**
   * Retrieves the ephemeris ID code for the supplied kernel pool, string value.
   * 
   * @param ephemerisCodeMap the mapping of ephemeris objects to their integer codes.
   * 
   * @param kernelPoolValue the string value retrieved from the kernel pool.
   * 
   * @return an integer code for the ephemeris object
   * 
   * @throws NumberFormatException if kernelPoolValue, in canonical form, is not contained in
   *         ephemerisCodeMap and is not parseable as an integer.
   */
  @VisibleForTesting
  int getBodyCode(Map<String, Integer> ephemerisCodeMap, String kernelPoolValue) {

    /*
     * First consult the map to see if the string is present.
     */
    String name = Utilities.canonicalizeSpiceName(kernelPoolValue);
    if (ephemerisCodeMap.containsKey(name)) {
      return ephemerisCodeMap.get(name);
    }

    /*
     * Try to parse name as an integer and return it.
     */
    return Integer.parseInt(name);
  }

  /**
   * Retrieves
   * 
   * @param id
   * @param name
   * @param variable
   * @param retriever
   * @return
   * @throws KernelPoolValidationException
   */
  @VisibleForTesting
  int getSingleIntegerFrameValue(int id, String name, String variable,
      KernelPoolValidatingRetriever retriever) throws KernelPoolValidationException {

    /*
     * First, see if the ID based keyword exists in the pool.
     */
    String keyword = formatter.format("FRAME_%d_%s", id, variable);
    if (retriever.getPool().hasKeyword(keyword)) {
      return retriever.getInteger(keyword);
    }

    /*
     * If it is absent, then try the name based keyword.
     */
    keyword = formatter.format("FRAME_%s_%s", name, variable);
    if (retriever.getPool().hasKeyword(keyword)) {
      return retriever.getInteger(keyword);
    }

    throw new KernelPoolValidationException("Unable to locate FRAME_#_" + variable
        + " keyword content in the kernel pool for: " + id + " or " + name);

  }
}
