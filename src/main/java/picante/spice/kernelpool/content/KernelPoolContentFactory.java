package picante.spice.kernelpool.content;

import java.util.List;
import java.util.Map;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import picante.spice.Utilities;
import picante.spice.kernelpool.UnwritableKernelPool;

/**
 * Factory class capturing kernel pool content instantiation code that is not tied to a
 * &quot;specific&quot; kernel type in the SPICE system.
 */
public class KernelPoolContentFactory {


  /**
   * Creates a map of ephemeris object names to their SPICE integer codes as presented in the
   * supplied pool.
   * <p>
   * Note: this does <b>not</b> contain the built in codes provided by SPICE. These are to be
   * overlaid on top of those codes.
   * </p>
   * 
   * @param pool the kernel pool from which to extract the string code mapping
   * 
   * @return a newly created map connecting strings to their SPICE integer codes. In the event that
   *         there are no mappings present in the pool, this returns an empty map.
   * 
   * @throws TextKernelContentInstantiationException if creating this map fails for any reason.
   */
  public ImmutableMap<String, Integer> createEphemerisIDMap(UnwritableKernelPool pool)
      throws TextKernelContentInstantiationException {

    ImmutableMap.Builder<String, Integer> builder = ImmutableMap.builder();

    List<String> strings = pool.getStrings("NAIF_BODY_NAME");
    List<Integer> codes = pool.getIntegers("NAIF_BODY_CODE");

    /*
     * If both keywords are absent, then return an empty map.
     */
    if ((strings == null) && (codes == null)) {
      return builder.build();
    }

    /*
     * Create a map to receive all the bindings. This is necessary as we permit duplicate bindings
     * to occur, and the Guava builder does not.
     */
    Map<String, Integer> map = Maps.newHashMap();

    /*
     * If either one or the other is null, then throw an exception, because a null entry is
     * analogous to an empty list.
     */
    if (strings == null) {
      throw new TextKernelContentInstantiationException(
          "The NAIF_BODY_NAME keyword is absent from the kernel pool,"
              + " but the NAIF_BODY_CODE keyword contains content.");
    }

    if (codes == null) {
      throw new TextKernelContentInstantiationException(
          "The NAIF_BODY_CODE keyword is absent from the kernel pool,"
              + " but the NAIF_BODY_NAME keyword contains content.");
    }

    if (strings.size() != codes.size()) {
      throw new TextKernelContentInstantiationException(
          "The length of the NAIF_BODY_CODE content in the kernel pool is: " + codes.size()
              + " , but the NAIF_BODY_NAME content is of length: " + strings.size()
              + ".  These must be precisely the same length.");
    }

    /*
     * Iterate forwards through the list, because we are populating a map and the last stuff in the
     * list takes precedence.
     */
    for (int i = 0; i < strings.size(); i++) {
      map.put(canonicalizeEphemerisName(strings.get(i)), codes.get(i));
    }

    builder.putAll(map);
    return builder.build();
  }

  /**
   * This method uses the standard utility method to canonicalize the input name. However, it
   * additionally performs a check for the presence of invalid tab characters.
   * 
   * @param name
   * @return
   * @throws TextKernelContentInstantiationException
   */
  private String canonicalizeEphemerisName(String name)
      throws TextKernelContentInstantiationException {
    if (name.contains("\t")) {
      throw new TextKernelContentInstantiationException("The name [" + name
          + "] specified in NAIF_BODY_NAME in" + " the kernel pool failed canonicalization.  "
          + "It contains a tab character which is invalid.");
    }

    return Utilities.canonicalizeSpiceName(name);
  }
}
