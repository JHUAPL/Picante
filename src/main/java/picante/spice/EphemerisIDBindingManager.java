package picante.spice;

import java.util.HashMap;
import java.util.Map;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import picante.mechanics.EphemerisID;
import picante.spice.NameIDBindingList.NameIDPair;
import picante.spice.provided.EphemerisNames;

/**
 * Class designed to handle the mechanics of binding crucible <code>EphemerisID</code>s to SPICE
 * ephemeris object integer codes.
 */
class EphemerisIDBindingManager {

  private final NameIDBindingList<EphemerisID> bindingList;
  private final EphemerisNames builtInEphemerisNames;

  public EphemerisIDBindingManager(NameIDBindingList<EphemerisID> bindingList,
      EphemerisNames builtInEphemerisNames) {
    this.bindingList = bindingList;
    this.builtInEphemerisNames = builtInEphemerisNames;
  }

  /**
   * This class exists to allow the method that does all the heavy lifting to return both a list of
   * unused bindings and the binding map itself.
   */
  static class BindingResult {

    private final ImmutableMap<Integer, EphemerisID> map;
    private final ImmutableList<NameIDPair<EphemerisID>> unused;
    private final ImmutableMap<String, Integer> codeMap;

    public BindingResult(ImmutableMap<Integer, EphemerisID> map,
        ImmutableList<NameIDPair<EphemerisID>> unused, ImmutableMap<String, Integer> codeMap) {
      this.map = map;
      this.unused = unused;
      this.codeMap = codeMap;
    }

    public ImmutableMap<String, Integer> getEphemerisCodeMap() {
      return codeMap;
    }

    public ImmutableMap<Integer, EphemerisID> getMap() {
      return map;
    }

    public ImmutableList<NameIDPair<EphemerisID>> getUnused() {
      return unused;
    }

  }

  public void add(String name, EphemerisID id) {
    bindingList.add(name, id);
  }

  public void addAll(Map<String, ? extends EphemerisID> bindings) {
    bindingList.addAll(bindings);
  }

  public void remove(String name) {
    bindingList.remove(name);
  }

  public void removeAll(Iterable<String> names) {
    bindingList.removeAll(names);
  }

  public void clear() {
    bindingList.clear();
  }

  /**
   * Creates the string to integer ID code mapping with the built-in code mappings first, then those
   * provided from the kernel pool overlaid.
   * 
   * @param kernelDefined canonicalized name code mappings provided from the kernel pool
   * 
   * @return a newly created map of string to integer bindings as requested by the currently
   *         configured SPICE environment
   */
  private ImmutableMap<String, Integer> createEphemerisNameCodeMap(
      Map<String, Integer> kernelDefined) {

    /*
     * Build the SPICE string name to SPICE integer ephemeris ID code mapping from the default
     * built-in mappings. Overlay any of the bindings specified in the kernel pool next, as this is
     * how SPICE operates.
     */
    HashMap<String, Integer> result = Maps.newHashMap();
    result.putAll(builtInEphemerisNames.getMap());
    result.putAll(kernelDefined);
    return ImmutableMap.copyOf(result);
  }

  /**
   * Derive from the supplied kernel pool and the internal state of this class a map of bindings
   * between SPICE integer codes for ephemeris objects and crucible <code>EphemerisID</code>s.
   * <p>
   * By default the built-in codes are placed into the mapping first, but are permitted to be
   * overridden. However, individual bindings requested by the user, once made, must be unbound
   * prior to replacing. Otherwise this method will generate an exception.
   * </p>
   * 
   * @param kernelDefined a map of canonicalized names code mappings provided from the kernel pool
   * 
   * @return a binding result which contains the desired map and a list of all the unused bindings.
   * 
   * @throws BindingConflictException if there is an inconsistency in the user supplied bindings.
   *         This breaks down into two cases:
   *         <ul>
   *         <li>The user has attempted to supply an equivalent <code>EphemerisID</code> for two
   *         distinct integer codes</li>
   *         <li>The user has attempted to supply two distinct <code>EphemerisID</code> for the same
   *         integer code</li>
   *         </ul>
   */
  public BindingResult createEphemerisIDMap(Map<String, Integer> kernelDefined)
      throws BindingConflictException {

    /*
     * This is a complicated process, primarily because users would rather specify bindings to the
     * SPICE names for ephemeris objects than their integer codes. Since SPICE permits multiple
     * string names to map to a single integer code, we are stuck validating that any strings a user
     * attempts to bind will result in a consistent integer code to crucible ephemeris ID mapping.
     * There are two situations when this would fail to be the case:
     * 
     * The user attempts to bind the same, or equivalent, ephemeris IDs to multiple distinct integer
     * ID codes.
     * 
     * The user attempts to bind distinct, or non-equivalent, ephemeris IDs to the same integer ID
     * code.
     * 
     * Both of these translate to the need for the key and value space of the desired map to be
     * unique (as defined by equals()). Fortunately the guava BiMap interface provides a mechanism
     * for this directly.
     */
    ImmutableMap<String, Integer> nameCodeMap = createEphemerisNameCodeMap(kernelDefined);
    BiMap<Integer, EphemerisID> userSuppliedBindings = HashBiMap.create();

    ImmutableList.Builder<NameIDPair<EphemerisID>> unusedBuilder = ImmutableList.builder();

    /*
     * For performance reasons the try block for the failed insertion into the user supplied BiMap
     * has been pulled outside of the for loop. This is really a mess because of all the possible
     * things that might go wrong and need to be trapped.
     */
    EphemerisID boundID = null;
    EphemerisID idToBeBound = null;
    String nameToBeBound = null;
    int code = 0;

    try {

      for (NameIDPair<EphemerisID> pair : bindingList) {

        idToBeBound = pair.getID();
        nameToBeBound = pair.getName();

        /*
         * First verify that the nameToBeBound has a corresponding ID code. If it does not then add
         * it to the unused list.
         */
        if (!nameCodeMap.containsKey(nameToBeBound)) {
          unusedBuilder.add(pair);
        } else {

          code = nameCodeMap.get(nameToBeBound);

          /*
           * Determine if a user supplied binding already exists for this code. If it does, then
           * ascertain whether it is equivalent to the supplied binding. Otherwise throw an
           * appropriate exception.
           */
          if (userSuppliedBindings.containsKey(code)) {
            boundID = userSuppliedBindings.get(code);
            if (!idToBeBound.equals(boundID)) {
              throw new BindingConflictException.DuplicateEphemerisCode(boundID, code, idToBeBound,
                  nameToBeBound);
            }

          }

          boundID = userSuppliedBindings.put(code, pair.getID());
        }

      }

    } catch (IllegalArgumentException e) {
      int conflictingCode = userSuppliedBindings.inverse().get(idToBeBound);
      boundID = userSuppliedBindings.get(conflictingCode);
      throw new BindingConflictException.DuplicateEphemerisID(boundID, conflictingCode, idToBeBound,
          nameToBeBound, e);
    }

    /*
     * Now all that remains is to merge the two maps and configure the BindingResult object for
     * return purposes.
     */
    Map<Integer, EphemerisID> data = Maps.newHashMap();
    data.putAll(builtInEphemerisNames.getStandardBindings());
    data.putAll(userSuppliedBindings);

    ImmutableBiMap.Builder<Integer, EphemerisID> builder =
        new ImmutableBiMap.Builder<Integer, EphemerisID>();
    builder.putAll(data);

    /*
     * Trap the case where a user has attempted to reuse a built-in code.
     */
    try {
      return new BindingResult(builder.build(), unusedBuilder.build(), nameCodeMap);
    } catch (IllegalArgumentException e) {
      throw new BindingConflictException.DuplicateEphemerisID(e);
    }

  }
}
