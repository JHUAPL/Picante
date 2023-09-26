package picante.spice;

import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.Maps;
import picante.mechanics.FrameID;
import picante.spice.NameIDBindingList.NameIDPair;
import picante.spice.adapters.SpiceFrameID;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.provided.FrameNames;

/**
 * Class designed to handle the mechanics of binding crucible <code>FrameID</code>s to SPICE frame
 * integer codes.
 * 
 * <p>
 * There are a few specific cases in the SPICE system that are not properly addressed here:
 * </p>
 * <ul>
 * <li>CIDFRM automagically maps 399 to IAU_EARTH, unless something in the kernels overrides
 * it.</li>
 * <li>FRINFO computes values for the various DSN station frames, even if data for them is not
 * supplied in the kernel pool.</li>
 * </ul>
 */
class FrameIDBindingManager {

  private final NameIDBindingList<FrameID> bindingList;
  private final FrameNames builtInFrameNames;

  public FrameIDBindingManager(NameIDBindingList<FrameID> bindingList,
      FrameNames builtInFrameNames) {
    this.bindingList = bindingList;
    this.builtInFrameNames = builtInFrameNames;
  }

  static class BindingResult {

    private final ImmutableMap<Integer, FrameID> frameCodeMap;
    private final ImmutableMap<FrameType, ImmutableSetMultimap<Integer, FrameID>> classCodeMap;
    private final ImmutableList<NameIDPair<FrameID>> unused;
    private final ImmutableMap<FrameID, Integer> frameCenterMap;

    public BindingResult(ImmutableMap<Integer, FrameID> frameCodeMap,
        ImmutableMap<FrameType, ImmutableSetMultimap<Integer, FrameID>> classCodeMap,
        ImmutableList<NameIDPair<FrameID>> unused, ImmutableMap<FrameID, Integer> frameCenterMap) {
      Preconditions.checkArgument(
          classCodeMap.keySet().containsAll(Arrays.asList(FrameType.values())),
          "All frame types not represented in the class code map.");
      this.frameCodeMap = frameCodeMap;
      this.classCodeMap = classCodeMap;
      this.frameCenterMap = frameCenterMap;
      this.unused = unused;
    }

    public ImmutableMap<Integer, FrameID> getFrameCodeMap() {
      return frameCodeMap;
    }

    public ImmutableSetMultimap<Integer, FrameID> getClassCodeMapForType(FrameType type) {
      return classCodeMap.get(type);
    }

    public ImmutableList<NameIDPair<FrameID>> getUnused() {
      return unused;
    }

    public ImmutableMap<FrameID, Integer> getFrameCenterMap() {
      return frameCenterMap;
    }

  }

  public void add(String name, FrameID id) {
    bindingList.add(name, id);
  }

  public void addAll(Map<String, ? extends FrameID> bindings) {
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

  @VisibleForTesting
  ImmutableBiMap<Integer, FrameInfo> createDefinitions(
      Map<Integer, FrameInfo> kernelDefinedFrames) {

    /*
     * SPICE silently blocks users from overriding any of the built-in frame definitions with kernel
     * content. So the first step is to simply overlay definitions with the kernel content.
     */
    BiMap<Integer, FrameInfo> result = HashBiMap.create(kernelDefinedFrames);

    for (Entry<Integer, FrameInfo> entry : builtInFrameNames.getMap().entrySet()) {
      result.forcePut(entry.getKey(), entry.getValue());
    }

    return ImmutableBiMap.copyOf(result);
  }

  @VisibleForTesting
  ImmutableMap<String, Integer> createAliasMap(Map<String, Integer> kernelDefinedAliases) {

    /*
     * Construct the built-in mapping from frame names to integers.
     */
    BiMap<Integer, FrameInfo> map = builtInFrameNames.getMap();

    Map<String, Integer> result = Maps.newHashMap(kernelDefinedAliases);

    /*
     * Override entries in the kernel defined aliases with the built-in names.
     */
    for (FrameInfo info : map.values()) {
      result.put(info.getName(), info.getCode());
    }

    return ImmutableMap.copyOf(result);
  }

  @VisibleForTesting
  ImmutableMap<FrameType, ImmutableSetMultimap<Integer, FrameID>> createClassMap(
      ImmutableMap<Integer, FrameID> frameCodeMapping,
      ImmutableBiMap<Integer, FrameInfo> definedFrames) {

    /*
     * At this point we have two-thirds of the desired results produced. All that remains is to
     * produce a mapping from each frame type to the multi-map connecting the class specific IDs to
     * the list of FrameIDs. This is necessary, because SPICE accepts more than one frame definition
     * that points to the same class-classID pair.
     */
    Map<FrameType, ImmutableSetMultimap.Builder<Integer, FrameID>> builderMap = Maps.newHashMap();
    for (FrameType type : FrameType.values()) {
      builderMap.put(type, new ImmutableSetMultimap.Builder<Integer, FrameID>());
    }

    /*
     * The frames map contains all the known frame definitions to the environment builder. The
     * integer codes, by construction, should correspond to the code in the corresponding FrameInfo
     * class. So all that remains is to build the multimaps.
     */
    for (FrameInfo info : definedFrames.values()) {

      int frameCode = info.getCode();
      ImmutableMultimap.Builder<Integer, FrameID> builder = builderMap.get(info.getType());

      /*
       * If the frame code has a corresponding frame ID stored in the frameData map, then use it.
       * Otherwise create a new frame ID and store it in the map.
       */
      if (frameCodeMapping.containsKey(frameCode)) {
        builder.put(info.getClassID(), frameCodeMapping.get(frameCode));
      } else {
        builder.put(info.getClassID(), new SpiceFrameID(frameCode));
      }

    }

    /*
     * Now build the final map for the binding list.
     */
    ImmutableMap.Builder<FrameType, ImmutableSetMultimap<Integer, FrameID>> builder =
        ImmutableMap.builder();

    for (FrameType type : FrameType.values()) {
      builder.put(type, builderMap.get(type).build());
    }

    return builder.build();

  }

  public BindingResult createFrameIDMap(Map<Integer, FrameInfo> kernelDefinitions,
      Map<String, Integer> kernelAliases) throws BindingConflictException {

    /*
     * If the supplied inputs come from the FrameKernel class, then consistency is enforced. The
     * strings should all be in canonical form. Overlay the built-in names and codes first.
     */
    ImmutableBiMap<Integer, FrameInfo> frames = createDefinitions(kernelDefinitions);
    ImmutableMap<String, Integer> aliases = createAliasMap(kernelAliases);

    ImmutableList.Builder<NameIDPair<FrameID>> unusedBuilder = ImmutableList.builder();

    BiMap<Integer, FrameID> userSuppliedBindings = HashBiMap.create();

    FrameID idToBeBound = null;
    String nameToBeBound = null;
    FrameID boundID = null;

    try {

      int code = 0;

      for (NameIDPair<FrameID> pair : bindingList) {

        idToBeBound = pair.getID();
        nameToBeBound = pair.getName();

        /*
         * Check to see if the bound string is contained in the definitions. If not add it to the
         * unused list.
         */
        if (!aliases.containsKey(nameToBeBound)) {
          unusedBuilder.add(pair);
        } else {

          code = aliases.get(nameToBeBound);

          /*
           * Determine if a user supplied binding already exists for this code. If it does, then
           * ascertain whether it is equivalent to the supplied binding. Otherwise throw an
           * appropriate exception.
           */
          if (userSuppliedBindings.containsKey(code)) {
            boundID = userSuppliedBindings.get(code);
            if (!idToBeBound.equals(boundID)) {
              throw new BindingConflictException.DuplicateFrameCode(boundID, code, idToBeBound,
                  nameToBeBound);
            }
          }

          boundID = userSuppliedBindings.put(code, pair.getID());

        }

      }

    } catch (IllegalArgumentException e) {
      int conflictingCode = userSuppliedBindings.inverse().get(idToBeBound);
      boundID = userSuppliedBindings.get(conflictingCode);
      throw new BindingConflictException.DuplicateFrameID(boundID, conflictingCode, idToBeBound,
          nameToBeBound, e);
    }

    /*
     * If we reach here, then we have built a mapping from the frame integer codes to user supplied
     * FrameIDs. Merge this mapping on top of the built-in default crucible bindings. This should be
     * a 1:1 and onto mapping, unless the user got sneaky and attempted to bind a built-in FrameID.
     */
    BiMap<Integer, FrameID> frameData = HashBiMap.create();

    /*
     * Make certain the user has not attempted to repurpose any of the built in crucible ID codes.
     */
    try {
      frameData.putAll(builtInFrameNames.getStandardBindings());
      frameData.putAll(userSuppliedBindings);
    } catch (IllegalArgumentException e) {
      throw new BindingConflictException.DuplicateFrameID(e);
    }

    ImmutableMap<Integer, FrameID> frameCodeMap = ImmutableMap.copyOf(frameData);

    /*
     * Create the FrameID to frame center ephemeris integer code mapping.
     */
    ImmutableMap.Builder<FrameID, Integer> frameCenterBuilder = ImmutableMap.builder();

    for (Integer frameCode : frameCodeMap.keySet()) {

      /*
       * Create an entry in the frame center map, if this integer has a corresponding frameID and an
       * entry in the frames (integer -> FrameInfo) map.
       */
      FrameID frameID = frameCodeMap.get(frameCode);

      if (frames.containsKey(frameCode)) {
        frameCenterBuilder.put(frameID, frames.get(frameCode).getCenterID());
      }

    }

    return new BindingResult(frameCodeMap, createClassMap(frameCodeMap, frames),
        unusedBuilder.build(), frameCenterBuilder.build());
  }
}
