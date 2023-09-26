package picante.spice.adapters;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.SetMultimap;
import picante.mechanics.FrameID;
import picante.mechanics.FrameSourceIOException;
import picante.mechanics.FrameTransformFunction;
import picante.spice.kernel.ck.CK;
import picante.spice.kernel.ck.CKSegment;
import picante.spice.kernel.ck.CKSegmentWithAV;
import picante.spice.kernel.pck.PCK;
import picante.spice.kernel.pck.PCKSegment;
import picante.spice.kernel.tk.fk.FrameKernel;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.fk.TKFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicStateTransformFunction;
import picante.spice.kernel.tk.pck.PCKFrameFunction;
import picante.spice.kernel.tk.pck.TextPCK;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;
import picante.spice.provided.InertialFrames;

public class FrameSourceFactory {

  /**
   * Map connecting SPICE integer frame ID codes to actual FrameIDs.
   */
  private final FinalKeyMap<Integer, FrameID> frameIdMap;

  /**
   * Map connecting SPICE integer CK (instrument) ID codes to actual Frame IDs. This map is
   * necessary because the C-kernel system in SPICE evolved before the consolidated frames API.
   */
  private final SetMultimap<Integer, FrameID> ckIdMap;

  /**
   * Map connecting SPICE integer PCK frame ID codes to actual Frame IDs. This map is necessary
   * because the PCK system in SPICE evolved before the consolidated frames API. Note: This approach
   * completely neglects the presence of the internally defined ID codes for the built-in PCK based
   * frames in SPICE.
   */
  private final SetMultimap<Integer, FrameID> pckIdMap;

  private final SetMultimap<Integer, FrameID> tkIdMap;

  // This we don't want. Dynamic frames are still frames and do not have separate IDs to track.
  // private final SetMultimap<Integer, FrameID> dynIdMap;

  /**
   * Registry of encoded SCLK converters necessary to map encoded SCLK used to index C-kernels into
   * TDB to meet the frame and state transformation primitive data source API.
   */
  private final SclkRegistry sclkRegistry;

  /*
   * These buffers exist only to inhibit the necessity of creating linked lists each time the
   * various adapters are to be brought up.
   */
  private final List<CKSegmentAdapter> ckSegmentsBuffer = Lists.newLinkedList();
  private final List<PCKAdapter> pckAdapterBuffer = Lists.newLinkedList();
  private final List<BinaryPCKSegmentAdapter> binaryPckAdapterBuffer = Lists.newLinkedList();
  private final List<TKFrameFunctionAdapter> tkFunctionAdapterBuffer = Lists.newLinkedList();

  /**
   * Creates a frame source factory, given the supplied code mappings and registry of SCLKs.
   *
   * @param frameIdMap
   * @param ckIdMap Due to the way NAIF handles C-kernel ID codes, this separate map is supplied to
   *        allow their ID code space to be handled separately.
   * @param sclkRegistry
   */
  public FrameSourceFactory(Map<Integer, ? extends FrameID> frameIdMap,
      SetMultimap<Integer, ? extends FrameID> ckIdMap,
      SetMultimap<Integer, ? extends FrameID> pckIdMap,
      SetMultimap<Integer, ? extends FrameID> tkIdMap,
      // See above -- we don't need this and SpiceEnvironmentBuilder will need to be changed back.
      // SetMultimap<Integer, ? extends FrameID> dynIdMap,
      SclkRegistry sclkRegistry) {
    this.frameIdMap = new FinalKeyMap<Integer, FrameID>(new HashMap<Integer, FrameID>(frameIdMap));
    this.ckIdMap = HashMultimap.create(ckIdMap);
    this.pckIdMap = HashMultimap.create(pckIdMap);
    this.tkIdMap = HashMultimap.create(tkIdMap);
    // TODO remove this when we are certain we don't want it.
    // this.dynIdMap = HashMultimap.create(dynIdMap);
    this.sclkRegistry = sclkRegistry;
  }

  public FinalKeyMap<Integer, FrameID> getFrameIDMap() {
    return frameIdMap;
  }

  /**
   * Creates a frame source containing all of the built-in inertial frame transformations available
   * in SPICE.
   *
   * @return a newly created <code>FrameSource</code> containing inertial frame definitions.
   */
  public ImmutableList<FrameTransformFunction> createBuiltInInertialFrameSource() {

    final List<InertialFrameAdapter> adapters =
        new ArrayList<InertialFrameAdapter>(InertialFrames.values().length);

    FrameID fromID;
    FrameID toID;

    for (InertialFrames frame : InertialFrames.values()) {

      if (frameIdMap.containsKey(frame.getFromID())) {
        fromID = frameIdMap.get(frame.getFromID());
      } else {
        fromID = new SpiceFrameID(frame.getFromID());
        frameIdMap.put(frame.getFromID(), fromID);
      }

      if (frameIdMap.containsKey(frame.getToID())) {
        toID = frameIdMap.get(frame.getToID());
      } else {
        toID = new SpiceFrameID(frame.getToID());
        frameIdMap.put(frame.getToID(), toID);
      }

      adapters.add(new InertialFrameAdapter(fromID, toID, frame));

    }

    return ImmutableList.copyOf(adapters);
  }

  /**
   * Creates a frame source from the supplied C-kernel.
   *
   * @param kernel the C-kernel to adapt.
   *
   * @return a newly created implementation of the <code>FrameSource</code> interface derived from
   *         the supplied CK.
   *
   * @throws AdapterInstantiationException if no appropriate SCLK can be found in the registry for
   *         the CK segment instrument ID.
   */
  public ImmutableList<FrameTransformFunction> createFrameSource(CK kernel)
      throws AdapterInstantiationException, FrameSourceIOException {

    List<CKSegmentAdapter> adapters = new ArrayList<CKSegmentAdapter>(kernel.getSize());

    for (int i = 0; i < kernel.getSize(); i++) {
      adapters.addAll(createCKSegmentAdapter(kernel.getSegment(i)));
    }

    return new CKAdapter(adapters, createCKMetaData(kernel)).getData();
  }

  /**
   * Instantiate CK meta data from the supplied CK.
   *
   * @param kernel the CK from which to create meta data
   *
   * @return a newly created instance of <code>CKMetaData</code>
   */
  private CKMetaData createCKMetaData(CK kernel) {
    return new CKMetaData(kernel.getName(), kernel.getInternalName(), kernel.getComments());
  }

  /**
   * Instantiate an adapter that implements the <code>FrameTransformFunction</code> interface from a
   * CK segment.
   * <p>
   * Any integer codes present in the supplied <code>CKSegment</code> that are not present as keys
   * in the maps supplied to the constructor of the factory will have either
   * <code>SpiceFrameID</code> or <code>SpiceCKFrameID</code> created as appropriate. These codes
   * will then be placed into the map, so subsequent references to the code will produce the same
   * object.
   * </p>
   *
   * @param segment the segment to adapt
   *
   * @return a newly created adapter that implements the frame transform function interface, or the
   *         state transform function if angular velocity is present, using the factory code
   *         mappings and SCLK registry to adapt SPICE integer codes and spacecraft clock times to
   *         the appropriate crucible interfaces.
   *
   * @throws AdapterInstantiationException if the requested segment does not have an appropriate
   *         SCLK present in the registry supplied to the factory.
   */
  private List<CKSegmentAdapter> createCKSegmentAdapter(CKSegment segment)
      throws AdapterInstantiationException {

    /*
     * Unfortunately C-kernel frame transformations occur "backwards" from that of the standard
     * frame API requires. So in this particular case the fromID is connected to the instrumentID
     * and the toID the referenceID (though the transformation stored natively in the C-kernel API
     * goes the other direction--which is corrected in the segment adapter implementations here).
     * 
     * Another additional complexity is the fact that SPICE supports multiple frame definitions for
     * each C-kernel. To support this, we allow for multiple adapters to be brought up, each with a
     * different frame ID. This should rarely ever happen, but in the event that it does this should
     * just function as expected.
     */
    Set<FrameID> fromIDs;
    FrameID toID;
    EncodedSCLKConverter converter;

    /*
     * After digging through how SPICE manages the CLASS_ID entries for C-kernel based frames, the
     * following was concluded: the instrument IDs (linked to the "fromID" in the higher level frame
     * API) are subject to an ID code to ID code mapping via the CLASS_ID entry in the CK frame
     * definition. However, the reference frame is NOT subject to this mapping, so it stands as a
     * basic frame code.
     */
    if (ckIdMap.containsKey(segment.getInstrumentID())) {
      fromIDs = ckIdMap.get(segment.getInstrumentID());
    } else {
      FrameID newID = new SpiceClassedFrameID(FrameType.CK, segment.getInstrumentID());
      ckIdMap.put(segment.getInstrumentID(), newID);
      fromIDs = ckIdMap.get(segment.getInstrumentID());
    }

    if (frameIdMap.containsKey(segment.getReferenceID())) {
      toID = frameIdMap.get(segment.getReferenceID());
    } else {
      toID = new SpiceFrameID(segment.getReferenceID());
      frameIdMap.put(segment.getReferenceID(), toID);
    }

    /*
     * Interrogate the encoded SCLK registry to locate the appropriate converter. If the appropriate
     * SCLK is not available in the registry, throw a new instantiation exception.
     */
    converter = sclkRegistry.getSclk(segment.getInstrumentID());

    /*
     * Loop over all entries in the fromID set, adding the appropriate adapters to the
     * ckSegmentsBuffer to return.
     */

    ckSegmentsBuffer.clear();

    for (FrameID fromID : fromIDs) {

      /*
       * Now return either the segment adapter with AV or without AV depending on the whether the
       * segment has angular velocity or not.
       */
      if (segment.hasAngularVelocity()) {
        ckSegmentsBuffer
            .add(new CKSegmentWithAVAdapter(fromID, toID, converter, (CKSegmentWithAV) segment));
      } else {
        ckSegmentsBuffer.add(new CKSegmentAdapter(fromID, toID, converter, segment));
      }

    }

    return ckSegmentsBuffer;

  }

  public ImmutableList<FrameTransformFunction> createFrameSource(TextPCK pck)
      throws FrameSourceIOException {

    List<PCKFrameFunction> functions = pck.getFrameFunctions();
    List<PCKAdapter> adapters = new ArrayList<PCKAdapter>(functions.size());

    for (PCKFrameFunction function : functions) {
      adapters.addAll(createPCKAdapter(function));
    }

    return new TextPCKAdapter(adapters).getData();
  }

  private List<PCKAdapter> createPCKAdapter(PCKFrameFunction function) {

    Set<FrameID> fromIDs;
    FrameID toID;

    /*
     * After digging through how SPICE manages the ID codes for PCK based frames, it seems exposing
     * the internally defined range of PCK frame codes was not terribly useful as they are not
     * directly exposed in any NAIF provided documentation other than private source. It seemed more
     * natural to reference directly the PCK body (or frame...) ID code.
     */
    if (pckIdMap.containsKey(function.getBodyCode())) {
      fromIDs = pckIdMap.get(function.getBodyCode());
    } else {
      FrameID newID = new SpiceClassedFrameID(FrameType.PCK, function.getBodyCode());
      pckIdMap.put(function.getBodyCode(), newID);
      fromIDs = pckIdMap.get(function.getBodyCode());
    }

    if (frameIdMap.containsKey(function.getReferenceCode())) {
      toID = frameIdMap.get(function.getReferenceCode());
    } else {
      toID = new SpiceFrameID(function.getReferenceCode());
      frameIdMap.put(function.getReferenceCode(), toID);
    }

    pckAdapterBuffer.clear();

    for (FrameID fromID : fromIDs) {
      pckAdapterBuffer.add(new PCKAdapter(fromID, toID, function));
    }

    return pckAdapterBuffer;
  }

  public ImmutableList<FrameTransformFunction> createFrameSource(PCK kernel)
      throws FrameSourceIOException {
    List<BinaryPCKSegmentAdapter> adapters =
        new ArrayList<BinaryPCKSegmentAdapter>(kernel.getSize());
    for (int i = 0; i < kernel.getSize(); i++) {
      adapters.addAll(createPCKSegmentAdapter(kernel.getSegment(i)));
    }
    return new BinaryPCKAdapter(adapters, createPCKMetaData(kernel)).getData();
  }

  private List<BinaryPCKSegmentAdapter> createPCKSegmentAdapter(PCKSegment segment) {

    /*
     * Unfortunately the binary PCK frame transformations occur "backwards" from that of the
     * standard frame API requirements. So in this particular case the fromID is connected to the
     * body frame code and the toID is connected to the reference frame code.
     */
    Set<FrameID> fromID;
    FrameID toID;

    if (pckIdMap.containsKey(segment.getBodyFrameID())) {
      fromID = pckIdMap.get(segment.getBodyFrameID());
    } else {
      FrameID newID = new SpiceClassedFrameID(FrameType.PCK, segment.getBodyFrameID());
      pckIdMap.put(segment.getBodyFrameID(), newID);
      fromID = pckIdMap.get(segment.getBodyFrameID());
    }

    if (frameIdMap.containsKey(segment.getReferenceFrameID())) {
      toID = frameIdMap.get(segment.getReferenceFrameID());
    } else {
      toID = new SpiceFrameID(segment.getReferenceFrameID());
      frameIdMap.put(segment.getReferenceFrameID(), toID);
    }

    binaryPckAdapterBuffer.clear();

    for (FrameID id : fromID) {
      binaryPckAdapterBuffer.add(new BinaryPCKSegmentAdapter(id, toID, segment));
    }

    return binaryPckAdapterBuffer;
  }

  private PCKMetaData createPCKMetaData(PCK kernel) {
    return new PCKMetaData(kernel.getName(), kernel.getInternalName(), kernel.getComments());
  }

  public ImmutableList<FrameTransformFunction> createFrameSource(FrameKernel fk)
      throws FrameSourceIOException {

    List<TKFrameFunction> functions = fk.getTKFrameFunctions();
    ImmutableList.Builder<TKFrameFunctionAdapter> builder = ImmutableList.builder();

    for (TKFrameFunction function : functions) {
      builder.addAll(createTKAdapter(function));
    }

    return new FKAdapter(builder.build()).getData();

  }

  public List<DynamicStateTransformFunction> createDynamicFrameSource(FrameKernel fk) {

    List<DynamicStateTransformFunction> stfList = new ArrayList<>();

    /*
     * the FrameKernel will provide DynamicFrameFunctions, which must be converted to inherit from
     * FrameTransformFunctions before being added. This is because FTFs use FrameIDs, not ints, and
     * those are not know at the time of creation of the DynamicFrameFunctions.Therefore that
     * conversion
     */
    for (DynamicFrameFunction frameFunction : fk.getDynamicFrameFunctions()) {
      stfList.add(createDynamicStateTransformFunction(frameFunction));
    }
    return stfList;

  }

  private List<TKFrameFunctionAdapter> createTKAdapter(TKFrameFunction function) {

    Set<FrameID> fromID;
    FrameID toID;

    if (tkIdMap.containsKey(function.getFromID())) {
      fromID = tkIdMap.get(function.getFromID());
    } else {
      FrameID newID = new SpiceClassedFrameID(FrameType.TK, function.getFromID());
      tkIdMap.put(function.getFromID(), newID);
      fromID = tkIdMap.get(function.getFromID());
    }

    if (frameIdMap.containsKey(function.getToID())) {
      toID = frameIdMap.get(function.getToID());
    } else {
      toID = new SpiceFrameID(function.getToID());
      frameIdMap.put(function.getToID(), toID);
    }

    tkFunctionAdapterBuffer.clear();

    for (FrameID id : fromID) {
      tkFunctionAdapterBuffer.add(new TKFrameFunctionAdapter(id, toID, function));
    }

    return tkFunctionAdapterBuffer;
  }

  private DynamicStateTransformFunction createDynamicStateTransformFunction(
      DynamicFrameFunction function) {
    FrameID fromID;
    FrameID toID;

    if (frameIdMap.containsKey(function.getFromID())) {
      fromID = frameIdMap.get(function.getFromID());
    } else {
      fromID = new SpiceFrameID(function.getFromID());
      frameIdMap.put(function.getFromID(), fromID);
    }

    if (frameIdMap.containsKey(function.getToID())) {
      toID = frameIdMap.get(function.getToID());
    } else {
      toID = new SpiceFrameID(function.getToID());
      frameIdMap.put(function.getToID(), toID);
    }
    return new DynamicStateTransformFunction(function, fromID, toID);
  }

}
