package picante.spice.adapters;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.collect.ImmutableList;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisSourceIOException;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;
import picante.spice.kernel.spk.SPK;
import picante.spice.kernel.spk.SPKSegment;

/**
 * A simple factory that derives an implementation of the <code>EphemerisSource</code> interface
 * from <code>SPK</code>s.
 * 
 * @see EphemerisSource
 * @see SPK
 */
public class EphemerisSourceFactory {

  /**
   * Map connecting SPICE integer ephemeris ID codes to actual EphemerisIDs.
   */
  private final FinalKeyMap<Integer, EphemerisID> ephemerisIdMap;

  /**
   * Map connecting SPICE integer frame ID codes to actual FrameIDs.
   */
  private final FinalKeyMap<Integer, FrameID> frameIdMap;

  /**
   * Creates an ephemeris source factory, given the supplied code mappings.
   * 
   * @param ephemerisIdMap a map connecting SPICE integer ephemeris ID codes with crucible
   *        <code>EphemerisID</code>s
   * @param frameIdMap a map connecting SPICE integer frame ID codes with crucible
   *        <code>FrameID</code>s
   */
  public EphemerisSourceFactory(Map<Integer, ? extends EphemerisID> ephemerisIdMap,
      Map<Integer, ? extends FrameID> frameIdMap) {
    this.ephemerisIdMap =
        new FinalKeyMap<Integer, EphemerisID>(new HashMap<Integer, EphemerisID>(ephemerisIdMap));
    this.frameIdMap = new FinalKeyMap<Integer, FrameID>(new HashMap<Integer, FrameID>(frameIdMap));
  }

  /**
   * Creates an <code>EphemerisSource</code> from the supplied SPK.
   * 
   * @param kernel the SPK from which to create an ephemeris source.
   * 
   * @return a newly created implementation of the <code>EphemerisSource</code> interface derived
   *         from the supplied SPK.
   */
  public ImmutableList<PositionVectorFunction> createEphemerisSource(SPK kernel)
      throws EphemerisSourceIOException {

    List<SPKSegmentAdapter> adapters = new ArrayList<SPKSegmentAdapter>(kernel.getSize());

    for (int i = 0; i < kernel.getSize(); i++) {
      adapters.add(createSegmentAdapter(kernel.getSegment(i)));
    }

    return new SPKAdapter(adapters, createMetaData(kernel)).getData();

  }

  /*
   * Jordi - added for SpiceEnvironment to access during getFrameSource
   */
  public FinalKeyMap<Integer, EphemerisID> getEphemerisIdMap() {
    return ephemerisIdMap;
  }

  /**
   * Instantiate SPK meta data from the supplied SPK.
   * 
   * @param kernel the SPK from which to create meta data.
   * 
   * @return a newly created instance of <code>SPKMetaData</code>
   */
  private SPKMetaData createMetaData(SPK kernel) {
    return new SPKMetaData(kernel.getName(), kernel.getInternalName(), kernel.getComments());
  }

  /**
   * Instantiate an adapter that implements the <code>StateVectorFunction</code> interface from an
   * SPK segment.
   * <p>
   * Any integer codes present in the supplied <code>SPKSegment</code> that are not present as keys
   * in the maps supplied to the constructor of the factory will have either
   * <code>SpiceEphemerisID</code>s or <code>SpiceFrameID</code>s created. These codes will then be
   * placed in the map, so subsequent references to the code will produce the same object.
   * </p>
   * 
   * @param segment the segment to adapt
   * 
   * @return a newly created adapter that implements the state vector function interface, using the
   *         factory code mappings to adapt SPICE integer codes to the appropriate crucible ID
   *         interfaces.
   */
  private SPKSegmentAdapter createSegmentAdapter(SPKSegment segment) {

    EphemerisID targetID;
    EphemerisID observerID;
    FrameID frameID;

    if (ephemerisIdMap.containsKey(segment.getTargetID())) {
      targetID = ephemerisIdMap.get(segment.getTargetID());
    } else {
      targetID = new SpiceEphemerisID(segment.getTargetID());
      ephemerisIdMap.put(segment.getTargetID(), targetID);
    }

    if (ephemerisIdMap.containsKey(segment.getObserverID())) {
      observerID = ephemerisIdMap.get(segment.getObserverID());
    } else {
      observerID = new SpiceEphemerisID(segment.getObserverID());
      ephemerisIdMap.put(segment.getObserverID(), observerID);
    }

    if (frameIdMap.containsKey(segment.getFrameID())) {
      frameID = frameIdMap.get(segment.getFrameID());
    } else {
      frameID = new SpiceFrameID(segment.getFrameID());
      frameIdMap.put(segment.getFrameID(), frameID);
    }

    return new SPKSegmentAdapter(targetID, observerID, frameID, segment);
  }

}
