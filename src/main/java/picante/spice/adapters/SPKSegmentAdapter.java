package picante.spice.adapters;

import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;
import picante.spice.kernel.spk.SPKSegment;

/**
 * Adapts an SPK segment to the <code>StateVectorFunction</code> interface.
 */
class SPKSegmentAdapter implements StateVectorFunction {

  /**
   * The ephemeris ID associated with the target body.
   */
  private final EphemerisID targetID;

  /**
   * The ephemeris ID associated with the observing body.
   */
  private final EphemerisID observerID;

  /**
   * The frame ID associated with the underlying segment frame.
   */
  private final FrameID frameID;

  /**
   * A reference to the actual SPK segment.
   */
  private final SPKSegment segment;

  /**
   * Creates an adapter from the supplied ID codes and segment.
   * 
   * @param targetID the target ID to associate with the segment
   * @param observerID the observer ID to associate with the segment
   * @param frameID the frame ID in which the segment data is expressed
   * @param segment the segment
   */
  protected SPKSegmentAdapter(EphemerisID targetID, EphemerisID observerID, FrameID frameID,
      SPKSegment segment) {
    super();
    this.targetID = targetID;
    this.observerID = observerID;
    this.frameID = frameID;
    this.segment = segment;
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    buffer = buffer == null ? new StateVector() : buffer;
    return segment.getState(time, buffer);
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    buffer = buffer == null ? new VectorIJK() : buffer;
    return segment.getPosition(time, buffer);
  }

  @Override
  public Coverage getCoverage() {
    return segment.getCoverage();
  }

  @Override
  public FrameID getFrameID() {
    return frameID;
  }

  @Override
  public EphemerisID getObserverID() {
    return observerID;
  }

  @Override
  public EphemerisID getTargetID() {
    return targetID;
  }

}
