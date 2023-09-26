package picante.mechanics.utilities;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisEvaluationException;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

/**
 * Simple implementation of the {@link StateVectorFunction} interface that copies the contents of a
 * single vector and defines a time invariant position between two objects expressed in some frame.
 */
@Deprecated
public class FixedOffsetPositionFunction implements StateVectorFunction {

  /**
   * The {@link EphemerisID} capturing the observer, or tail of the vector
   */
  private final EphemerisID observerID;

  /**
   * The {@link EphemerisID} capturing the target, or head of the vector
   */
  private final EphemerisID targetID;

  /**
   * The {@link FrameID} defining the frame in which vector is expressed
   */
  private final FrameID frameID;

  /**
   * The {@link Coverage} for which this relationship is valid
   */
  private final Coverage coverage;

  /**
   * The unwritable buffer which caches the vector contents
   */
  private final UnwritableVectorIJK vector;

  /**
   * Creates a time invariant {@link StateVectorFunction} connecting an observer to a target in some
   * frame
   * 
   * @param targetID the {@link EphemerisID} of the target object, at the head of the supplied
   *        vector
   * @param observerID the {@link EphemerisID} of the observer object, at the tail of the supplied
   *        vector
   * @param frameID the {@link FrameID} describing the frame in which vector is expressed
   * @param coverage the {@link Coverage} object which defines the time validity of the function
   * @param vector the vector, whose contents are copied into an internal buffer defining the time
   *        invariant relationship between target and observer
   */
  public FixedOffsetPositionFunction(EphemerisID targetID, EphemerisID observerID, FrameID frameID,
      Coverage coverage, UnwritableVectorIJK vector) {
    this.observerID = observerID;
    this.targetID = targetID;
    this.frameID = frameID;
    this.coverage = coverage;
    this.vector = new UnwritableVectorIJK(vector);
  }

  /**
   * Creates a time invariant {@link StateVectorFunction} connecting an observer to a target in some
   * frame defined for {@link Coverage#ALL_TIME}
   * 
   * @param targetID the {@link EphemerisID} of the target object, at the head of the supplied
   *        vector
   * @param observerID the {@link EphemerisID} of the observer object, at the tail of the supplied
   *        vector
   * @param frameID the {@link FrameID} describing the frame in which vector is expressed
   * @param vector the vector, whose contents are copied into an internal buffer defining the time
   *        invariant relationship between target and observer
   */
  public FixedOffsetPositionFunction(EphemerisID targetID, EphemerisID observerID, FrameID frameID,
      UnwritableVectorIJK vector) {
    this(targetID, observerID, frameID, Coverage.ALL_TIME, vector);
  }

  @Override
  public EphemerisID getObserverID() {
    return observerID;
  }

  @Override
  public EphemerisID getTargetID() {
    return targetID;
  }

  @Override
  public FrameID getFrameID() {
    return frameID;
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {

    if (!coverage.contains(time)) {
      throw new EphemerisEvaluationException("Unable to evaluate position at time: " + time
          + ".  It lies outside the reported coverage.");
    }

    return buffer.setTo(vector);
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {

    if (!coverage.contains(time)) {
      throw new EphemerisEvaluationException("Unable to evaluate state at time: " + time
          + ".  It lies outside the reported coverage.");
    }

    buffer.setPosition(vector);
    buffer.setVelocity(VectorIJK.ZERO);
    return buffer;
  }

}
