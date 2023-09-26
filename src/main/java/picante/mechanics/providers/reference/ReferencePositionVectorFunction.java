package picante.mechanics.providers.reference;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisEvaluationException;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.SourceException;

/**
 * Implementation of the position vector function that is backed by the reference implementation's
 * chain link engine. Requests made to this function for body position vectors are evaluated at the
 * time of the request, and as such may result in runtime exceptions.
 * <p>
 * Due to the way in which calculations are buffered, this implementation of the position vector
 * function interface is not safely accessible from multiple threads.
 * </p>
 * 
 * @param <B> this class is generic only to allow the constructor to be supplied an instance of the
 *        chain link engine that links state vector functions instead of position vector functions.
 *        Essentially this allows the <code>ReferenceStateVectorFunction</code> to inherit from this
 *        class directly, which greatly simplifies its implementation.
 */
class ReferencePositionVectorFunction<B extends PositionVectorFunction>
    implements PositionVectorFunction {

  /**
   * Reference to an instance of a chain link engine supplied by the reference ephemeris provider at
   * construction time.
   */
  final ChainLinkEngine<EphemerisID, B, EphemerisCodeProvider<B>> engine;

  /**
   * A linked list that is utilized by this routine to capture the connection functions that link
   * the target and observer codes at the desired time.
   */
  final List<B> linkBuffer = new LinkedList<B>();

  /**
   * A map containing previously requested frame transformation functions. This acts as a buffer, so
   * each position vector request does not result in a flurry of repeated frame transform function
   * creations.
   */
  private final Map<FrameCodePair, FrameTransformFunction> transformMap =
      new HashMap<FrameCodePair, FrameTransformFunction>();

  /**
   * A frame code pairing, used to buffer frame transform functions requested from the internal
   * frame provider. Note: this buffering is acceptable, since the frame provider utilized by this
   * ephemeris provider implementation is the reference frame provider. Its contents can not change,
   * once supplied to the ephemeris provider.
   */
  private final FrameCodePair testPair = new FrameCodePair();

  /**
   * A reference to the internal frame provider utilized by this instance to perform all the
   * necessary rotations.
   */
  final ReferenceFrameProvider provider;

  /**
   * The ephemeris ID of the position vector function's tail.
   */
  final EphemerisID observerID;

  /**
   * The ephemeris ID of the position vector function's head.
   */
  final EphemerisID targetID;

  /**
   * The frame ID of the frame in which the position vector function is expressed.
   */
  final FrameID frameID;

  /**
   * Vector used to capture the link of the forward (from target to common node) chain.
   */
  private final VectorIJK fwdBuffer = new VectorIJK();

  /**
   * Vector used to capture the link of the backward (from observer to common node) chain.
   */
  private final VectorIJK bwdBuffer = new VectorIJK();

  /**
   * Temporary vector used in the evaluation methods of this class.
   */
  private final VectorIJK tmpBuffer = new VectorIJK();

  /**
   * Rotation matrix used as a local buffer.
   */
  private final RotationMatrixIJK matrix = new RotationMatrixIJK();

  /**
   * Constructs a reference position vector function from the supplied ID codes and instance of the
   * chain link engine.
   * 
   * @param targetID the object's ephemeris ID at the head of the position vector
   * @param observerID the object's ephemeris ID at the tail of the position vector
   * @param frameID the frame ID of the frame used to express vectors evaluated from this function
   * @param engine the chain link engine with the linking position vector functions
   * @param provider the instance of the reference frame provider implementation to support
   *        necessary frame transforms
   */
  public ReferencePositionVectorFunction(EphemerisID targetID, EphemerisID observerID,
      FrameID frameID, ChainLinkEngine<EphemerisID, B, EphemerisCodeProvider<B>> engine,
      ReferenceFrameProvider provider) {
    super();
    this.targetID = targetID;
    this.observerID = observerID;
    this.frameID = frameID;
    this.engine = engine;
    this.provider = provider;
  }

  /**
   * {@inheritDoc}
   * 
   * Note: the reference implementation pays no attention to the coverages presented by the
   * functions supplied by the source. It merely requests a link connecting the frames at the
   * requested time. If it fails due to missing coverage, then it throws a
   * ReferenceFrameLinkEvaluation exception. As such, coverage returned by this method covers all
   * applicable times.
   */
  @Override
  public Coverage getCoverage() {
    return Coverage.ALL_TIME;
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

  /**
   * {@inheritDoc}
   * 
   * @throws ReferenceEphemerisLinkEvaluationException if the function is unable to link the
   *         observer and target ID codes at the requested time
   * 
   * @throws ReferenceFrameLinkEvaluationException if the frame transformations provided by the
   *         supporting frame provider are insufficient to support the calculation
   */
  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {

    buffer = buffer == null ? new VectorIJK() : buffer;

    /*
     * First see if the engine is capable of connecting the target and observer IDs at the requested
     * time.
     */
    if (!engine.populateLinkage(targetID, observerID, time, linkBuffer)) {
      throw new ReferenceEphemerisLinkEvaluationException(targetID, observerID, time, linkBuffer);
    }

    /*
     * Now, at this point we are going to compute the position of the target relative to the common
     * node (indicated by the presence of a SEPARATOR in the linkBuffer) in the frame defining the
     * last link prior to the common node. Then, we will compute the position of the observer
     * relative to the common node in the frame defining the last link in the reverse chain.
     */
    ListIterator<B> iterator = linkBuffer.listIterator();
    PositionVectorFunction function = iterator.next();

    FrameID fwdFrame;
    FrameID bwdFrame;
    FrameID currFrame;

    if (!function.equals(ReferenceEphemerisProvider.SEPARATOR)) {

      /*
       * Since the first link in the chain isn't the separator, we have to evaluate the forward
       * chain.
       */
      fwdFrame = function.getFrameID();
      function.getPosition(time, fwdBuffer);

      /*
       * Handle the simple case, when the chain has but one link connecting the target to the
       * observer.
       */
      if (!iterator.hasNext()) {
        transformVectorInPlace(fwdFrame, frameID, time, fwdBuffer);
        buffer.setTo(fwdBuffer);
        return buffer;
      }

      function = iterator.next();

      /*
       * Since we have more than one link in the chain, loop through until we reach the separator or
       * the end of the chain.
       */
      while (!function.equals(ReferenceEphemerisProvider.SEPARATOR)) {

        function.getPosition(time, tmpBuffer);

        /*
         * Now pull up the currently buffered fwdBuffer to the same frame.
         */
        currFrame = function.getFrameID();
        transformVectorInPlace(fwdFrame, currFrame, time, fwdBuffer);

        VectorIJK.add(fwdBuffer, tmpBuffer, fwdBuffer);

        /*
         * Check to see if the iterator has additional links in the chain.
         */
        if (!iterator.hasNext()) {
          transformVectorInPlace(currFrame, frameID, time, fwdBuffer);
          buffer.setTo(fwdBuffer);
          return buffer;
        }

        /*
         * Prepare to process the next function in the list.
         */
        fwdFrame = currFrame;
        function = iterator.next();
      }

    } else {
      fwdBuffer.clear();
      /*
       * Configure the iterator to consume the next function, as we are merely skipping past the
       * initial separator.
       */
      function = iterator.next();
      fwdFrame = function.getFrameID();
    }

    /*
     * At this point we have properly populated the fwdBuffer with the vector connecting the common
     * node to the target. Further, the frame in which this vector is currently expressed is
     * captured in fwdFrame. Start at the end of the list and work backwards until we reach the
     * separator. Performing the same set of computations for the observer linkage.
     */
    iterator = linkBuffer.listIterator(linkBuffer.size());

    function = iterator.previous();

    bwdFrame = function.getFrameID();
    function.getPosition(time, bwdBuffer);

    function = iterator.previous();

    while (!function.equals(ReferenceEphemerisProvider.SEPARATOR)) {

      function.getPosition(time, tmpBuffer);

      /*
       * Pull up the current bwdBuffer to the current function's frame.
       */
      currFrame = function.getFrameID();
      transformVectorInPlace(bwdFrame, currFrame, time, bwdBuffer);

      VectorIJK.add(bwdBuffer, tmpBuffer, bwdBuffer);

      /*
       * Prepare to process the next function in the list.
       */
      bwdFrame = currFrame;
      function = iterator.previous();

    }

    /*
     * At this point we have two vectors, one connecting the target to a common node (which may be
     * the zero vector), and another connecting the observer to the common node. Combine these
     * vectors into buffer and then rotate into the requested frame.
     */
    transformVectorInPlace(fwdFrame, bwdFrame, time, fwdBuffer);
    VectorIJK.subtract(fwdBuffer, bwdBuffer, buffer);
    transformVectorInPlace(bwdFrame, frameID, time, buffer);

    return buffer;
  }

  /**
   * Implementation detail method, that transforms a position vector from one frame to another in
   * place.
   * 
   * @param fromID the frame ID of the frame in which the vector is expressed
   * @param toID the frame ID of the frame in which the vector is desired
   * @param time the time at which the vector was computed and the frame transformation is to be
   *        evaluated
   * @param vector the vector to move from <code>fromID</code> to <code>toID</code> in place
   */
  private void transformVectorInPlace(FrameID fromID, FrameID toID, double time, VectorIJK vector) {

    /*
     * Check to see if anything should be done.
     */
    if (fromID.equals(toID)) {
      return;
    }

    /*
     * Compute the transformation.
     */
    getTransform(fromID, toID).getTransform(time, matrix).mxv(vector, vector);

  }

  /**
   * Compute the frame transform function that connects two frame IDs.
   * 
   * @param fromID the frame ID of the frame in which this transform receives vectors
   * @param toID the frame ID of the frame into which this transform takes vectors
   * 
   * @return a frame transform function that performs the desired rotation.
   */
  FrameTransformFunction getTransform(FrameID fromID, FrameID toID) {

    testPair.fromID = fromID;
    testPair.toID = toID;

    FrameTransformFunction function = transformMap.get(testPair);

    if (function == null) {
      try {
        function = provider.createFrameTransformFunction(fromID, toID, Coverage.ALL_TIME);
      } catch (SourceException e) {
        throw new EphemerisEvaluationException("Unable to derive frame transformation.", e);
      }

      transformMap.put(new FrameCodePair(fromID, toID), function);
    }

    return function;
  }

}


/**
 * Simple class that aggregates the key ID codes associated with a frame transform (or state
 * transform) to allow a map to cache the functions locally within the reference implementation
 * classes.
 */
class FrameCodePair {

  public FrameID fromID;
  public FrameID toID;

  public FrameCodePair() {}

  public FrameCodePair(FrameID fromID, FrameID toID) {
    super();
    this.fromID = fromID;
    this.toID = toID;
  }

  public boolean isInertialTransform() {
    return fromID.isInertial() && toID.isInertial();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((fromID == null) ? 0 : fromID.hashCode());
    result = prime * result + ((toID == null) ? 0 : toID.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final FrameCodePair other = (FrameCodePair) obj;
    if (fromID == null) {
      if (other.fromID != null) {
        return false;
      }
    } else if (!fromID.equals(other.fromID)) {
      return false;
    }
    if (toID == null) {
      if (other.toID != null) {
        return false;
      }
    } else if (!toID.equals(other.toID)) {
      return false;
    }
    return true;
  }

}
