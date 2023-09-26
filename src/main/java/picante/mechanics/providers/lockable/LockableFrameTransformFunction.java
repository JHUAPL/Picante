package picante.mechanics.providers.lockable;

import java.util.List;
import java.util.ListIterator;
import com.google.common.collect.Lists;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.utilities.ChainLinkEngine;

/**
 * Implementation of the frame transform function that is backed by the reference implementation's
 * chain link engine. Requests made to this function for frame transforms are evaluated at the time
 * of request, and as such may result in runtime exceptions.
 * <p>
 * Due to the way in which calculations are buffered, this implementation of the frame transform
 * function interface is not safely accessible from multiple threads.
 * </p>
 * 
 * @param <F> this class is generic only to allow the constructor to be supplied an instance of the
 *        chain link engine that links state transform functions instead of frame transform
 *        functions. Essentially this allows the <code>ReferenceStateTransformFunction</code> to
 *        inherit from this class directly, which greatly simplifies its implementation.
 */
class LockableFrameTransformFunction<F extends FrameTransformFunction>
    implements FrameTransformFunction {

  /**
   * Reference to an instance of a chain link engine supplied by the reference frame provider at
   * construction time.
   */
  final ChainLinkEngine<FrameID, F> engine;

  /**
   * The frame ID of the originating frame.
   */
  final FrameID fromID;

  /**
   * The frame ID of the destination frame.
   */
  final FrameID toID;

  /**
   * Constructs a reference frame transform function from the supplied ID codes and instance of the
   * chain link engine.
   * 
   * @param fromID frame ID for the originating frame
   * @param toID frame ID for the destination frame
   * @param engine the chain link engine with the linking frame transform functions.
   */
  LockableFrameTransformFunction(FrameID fromID, FrameID toID, ChainLinkEngine<FrameID, F> engine) {
    super();
    this.fromID = fromID;
    this.toID = toID;
    this.engine = engine;
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
  public FrameID getFromID() {
    return fromID;
  }

  @Override
  public FrameID getToID() {
    return toID;
  }

  /**
   * {@inheritDoc}
   * 
   * @throws LockableFrameLinkEvaluationException if the function is unable to link the to and from
   *         ID codes at the requested time
   */
  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

    buffer = buffer == null ? new RotationMatrixIJK() : buffer;

    RotationMatrixIJK transform = new RotationMatrixIJK();

    /*
     * First, see if the engine is capable of connecting the from and to IDs at the requested time.
     */
    List<F> linkBuffer = Lists.newLinkedList();
    if (!engine.populateLinkage(fromID, toID, time, linkBuffer)) {
      throw new LockableFrameLinkEvaluationException(fromID, toID, time, linkBuffer);
    }

    /*
     * Start by initializing the supplied buffer to the multiplicative identity.
     * 
     * TODO: Optimize this. There is no reason the first link in the chain couldn't be directly
     * copied or inverted and copied into the supplied buffer.
     */
    buffer.setTo(RotationMatrixIJK.IDENTITY);

    /*
     * Iterate over the link buffer, applying the transforms until reaching the separator marker
     * function or the end of the list.
     */
    ListIterator<F> iterator = linkBuffer.listIterator();
    FrameTransformFunction function = iterator.next();

    while (!function.equals(LockableFrameProvider.SEPARATOR)) {
      function.getTransform(time, transform);
      RotationMatrixIJK.mxm(transform, buffer, buffer);

      if (!iterator.hasNext()) {
        return buffer;
      }

      function = iterator.next();
    }

    /*
     * If we reach here, then we have found an instance of the separator. Loop until reaching the
     * end of the chain, combining the inverse transforms in the remaining part of the list.
     */
    while (iterator.hasNext()) {
      function = iterator.next();
      function.getTransform(time, transform);

      RotationMatrixIJK.mtxm(transform, buffer, buffer);
    }

    return buffer;
  }
}
