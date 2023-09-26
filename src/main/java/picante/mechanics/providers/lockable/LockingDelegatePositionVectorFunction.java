package picante.mechanics.providers.lockable;

import picante.math.intervals.Interval;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;

/**
 * Implementation of the position vector function interface that provides synchronization against a
 * supplied lock object.
 * <p>
 * Note: the function interrogates the delegate at construction time for the frame and ephemeris ID
 * codes. The results are cached and utilized to implement the delegate methods that retrieve them.
 * This was done largely to codify the fact that these IDs are used as keys in maps created at
 * construction time.
 * </p>
 * <p>
 * This class also implements the {@link Coverage} interface and returns itself in the
 * {@link PositionVectorFunction#getCoverage()} method. This was done to consolidate the
 * synchronization code into a single class.
 * </p>
 */
class LockingDelegatePositionVectorFunction implements PositionVectorFunction, Lockable, Coverage {

  /**
   * The object upon which to synchronize.
   */
  private final Object lock;

  /**
   * The delegate function
   */
  private final PositionVectorFunction delegate;

  private final FrameID frameID;
  private final EphemerisID observerID;
  private final EphemerisID targetID;

  /**
   * Creates a locking delegate that synchronizes on the supplied lock.
   * 
   * @param lock the lock to utilize in synchronization
   * @param delegate the delegate to wrap
   */
  LockingDelegatePositionVectorFunction(Object lock, PositionVectorFunction delegate) {
    super();
    this.lock = lock;
    this.delegate = delegate;
    this.frameID = delegate.getFrameID();
    this.observerID = delegate.getObserverID();
    this.targetID = delegate.getTargetID();
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
    return this;
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    synchronized (getLock()) {
      return delegate.getPosition(time, buffer);
    }
  }

  @Override
  public Object getLock() {
    return lock;
  }

  @Override
  public boolean contains(double time) {
    synchronized (getLock()) {
      return delegate.getCoverage().contains(time);
    }
  }

  @Override
  public Interval getBoundingInterval(Interval buffer) {
    synchronized (getLock()) {
      return delegate.getCoverage().getBoundingInterval(buffer);
    }
  }

  @Override
  public Interval getBracketingInterval(double time, Interval buffer) {
    synchronized (getLock()) {
      return delegate.getCoverage().getBracketingInterval(time, buffer);
    }
  }

  @Override
  public boolean hasNextInterval(double time) {
    synchronized (getLock()) {
      return delegate.getCoverage().hasNextInterval(time);
    }
  }

  @Override
  public Interval getNextInterval(double time, Interval buffer) {
    synchronized (getLock()) {
      return delegate.getCoverage().getNextInterval(time, buffer);
    }
  }

}
