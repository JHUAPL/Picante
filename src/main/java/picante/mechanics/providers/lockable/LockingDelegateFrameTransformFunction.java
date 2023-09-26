package picante.mechanics.providers.lockable;

import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;

/**
 * Implementation of the frame transform function interface that provides synchronization against a
 * supplied lock object.
 * <p>
 * Note: the function interrogates the delegate at construction time for the frame ID codes. The
 * results are cached and utilized to implement the delegate methods that retrieve them. This was
 * done largely to codify the fact that these IDs are used as keys in maps created at construction
 * time.
 * </p>
 * <p>
 * This class also implements the {@link Coverage} interface and returns itself in the
 * {@link FrameTransformFunction#getCoverage()} method. This was done to consolidate the
 * synchronization code into a single class.
 * </p>
 */
class LockingDelegateFrameTransformFunction implements FrameTransformFunction, Lockable, Coverage {

  /**
   * The object upon which to synchronize.
   */
  private final Object lock;

  private final FrameID fromID;
  private final FrameID toID;

  /**
   * The delegate function
   */
  private final FrameTransformFunction delegate;

  /**
   * Creates a locking delegate that synchronizes on the supplied lock.
   * 
   * @param lock the lock to utilize in synchronization
   * @param delegate the delegate to wrap
   */
  LockingDelegateFrameTransformFunction(Object lock, FrameTransformFunction delegate) {
    super();
    this.lock = lock;
    this.fromID = delegate.getFromID();
    this.toID = delegate.getToID();
    this.delegate = delegate;
  }

  @Override
  public Object getLock() {
    return lock;
  }

  @Override
  public FrameID getFromID() {
    return fromID;
  }

  @Override
  public FrameID getToID() {
    return toID;
  }

  @Override
  public Coverage getCoverage() {
    return this;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    synchronized (getLock()) {
      return delegate.getTransform(time, buffer);
    }
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
