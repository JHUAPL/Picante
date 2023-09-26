package picante.mechanics.providers.lockable;

import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;

/**
 * Extension of the frame transform function that provides locking synchronization for the state
 * transform function.
 */
class LockingDelegateStateTransformFunction extends LockingDelegateFrameTransformFunction
    implements StateTransformFunction {

  private final StateTransformFunction delegate;

  /**
   * Creates a locking delegate that synchronizes on the supplied lock.
   * 
   * @param lock the lock to utilize in synchronization
   * @param delegate the delegate to wrap
   */
  LockingDelegateStateTransformFunction(Object lock, StateTransformFunction delegate) {
    super(lock, delegate);
    this.delegate = delegate;
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    synchronized (getLock()) {
      return delegate.getStateTransform(time, buffer);
    }
  }

}
