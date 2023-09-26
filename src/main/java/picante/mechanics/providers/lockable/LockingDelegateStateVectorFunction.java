package picante.mechanics.providers.lockable;

import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

/**
 * Extension of the position vector function delegate that provides state vector function
 * capabilities with locking.
 */
class LockingDelegateStateVectorFunction extends LockingDelegatePositionVectorFunction
    implements StateVectorFunction, Lockable {

  private final StateVectorFunction delegate;

  /**
   * Creates a locking delegate that synchronizes on the supplied lock.
   * 
   * @param lock the lock to utilize in synchronization
   * @param delegate the delegate to wrap
   */
  LockingDelegateStateVectorFunction(Object lock, StateVectorFunction delegate) {
    super(lock, delegate);
    this.delegate = delegate;
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    synchronized (getLock()) {
      return delegate.getState(time, buffer);
    }
  }

}
