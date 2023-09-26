package picante.mechanics;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.VectorIJK;

public class ForwardingStateVectorFunction implements StateVectorFunction {

  protected final StateVectorFunction delegate;

  public ForwardingStateVectorFunction(StateVectorFunction delegate) {
    super();
    this.delegate = Preconditions.checkNotNull(delegate);
  }

  @Override
  public EphemerisID getObserverID() {
    return delegate.getObserverID();
  }

  @Override
  public EphemerisID getTargetID() {
    return delegate.getTargetID();
  }

  @Override
  public FrameID getFrameID() {
    return delegate.getFrameID();
  }

  @Override
  public Coverage getCoverage() {
    return delegate.getCoverage();
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    return delegate.getPosition(time, buffer);
  }


  @Override
  public StateVector getState(double time, StateVector buffer) {
    return delegate.getState(time, buffer);
  }

}
