package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;

class GeometricPositionVectorFunction implements AberratedPositionVectorFunction {

  private final PositionVectorFunction delegate;

  GeometricPositionVectorFunction(PositionVectorFunction delegate) {
    super();
    this.delegate = checkNotNull(delegate);
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
  public AberrationCorrection getCorrection() {
    return AberrationCorrection.NONE;
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    return delegate.getPosition(time, buffer);
  }

  /**
   * {@inheritDoc}
   * 
   * @throws UnsupportedOperationException this is a geometric state, there is no light time.
   */
  @Override
  public double getLightTime(@SuppressWarnings("unused") double time) {
    throw new UnsupportedOperationException("Geometric positions can not produce light times.");
  }

}
