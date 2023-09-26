package picante.mechanics.utilities;

import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;

/**
 * adapts an existing {@link PositionVectorFunction} by rescaling the length of the resulting vector
 * 
 * @author vandejd1
 */
@Deprecated
public class ScaledPositionVectorFunction implements PositionVectorFunction {
  final PositionVectorFunction srcFunc;

  final double scaleFactor;

  public ScaledPositionVectorFunction(PositionVectorFunction srcFunc, double scaleFactor) {
    this.srcFunc = srcFunc;
    this.scaleFactor = scaleFactor;
  }

  @Override
  public EphemerisID getObserverID() {
    return srcFunc.getObserverID();
  }

  @Override
  public EphemerisID getTargetID() {
    return srcFunc.getTargetID();
  }

  @Override
  public FrameID getFrameID() {
    return srcFunc.getFrameID();
  }

  @Override
  public Coverage getCoverage() {
    return srcFunc.getCoverage();
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    return srcFunc.getPosition(time, buffer).scale(scaleFactor);
  }

}
