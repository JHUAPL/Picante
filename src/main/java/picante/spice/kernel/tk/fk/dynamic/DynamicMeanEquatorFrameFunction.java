package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;

import com.google.common.base.Preconditions;
import com.google.common.primitives.ImmutableDoubleArray;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.spice.GeneralAberratedEphemerisProvider;
import picante.spice.jspice.Spicelib;

public class DynamicMeanEquatorFrameFunction implements DynamicFrameFunction {
  private final int fromID;
  private final int toID;
  private boolean defined = false;

  public DynamicMeanEquatorFrameFunction(int fromID, int toID) {
    super();
    Preconditions.checkNotNull(fromID);
    Preconditions.checkNotNull(toID);
    this.fromID = fromID;
    this.toID = toID;
  }

  @SuppressWarnings("unused")
  @Override
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap) {
    defined = true;
  }

  @Override
  public int getFromID() {
    return fromID;
  }

  @Override
  public int getToID() {
    return toID;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getTransform().");
    StateTransform precessionST = ida2st(Spicelib.ZZEPRC76(time));
    return buffer.setTo(precessionST.createInverse().getRotation());
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getStateTransform().");
    StateTransform precessionST = ida2st(Spicelib.ZZEPRC76(time));
    return buffer.setTo(precessionST.createInverse());
  }

  private static StateTransform ida2st(ImmutableDoubleArray array) {
    RotationMatrixIJK rotMat = new RotationMatrixIJK(array.get(0), array.get(1), array.get(2),
        array.get(6), array.get(7), array.get(8), array.get(12), array.get(13), array.get(14));
    MatrixIJK dRotMat = new MatrixIJK(array.get(3), array.get(4), array.get(5), array.get(9),
        array.get(10), array.get(11), array.get(15), array.get(16), array.get(17));
    StateTransform stateTransform = new StateTransform(rotMat, dRotMat);
    return stateTransform;
  }
}
