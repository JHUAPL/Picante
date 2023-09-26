package picante.spice.kernel.tk.fk.dynamic;

import java.util.List;
import java.util.Map;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.DifferentiatedEulerAngles;
import picante.mechanics.rotations.EulerAngles;
import picante.spice.GeneralAberratedEphemerisProvider;
import picante.spice.kernelpool.content.DynamicUnits;

public class DynamicEulerFrameFunction implements DynamicFrameFunction {
  private final int fromID;
  private final int toID;
  private final double epoch;
  private final List<EulerAngles.Axis> axis;
  private final List<Double> angle1Coeffs;
  private final List<Double> angle2Coeffs;
  private final List<Double> angle3Coeffs;
  private final DynamicUnits units;
  private boolean defined = false;

  public DynamicEulerFrameFunction(int fromID, int toID, double epoch, List<EulerAngles.Axis> axis,
      List<Double> angle1Coeffs, List<Double> angle2Coeffs, List<Double> angle3Coeffs,
      DynamicUnits units) {
    super();
    Preconditions.checkNotNull(fromID);
    Preconditions.checkNotNull(toID);
    this.fromID = fromID;
    this.toID = toID;
    this.epoch = epoch;
    this.axis = axis;
    this.angle1Coeffs = angle1Coeffs;
    this.angle2Coeffs = angle2Coeffs;
    this.angle3Coeffs = angle3Coeffs;
    this.units = units;
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
    double t = time - epoch;
    double angle1 = calculateAngle(angle1Coeffs, 0, t)[0];
    double angle2 = calculateAngle(angle2Coeffs, 0, t)[0];
    double angle3 = calculateAngle(angle3Coeffs, 0, t)[0];
    angle1 = units.convertToRadians(angle1);
    angle2 = units.convertToRadians(angle2);
    angle3 = units.convertToRadians(angle3);
    EulerAngles eulerAngles =
        EulerAngles.create(axis.get(0), axis.get(1), axis.get(2), angle1, angle2, angle3);
    return eulerAngles.getRotation(buffer);
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    Preconditions.checkState(defined,
        "You must call the define(...) method before calling getStateTransform().");
    double t = time - epoch;
    double[] angles1 = calculateAngle(angle1Coeffs, 1, t);
    double[] angles2 = calculateAngle(angle2Coeffs, 1, t);
    double[] angles3 = calculateAngle(angle3Coeffs, 1, t);
    angles1[0] = units.convertToRadians(angles1[0]);
    angles2[0] = units.convertToRadians(angles2[0]);
    angles3[0] = units.convertToRadians(angles3[0]);
    angles1[1] = units.convertToRadians(angles1[1]);
    angles2[1] = units.convertToRadians(angles2[1]);
    angles3[1] = units.convertToRadians(angles3[1]);
    DifferentiatedEulerAngles diffEulerAngles =
        DifferentiatedEulerAngles.create(axis.get(0), axis.get(1), axis.get(2), angles1[0],
            angles2[0], angles3[0], angles1[1], angles2[1], angles3[1]);
    return diffEulerAngles.getTransform(buffer);
  }


  /**
   * Follows NAIF's polyds subroutine from the SPICE toolkit
   * 
   * @param angleCoeffs
   * @param deriv
   * @param t
   * @return
   */
  static double[] calculateAngle(List<Double> angleCoeffs, int deriv, double t) {
    double[] polyDerivs = new double[deriv + 1];
    for (int i = 0; i < (deriv + 1); i++) {
      polyDerivs[i] = 0.0;
    }
    int deg = angleCoeffs.size() - 1;

    int k = deg;
    int i = deriv;
    double scale = deriv;

    while (k >= 0) {
      while (i > 0) {
        polyDerivs[i] = t * polyDerivs[i] + scale * polyDerivs[i - 1];
        scale--;
        i--;
      }
      polyDerivs[0] = t * polyDerivs[0] + angleCoeffs.get(k);
      k--;
      i = deriv;
      scale = deriv;
    }
    return polyDerivs;
  }
}
