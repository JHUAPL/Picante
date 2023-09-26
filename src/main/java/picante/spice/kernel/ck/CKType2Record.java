package picante.spice.kernel.ck;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.Quaternion;
import picante.mechanics.rotations.WrapperWithRate;

public class CKType2Record {

  private final WrapperWithRate<Quaternion> transform =
      new WrapperWithRate<Quaternion>(new Quaternion());
  private double secondsPerTickRate;

  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer) {
    return transform.getRotation(buffer);
  }

  public VectorIJK getAngularRate(VectorIJK buffer) {
    return transform.getRate(buffer);
  }

  public double getSecondsPerTickRate() {
    return secondsPerTickRate;
  }

  public void setRecord(double q0, double q1, double q2, double q3, double av1, double av2,
      double av3, double tickRate) {
    this.transform.getRotation().setTo(q0, q1, q2, q3);
    this.transform.getRate().setTo(av1, av2, av3);
    this.secondsPerTickRate = tickRate;
  }

}
