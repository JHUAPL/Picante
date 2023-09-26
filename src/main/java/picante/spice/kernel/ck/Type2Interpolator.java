package picante.spice.kernel.ck;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.rotations.AxisAndAngle;

class Type2Interpolator {

  private final AxisAndAngle axisAngle = new AxisAndAngle();

  /**
   * 
   * @param startTime
   * @param secondsPerTimeUnit
   * @param matrix
   * @param rate rate is given in the same reference frame as the "from" frame of matrix
   * @param time
   * @param buffer
   * 
   * @return
   */
  public RotationMatrixIJK interpolate(double startTime, double secondsPerTimeUnit,
      UnwritableRotationMatrixIJK matrix, UnwritableVectorIJK rate, double time,
      RotationMatrixIJK buffer) {

    double angle = computeElapsedTime(startTime, secondsPerTimeUnit, time) * rate.getLength();

    /*
     * If rate is the zero vector, then the rotation should be the identity. Comparing against
     * VectorIJK.ZERO with equals() is faster, but may have issues if there are -0.0 values in the
     * vector components. The length check, while slower, is more robust.
     */
    if (rate.getLength() == 0.0) {
      buffer.setTo(RotationMatrixIJK.IDENTITY);
    } else {
      axisAngle.setTo(rate, angle);
      axisAngle.getRotation(buffer);
    }

    /*
     * Rotate each of the axis vectors of the to frame by angle radians about rate. The resulting
     * matrix is the transpose of the requested C-matrix.
     */
    RotationMatrixIJK.mxmt(matrix, buffer, buffer);

    return buffer;

  }

  private double computeElapsedTime(double startTime, double secondsPerTimeUnit, double time) {
    return (time - startTime) * secondsPerTimeUnit;
  }

}
