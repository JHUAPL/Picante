package picante.mechanics.rotations;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * This extension of {@link RotationMatrixIJK} exists to allow code in this package that is
 * constructing rotation matrices that are valid to bypass the checking inherent in the
 * {@link RotationMatrixIJK#setTo(double, double, double, double, double, double, double, double, double)}
 * method family.
 * <p>
 * The basic pattern to utilize this class is, replace this code:
 * 
 * <pre>
 * RotationMatrixIJK buffer;
 * buffer.setTo(...);
 * </pre>
 * 
 * with:
 * 
 * <pre>
 * RotationMatrixIJK buffer;
 * PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();
 * assigner.setToWithoutCheck(...);
 * buffer.setTo(assigner);
 * </pre>
 * 
 * </p>
 * <p>
 * This will by-pass the check that the final setTo methods on RotationMatrixIJK are performing, so
 * it should only be used when the values being assigned are <b>clearly</b> a rotation as determined
 * by the normal setTo method.
 * </p>
 * <p>
 * In performance testing on my mac laptop, I determined that this was as fast as disabling the
 * check in the setTo() methods, and over 13 times faster than performing the check needlessly.
 * </p>
 */
class PrivilegedRotationMatrixIJK extends RotationMatrixIJK {

  PrivilegedRotationMatrixIJK() {}

  /**
   * @see RotationMatrixIJK#setTo(double, double, double, double, double, double, double, double,
   *      double)
   */
  PrivilegedRotationMatrixIJK setToWithoutCheck(double ii, double ji, double ki, double ij,
      double jj, double kj, double ik, double jk, double kk) {
    this.ii = ii;
    this.ji = ji;
    this.ki = ki;
    this.ij = ij;
    this.jj = jj;
    this.kj = kj;
    this.ik = ik;
    this.jk = jk;
    this.kk = kk;
    return this;
  }

  /**
   * @see RotationMatrixIJK#setTo(double[][])
   */
  RotationMatrixIJK setToWithoutCheck(double[][] data) {
    setToWithoutCheck(data[0][0], data[1][0], data[2][0], data[0][1], data[1][1], data[2][1],
        data[0][2], data[1][2], data[2][2]);
    return this;
  }

  /**
   * @see RotationMatrixIJK#setTo(UnwritableVectorIJK, UnwritableVectorIJK, UnwritableVectorIJK)
   */
  RotationMatrixIJK setToWithoutCheck(UnwritableVectorIJK ithColumn, UnwritableVectorIJK jthColumn,
      UnwritableVectorIJK kthColumn) {
    return setToWithoutCheck(ithColumn.getI(), ithColumn.getJ(), ithColumn.getK(), jthColumn.getI(),
        jthColumn.getJ(), jthColumn.getK(), kthColumn.getI(), kthColumn.getJ(), kthColumn.getK());
  }

  /**
   * @see RotationMatrixIJK#setTo(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK)
   */
  RotationMatrixIJK setToWithoutCheck(double scaleI, UnwritableVectorIJK ithColumn, double scaleJ,
      UnwritableVectorIJK jthColumn, double scaleK, UnwritableVectorIJK kthColumn) {
    return setToWithoutCheck(scaleI * ithColumn.getI(), scaleI * ithColumn.getJ(),
        scaleI * ithColumn.getK(), scaleJ * jthColumn.getI(), scaleJ * jthColumn.getJ(),
        scaleJ * jthColumn.getK(), scaleK * kthColumn.getI(), scaleK * kthColumn.getJ(),
        scaleK * kthColumn.getK());
  }

}
