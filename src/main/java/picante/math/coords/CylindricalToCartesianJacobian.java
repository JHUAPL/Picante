package picante.math.coords;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

class CylindricalToCartesianJacobian implements Transformation<CylindricalVector> {

  /**
   * return
   * 
   * <pre>
   *      .-                                  -.
   *      |  dx/dr     dx/dlong       dx/dz    |
   *      |                                    |
   *      |  dy/dr     dy/dlong       dy/dz    |
   *      |                                    |
   *      |  dz/dr     dz/dlong       dz/dz    |
   *      `-                                  -'
   *      
   *      .-                                  -.
   *      |  cos(long)  -sin(long)*r    0      |
   *      |                                    |
   *      |  sin(long)   cos(long)*r    0      |
   *      |                                    |
   *      |     0           0           1      |
   *      `-                                  -'
   * </pre>
   * 
   */
  @Override
  public MatrixIJK getTransformation(CylindricalVector coordPosition, MatrixIJK buffer) {
    /*
     * from SPICE's routine in drdcyl.f
     * 
     * JACOBI (DX,DR) = DCOS( LONG )
     * 
     * JACOBI (DY,DR) = DSIN( LONG )
     * 
     * JACOBI (DZ,DR) = 0.0D0
     * 
     * 
     * JACOBI (DX,DLON) = -DSIN( LONG ) * R
     * 
     * JACOBI (DY,DLON) = DCOS( LONG ) * R
     * 
     * JACOBI (DZ,DLON) = 0.0D0
     * 
     * 
     * JACOBI (DX,DZ) = 0.0D0
     * 
     * JACOBI (DY,DZ) = 0.0D0
     * 
     * JACOBI (DZ,DZ) = 1.0D0
     */

    double r = coordPosition.getCylindricalRadius();
    double lon = coordPosition.getLongitude();

    double cosLon = cos(lon);
    double sinLon = sin(lon);

    double j_DX_DR = cosLon;
    double j_DY_DR = sinLon;
    double j_DZ_DR = 0.0;

    double j_DX_DLON = -sinLon * r;
    double j_DY_DLON = cosLon * r;
    double j_DZ_DLON = 0.0;

    double j_DX_DZ = 0.0;
    double j_DY_DZ = 0.0;
    double j_DZ_DZ = 1.0;

    return buffer.setTo(j_DX_DR, j_DY_DR, j_DZ_DR, j_DX_DLON, j_DY_DLON, j_DZ_DLON, j_DX_DZ,
        j_DY_DZ, j_DZ_DZ);
  }

  @Override
  public MatrixIJK getInverseTransformation(CylindricalVector coordPosition, MatrixIJK buffer) {
    try {
      return getTransformation(coordPosition, buffer).invort();
    } catch (UnsupportedOperationException e) {
      throw new PointOnAxisException(e);
    }
  }

  @Override
  public UnwritableVectorIJK mxv(UnwritableMatrixIJK jacobian, CylindricalVector coordVelocity) {
    return jacobian.mxv(coordVelocity.getVectorIJK());
  }

  @Override
  public CylindricalVector mxv(UnwritableMatrixIJK inverseJacobian,
      UnwritableVectorIJK cartVelocity) {
    UnwritableVectorIJK vect = inverseJacobian.mxv(cartVelocity);
    return new CylindricalVector(vect.getI(), vect.getJ(), vect.getK());
  }

}
