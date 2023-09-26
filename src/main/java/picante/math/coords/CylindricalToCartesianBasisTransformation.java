package picante.math.coords;

import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

class CylindricalToCartesianBasisTransformation implements Transformation<CylindricalVector> {

  /**
   * return
   * 
   * <pre>
   *      .-                                  -.
   *      |  cos(long)  -sin(long)      0      |
   *      |                                    |
   *      |  sin(long)   cos(long)      0      |
   *      |                                    |
   *      |     0           0           1      |
   *      `-                                  -'
   * </pre>
   * 
   */
  @Override
  public MatrixIJK getTransformation(CylindricalVector coordPosition, MatrixIJK buffer) {

    double lon = coordPosition.getLongitude();

    double cosLon = cos(lon);
    double sinLon = sin(lon);

    double j_DX_DR = cosLon;
    double j_DY_DR = sinLon;
    double j_DZ_DR = 0.0;

    double j_DX_DLON = -sinLon;
    double j_DY_DLON = cosLon;
    double j_DZ_DLON = 0.0;

    double j_DX_DZ = 0.0;
    double j_DY_DZ = 0.0;
    double j_DZ_DZ = 1.0;

    return buffer.setTo(j_DX_DR, j_DY_DR, j_DZ_DR, j_DX_DLON, j_DY_DLON, j_DZ_DLON, j_DX_DZ,
        j_DY_DZ, j_DZ_DZ);
  }

  @Override
  public MatrixIJK getInverseTransformation(CylindricalVector coordPosition, MatrixIJK buffer) {
    return getTransformation(coordPosition, buffer).invort();
  }

  @Override
  public UnwritableVectorIJK mxv(UnwritableMatrixIJK jacobian, CylindricalVector coordValue) {
    return jacobian.mxv(coordValue.getVectorIJK());
  }

  @Override
  public CylindricalVector mxv(UnwritableMatrixIJK inverseTransformation,
      UnwritableVectorIJK cartVelocity) {
    UnwritableVectorIJK vect = inverseTransformation.mxv(cartVelocity);
    return new CylindricalVector(vect.getI(), vect.getJ(), vect.getK());
  }

}
