package picante.math.cones;

import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Basic abstract class that implements the derived new shape projection methods for all
 * implementations to utilize.
 * 
 * @author J.E.Turner
 */
abstract class AbstractProjection implements Projection {

  @Override
  public Class<? extends Projection> getOriginalClass() {
    return getClass();
  }


  @Override
  public Projection rotate(UnwritableRotationMatrixIJK rotation) {
    return new RotatedProjection(this, rotation);
  }


}

