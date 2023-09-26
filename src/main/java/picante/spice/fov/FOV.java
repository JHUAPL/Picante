package picante.spice.fov;

import picante.math.cones.Cone;
import picante.math.cones.Cones;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.FrameID;
import picante.mechanics.rotations.AxisAndAngle;
import picante.surfaces.Ellipse;

public class FOV {
  private final FrameID frameID;
  private final Cone cone;
  private final FOVSpice fovSpice;

  public FrameID getFrameID() {
    return frameID;
  }

  /**
   * 
   * @return a {@link Cone} describing the field of view.
   */
  public Cone getCone() {
    return cone;
  }

  /**
   * 
   * @return an {@link FOVSpice} object. This contains the standard SPICE FOV definition, such as
   *         boresight, shape, and vector of boundary points
   */
  public FOVSpice getFovSpice() {
    return fovSpice;
  }

  FOV(FrameID frameID, FOVSpice fovSpice) {
    this.frameID = frameID;
    this.fovSpice = fovSpice;

    switch (fovSpice.getShape()) {
      case CIRCLE: {
        UnwritableVectorIJK boundary0 = fovSpice.getBounds().get(0).createUnitized();
        UnwritableVectorIJK center = fovSpice.getBoresight().createUnitized();
        double angle = center.getSeparation(boundary0);
        boundary0 = boundary0.createScaled(1 / Math.cos(angle));
        UnwritableVectorIJK semiMajor = VectorIJK.subtract(boundary0, center);

        AxisAndAngle aaa = new AxisAndAngle(center, Math.PI / 2);
        UnwritableVectorIJK boundary1 = aaa.getRotation(new RotationMatrixIJK()).mxv(boundary0);

        UnwritableVectorIJK semiMinor = VectorIJK.subtract(boundary1, center);
        Ellipse limb = Ellipse.create(center, semiMajor, semiMinor);
        this.cone = Cones.createEllipticalCone(VectorIJK.ZERO, limb);
      }
        break;
      case ELLIPSE: {
        UnwritableVectorIJK center = fovSpice.getBoresight().createUnitized();

        UnwritableVectorIJK boundary = fovSpice.getBounds().get(0).createUnitized();
        double angle = center.getSeparation(boundary);
        boundary = boundary.createScaled(1 / Math.cos(angle));
        UnwritableVectorIJK semiMajor = VectorIJK.subtract(boundary, center);

        boundary = fovSpice.getBounds().get(1).createUnitized();
        angle = center.getSeparation(boundary);
        boundary = boundary.createScaled(1 / Math.cos(angle));
        UnwritableVectorIJK semiMinor = VectorIJK.subtract(boundary, center);

        if (semiMinor.getLength() > semiMajor.getLength()) {
          UnwritableVectorIJK tmp = semiMinor;
          semiMinor = semiMajor;
          semiMajor = tmp;
        }

        Ellipse limb = Ellipse.create(center, semiMajor, semiMinor);
        this.cone = Cones.createEllipticalCone(VectorIJK.ZERO, limb);
      }
        break;
      case POLYGON:
      case RECTANGLE:
        this.cone = Cones.createPolygonalCone(VectorIJK.ZERO, fovSpice.getBoresight(),
            fovSpice.getBounds());
        break;
      default:
        this.cone = null;
        break;
    }
  }

}

