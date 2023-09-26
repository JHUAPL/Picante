package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

class EllipsoidalIntersectionComputer implements SurfaceIntersectionComputer {

  private final double a;
  private final double b;
  private final double c;

  EllipsoidalIntersectionComputer(double a, double b, double c) {
    this.a = a;
    this.b = b;
    this.c = c;
  }

  private void scaleToUnit(VectorIJK vector) {
    vector.setTo(vector.getI() / a, vector.getJ() / b, vector.getK() / c);
  }

  private void invertScaleToUnit(VectorIJK vector) {
    vector.setTo(vector.getI() * a, vector.getJ() * b, vector.getK() * c);
  }

  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {
    checkArgument(!ray.equals(VectorIJK.ZERO), "Query ray can not be of zero length.");

    VectorIJK x = new VectorIJK();
    VectorIJK y = new VectorIJK();
    VectorIJK ux = new VectorIJK();
    VectorIJK p = new VectorIJK();
    VectorIJK yproj = new VectorIJK();

    x.setTo(ray);
    scaleToUnit(x);

    y.setTo(source);
    scaleToUnit(y);

    VectorIJK.planeProject(y, x, p);
    VectorIJK.subtract(y, p, yproj);

    double ymag = y.getLength();
    double pmag = p.getLength();

    ux.setToUnitized(x);

    /*
     * There are three cases, the source point lies outside the sphere, the source point lies on the
     * sphere, or it is inside the sphere. Start by handling the outside the sphere case:
     */
    if (ymag > 1.0) {

      /*
       * If p is outside of the unit sphere, or if x points in the same direction as yproj
       * (indicating the ray points away from the sphere), then there can be no intersection.
       */
      if ((pmag > 1.0) || (yproj.getDot(x) > 0.0)) {
        return false;
      }
    }

    /*
     * At this point an intersection exists, as we are either inside the sphere or on the sphere
     * itself.
     */
    return true;
  }

  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {

    checkArgument(!ray.equals(VectorIJK.ZERO), "Query ray can not be of zero length.");

    VectorIJK x = new VectorIJK();
    VectorIJK y = new VectorIJK();
    VectorIJK ux = new VectorIJK();
    VectorIJK p = new VectorIJK();
    VectorIJK yproj = new VectorIJK();

    x.setTo(ray);
    scaleToUnit(x);

    y.setTo(source);
    scaleToUnit(y);

    VectorIJK.planeProject(y, x, p);
    VectorIJK.subtract(y, p, yproj);

    double ymag = y.getLength();
    double pmag = p.getLength();

    ux.setToUnitized(x);

    /*
     * There are three cases, the source point lies outside the sphere, the source point lies on the
     * sphere, or it is inside the sphere. Start by handling the outside the sphere case:
     */
    double sign = 1.0;

    if (ymag > 1.0) {

      /*
       * If p is outside of the unit sphere, or if x points in the same direction as yproj
       * (indicating the ray points away from the sphere), then there can be no intersection.
       */
      if ((pmag > 1.0) || (yproj.getDot(x) > 0.0)) {
        throw new NoIntersectionException("No intersection exists.");
      }

      /*
       * If pmag is precisely 1.0, then it is the single unique point of intersection.
       */
      if (pmag == 1.0) {
        buffer.setTo(p);
        invertScaleToUnit(buffer);
        return buffer;
      }

      /*
       * Set the sign to a negative value, as we have a non-trivial intersection and the component
       * along UX we are adding to P points towards Y.
       */
      sign = -1.0;
    }

    if (ymag == 1.0) {

      /*
       * The source lies on the ellipsoid, so it is clearly the first point of intersection.
       */
      buffer.setTo(source);
      return buffer;
    }

    /*
     * There is a little work left to do at this point. scale is the length of the half chord at p
     * away from the unit sphere's center to either intersection point.
     */
    double scale = Math.sqrt(Math.max(0.0, 1 - pmag * pmag));

    VectorIJK.combine(1.0, p, sign * scale, ux, buffer);
    invertScaleToUnit(buffer);

    return buffer;

  }

}
