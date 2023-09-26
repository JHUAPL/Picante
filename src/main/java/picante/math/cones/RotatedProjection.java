package picante.math.cones;

import java.awt.Shape;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

class RotatedProjection extends DerivativeProjection {

  public RotatedProjection(Projection shapeProjection, UnwritableRotationMatrixIJK rotMat) {
    super(shapeProjection);
    this.shapeProjection = shapeProjection;
    this.rotMat = UnwritableRotationMatrixIJK.copyOf(rotMat);
  }


  private final Projection shapeProjection;
  private final UnwritableRotationMatrixIJK rotMat;


  @Override
  public <P extends Point2D> P project(Point2D latLon, P buffer) {
    /*
     * Rotate to, then project
     */
    Point2D rotatedLatLon = rotateTo(latLon);
    shapeProjection.project(rotatedLatLon, buffer);

    return buffer;
  }

  @Override
  public <P extends Point2D> P invert(Point2D mapLocation, P buffer) {

    /*
     * Project, then rotate from
     */
    shapeProjection.invert(mapLocation, buffer);
    buffer.setLocation(rotateFrom(buffer));
    return buffer;
  }

  @Override
  public double getNativeAspectRatio() {
    return shapeProjection.getNativeAspectRatio();
  }

  @Override
  public Shape getValidRegion() {
    return shapeProjection.getValidRegion();
  }

  @Override
  public <R extends Path2D> R projectOpenPath(PathPlotter plotter, double step, R buffer) {
    /*
     * Rotate to, then project
     */
    PathPlotter rotatedPlotter = PathPlotters.rotate(plotter, rotMat);
    return shapeProjection.projectOpenPath(rotatedPlotter, step, buffer);
  }

  @Override
  public <R extends Path2D> R projectClosedPath(ClosedPathPlotter plotter, double step, R buffer) {
    /*
     * Rotate to, then project
     */
    ClosedPathPlotter rotatedPlotter = PathPlotters.rotate(plotter, rotMat);
    return shapeProjection.projectClosedPath(rotatedPlotter, step, buffer);
  }

  private Point2D rotateTo(Point2D latLon) {
    UnwritableVectorIJK vector =
        CoordConverters.convert(new LatitudinalVector(1.0, latLon.getY(), latLon.getX()));
    UnwritableVectorIJK rotatedVector = rotMat.mxv(vector);
    LatitudinalVector rotatedLatVector = CoordConverters.convertToLatitudinal(rotatedVector);
    return new Point2D.Double(rotatedLatVector.getLongitude(), rotatedLatVector.getLatitude());
  }

  private Point2D rotateFrom(Point2D latLon) {
    UnwritableVectorIJK vector =
        CoordConverters.convert(new LatitudinalVector(1.0, latLon.getY(), latLon.getX()));
    UnwritableVectorIJK rotatedVector = rotMat.mtxv(vector);
    LatitudinalVector rotatedLatVector = CoordConverters.convertToLatitudinal(rotatedVector);
    return new Point2D.Double(rotatedLatVector.getLongitude(), rotatedLatVector.getLatitude());
  }

  @Override
  public Point2D getReference() {
    /*
     * Return the parent shape projection reference point. This is because the operation here is
     * rotate, then project. So the projection is the same, it is merely centered on a different
     * point.
     */
    return shapeProjection.getReference();
  }

}


