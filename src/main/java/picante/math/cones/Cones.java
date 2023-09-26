package picante.math.cones;

import java.awt.geom.Path2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.operation.union.CascadedPolygonUnion;
import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.surfaces.Ellipse;
import picante.surfaces.Ellipsoid;
import picante.surfaces.NoIntersectionException;
import picante.surfaces.Plane;

/**
 * Static utility method collection for manipulating and working with {@link Cone}s and
 * {@link ConeFunction}s
 * 
 * @author C.M. O'Shea
 * @author R.T. Poffenbarger
 *
 */
public class Cones {

  /**
   * Block construction of the class, there's no need for instances of it to exist.
   */
  private Cones() {}

  /**
   * Creates a constant {@link ConeFunction}
   * 
   * @param cone the constant value
   * 
   * @return a {@link ConeFunction} that returns the supplied cone for any provided input
   */
  public static ConeFunction createConstant(Cone cone) {
    return (t) -> cone;
  }


  /**
   * Creates a {@link ClosedPathPlotter} from a cone with a vertex at the center of the map
   * projected body.
   * 
   * @param cone the cone to plot
   * 
   * @return the closed path plotter
   */
  static ClosedPathPlotter plot(Cone cone) {
    return new ClosedPathPlotter() {
      @Override
      public Point2D plot(double p, Point2D buffer) {
        return PathPlotters.convert(cone.getEdge(p), buffer);
      }

      @Override
      public boolean isReferencePointInterior() {
        return true;
      }

      @Override
      public Point2D getReferencePoint(Point2D buffer) {
        return PathPlotters.convert(cone.getInteriorPoint(), buffer);
      }

      @Override
      public UnwritableInterval getDomain() {
        return cone.getParameterDomain();
      }
    };
  }

  /**
   * Create an {@link EllipticalCone}. The first generating vector of the ellipse is the projection
   * of the reference vector onto the plane normal to the boresight, scaled by the tangent of the
   * reference angle. The second generating vector is the unitized cross product of the first
   * generating vector and the boresight, scaled by the tangent or the cross angle.
   * 
   * @param vertex cone vertex
   * @param boresight cone axis
   * @param refVector
   * @param refAngle
   * @param crossAngle
   * @return
   */
  public static EllipticalCone createEllipticalCone(UnwritableVectorIJK vertex,
      UnwritableVectorIJK boresight, UnwritableVectorIJK refVector, double refAngle,
      double crossAngle) {

    UnwritableVectorIJK interiorPoint = boresight.createUnitized();
    VectorIJK axis1 = VectorIJK.planeProject(refVector, interiorPoint);
    VectorIJK axis2 = VectorIJK.uCross(axis1, interiorPoint);
    axis1.unitize().scale(Math.tan(refAngle));
    axis2.unitize().scale(Math.tan(crossAngle));

    return new EllipticalCone(vertex, interiorPoint, axis1, axis2);
  }

  /**
   * Create an {@link EllipticalCone} from a vertex and Ellipse
   */
  public static EllipticalCone createEllipticalCone(UnwritableVectorIJK bodyToConeVertex,
      Ellipse limb) {
    UnwritableVectorIJK semiMajor = new VectorIJK(limb.getSemiMajorAxis());
    UnwritableVectorIJK semiMinor = new VectorIJK(limb.getSemiMinorAxis());
    return new EllipticalCone(bodyToConeVertex, limb.getCenter(), semiMajor, semiMinor);
  }


  /**
   * Create a {@link PolygonalCone}
   */
  public static PolygonalCone createPolygonalCone(UnwritableVectorIJK vertex,
      UnwritableVectorIJK interiorPt, List<UnwritableVectorIJK> pts3d) {
    return new PolygonalCone(vertex, interiorPt, pts3d);
  }

  /**
   * Create a {@link PolygonalCone} with four sides. Follows the logic in the NAIF routine GETFOV:
   * 
   * <pre>
           (1) Normalize BSIGHT, label it B.
  
           (2) Compute the unit vector in the plane defined by REFVEC
               and B that is normal to B and pointing towards
               REFVEC, label this B1.
  
           (3) Cross B and B1 to obtain B2. These three vectors
               form a basis that is 'aligned' with the FOV cone.
  
           (4) Compute the inward normals to the sides of the
               rectangular cone in a counter-clockwise order
               about the boresight:
  
                 NORMAL(1) = -COS(REFANG)*B1 + SIN(REFANG)*B
                 NORMAL(2) = -COS(CRSANG)*B2 + SIN(CRSANG)*B
                 NORMAL(3) =  COS(REFANG)*B1 + SIN(REFANG)*B
                 NORMAL(4) =  COS(CRSANG)*B2 + SIN(CRSANG)*B
  
           (5) Compute the appropriate cross products to obtain
               a set of boundary corner vectors:
  
                 BOUNDS(1) = NORMAL(1) x NORMAL(2)
                 BOUNDS(2) = NORMAL(2) x NORMAL(3)
                 BOUNDS(3) = NORMAL(3) x NORMAL(4)
                 BOUNDS(4) = NORMAL(4) x NORMAL(1)
  
           (6) Unitize BOUNDS.
   * </pre>
   * 
   * 
   * @param vertex
   * @param boresight axis of the cone
   * @param refVector together with the boresight vector defines the plane in which refAngle is
   *        measured
   * @param refAngle 1/2 of the total angular extent of the cone in the plane defined by the
   *        boresight and refVector
   * @param crossAngle 1/2 of the total angular extent of the cone in the plane defined by the
   *        boresight and perpendicular to the refAngle plane
   * @return
   */
  public static PolygonalCone createRectangularCone(UnwritableVectorIJK vertex,
      UnwritableVectorIJK boresight, UnwritableVectorIJK refVector, double refAngle,
      double crossAngle) {

    UnwritableVectorIJK b = boresight.createUnitized();
    Plane p = new Plane(b, vertex);
    VectorIJK b1 = p.projectOnto(refVector).unitize();
    VectorIJK b2 = VectorIJK.cross(b, b1);

    double cosRef = Math.cos(refAngle);
    double sinRef = Math.sin(refAngle);
    double cosCrs = Math.cos(crossAngle);
    double sinCrs = Math.sin(crossAngle);

    VectorIJK normal1 = VectorIJK.combine(-cosRef, b1, sinRef, b);
    VectorIJK normal2 = VectorIJK.combine(-cosCrs, b2, sinCrs, b);
    VectorIJK normal3 = VectorIJK.combine(cosRef, b1, sinRef, b);
    VectorIJK normal4 = VectorIJK.combine(cosCrs, b2, sinCrs, b);

    List<UnwritableVectorIJK> bounds = new ArrayList<>();
    bounds.add(VectorIJK.uCross(normal1, normal2));
    bounds.add(VectorIJK.uCross(normal2, normal3));
    bounds.add(VectorIJK.uCross(normal3, normal4));
    bounds.add(VectorIJK.uCross(normal4, normal1));

    return new PolygonalCone(vertex, boresight, bounds);
  }


  public static Optional<Cone> union(Cone cone1, double step1, Cone cone2, double step2) {
    /*
     * Make sure cones have vertex in the same location
     */
    Preconditions.checkArgument(cone1.getVertex().equals(cone2.getVertex()),
        "The cones to be intersected must have the same vertex");

    /*
     * project onto the rotated shape projection with X axis = interior point of cone 1 and Z axis =
     * either J or K and create Path2Ds from the cones
     */
    Projection projection = createRotatedProjectionFromCone(cone1);
    Path2D.Double cone1path = getConeAsPath(cone1, step1, projection);
    Path2D.Double cone2path = getConeAsPath(cone2, step2, projection);

    /*
     * convert to JTS Shapes and find union
     */
    Function<PathIterator, Geometry> pathToGeometryConverter =
        JtsUtilities.pathIteratorToGeometryConverter();
    Geometry jtsShape1 = pathToGeometryConverter.apply(cone1path.getPathIterator(null)).buffer(0);
    Geometry jtsShape2 = pathToGeometryConverter.apply(cone2path.getPathIterator(null)).buffer(0);
    Geometry union = jtsShape1.union(jtsShape2);

    return Cones.createFromGeometry(union, projection, cone1.getVertex());

  }

  public static Optional<Cone> intersect(Cone cone1, double step1, Cone cone2, double step2) {
    /*
     * Make sure cones have vertex in the same location
     */
    Preconditions.checkArgument(cone1.getVertex().equals(cone2.getVertex()),
        "The cones to be intersected must have the same vertex");

    /*
     * project onto the rotated shape projection with X axis = interior point of cone 1 and Z axis =
     * either J or K and create Path2Ds from the cones
     */
    Projection projection = createRotatedProjectionFromCone(cone1);
    Path2D.Double cone1path = getConeAsPath(cone1, step1, projection);
    Path2D.Double cone2path = getConeAsPath(cone2, step2, projection);

    /*
     * convert to JTS Shapes and find intersection
     */
    Function<PathIterator, Geometry> pathToGeometryConverter =
        JtsUtilities.pathIteratorToGeometryConverter();
    Geometry jtsShape1 = pathToGeometryConverter.apply(cone1path.getPathIterator(null)).buffer(0);
    Geometry jtsShape2 = pathToGeometryConverter.apply(cone2path.getPathIterator(null)).buffer(0);
    Geometry intersection = jtsShape1.intersection(jtsShape2);

    return Cones.createFromGeometry(intersection, projection, cone1.getVertex());

  }

  /**
   * Subtract cone2 from cone1. Note, the resulting cone will likely not be convex and therefore
   * several methods in these interfaces may not work as expected. If there is no resulting cone,
   * this will return an empty list. There can also be multiple cones returned (image a cone
   * splitting another one right down the middle)
   * 
   * @param cone1 the original cone
   * @param step1 the step size for cone1
   * @param cone2 the cone to subtract from the original cone
   * @param step2 the step size for cone2
   * @return list of cones resulting from the subtraction
   */
  public static List<Cone> subtract(Cone cone1, double step1, Cone cone2, double step2) {
    List<Cone> cones = Lists.newArrayList();

    /*
     * Make sure cones have vertex in the same location
     */
    Preconditions.checkArgument(cone1.getVertex().equals(cone2.getVertex()),
        "The cones to be subtracted must have the same vertex");

    Projection projection = createRotatedProjectionFromCone(cone1);
    Path2D.Double cone1path = getConeAsPath(cone1, step1, projection);
    Path2D.Double cone2path = getConeAsPath(cone2, step2, projection);

    /*
     * convert to JTS Shapes and find intersection
     */
    Function<PathIterator, Geometry> pathToGeometryConverter =
        JtsUtilities.pathIteratorToGeometryConverter();
    Geometry jtsShape1 = pathToGeometryConverter.apply(cone1path.getPathIterator(null)).buffer(0);
    Geometry jtsShape2 = pathToGeometryConverter.apply(cone2path.getPathIterator(null)).buffer(0);
    Geometry subtraction = jtsShape1.difference(jtsShape2);

    /*
     * the result here could be multiple shapes
     */
    if (subtraction instanceof MultiPolygon) {
      Geometry geom;
      Optional<Cone> cone;

      int numGeoms = subtraction.getNumGeometries();
      for (int n = 0; n < numGeoms; n++) {
        geom = subtraction.getGeometryN(n);
        cone = Cones.createFromGeometry(geom, projection, cone1.getVertex());
        if (cone.isPresent()) {
          cones.add(cone.get());
        }
      }
    } else {
      /*
       * just return the Cone made from this geometry
       */
      Optional<Cone> subtractionCone =
          Cones.createFromGeometry(subtraction, projection, cone1.getVertex());
      if (subtractionCone.isPresent()) {
        cones.add(subtractionCone.get());
      }
    }
    /*
     * Create a cone that represents this intersection
     */
    return cones;
  }


  /**
   * Take a JTS Geometry, find the edge vectors and interior point and create a cone in 3D
   * 
   * @param geometry
   * @param projection
   * @param vertex
   * @return
   */
  private static Optional<Cone> createFromGeometry(Geometry geometry, Projection projection,
      UnwritableVectorIJK vertex) {

    // TODO pass the tolerance as input
    if (!geometry.isEmpty() && !(geometry.getArea() < 1E-14)) {
      List<UnwritableVectorIJK> pts3D = getListOfPointsFromJTS(geometry, projection);

      // find an interior point
      Point interiorPt = geometry.getInteriorPoint();
      Point2D.Double interiorPtDouble = new Point2D.Double(interiorPt.getX(), interiorPt.getY());

      // normalized -> latlon -> 3D
      Point2D.Double latlon = projection.invert(interiorPtDouble, new Point2D.Double());
      UnwritableVectorIJK cartesianIntPoint =
          CoordConverters.convert(new LatitudinalVector(1, latlon.getY(), latlon.getX()));

      PolygonalCone cone = Cones.createPolygonalCone(vertex, cartesianIntPoint, pts3D);
      return Optional.of(cone);
    } else {
      // there is no intersection
      return Optional.empty();
    }
  }

  /**
   * Intersect and Subtract any number of cones to form a new cone. Intersect each in the list of
   * toIntersect, then subtract each cone in the list of toSubtract. Since subtracting can result in
   * a list, this method must return a list of Cones.
   * 
   * @param toIntersect the list of cones included in the intersection
   * @param toSubtract the list of cones included in the subtraction
   * @param stepIntersect the list of steps to use for the toIntersect list of cones
   * @param stepSubtract the list of steps to use for the toSubtract list of cones
   * @return a list of resulting cones
   */
  public static List<Cone> intersectAndSubtract(List<Cone> toIntersect, List<Double> stepIntersect,
      List<Cone> toSubtract, List<Double> stepSubtract) {
    List<Cone> cones = Lists.newArrayList();

    Preconditions.checkArgument(stepIntersect.size() == toIntersect.size(),
        "The number of step sizes does not match the number of cones provided for the intersection");
    Preconditions.checkArgument(stepSubtract.size() == toSubtract.size(),
        "The number of step sizes does not match the number of cones provided for the subtraction");

    if (toIntersect.size() == 0) {
      return cones;
    }

    Optional<Cone> intersection = Optional.of(toIntersect.get(0));
    double step1 = stepIntersect.get(0);
    if (toIntersect.size() > 1) {
      for (int iCone = 1; iCone < toIntersect.size(); iCone++) {
        if (intersection.isPresent()) {
          intersection = Cones.intersect(intersection.get(), step1, toIntersect.get(iCone),
              stepIntersect.get(iCone));
          step1 = 1;
        }
      }
    }

    /*
     * if nothing left after intersection, return empty list.
     */
    if (!intersection.isPresent()) {
      return cones;
    }


    if (toSubtract.isEmpty()) {
      /*
       * nothing to subtract, return intersection
       */
      cones.add(intersection.get());
      return cones;
    }


    /*
     * now we have the intersection and want to subtract the rest.
     */
    List<Cone> previousSubtractionResult = Lists.newArrayList();
    List<Cone> currentSubtractionResult = Lists.newArrayList();

    // subtract the first cone in the list
    previousSubtractionResult
        .addAll(Cones.subtract(intersection.get(), step1, toSubtract.get(0), stepSubtract.get(0)));
    /*
     * subtract the remaining from the resulting list
     */
    for (int iCone = 1; iCone < toSubtract.size(); iCone++) {
      if (previousSubtractionResult.isEmpty()) {
        /*
         * return if no cones remain in list.
         */
        return cones;
      }
      currentSubtractionResult.clear();
      for (Cone cone : previousSubtractionResult) {
        currentSubtractionResult
            .addAll(Cones.subtract(cone, step1, toSubtract.get(iCone), stepSubtract.get(iCone)));
      }
      previousSubtractionResult.clear();
      previousSubtractionResult.addAll(currentSubtractionResult);
      currentSubtractionResult.clear();
      step1 = 1;
    }

    return previousSubtractionResult;

  }



  /**
   * Intersect and Subtract any number of cones to form a new cone. Intersect each in the list of
   * toIntersect, then subtract each cone in the list of toSubtract. Use the step size stepIntersect
   * for each intersection path plotter and the step size stepSubtract for each subtraction path
   * plotter.
   * 
   * @param toIntersect the list of cones included in the intersection
   * @param stepIntersect the step size for each of the cones in toIntersect
   * @param toSubtract the list of cones included in the subtraction
   * @param stepSubtract the step size for each of the ocnes in toSubtract
   * @return
   */
  public static List<Cone> intersectAndSubtract(List<Cone> toIntersect, double stepIntersect,
      List<Cone> toSubtract, double stepSubtract) {
    List<Double> intersectSteps = Collections.nCopies(toIntersect.size(), stepIntersect);
    List<Double> subtractSteps = Collections.nCopies(toSubtract.size(), stepSubtract);
    return intersectAndSubtract(toIntersect, intersectSteps, toSubtract, subtractSteps);

  }



  /**
   * Given a cone and a projection, create path plotters and project them into the given projection
   * 
   * @param cone the cone to project
   * @param step the step used to project the path
   * @param projection the projection in which to project the path
   * @return
   */
  private static Path2D.Double getConeAsPath(Cone cone, double step, Projection projection) {
    /*
     * create the path plotters
     */
    ClosedPathPlotter conePlotter = Cones.plot(cone);

    /*
     * create paths from path plotters in this projection
     */
    Path2D.Double path = projection.projectClosedPath(conePlotter, step, new Path2D.Double());
    return path;
  }

  /**
   * Given a geometry and projection, create a list of VectorIJK which are the cartesian points
   * around the boundary of the geometry
   * 
   * @param intersection
   * @param projection
   * @return
   */
  private static List<UnwritableVectorIJK> getListOfPointsFromJTS(Geometry intersection,
      Projection projection) {
    List<UnwritableVectorIJK> pts3D = Lists.newArrayList();
    /*
     * The area is in a simple cylindrical projection around the vertex. Want to invert back to 3D.
     * Loop through path iterator of the intersection - create list of Point2Ds
     */
    List<Point2D.Double> points = Lists.newArrayList();
    Coordinate[] coords = intersection.getCoordinates();
    for (Coordinate coord : coords) {
      points.add(new Point2D.Double(coord.x, coord.y));
    }

    /*
     * Convert list of projected points to list of points in 3D. these are cartesian points around
     * the Vertex of the intersection cone
     */
    for (Point2D.Double pt : points) {
      // get point as lat, lon on sphere and convert to cartesian
      Point2D.Double latlon = projection.invert(pt, new Point2D.Double());
      UnwritableVectorIJK cartesian =
          CoordConverters.convert(new LatitudinalVector(1, latlon.getY(), latlon.getX()));
      pts3D.add(cartesian);
    }

    return pts3D;
  }


  /**
   * Create a rotated simple cylindrical projection, with the given cone as a reference. That is,
   * the X axis is the interior point of cone 1 and Y axis is the K vector or J vector. Z completes
   * the frame.
   * 
   * @param cone1 the cone used to rotate the projection
   * @return
   */
  private static Projection createRotatedProjectionFromCone(Cone cone1) {
    UnwritableVectorIJK x = cone1.getInteriorPoint().createUnitized();
    UnwritableVectorIJK y = VectorIJK.K;
    VectorIJK z;
    try {
      z = VectorIJK.uCross(x, y);
    } catch (Exception e) {
      y = VectorIJK.J;
      z = VectorIJK.uCross(x, y);
    }
    y = VectorIJK.uCross(z, x);
    RotationMatrixIJK rotMat = new RotationMatrixIJK(x, y, z).transpose();
    Projection simpleProj = ProjectionBuilders.simpleCylindrical().withLowerBranch(-Math.PI)
        .withSplit(Math.PI).withOverPoleBorder(Math.toRadians(45)).build();
    return simpleProj.rotate(rotMat);
  }

  /**
   * Create a projection of the cone on a cylindrical projection. Test if the projection of the
   * supplied point lies inside the outline of the projected cone.
   * 
   * @param cone
   * @param point
   * @param step step size used to generated the projected outline of the cone
   * @return true if the point is contained within the cone.
   */
  public static boolean contains(Cone cone, UnwritableVectorIJK point, double step) {
    Projection projection = createRotatedProjectionFromCone(cone);

    ClosedPathPlotter conePlotter = Cones.plot(cone);
    Path2D.Double conePath = projection.projectClosedPath(conePlotter, step, new Path2D.Double());

    Point2D projectedPoint =
        projection.project(PathPlotters.convert(VectorIJK.subtract(point, cone.getVertex())));

    return conePath.contains(projectedPoint);
  }

  /**
   * Checks if an intersection exists between these two cones
   * 
   * @param cone1 the first cone to be intersected - the reference cone for the
   *        {@link RotatedShapeProjection}
   * @param cone2 the second cone to be intersected
   * @return boolean there is an intersection
   */
  public static boolean intersects(Cone cone1, Cone cone2) {
    double step = .01;

    /*
     * Make sure cones have vertex in the same location
     */
    Preconditions.checkArgument(cone1.getVertex().equals(cone2.getVertex()),
        "The cones to be intersected must have the same vertex");

    Projection projection = createRotatedProjectionFromCone(cone1);
    Path2D.Double cone1path = getConeAsPath(cone1, step, projection);
    Path2D.Double cone2path = getConeAsPath(cone2, step, projection);

    /*
     * convert to JTS Shapes and find intersection
     */
    Function<PathIterator, Geometry> pathToGeometryConverter =
        JtsUtilities.pathIteratorToGeometryConverter();
    Geometry jtsShape1 = pathToGeometryConverter.apply(cone1path.getPathIterator(null)).buffer(0);
    Geometry jtsShape2 = pathToGeometryConverter.apply(cone2path.getPathIterator(null)).buffer(0);

    return jtsShape1.intersects(jtsShape2);
  }



  /**
   * 
   * @param originalCone the original Cone to flip
   * @return a new Cone with a new vertex, but whose edges end at the same points as the original
   *         cone
   */
  public static Cone flipToNewVertex(Cone originalCone, UnwritableVectorIJK newVertex) {



    Cone newCone = new Cone() {
      UnwritableVectorIJK oldVertex = originalCone.getVertex();
      UnwritableVectorIJK oldInterior = originalCone.getInteriorPoint();

      UnwritableVectorIJK oldVertexToNewVertex = VectorIJK.subtract(newVertex, oldVertex);



      @Override
      public UnwritableVectorIJK getVertex() {
        return newVertex;
      }

      @Override
      public UnwritableInterval getParameterDomain() {
        return originalCone.getParameterDomain();
      }

      @Override
      public UnwritableVectorIJK getInteriorPoint() {
        return flip(oldInterior);
      }

      @Override
      public UnwritableVectorIJK getEdge(double parameter) {
        return flip(originalCone.getEdge(parameter));
      }

      private UnwritableVectorIJK flip(UnwritableVectorIJK edgeVector) {
        return VectorIJK.subtract(edgeVector, oldVertexToNewVertex);
      }

    };

    return newCone;

  }


  /**
   * Create a cone whos edges intercept a body from a cone in freespace and a body ellipsoid.
   * Intercect the given cone with the limb cone as seen from the cone vertex. Trace the edges of
   * the resulting intersected cone down onto the surface of the body. Then create a cone from this
   * resultant cone
   * 
   * @param cone the cone to trace down to the body
   * @param bodyEllipsoid the body
   * @param coneStep the step used to walk around the given cone
   * @param limbStep the step used to walk around the limb
   * @return
   */
  public static Optional<Cone> createConeOnBody(Cone cone, Ellipsoid bodyEllipsoid, double coneStep,
      double limbStep) {
    Ellipse limb = bodyEllipsoid.computeLimb(cone.getVertex(), Ellipse.create());
    // contracting the limb by a small amount to fix round off error in the
    // intersection computer
    limb.setToGenerating(limb.getCenter(), limb.getSemiMajorAxis().scale(0.9999999),
        limb.getSemiMinorAxis().scale(0.9999999));
    limb.offset(cone.getVertex().createNegated());

    /*
     * Create a limb cone and intersect with the given cone
     */
    Cone limbCone = Cones.createEllipticalCone(cone.getVertex(), limb);
    Optional<Cone> intersected = Cones.intersect(cone, coneStep, limbCone, limbStep);


    /*
     * Trace edges of resulting cone down to europa to draw the points on the body
     */
    if (intersected.isPresent()) { // empty cone = no intersection - nothing to draw
      try {
        PolygonalCone intersectedCone = (PolygonalCone) intersected.get();
        List<UnwritableVectorIJK> cartesianSCpts = intersectedCone.getCorners();
        List<UnwritableVectorIJK> ptsOnEuropa = Lists.newArrayList();
        for (UnwritableVectorIJK cartesian : Iterables.limit(cartesianSCpts,
            cartesianSCpts.size())) {
          VectorIJK thisPt =
              bodyEllipsoid.compute(intersectedCone.getVertex(), cartesian, new VectorIJK());
          ptsOnEuropa.add(thisPt);
        }
        // get interior point on Europa
        VectorIJK interior = bodyEllipsoid.compute(intersectedCone.getVertex(),
            intersectedCone.getInteriorPoint(), new VectorIJK());

        /*
         * Create a cone on the body
         */
        PolygonalCone polyCone = Cones.createPolygonalCone(VectorIJK.ZERO, interior, ptsOnEuropa);
        return Optional.of(polyCone);
      } catch (NoIntersectionException e) {
        // TODO this should never happen - throw an error?
        System.out.println("no intersection on body");
        return Optional.empty();
      }
    } else {
      // the cone doesn't intersect the body
      return Optional.empty();
    }
  }

  /**
   * Create a path plotter from a cone in freespace and a body ellipsoid. Subtract the given cone
   * from the limb cone as seen from the cone vertex. Trace the edges of the resulting intersected
   * cone down onto the surface of the body. Then create a Cone from the resulting list of points.
   * 
   * @param cone the cone to trace down to the body
   * @param bodyEllipsoid the body
   * @param coneStep the step used to walk around the given cone
   * @param limbStep the step used to walk around the limb
   * @return
   */
  public static Optional<Cone> subtractConeFromBody(Cone cone, Ellipsoid bodyEllipsoid,
      double coneStep, double limbStep) {
    UnwritableVectorIJK source = cone.getVertex();
    Ellipse limb = bodyEllipsoid.computeLimb(source, Ellipse.create());
    // contracting the limb by a small amount to fix round off error in the
    // intersection computer
    limb.setToGenerating(limb.getCenter(), limb.getSemiMajorAxis().scale(0.9999999),
        limb.getSemiMinorAxis().scale(0.9999999));
    limb.offset(source.createNegated());

    /*
     * Create a limb cone and subtract the given cone from it
     */
    Cone limbCone = Cones.createEllipticalCone(source, limb);
    List<Cone> subtractedList = Cones.subtract(limbCone, limbStep, cone, coneStep);
    if (subtractedList.size() == 0) {
      return Optional.empty();
    }
    if (subtractedList.size() != 1) {
      throw new RuntimeException("I WASNT EXPECTING THIS"); // TODO
    } else {

      Cone subtracted = subtractedList.get(0);


      try {
        PolygonalCone intersectedCone = (PolygonalCone) subtracted;
        List<UnwritableVectorIJK> cartesianSCpts = intersectedCone.getCorners();
        List<UnwritableVectorIJK> ptsOnEuropa = Lists.newArrayList();
        for (UnwritableVectorIJK cartesian : cartesianSCpts) {
          ptsOnEuropa
              .add(bodyEllipsoid.compute(intersectedCone.getVertex(), cartesian, new VectorIJK()));
        }

        // get interior point on Europa
        VectorIJK interior = bodyEllipsoid.compute(intersectedCone.getVertex(),
            intersectedCone.getInteriorPoint(), new VectorIJK());


        PolygonalCone polyCone = Cones.createPolygonalCone(VectorIJK.ZERO, interior, ptsOnEuropa);
        return Optional.of(polyCone);
      } catch (NoIntersectionException e) {
        // TODO this should never happen - throw an error?
        return Optional.empty();
      }
    }
  }

  public static Optional<Cone> union(List<Cone> toUnion, double stepSize) {
    Optional<Cone> union = Optional.of(toUnion.get(0));

    if (toUnion.size() > 1) {

      /*
       * get the middle cone to use its vertex and use as the projection cone
       */
      Cone midCone = toUnion.get(toUnion.size() / 2);

      Function<PathIterator, Geometry> pathToGeometryConverter =
          JtsUtilities.pathIteratorToGeometryConverter();

      Projection projection = createRotatedProjectionFromCone(midCone);

      List<Geometry> jtsCones = Lists.newArrayList();
      for (int iCone = 0; iCone < toUnion.size(); iCone++) {
        Path2D.Double conePath = getConeAsPath(toUnion.get(iCone), stepSize, projection);
        jtsCones.add(pathToGeometryConverter.apply(conePath.getPathIterator(null)).buffer(0));
      }

      Geometry unionGeom = CascadedPolygonUnion.union(jtsCones);
      union = Cones.createFromGeometry(unionGeom, projection, midCone.getVertex());
    }
    return union;
  }


}


