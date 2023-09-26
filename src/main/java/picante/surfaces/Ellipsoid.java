package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import java.util.List;
import java.util.stream.IntStream;
import com.google.common.collect.Lists;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class Ellipsoid implements Surface {

  private final double a;
  private final double b;
  private final double c;

  private final double minRadius;
  private final double maxRadius;

  private final EllipsoidalSurfaceNormalComputer normalComputer;
  private final EllipsoidalIntersectionComputer intersectionComputer;
  private final EllipsoidalPlaneIntersectionComputer planeIntersectionComputer;
  private final EllipsoidalLimbComputer limbComputer;

  /*
   * constants for sub routines
   */
  private static final double CONV_TOL = 1E-16;
  private static final double MARGIN = 100;
  private static final int MAX_ITER_NEARPT = 6;
  private static final int MAX_ITER_LAMBDA = 2048;

  Ellipsoid(double a, double b, double c) {
    super();
    checkArgument(a > 0, "Radius [A] of ellipsoid: %s is not strictly positive.", a);
    checkArgument(b > 0, "Radius [B] of ellipsoid: %s is not strictly positive.", b);
    checkArgument(c > 0, "Radius [C] of ellipsoid: %s is not strictly positive.", c);

    this.a = a;
    this.b = b;
    this.c = c;

    minRadius = Math.min(a, Math.min(b, c));
    maxRadius = Math.max(a, Math.max(b, c));

    this.normalComputer = new EllipsoidalSurfaceNormalComputer(a, b, c, minRadius);
    this.intersectionComputer = new EllipsoidalIntersectionComputer(a, b, c);
    this.planeIntersectionComputer = new EllipsoidalPlaneIntersectionComputer(a, b, c);
    this.limbComputer = new EllipsoidalLimbComputer(a, b, c);
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    return normalComputer.computeOutwardNormal(surfacePoint, buffer);
  }

  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {
    return intersectionComputer.intersects(source, ray);
  }

  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {
    return intersectionComputer.compute(source, ray, buffer);
  }

  public boolean intersects(UnwritablePlane plane) {
    return planeIntersectionComputer.intersects(plane);
  }

  public Ellipse intersect(UnwritablePlane plane, Ellipse buffer) {
    return planeIntersectionComputer.intersect(plane, buffer);
  }

  public Ellipse computeLimb(UnwritableVectorIJK viewPoint, Ellipse buffer) {
    return limbComputer.computeLimb(viewPoint, buffer);
  }

  /**
   * Find nearest point on a triaxial ellipsoid to a specified line
   * 
   * @param source the source of the line
   * @param ray the direction vector for the line
   */
  public PointAndDistance computeNearPoint(UnwritableVectorIJK source, UnwritableVectorIJK ray) {

    /*
     * unitize ray
     */
    UnwritableVectorIJK unitizedRay = ray.createUnitized();
    double mag = ray.getLength();

    if (mag == 0) {
      throw new RuntimeException("Line direction vector is the zero vector.");
    }

    /*
     * Scale the semi-axes lengths for better numerical behavior. If squaring any one of the scaled
     * lengths causes it to underflow to zero, we have an error. Otherwise, scale the point on the
     * input line too.
     */
    double scale = maxRadius;
    double scaleA = a / scale;
    double scaleB = b / scale;
    double scaleC = c / scale;

    if (scaleA * scaleA == 0 || scaleB * scaleB == 0 || scaleC * scaleC == 0) {
      throw new RuntimeException(
          String.format("Semi-Axis of Ellipsoid too small. A = %d, B = %d, C = %d", a, b, c));
    }

    /*
     * Scale the source point
     */
    VectorIJK scaledSource =
        new VectorIJK(source.getI() / scale, source.getJ() / scale, source.getK() / scale);


    /*
     * Hand off the intersection case to Ellipsoid.compute. Ellipsoid.compute determines whether
     * rays intersect an ellipsoid, so we treat the line as a pair of rays.
     */
    UnwritableVectorIJK oppDirection = unitizedRay.createNegated();
    Ellipsoid scaled = new Ellipsoid(scaleA, scaleB, scaleC);

    VectorIJK nearPoint = new VectorIJK();
    if (scaled.intersects(scaledSource, unitizedRay)) {
      scaled.compute(scaledSource, unitizedRay, nearPoint);
      nearPoint.scale(scale);
      return new PointAndDistance(nearPoint, 0);
    } else if (scaled.intersects(scaledSource, oppDirection)) {
      scaled.compute(scaledSource, unitizedRay, nearPoint);
      nearPoint.scale(scale);
      return new PointAndDistance(nearPoint, 0);
    }

    /*
     * Getting here means the line doesn't intersect the ellipsoid.
     * 
     * Find the candidate ellipse candEllipse. normal is a normal vector to the plane containing the
     * candidate ellipse. Mathematically the ellipse must exist, since it's the intersection of an
     * ellipsoid centered at the origin and a plane containing the origin. Only numerical problems
     * can prevent the intersection from being found.
     */
    VectorIJK normal = new VectorIJK(unitizedRay.getI() / (scaleA * scaleA),
        unitizedRay.getJ() / (scaleB * scaleB), unitizedRay.getK() / (scaleC * scaleC));
    Plane candPlane = new Plane(normal, 0);
    Ellipse candEllipse = new Ellipse();

    if (!scaled.intersects(candPlane)) {
      throw new RuntimeException("Candidate ellipse could not be found.");
    }
    scaled.intersect(candPlane, candEllipse);

    /*
     * Project the candidate ellipse onto a plane orthogonal to the line. We'll call the plane
     * projPlane and the projected ellipse projEllipse.
     */
    Plane projPlane = new Plane(unitizedRay, 0);
    Ellipse projEllipse = Ellipse.projectOnto(candEllipse, projPlane);

    /*
     * Find the point on the line lying in the projection plane, and then find the near point
     * projNearPt on the projected ellipse. Here projPt is the point on the line lying in the
     * projection plane. The distance between projPt and projNearPt is dist.
     */
    VectorIJK projPt = projPlane.projectOnto(scaledSource);
    PointAndDistance nearPtDist = projEllipse.computeNearPoint(projPt);
    UnwritableVectorIJK projNearPt = nearPtDist.getPoint();
    double dist = nearPtDist.getDistance();

    /*
     * find the near point pnear on the llipsoid by taking the inverse orthogonal projection of
     * projNearPt. This is the point on the candidate ellipse that projects to projNearPt.
     */
    VectorIJK pNear = projPlane.inverseProjectOnto(projNearPt, candPlane);

    /*
     * undo the scaling and return as a PointAndDistance pair
     */
    pNear.scale(scale);
    dist = scale * dist;
    return new PointAndDistance(pNear, dist);
  }


  /**
   * This routine (an implementation of SPICE's NEARPT) locates the point on the surface of an
   * ellipsoid that is nearest to a specified position. It also returns the altitude of the position
   * above the ellipsoid.
   */
  public PointAndDistance computeNearPoint(VectorIJK position) {

    /*
     * initialize a few vectors and values
     */
    VectorIJK bestPt = new VectorIJK();
    VectorIJK ePoint = new VectorIJK();
    VectorIJK err = new VectorIJK();
    VectorIJK errP = new VectorIJK();
    VectorIJK sPoint = new VectorIJK();
    VectorIJK term = new VectorIJK();
    VectorIJK tlambda = new VectorIJK();
    VectorIJK normal;

    double newErr;
    double height;
    double bestHt = Double.MAX_VALUE;
    double lambda = Double.MAX_VALUE;
    double oldErr = Double.MAX_VALUE;
    double sign = 0;
    double q;
    double qLower;
    double qUpper;
    double lower;
    double upper;

    int i; // loop variable
    int itr;
    boolean trim;

    /*
     * First order the axes of the ellipsoid and corresponding component of position by the size of
     * lengths of axes. Knowing which axes are smallest will simplify our task of computing lambda
     * when the time comes.
     */
    List<Double> pointL = Lists.newArrayList(position.getI(), position.getJ(), position.getK());
    List<Double> axisL = Lists.newArrayList(a, b, c);

    int[] iOrder = IntStream.range(0, 3).boxed()
        .sorted((x, y) -> axisL.get(x).compareTo(axisL.get(y))).mapToInt(ele -> ele).toArray();

    VectorIJK axis =
        new VectorIJK(axisL.get(iOrder[0]), axisL.get(iOrder[1]), axisL.get(iOrder[2]));
    VectorIJK point =
        new VectorIJK(pointL.get(iOrder[0]), pointL.get(iOrder[1]), pointL.get(iOrder[2]));
    Ellipsoid orderedEllipsoid = new Ellipsoid(axis.get(0), axis.get(1), axis.get(2));



    /*
     * Rescale everything to avoid underflows when squaring quantities and copy the original
     * starting point.
     */
    double scale = 1 / axis.getI();
    axis.scale(scale);
    point.scale(scale);
    VectorIJK original = new VectorIJK(point);

    /*
     * save the norm of the scaled input point
     */
    double pNorm = point.getLength();

    /*
     * The scaled axis lengths must be small enough so they can be squared
     */
    double tooBig = Math.sqrt(Double.MAX_VALUE / MARGIN);

    for (i = 1; i < 3; i++) {
      if (axis.get(i) > tooBig) {
        throw new RuntimeException(String
            .format("Ratio of length of axis #%s to length of axis #%s is %s; this value may cause"
                + " numeric overflow.", iOrder[i], iOrder[0], axis.get(i)));
      }
    }

    /*
     * we also must limit the size of the products
     * 
     * axis(I)*point(I), I = 1, 3
     * 
     * We can safely check these by comparing the products of the square roots of the factors to
     * TOOBIG.
     */
    for (i = 0; i < 3; i++) {
      double prodct = Math.sqrt(axis.get(i)) * Math.sqrt(Math.abs(point.get(i)));
      if (prodct > tooBig) {
        throw new RuntimeException(String.format(
            "Product of length of scaled axis #%s and size of corresponding scaled component "
                + "of position is > %s; these values may cause numeric overflow.",
            iOrder[i], tooBig * tooBig));
      }
    }

    /*
     * compute the squared lengths of the scaled axes
     */
    VectorIJK axisSquared = new VectorIJK(axis.getI() * axis.getI(), axis.getJ() * axis.getJ(),
        axis.getK() * axis.getK());

    /*
     * We will need to "solve" for the NEARPT at least 3 times. SOLUTN is the counter that keeps
     * track of how many times we have actually solved for a near point. SOLVNG indicates whether we
     * should continue solving for NEARPT.
     */
    int snglpt = 3;
    int solution = 1;
    boolean solving = true;



    while (solving) {
      VectorIJK copy = new VectorIJK(point);
      for (i = 0; i < 3; i++) {
        if (((0.5 * point.get(i) + axis.get(i)) == axis.get(i))
            || ((0.5 * point.get(i) - axis.get(i)) == -axis.get(i))) {
          point.set(i, 0.0);
        }
      }

      boolean inside = false;

      q = (point.get(0) / axis.get(0)) * (point.get(0) / axis.get(0))
          + (point.get(1) / axis.get(1)) * (point.get(1) / axis.get(1))
          + (point.get(2) / axis.get(2)) * (point.get(2) / axis.get(2)) - 1;

      if (solution == 1) {
        if (q >= 0) {
          sign = 1;
        } else {
          sign = -1;
        }
      }

      if (q == 0) {
        /*
         * In this case the point is already on the ellipsoid We simply set our bracketing values,
         * QLOWER and QUPPER, to zero so that that bisection loop won't ever get executed.
         */
        qLower = 0;
        qUpper = 0;
        lower = 0;
        upper = 0;
        lambda = 0;

        inside = false;
      } else if (q > 0) {
        /*
         * The input point is outside the ellipsoid. We want to choose our lower bracketing value so
         * that the bracketing values for lambda aren't too far apart. So we just make sure that the
         * largest term of the expression for Q isn't bigger than 4.
         */
        for (i = 0; i < 3; i++) {
          tlambda.set(i, (0.5 * Math.abs(point.get(i)) - axis.get(i)) * axis.get(i));
        }
        lower = Math.max(0, tlambda.max());

        /*
         * Choose the next value of lambda so that the largest term of Q will be no more than 1/4.
         */
        upper = 2 * Math.max(Math.abs(axis.get(0) * point.get(0)),
            Math.max(Math.abs(axis.get(1) * point.get(1)), Math.abs(axis.get(2) * point.get(2))));
        lambda = upper;

        inside = false;
      } else {
        /*
         * the point is inside the ellipsoid in this case.
         */
        inside = true;

        snglpt = 3;
        for (i = 2; i > -1; i--) {
          if (point.get(i) != 0) {
            snglpt = i;
          }
        }

        /*
         * If there is a singular point, compute LAMBDA so that the largest term of Q is equal to 4.
         */
        if (snglpt <= 2) {
          for (i = 0; i < 3; i++) {

            if (point.get(i) == 0) {
              tlambda.set(i, -axisSquared.get(2));
            } else {
              tlambda.set(i, axis.get(i) * (0.5 * Math.abs(point.get(i)) - axis.get(i)));
            }
          }
          lambda = tlambda.max();
          lower = lambda;
          upper = Math.max(lower, 0.);
        } else {
          /*
           * the point must be at the origin. The closest point is at the end of the shortest
           * semi-major axis.
           */
          if (copy.get(0) < 0) {
            point.set(0, -axis.get(0));
            copy.set(0, -axis.get(0));
          } else {
            point.set(0, axis.get(0));
            copy.set(0, axis.get(0));
          }

          copy.set(1, 0);
          copy.set(2, 0);

          upper = 0;
          lower = 0;
          lambda = 0;
          q = 0;

          inside = false;
        }
      }

      /*
       * now compute the value of q at the two bracketing values of lambda
       */
      double denom;
      double denom2;
      double denom3;
      double factor;
      for (i = 0; i < 3; i++) {
        if (point.get(i) == 0) {
          term.set(i, 0);
        } else {
          denom = axis.get(i) + (lambda / axis.get(i));
          trim = 0.5 * Math.abs(point.get(i)) > denom;
          if (inside && trim) {
            factor = 2;
          } else {
            if (denom == 0) {
              throw new RuntimeException(String.format("axis(%s) + lambda/axis(%s)", i, i));
            }
            factor = point.get(i) / denom;
          }
          term.set(i, factor * factor);
        }
      }

      if (!inside) {
        qLower = q;
        qUpper = term.get(0) + term.get(1) + term.get(2) - 1;
      } else {
        qUpper = q;
        qLower = term.get(0) + term.get(1) + term.get(2) - 1;
      }

      /*
       * bracket q lower and q upper
       */
      qLower = Math.max(0, qLower);
      qUpper = Math.min(0, qUpper);

      lambda = upper;
      q = qUpper;

      /*
       * Refine the estimate of lambda
       */
      itr = 0;

      while (upper - lower > 0) {

        itr = itr + 1;
        if (itr > MAX_ITER_LAMBDA) {
          throw new RuntimeException("Iteration limit # exceeded in NEARPT.");
        }

        /*
         * bracket lower, qlower, and qupper
         */
        lower = Math.min(lower, upper);
        qLower = Math.max(0, qLower);
        qUpper = Math.min(0, qUpper);

        if (q == 0) {
          // we've found the root
          lower = lambda;
          upper = lambda;
        } else {
          if (q < 0) {
            upper = lambda;
            qUpper = q;
          } else {
            lower = lambda;
            qLower = q;
          }
          lambda = 0.5 * lower + 0.5 * upper;
          lambda = bracket(lambda, lower, upper);
        }
        if (approx(lambda, lower, CONV_TOL) || approx(lambda, upper, CONV_TOL)) {
          if (Math.abs(qLower) < Math.abs(qUpper)) {
            upper = lower;
          } else {
            lower = upper;
          }
        }

        /*
         * If LOWER and UPPER aren't the same, we compute the value of Q at our new guess for
         * LAMBDA.
         */
        if (upper - lower > 0) {
          for (i = 0; i < 3; i++) {
            if (point.get(i) == 0) {
              term.set(i, 0);
            } else {
              denom = axis.get(i) + lambda / axis.get(i);
              trim = 0.5 * Math.abs(point.get(i)) > denom;
              if (inside && trim) {
                factor = 2;
              } else {
                if (denom == 0) {
                  throw new RuntimeException(String.format("axis(%s) + lambda/axis(%s)", i, i));
                }
                factor = point.get(i) / denom;
              }
              term.set(i, factor * factor);
            }
          }
          q = term.get(0) + term.get(1) + term.get(2) - 1;
        }
      }

      lambda = lower;
      for (i = 0; i < 3; i++) {
        if (point.get(i) == 0) {
          sPoint.set(i, 0);
        } else {
          denom = 1 + lambda / axisSquared.get(i);
          if (denom <= 0) {
            throw new RuntimeException(
                String.format("Denominator in expression for SPOINT(%s) is %s.", i, denom));
          }
          sPoint.set(i, copy.get(i) / denom);
        }
      }

      /*
       * Handling points on the central plane
       */
      if (inside && (snglpt == 1 || snglpt == 2)) {

        boolean extra = false;
        double temp;

        if ((axis.get(0) != axis.get(snglpt))
            && Math.abs(point.get(1)) <= axis.get(1) - axisSquared.get(0) / axis.get(1)
            && Math.abs(point.get(2)) <= axis.get(2) - axisSquared.get(0) / axis.get(2)) {
          if (axis.get(0) == axis.get(1)) {
            /*
             * prolate case
             */
            denom3 = 1 - axisSquared.get(0) / axisSquared.get(2);
            if (denom3 > 0) {
              ePoint.set(1, 0);
              ePoint.set(2, point.get(2) / denom3);

              temp = 1 - (ePoint.get(1) / axis.get(1)) * (ePoint.get(1) / axis.get(1))
                  - (ePoint.get(2) / axis.get(2)) * (ePoint.get(2) / axis.get(2));
              if (temp > 0) {
                ePoint.set(0, axis.get(0) * Math.sqrt(Math.max(0, temp)));
                extra = true;
              }
            }
          } else {
            /*
             * triaxial case
             */
            denom2 = 1 - axisSquared.get(0) / axisSquared.get(1);
            denom3 = 1 - axisSquared.get(0) / axisSquared.get(2);

            if (denom2 > 0 && denom3 > 0) {

              ePoint.set(1, point.get(1) / denom2);
              ePoint.set(2, point.get(2) / denom3);

              temp = 1 - (ePoint.get(1) / axis.get(1)) * (ePoint.get(1) / axis.get(1))
                  - (ePoint.get(2) / axis.get(2)) * (ePoint.get(2) / axis.get(2));

              if (temp > 0) {
                ePoint.set(0, axis.get(0) * Math.sqrt(temp));
                extra = true;
              }
            }
          }
        }

        if (extra) {
          if (ePoint.getDistance(point) < sPoint.getDistance(point)) {
            sPoint = ePoint;
          }
        }
      }

      if (solution == 1) {
        point.setTo(sPoint);
        bestPt.setTo(sPoint);
        bestHt = bestPt.getDistance(original);
      } else if (solution == 2) {
        height = sign * sPoint.getDistance(original);
        normal = orderedEllipsoid.computeOutwardNormal(sPoint);

        for (i = 0; i < 3; i++) {
          err.set(i, original.get(i) - sPoint.get(i) - height * normal.get(i));
        }


        /*
         * Find the component of the error vector that is perpendicular to the normal, and shift our
         * solution point by this component.
         */
        VectorIJK.planeProject(err, normal, errP);


        /*
         * the sign of the original point's altitude tells us whether the point is outside the
         * ellipsoid
         */
        if (sign >= 0) {
          if (pNorm == 0) {
            throw new RuntimeException("Norm of the scaled point is 0.");
          }
          errP.scale(sPoint.getLength() / pNorm);
        }

        VectorIJK.add(sPoint, errP, point);

        oldErr = err.getLength();
        bestHt = height;

        /*
         * store the current solution point
         */
        bestPt.setTo(sPoint);
      } else if (solution > 2) {
        height = sign * sPoint.getDistance(original);
        normal = orderedEllipsoid.computeOutwardNormal(sPoint);

        for (i = 0; i < 3; i++) {
          err.set(i, original.get(i) - sPoint.get(i) - height * normal.get(i));
        }
        newErr = err.getLength();

        if (newErr < oldErr) {
          oldErr = newErr;
          bestHt = height;
          bestPt.setTo(sPoint);

          if (solution <= MAX_ITER_LAMBDA) {
            VectorIJK.planeProject(err, normal, errP);

            if (sign >= 0) {
              if (pNorm == 0) {
                throw new RuntimeException("Norm of the scaled point is 0.");
              }
              errP.scale(sPoint.getLength() / pNorm);
            }
            VectorIJK.add(sPoint, errP, point);
          }
        } else {
          solving = false;
        }
      }

      solution = solution + 1;
      solving = solving && solution <= MAX_ITER_NEARPT;

    }

    /*
     * rescale and reorder the compoinents of the solution point. Scale and copy the value of bestht
     * into the output argument
     */

    bestPt.scale(1 / scale);

    VectorIJK nearPt = new VectorIJK();
    for (i = 0; i < 3; i++) {
      nearPt.set(iOrder[i], bestPt.get(i));
    }

    double alt = bestHt / scale;

    return new PointAndDistance(nearPt, alt);
  }

  /**
   * Bracket a number. That is, given a number and an acceptable interval, make sure that the number
   * is contained in the interval. (If the number is already in the interval, leave it alone. If
   * not, set it to the nearest endpoint of the interval.)
   * 
   * @param d
   * @param end1
   * @param end2
   * @return
   */
  private double bracket(double d, double end1, double end2) {
    if (end1 < end2) {
      return Math.max(end1, Math.min(end2, d));
    } else {
      return Math.max(end2, Math.min(end1, d));
    }
  }

  /**
   * True if two double precision numbers are equal to within some tolerance.
   */
  private boolean approx(double x, double y, double tol) {
    return Math.abs(x - y) <= tol;
  }



  /**
   * Determines if the supplied location is interior to the surface
   * 
   * @param location the supplied location
   * @return true if location is interior
   */
  public boolean isInterior(UnwritableVectorIJK location) {
    double x = location.getI();
    double y = location.getJ();
    double z = location.getK();

    double value = x * x / (a * a) + y * y / (b * b) + z * z / (c * c);

    return value < 1;
  }

  public VectorIJK getRadii(VectorIJK buffer) {
    return buffer.setTo(a, b, c);
  }

  public double getA() {
    return a;
  }

  public double getB() {
    return b;
  }

  public double getC() {
    return c;
  }

  public double getMinRadius() {
    return minRadius;
  }

  public double getMaxRadius() {
    return maxRadius;
  }



  public static class PointAndDistance {

    private final UnwritableVectorIJK point;
    private final double distance;

    public PointAndDistance(UnwritableVectorIJK zero, double distance) {
      super();
      this.point = UnwritableVectorIJK.copyOf(zero);
      this.distance = distance;
    }

    public UnwritableVectorIJK getPoint() {
      return point;
    }

    public double getDistance() {
      return distance;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      long temp;
      temp = Double.doubleToLongBits(distance);
      result = prime * result + (int) (temp ^ (temp >>> 32));
      result = prime * result + ((point == null) ? 0 : point.hashCode());
      return result;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      PointAndDistance other = (PointAndDistance) obj;
      if (Double.doubleToLongBits(distance) != Double.doubleToLongBits(other.distance)) {
        return false;
      }
      if (point == null) {
        if (other.point != null) {
          return false;
        }
      } else if (!point.equals(other.point)) {
        return false;
      }
      return true;
    }
  }
}
