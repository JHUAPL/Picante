package picante.math.functions;

import static picante.math.PicanteMath.abs;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public class DifferentiableVectorIJKFunctions {

  /**
   * Constructor is private to block instantiations.
   */
  private DifferentiableVectorIJKFunctions() {}

  public static DifferentiableVectorIJKFunction create(final UnwritableVectorIJK constant) {
    return new DifferentiableVectorIJKFunction() {

      private final UnwritableVectorIJK value = UnwritableVectorIJK.copyOf(constant);

      @Override
      public VectorIJK evaluate(@SuppressWarnings("unused") double t, VectorIJK buffer) {
        return buffer.setTo(value);
      }

      @Override
      public VectorIJK differentiate(@SuppressWarnings("unused") double t, VectorIJK buffer) {
        return buffer.setTo(VectorIJK.ZERO);
      }
    };
  }

  public static DifferentiableVectorIJKFunction create(final DifferentiableUnivariateFunction i,
      final DifferentiableUnivariateFunction j, final DifferentiableUnivariateFunction k) {

    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return buffer.setTo(i.evaluate(t), j.evaluate(t), k.evaluate(t));
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        return buffer.setTo(i.differentiate(t), j.differentiate(t), k.differentiate(t));
      }
    };

  }

  public static VectorIJKFunction velocity(final DifferentiableVectorIJKFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.differentiate(t, buffer);
      }
    };
  }

  public static DifferentiableVectorIJKFunction unitize(
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).unitize();
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = new VectorIJK();
        function.evaluate(t, internalBuffer);
        double length = internalBuffer.getLength();
        internalBuffer.unitize();
        VectorIJK.planeProject(function.differentiate(t, buffer), internalBuffer, buffer);
        return buffer.scale(1.0 / length);
      }
    };
  }

  public static DifferentiableVectorIJKFunction add(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.add(a.evaluate(t, new VectorIJK()), b.evaluate(t, buffer), buffer);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        return VectorIJK.add(a.differentiate(t, new VectorIJK()), b.differentiate(t, buffer),
            buffer);
      }
    };
  }

  public static DifferentiableVectorIJKFunction subtract(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.subtract(a.evaluate(t, new VectorIJK()), b.evaluate(t, buffer), buffer);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        return VectorIJK.subtract(a.differentiate(t, new VectorIJK()), b.differentiate(t, buffer),
            buffer);
      }
    };
  }

  public static DifferentiableVectorIJKFunction multiply(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = new VectorIJK();
        a.evaluate(t, internalBuffer);
        b.evaluate(t, buffer);
        return buffer.setTo(internalBuffer.getI() * buffer.getI(),
            internalBuffer.getJ() * buffer.getJ(), internalBuffer.getK() * buffer.getK());
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = new VectorIJK();
        a.evaluate(t, internalBuffer);
        b.differentiate(t, buffer);
        double i = internalBuffer.getI() * buffer.getI();
        double j = internalBuffer.getJ() * buffer.getJ();
        double k = internalBuffer.getK() * buffer.getK();
        a.differentiate(t, internalBuffer);
        b.evaluate(t, buffer);
        return buffer.setTo(i + internalBuffer.getI() * buffer.getI(),
            j + internalBuffer.getJ() * buffer.getJ(), k + internalBuffer.getK() * buffer.getK());
      }
    };
  }

  // public static DifferentiableVectorIJKFunction divide(
  // final DifferentiableVectorIJKFunction a,
  // final DifferentiableVectorIJKFunction b) {
  // throw new UnsupportedOperationException();
  // }

  public static DifferentiableVectorIJKFunction compose(final DifferentiableVectorIJKFunction a,
      final DifferentiableUnivariateFunction b) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return a.evaluate(b.evaluate(t), buffer);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        a.differentiate(b.evaluate(t), buffer);
        return buffer.scale(b.differentiate(t));
      }
    };
  }

  // public static DifferentiableVectorIJKFunction compose(
  // final DifferentiableVectorIJKFunction a,
  // final DifferentiableVectorIJKFunction b) {
  // throw new UnsupportedOperationException();
  // }

  // public static DifferentiableVectorIJKFunction compose(
  // final DifferentiableUnivariateFunction a,
  // final DifferentiableVectorIJKFunction b) {
  // throw new UnsupportedOperationException();
  // }

  public static DifferentiableVectorIJKFunction negate(
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).negate();
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        return function.differentiate(t, buffer).negate();
      }
    };
  }

  public static DifferentiableVectorIJKFunction scale(final double scale,
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).scale(scale);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        return function.differentiate(t, buffer).scale(scale);
      }
    };
  }

  public static DifferentiableVectorIJKFunction scale(final DifferentiableUnivariateFunction scale,
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).scale(scale.evaluate(t));
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = new VectorIJK();
        function.differentiate(t, internalBuffer);
        internalBuffer.scale(scale.evaluate(t));

        function.evaluate(t, buffer);
        buffer.scale(scale.differentiate(t));

        return VectorIJK.add(internalBuffer, buffer, buffer);
      }
    };
  }

  public static DifferentiableVectorIJKFunction scale(final UnwritableVectorIJK scale,
      final DifferentiableVectorIJKFunction function) {
    return multiply(create(scale), function);
  }

  // public static DifferentiableVectorIJKFunction rotate(
  // final DifferentiableVectorIJKFunction function,
  // final UnwritableVectorIJK axis, final double angle) {
  // return rotate(function, create(axis),
  // DifferentiableUnivariateFunctions.create(angle));
  // }

  // public static DifferentiableVectorIJKFunction rotate(
  // final DifferentiableVectorIJKFunction function,
  // final UnwritableVectorIJK axis,
  // final DifferentiableUnivariateFunction angle) {
  // return rotate(function, create(axis), angle);
  // }

  // public static DifferentiableVectorIJKFunction rotate(
  // final DifferentiableVectorIJKFunction function,
  // final DifferentiableVectorIJKFunction axis, final double angle) {
  // return rotate(function, axis,
  // DifferentiableUnivariateFunctions.create(angle));
  // }

  // public static DifferentiableVectorIJKFunction rotate(
  // final DifferentiableVectorIJKFunction function,
  // final DifferentiableVectorIJKFunction axis,
  // final DifferentiableUnivariateFunction angle) {
  // throw new UnsupportedOperationException();
  // }

  // public static DifferentiableVectorIJKFunction planeProject(
  // final DifferentiableVectorIJKFunction function,
  // final UnwritableVectorIJK normal) {
  // return planeProject(function, create(normal));
  // }

  // public static DifferentiableVectorIJKFunction planeProject(
  // final DifferentiableVectorIJKFunction function,
  // final DifferentiableVectorIJKFunction normal) {
  // throw new UnsupportedOperationException();
  // }

  // public static DifferentiableVectorIJKFunction project(
  // final DifferentiableVectorIJKFunction function,
  // final UnwritableVectorIJK onto) {
  // return project(function, create(onto));
  // }

  // public static DifferentiableVectorIJKFunction project(
  // final DifferentiableVectorIJKFunction function,
  // final DifferentiableVectorIJKFunction onto) {
  // throw new UnsupportedOperationException();
  // }

  // public static DifferentiableVectorIJKFunction combine(
  // final DifferentiableUnivariateFunction scaleA,
  // final DifferentiableVectorIJKFunction a,
  // final DifferentiableUnivariateFunction scaleB,
  // final DifferentiableVectorIJKFunction b) {
  // return new AbstractImplementation() {
  //
  // @Override
  // public VectorIJK evaluate(double t, VectorIJK buffer) {
  // return VectorIJK.combine(scaleA.evaluate(t),
  // a.evaluate(t, getInternalBuffer()), scaleB.evaluate(t),
  // b.evaluate(t, buffer), buffer);
  // }
  //
  // @Override
  // public VectorIJK differentiate(double t, VectorIJK buffer) {
  //
  // return null;
  // }
  // };
  // }

  // public static DifferentiableVectorIJKFunction combine(
  // final DifferentiableUnivariateFunction scaleA,
  // final DifferentiableVectorIJKFunction a,
  // final DifferentiableUnivariateFunction scaleB,
  // final DifferentiableVectorIJKFunction b,
  // final DifferentiableUnivariateFunction scaleC,
  // final DifferentiableVectorIJKFunction c) {
  // throw new UnsupportedOperationException();
  // }

  public static DifferentiableVectorIJKFunction uCross(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return unitize(cross(a, b));
  }

  public static DifferentiableVectorIJKFunction cross(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.cross(a.evaluate(t, new VectorIJK()), b.evaluate(t, buffer), buffer);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = new VectorIJK();
        VectorIJK otherBuffer = new VectorIJK();
        VectorIJK.cross(a.differentiate(t, internalBuffer), b.evaluate(t, otherBuffer), buffer);
        VectorIJK.cross(a.evaluate(t, internalBuffer), b.differentiate(t, otherBuffer),
            otherBuffer);
        return VectorIJK.add(buffer, otherBuffer, buffer);
      }
    };
  }

  public static DifferentiableUnivariateFunction dot(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        VectorIJK internalBuffer = new VectorIJK();
        VectorIJK otherBuffer = new VectorIJK();
        a.evaluate(t, internalBuffer);
        b.evaluate(t, otherBuffer);
        return internalBuffer.getDot(otherBuffer);
      }

      @Override
      public double differentiate(double t) {
        VectorIJK internalBuffer = new VectorIJK();
        VectorIJK otherBuffer = new VectorIJK();
        a.evaluate(t, internalBuffer);
        b.differentiate(t, otherBuffer);
        double result = internalBuffer.getDot(otherBuffer);
        a.differentiate(t, internalBuffer);
        b.evaluate(t, otherBuffer);
        return result + internalBuffer.getDot(otherBuffer);
      }
    };
  }

  public static DifferentiableUnivariateFunction length(
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, new VectorIJK()).getLength();
      }

      @Override
      public double differentiate(double t) {
        VectorIJK internalBuffer = new VectorIJK();
        VectorIJK otherBuffer = new VectorIJK();
        function.evaluate(t, internalBuffer);
        function.differentiate(t, otherBuffer);
        return internalBuffer.getDot(otherBuffer) / internalBuffer.getLength();
      }
    };
  }

  public static DifferentiableUnivariateFunction separation(final DifferentiableVectorIJKFunction a,
      final DifferentiableVectorIJKFunction b) {
    return new DifferentiableUnivariateFunction() {

      /*
       * This isn't the most efficient way to handle this, but it works for now.
       */
      private final DifferentiableVectorIJKFunction aHat = unitize(a);
      private final DifferentiableVectorIJKFunction bHat = unitize(b);
      private final UnivariateFunction normAHatCrossBHat =
          VectorIJKFunctions.length(VectorIJKFunctions.cross(aHat, bHat));
      private final UnivariateFunction diffAHatDotBHat =
          DifferentiableUnivariateFunctions.derivative(dot(aHat, bHat));

      @Override
      public double evaluate(double t) {
        return a.evaluate(t, new VectorIJK()).getSeparation(b.evaluate(t, new VectorIJK()));
      }

      @Override
      public double differentiate(double t) {
        double denominator = normAHatCrossBHat.evaluate(t);

        /*
         * Handle the parallel and anti-parallel cases.
         */
        if (denominator == 0.0) {
          return 0.0;
        }

        double numerator = diffAHatDotBHat.evaluate(t);

        /*
         * Check for numeric overflow and throw a runtime exception if there's a problem. Before
         * attempting the division, verify that: denominator > fudge factor * | numerator | /
         * Double.MAX_VALUE. A fudge factor of 10 should be sufficient, and note that the inequality
         * is strict.
         */
        boolean safe = denominator > 10.0 * abs(numerator) / Double.MAX_VALUE;

        if (!safe) {
          throw new UnsupportedOperationException(
              "Unable to compute derivative, numeric overflow has occurred.");
        }

        return -numerator / denominator;
      }
    };
  }

  public static DifferentiableUnivariateFunction extractI(
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, new VectorIJK()).getI();
      }

      @Override
      public double differentiate(double t) {
        return function.differentiate(t, new VectorIJK()).getI();
      }
    };
  }

  public static DifferentiableUnivariateFunction extractJ(
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, new VectorIJK()).getJ();
      }

      @Override
      public double differentiate(double t) {
        return function.differentiate(t, new VectorIJK()).getJ();
      }
    };
  }

  public static DifferentiableUnivariateFunction extractK(
      final DifferentiableVectorIJKFunction function) {
    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, new VectorIJK()).getK();
      }

      @Override
      public double differentiate(double t) {
        return function.differentiate(t, new VectorIJK()).getK();
      }
    };
  }

  /**
   * Creates a function that estimates the derivative by utilizing a quadratic approximating
   * function over some small, delta interval surrounding the value of interest.
   * <p>
   * This is the same as simply averaging the forward and backward derivative estimates usually
   * utilized to numerically estimate derivatives.
   * </p>
   * 
   * @param function the function to differentiate numerically
   * @param deltaT the delta in the domain argument to step away from the point of interest in
   *        evaluating the derivative
   * 
   * @return a newly created differentiable vector function that differentiates a quadratic
   *         approximate to estimate the derivative
   */
  public static DifferentiableVectorIJKFunction quadraticApproximation(
      final VectorIJKFunction function, final double deltaT) {

    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {

        /*
         * Approximate the derivative numerically by assuming a quadratic approximation. Evaluate
         * the function at t-deltaT and t+deltaT, combine the results to produce the derivative
         * estimate.
         */
        VectorIJK tPlus = function.evaluate(t + deltaT, new VectorIJK());
        function.evaluate(t - deltaT, buffer);

        return VectorIJK.combine(0.5 / deltaT, tPlus, -0.5 / deltaT, buffer, buffer);
      }
    };
  }
}
