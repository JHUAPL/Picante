package picante.math.functions;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Class of static utility methods for manipulating {@link VectorIJKFunction}s.
 */
public class VectorIJKFunctions {

  /**
   * Private class that captures a univariate function with one internal buffer.
   */
  private static abstract class AbstractFunction implements UnivariateFunction {
    private final VectorIJK internalBuffer = new VectorIJK();

    VectorIJK getInternalBuffer() {
      return internalBuffer;
    }
  }

  /**
   * Private class that captures a univariate function with two internal buffers.
   */
  private static abstract class AbstractFunctionTwo extends AbstractFunction {
    private final VectorIJK otherBuffer = new VectorIJK();

    VectorIJK getOtherBuffer() {
      return otherBuffer;
    }
  }

  /**
   * Private class that captures a vector function with one internal buffer.
   */
  private static abstract class AbstractImplementation implements VectorIJKFunction {
    private final VectorIJK internalBuffer = new VectorIJK();

    VectorIJK getInternalBuffer() {
      return internalBuffer;
    }
  }

  /**
   * Private class that captures a vector function with two internal buffers.
   */
  private static abstract class AbstractImplementationTwo extends AbstractImplementation {
    private final VectorIJK otherBuffer = new VectorIJK();

    VectorIJK getOtherBuffer() {
      return otherBuffer;
    }
  }

  /**
   * Constructor is private to block instantiation.
   */
  private VectorIJKFunctions() {}

  /**
   * Creates a vector function that evaluates to a constant vector.
   * <p>
   * Note: the constant is copied into a local buffer in the returned function, so it is not a view.
   * </p>
   * 
   * @param constant a vector defining the constant value
   * 
   * @return a newly created function that includes a copy of the supplied constant vector, not a
   *         view.
   */
  public static VectorIJKFunction create(final UnwritableVectorIJK constant) {
    return new VectorIJKFunction() {

      private final UnwritableVectorIJK value = UnwritableVectorIJK.copyOf(constant);

      @Override
      public VectorIJK evaluate(@SuppressWarnings("unused") double t, VectorIJK buffer) {
        return buffer.setTo(value);
      }
    };

  }

  /**
   * Creates a vector function from three separate functions
   * 
   * @param i the function defining the ith component
   * @param j the function defining the jth component
   * @param k the function defining the kth component
   * 
   * @return a newly created function that uses the three supplied functions to define a vector
   *         function
   */
  public static VectorIJKFunction create(final UnivariateFunction i, final UnivariateFunction j,
      final UnivariateFunction k) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return buffer.setTo(i.evaluate(t), j.evaluate(t), k.evaluate(t));
      }
    };
  }

  /**
   * Creates a vector function by unitizing the supplied function.
   * <p>
   * Note: if the function ever evaluates to {@link VectorIJK#ZERO} then the generated function will
   * throw {@link UnsupportedOperationException}
   * </p>
   * 
   * @param function the function to unitize
   * 
   * @return a unitized version of the supplied vector function, that computes
   *         <code>a / ||a||</code>
   */
  public static VectorIJKFunction unitize(final VectorIJKFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).unitize();
      }
    };
  }

  /**
   * Creates a vector function by adding the two supplied functions.
   * 
   * @param a a vector function
   * @param b another vector function
   * 
   * @return a newly created vector function that computes the component-wise sum
   *         <code>( a + b )</code>
   */
  public static VectorIJKFunction add(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = getInternalBuffer();
        a.evaluate(t, internalBuffer);
        b.evaluate(t, buffer);
        return VectorIJK.add(internalBuffer, buffer, buffer);
      }
    };
  }

  /**
   * Creates a vector function by subtracting the two supplied functions
   * 
   * @param a a vector function, the minuend
   * @param b another vector function, the subtrahend
   * 
   * @return a newly created vector function that computes the difference <code>( a - b )</code>
   */
  public static VectorIJKFunction subtract(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = getInternalBuffer();
        a.evaluate(t, internalBuffer);
        b.evaluate(t, buffer);
        return VectorIJK.subtract(internalBuffer, buffer, buffer);
      }
    };
  }

  /**
   * Creates a vector function by multiplying the components of the supplied functions
   * 
   * @param a a vector function
   * @param b another vector function
   * 
   * @return a newly created function that computes the component-wise product of a and b
   */
  public static VectorIJKFunction multiply(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = getInternalBuffer();
        a.evaluate(t, internalBuffer);
        b.evaluate(t, buffer);
        return buffer.setTo(internalBuffer.getI() * buffer.getI(),
            internalBuffer.getJ() * buffer.getJ(), internalBuffer.getK() * buffer.getK());
      }
    };
  }

  /**
   * Creates a vector function by dividing the components of the supplied functions
   * 
   * @param a a vector function, the dividends
   * @param b another vector function, the divisors
   * 
   * @return a newly created function that computes the component-wise quotient of
   *         <code> a / b </code>
   */
  public static VectorIJKFunction divide(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = getInternalBuffer();
        a.evaluate(t, internalBuffer);
        b.evaluate(t, buffer);
        return buffer.setTo(internalBuffer.getI() / buffer.getI(),
            internalBuffer.getJ() / buffer.getJ(), internalBuffer.getK() / buffer.getK());
      }
    };
  }

  /**
   * Creates a vector function by composing the vector function with the univariate function.
   * 
   * @param a the vector function
   * @param b the univariate function
   * 
   * @return a newly created function that computes the composition <code>( a(b(t)) )</code>
   */
  public static VectorIJKFunction compose(final VectorIJKFunction a, final UnivariateFunction b) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return a.evaluate(b.evaluate(t), buffer);
      }
    };
  }

  /**
   * Creates a vector function by composing one vector function with another, component-wise.
   * 
   * @param a a vector function
   * @param b another vector function
   * 
   * @return a newly created function that computes the composition, component-wise of a with b.
   */
  public static VectorIJKFunction compose(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementationTwo() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = getInternalBuffer();
        VectorIJK otherBuffer = getOtherBuffer();
        b.evaluate(t, internalBuffer);
        a.evaluate(internalBuffer.getI(), otherBuffer);
        buffer.setI(otherBuffer.getI());
        a.evaluate(internalBuffer.getJ(), otherBuffer);
        buffer.setJ(otherBuffer.getJ());
        a.evaluate(internalBuffer.getK(), otherBuffer);
        buffer.setK(otherBuffer.getK());
        return buffer;
      }
    };

  }

  /**
   * Creates a vector function by composing, component-wise a univariate function with each element
   * of the vector.
   * 
   * @param a a univariate function
   * @param b the vector function
   * 
   * @return a newly created function that computes <code>( a(b(t)) )</code>
   */
  public static VectorIJKFunction compose(final UnivariateFunction a, final VectorIJKFunction b) {
    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK internalBuffer = getInternalBuffer();
        b.evaluate(t, internalBuffer);
        return buffer.setTo(a.evaluate(internalBuffer.getI()), a.evaluate(internalBuffer.getJ()),
            a.evaluate(internalBuffer.getK()));
      }
    };
  }

  /**
   * Creates a vector function that is the negative of another.
   * 
   * @param function the function to negate.
   * 
   * @return a newly created function that computes <code>-function</code>
   */
  public static VectorIJKFunction negate(final VectorIJKFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).negate();
      }
    };
  }

  /**
   * Creates a vector function scaled by a single scalar value
   * 
   * @param scale the scalar multiple
   * @param function the vector function
   * 
   * @return a newly created function that computes <code>scale * function</code>
   */
  public static VectorIJKFunction scale(final double scale, final VectorIJKFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).scale(scale);
      }
    };
  }

  /**
   * Creates a vector function scaled by a single scalar function
   * 
   * @param scale the scalar function
   * @param function the vector function
   * 
   * @return a newly created function that computes <code>scale(t) * function</code>
   */
  public static VectorIJKFunction scale(final UnivariateFunction scale,
      final VectorIJKFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, buffer).scale(scale.evaluate(t));
      }
    };
  }

  /**
   * Creates a vector function scaled component-wise by a vector
   * <p>
   * Note: the function copies the supplied vector, so it is not a view.
   * </p>
   * 
   * @param scale the vector to component-wise multiply against the function
   * @param function the function
   * 
   * @return a newly created function that computes the component-wise product of function with the
   *         scale
   */
  public static VectorIJKFunction scale(final UnwritableVectorIJK scale,
      final VectorIJKFunction function) {
    return multiply(create(scale), function);
  }

  /**
   * Creates a vector function from an existing function by rotation by angle about an axis
   * <p>
   * Note: a copy of the supplied axis is made and stored in the returned function, so it is not a
   * view.
   * </p>
   * 
   * @param function the vector function to rotate
   * @param axis the axis about which to rotate
   * @param angle the angle of the rotation, in radians
   * 
   * @return a newly created function which rotates the original vector function by the supplied
   *         axis and angle.
   */
  public static VectorIJKFunction rotate(final VectorIJKFunction function,
      final UnwritableVectorIJK axis, final double angle) {
    return rotate(function, create(axis), UnivariateFunctions.create(angle));
  }

  public static VectorIJKFunction rotate(final UnwritableVectorIJK vector,
      final UnwritableVectorIJK axis, final UnivariateFunction angle) {
    return rotate(create(vector), create(axis), angle);
  }

  /**
   * Creates a vector function from an existing function by rotation about an axis by the specified
   * angle function
   * 
   * @param function the vector function to rotate
   * @param axis the constant axis about which to rotate
   * @param angle a function describing the angle of rotation, in radians
   * 
   * @return a newly created function which rotates the original vector function by the supplied
   *         axis with the angle function
   */
  public static VectorIJKFunction rotate(final VectorIJKFunction function,
      final UnwritableVectorIJK axis, final UnivariateFunction angle) {
    return rotate(function, create(axis), angle);
  }

  /**
   * Creates a vector function from an existing function by rotation about a specified axis function
   * and a constant angle
   * 
   * @param function the vector function to rotate
   * @param axis the vector function defining the axis of rotation
   * @param angle the constant angle, in radians
   * 
   * @return a newly created function that rotates the original vector function by the axis provided
   *         by another vector function and a constant angle
   */
  public static VectorIJKFunction rotate(final VectorIJKFunction function,
      final VectorIJKFunction axis, final double angle) {
    return rotate(function, axis, UnivariateFunctions.create(angle));
  }

  /**
   * Creates a vector function from an existing function by rotation about a specified axis function
   * and angle function
   * 
   * @param function the vector function to rotate
   * @param axis the vector function defining the axis of rotation
   * @param angle the function providing the angle, in radians
   * 
   * @return a newly created function that rotates the original vector function by the axis provided
   *         by another vector function and an angle function
   */
  public static VectorIJKFunction rotate(final VectorIJKFunction function,
      final VectorIJKFunction axis, final UnivariateFunction angle) {
    return new AbstractImplementationTwo() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.rotate(function.evaluate(t, getInternalBuffer()),
            axis.evaluate(t, getOtherBuffer()), angle.evaluate(t), buffer);
      }
    };
  }

  /**
   * Creates a vector function from an existing function by projecting the existing function into
   * the plane normal to the supplied normal vector.
   * <p>
   * A copy of the supplied normal vector is created, so the function returned does not provide a
   * view
   * </p>
   * 
   * @param function the vector function to project
   * @param normal the normal vector
   * 
   * @return a newly created vector function that projects the supplied function onto the plane
   *         normal to the supplied vector
   */
  public static VectorIJKFunction planeProject(final VectorIJKFunction function,
      final UnwritableVectorIJK normal) {
    return planeProject(function, create(normal));
  }

  /**
   * Creates a vector function from an existing function by projecting the existing function onto
   * the plane normal to vectors provided by another function
   * 
   * @param function the vector function to project
   * @param normal the function defining the normal to the plane
   * 
   * @return a newly created vector function that projects the supplied function onto the plane
   *         normal to the supplied vector
   */
  public static VectorIJKFunction planeProject(final VectorIJKFunction function,
      final VectorIJKFunction normal) {

    return new AbstractImplementationTwo() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.planeProject(function.evaluate(t, getInternalBuffer()),
            normal.evaluate(t, getOtherBuffer()), buffer);
      }
    };

  }

  /**
   * Creates a vector function from an existing function by projecting the existing function onto a
   * vector.
   * <p>
   * Note: a copy of the supplied vector is created, so the function returned does not provide a
   * view
   * </p>
   * 
   * @param function the vector function to project
   * @param onto the vector on which to project the function
   * 
   * @return a newly created vector function that projects the supplied function onto a vector
   */
  public static VectorIJKFunction project(final VectorIJKFunction function,
      final UnwritableVectorIJK onto) {
    return project(function, create(onto));
  }

  /**
   * Creates a vector function from an existing function by projecting the existing function onto a
   * vector provided by another function.
   * 
   * @param function the vector function to project
   * @param onto the vector function providing the vector onto which to project
   * 
   * @return a newly created vector function that projects the supplied function onto a vector
   *         provided by onto
   */
  public static VectorIJKFunction project(final VectorIJKFunction function,
      final VectorIJKFunction onto) {
    return new AbstractImplementationTwo() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.project(function.evaluate(t, getInternalBuffer()),
            onto.evaluate(t, getOtherBuffer()), buffer);
      }
    };
  }

  /**
   * Creates a vector function by linearly combining two existing vector functions.
   * 
   * @param scaleA the scalar multiplier to apply to a in the combination
   * @param a a vector function to combine
   * @param scaleB the scalar multiplier to apply to b in the combination
   * @param b another vector function to combine
   * 
   * @return a newly created vector function that computes <code> ( scaleA * a + scaleB * b )</code>
   */
  public static VectorIJKFunction combine(final UnivariateFunction scaleA,
      final VectorIJKFunction a, final UnivariateFunction scaleB, final VectorIJKFunction b) {

    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.combine(scaleA.evaluate(t), a.evaluate(t, getInternalBuffer()),
            scaleB.evaluate(t), b.evaluate(t, buffer), buffer);
      }
    };

  }

  /**
   * Creates a vector function by linearly combining three existing vector functions.
   * 
   * @param scaleA the scalar multiplier to apply to a
   * @param a a vector function
   * @param scaleB the scalar multiplier to apply to b
   * @param b another vector function
   * @param scaleC the scalar multiplier to apply to c
   * @param c yet another vector function
   * 
   * @return a newly created vector function that computes
   *         <code>( scaleA * a + scaleB * b + scaleC * c )</code>
   */
  public static VectorIJKFunction combine(final UnivariateFunction scaleA,
      final VectorIJKFunction a, final UnivariateFunction scaleB, final VectorIJKFunction b,
      final UnivariateFunction scaleC, final VectorIJKFunction c) {

    return new AbstractImplementationTwo() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.combine(scaleA.evaluate(t), a.evaluate(t, getInternalBuffer()),
            scaleB.evaluate(t), b.evaluate(t, getOtherBuffer()), scaleC.evaluate(t),
            c.evaluate(t, buffer), buffer);
      }
    };

  }

  /**
   * Creates a vector function by computing the unitized cross product of two existing functions
   * 
   * @param a a vector function
   * @param b the other vector function
   * 
   * @return a newly created vector function that computes <code>( a x b )/||a||/||b||</code>
   */
  public static VectorIJKFunction uCross(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementationTwo() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.uCross(a.evaluate(t, getInternalBuffer()), b.evaluate(t, getOtherBuffer()),
            buffer);
      }
    };
  }

  /**
   * Creates a vector function by computing the cross product of two existing functions
   * 
   * @param a a vector function
   * @param b the other vector function
   * 
   * @return a newly created vector function that computes <code>( a x b )</code>
   */
  public static VectorIJKFunction cross(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractImplementation() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return VectorIJK.cross(a.evaluate(t, getInternalBuffer()), b.evaluate(t, buffer), buffer);
      }
    };
  }

  /**
   * Creates a univariate function by computing the dot product of two vector functions
   * 
   * @param a a vector function
   * @param b another vector function
   * 
   * @return a newly created vector function that computes the dot product
   *         <code>$lt; a , b &gt;</code>
   */
  public static UnivariateFunction dot(final VectorIJKFunction a, final VectorIJKFunction b) {
    return new AbstractFunctionTwo() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t, getInternalBuffer()).getDot(b.evaluate(t, getOtherBuffer()));
      }
    };
  }

  /**
   * Creates a univariate function from a vector function by computing its length
   * 
   * @param function a vector function
   * 
   * @return a function capturing the length of the supplied vector function
   */
  public static UnivariateFunction length(final VectorIJKFunction function) {
    return new AbstractFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, getInternalBuffer()).getLength();
      }
    };
  }

  /**
   * Creates a univariate function from the two supplied vector functions by computing their angular
   * separation
   * 
   * @param a a vector function
   * @param b another vector function
   * 
   * @return a univariate function that computes the angular separation between a and b
   */
  public static UnivariateFunction separation(final VectorIJKFunction a,
      final VectorIJKFunction b) {
    return new AbstractFunctionTwo() {

      @Override
      public double evaluate(double t) {
        return a.evaluate(t, getInternalBuffer()).getSeparation(b.evaluate(t, getOtherBuffer()));
      }
    };
  }

  /**
   * Creates a univariate function from the ith component of the supplied vector function
   * 
   * @param function the vector function
   * 
   * @return a newly created univariate function capturing the ith component of the vector function
   */
  public static UnivariateFunction extractI(final VectorIJKFunction function) {
    return new AbstractFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, getInternalBuffer()).getI();
      }
    };
  }

  /**
   * Creates a univariate function from the jth component of the supplied vector function
   * 
   * @param function the vector function
   * 
   * @return a newly created univariate function capturing the jth component of the vector function
   */
  public static UnivariateFunction extractJ(final VectorIJKFunction function) {
    return new AbstractFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, getInternalBuffer()).getJ();
      }
    };
  }

  /**
   * Creates a univariate function from the kth component of the supplied vector function
   * 
   * @param function the vector function
   * 
   * @return a newly created univariate function capturing the kth component of the vector function
   */
  public static UnivariateFunction extractK(final VectorIJKFunction function) {
    return new AbstractFunction() {

      @Override
      public double evaluate(double t) {
        return function.evaluate(t, getInternalBuffer()).getK();
      }
    };
  }

  /**
   * Creates a vector function that rotates each result using the given rotation matrix
   * 
   * @param function the vector function
   * @param rotationMatrix the rotation to apply to each result from the function
   * 
   * @return a newly created vector function that rotates its results by the given matrix
   */
  public static VectorIJKFunction rotate(final VectorIJKFunction function,
      final UnwritableRotationMatrixIJK rotationMatrix) {
    return new VectorIJKFunction() {
      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK v = function.evaluate(t, buffer);
        return rotationMatrix.mxv(v, v);
      }
    };
  }

  /**
   * Creates a vector function that rotates each result using the given rotation matrix
   * 
   * @param function the vector function
   * @param rotationMatrix the rotation to apply to each result from the function
   * 
   * @return a newly created vector function that rotates its results by the given matrix
   */
  public static VectorIJKFunction rotate(final VectorIJKFunction function,
      final RotationMatrixIJKFunction rotationMatrixFunction) {
    return new VectorIJKFunction() {
      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        VectorIJK v = function.evaluate(t, buffer);
        return rotationMatrixFunction.evaluate(t, new RotationMatrixIJK()).mxv(v, v);
      }
    };
  }

}
