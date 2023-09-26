package picante.math.functions;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

public class RotationMatrixIJKFunctions {

  private static abstract class AbstractImplementation implements RotationMatrixIJKFunction {
    final RotationMatrixIJK internalBuffer = new RotationMatrixIJK();
  }

  private static abstract class AbstractImplementationTwo extends AbstractImplementation {
    final RotationMatrixIJK otherBuffer = new RotationMatrixIJK();
  }

  private static abstract class AbstractVectorFunction implements VectorIJKFunction {
    final RotationMatrixIJK internalBuffer = new RotationMatrixIJK();
  }

  /**
   * Constructor is private to block instantiations.
   */
  private RotationMatrixIJKFunctions() {}

  public static RotationMatrixIJKFunction create(final UnwritableRotationMatrixIJK constant) {
    return new AbstractImplementation() {
      {
        internalBuffer.setTo(constant);
      }

      @Override
      public RotationMatrixIJK evaluate(@SuppressWarnings("unused") double t,
          RotationMatrixIJK buffer) {
        return buffer.setTo(internalBuffer);
      }

    };
  }

  public static RotationMatrixIJKFunction mxm(final RotationMatrixIJKFunction a,
      final RotationMatrixIJKFunction b) {
    return new AbstractImplementationTwo() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return RotationMatrixIJK.mxm(a.evaluate(t, internalBuffer), b.evaluate(t, otherBuffer),
            buffer);
      }
    };
  }

  public static RotationMatrixIJKFunction mtxm(final RotationMatrixIJKFunction a,
      final RotationMatrixIJKFunction b) {
    return new AbstractImplementationTwo() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return RotationMatrixIJK.mtxm(a.evaluate(t, internalBuffer), b.evaluate(t, otherBuffer),
            buffer);
      }
    };
  }

  public static RotationMatrixIJKFunction mxmt(final RotationMatrixIJKFunction a,
      final RotationMatrixIJKFunction b) {
    return new AbstractImplementationTwo() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return RotationMatrixIJK.mxmt(a.evaluate(t, internalBuffer), b.evaluate(t, otherBuffer),
            buffer);
      }
    };
  }

  public static RotationMatrixIJKFunction transpose(final RotationMatrixIJKFunction function) {
    return new RotationMatrixIJKFunction() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return function.evaluate(t, buffer).transpose();
      }
    };
  }

  public static RotationMatrixIJKFunction sharpen(final RotationMatrixIJKFunction function) {
    return new RotationMatrixIJKFunction() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return buffer.setTo(function.evaluate(t, buffer)).sharpen();
      }
    };
  }

  // public static RotationMatrixIJKFunction createTwoVectorFunction(
  // final AxisPair pairing, final VectorIJKFunction primaryVector,
  // final VectorIJKFunction secondaryVector) {
  // return null;
  // }

  public static VectorIJKFunction mxv(final RotationMatrixIJKFunction function,
      final VectorIJKFunction vector) {
    return new AbstractVectorFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, internalBuffer).mxv(vector.evaluate(t, buffer), buffer);
      }
    };
  }

  public static VectorIJKFunction mtxv(final RotationMatrixIJKFunction function,
      final VectorIJKFunction vector) {
    return new AbstractVectorFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.evaluate(t, internalBuffer).mtxv(vector.evaluate(t, buffer), buffer);
      }
    };
  }

}
