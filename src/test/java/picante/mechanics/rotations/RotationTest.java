package picante.mechanics.rotations;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;

public abstract class RotationTest {

  private double tolerance = 1.0E-16;

  protected RotationMatrixIJK matrix;
  protected RotationMatrixIJK testMatrix;
  protected Rotation rotation;

  public abstract Rotation createRotation(RotationMatrixIJK matrix);

  protected void setTolerance(double tolerance) {
    this.tolerance = tolerance;
  }

  @Before
  public void setUp() throws Exception {
    matrix = new RotationMatrixIJK(0.46865981337225204, -0.5847388119079097, 0.6621468879171843,
        0.5103093929582195, 0.7910490458898228, 0.33738069069734045, -0.7210702481648369,
        0.17978300488457546, 0.6691306063588582);
    testMatrix = new RotationMatrixIJK();
    rotation = createRotation(matrix);
  }

  @Test
  public void testInterfaceSetTo() {
    matrix = new RotationMatrixIJK(0.23262501680698122, -0.11345880161244597, 0.9659258262890683,
        0.6851945196756121, 0.7239556287790326, -0.0799794834045749, -0.6902130625843436,
        0.6804523112576097, 0.2461515393860416);

    Rotation returned = rotation.setTo(matrix);

    assertEquivalentMatrix(new RotationMatrixIJK(0.23262501680698122, -0.11345880161244597,
        0.9659258262890683, 0.6851945196756121, 0.7239556287790326, -0.0799794834045749,
        -0.6902130625843436, 0.6804523112576097, 0.2461515393860416), matrix);

    assertSame(returned, rotation);
    assertComponentEquals(matrix, rotation.getRotation(new RotationMatrixIJK()), tolerance);
  }

  @Test
  public void testInterfaceGetRotation() {

    RotationMatrixIJK returned = rotation.getRotation(testMatrix);

    assertSame(returned, testMatrix);
    assertComponentEquals(matrix, testMatrix, tolerance);

  }

}
