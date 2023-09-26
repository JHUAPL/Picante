package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentDouble;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import static picante.junit.AssertTools.assertEquivalentVector;
import org.junit.Before;
import org.junit.Test;
import picante.math.PicanteMath;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

public class QuaternionTest extends RotationTest {

  private static final double TOLERANCE = 1.0E-15;

  private Quaternion q1;
  private Quaternion q2;
  private Quaternion q3;

  @Override
  public Rotation createRotation(RotationMatrixIJK matrix) {
    return new Quaternion(matrix);
  }

  @Override
  @Before
  public void setUp() throws Exception {
    super.setUp();

    /*
     * Configure the tolerance against the default from the parent test. This is necessary to ensure
     * that the interface tests pass.
     */
    super.setTolerance(TOLERANCE);

    q1 = new Quaternion(new VectorIJK(1, 2, 3), Math.toRadians(30.0));
    q2 = new Quaternion(new VectorIJK(-1, 0, 1), Math.toRadians(62.0));
    q3 = new Quaternion(new VectorIJK(-1, 4, 6), Math.toRadians(200.0));
  }

  @Test
  public void testMultiply() {
    Quaternion buffer = new Quaternion();
    Quaternion returned = Quaternion.multiply(q1, q2, buffer);

    assertSame(returned, buffer);

    VectorIJK q1v = q1.getVector(new VectorIJK());
    VectorIJK q2v = q2.getVector(new VectorIJK());

    assertEquals(q1.getScalar() * q2.getScalar() - q1v.getDot(q2v), buffer.getScalar(), 0.0);

    VectorIJK tmp = VectorIJK.cross(q1v, q2v, new VectorIJK());

    assertEquivalentVector(VectorIJK.combine(q1.getScalar(), q2v, q2.getScalar(), q1v, 1.0, tmp),
        buffer.getVector(new VectorIJK()));
  }

  @Test
  public void testMultiplyNewQuaternion() {
    Quaternion returned = Quaternion.multiply(q1, q2);

    assertNotSame(returned, q1);
    assertNotSame(returned, q2);

    VectorIJK q1v = q1.getVector(new VectorIJK());
    VectorIJK q2v = q2.getVector(new VectorIJK());

    assertEquals(q1.getScalar() * q2.getScalar() - q1v.getDot(q2v), returned.getScalar(), 0.0);

    VectorIJK tmp = VectorIJK.cross(q1v, q2v, new VectorIJK());

    assertEquivalentVector(VectorIJK.combine(q1.getScalar(), q2v, q2.getScalar(), q1v, 1.0, tmp),
        returned.getVector(new VectorIJK()));
  }

  @Test
  public void testQuaternion() {
    Quaternion q = new Quaternion();

    assertEquals(1.0, q.getScalar(), 0.0);

    VectorIJK vector = q.getVector(new VectorIJK());
    assertEqualVector(VectorIJK.ZERO, vector);
  }

  @Test
  public void testQuaternionDoubleDoubleDoubleDouble() {

    Quaternion q = new Quaternion(0.5, -0.5, 1.0 / 3.0, -Math.sqrt(7.0 / 18.0));

    assertEquals(0.5, q.getScalar(), 0.0);

    VectorIJK vector = q.getVector(new VectorIJK());

    assertEquivalentVector(new VectorIJK(-0.5, 1.0 / 3.0, -Math.sqrt(7.0 / 18.0)), vector);

    /*
     * Check the sign change on the scalar component. The quaternion class should not be normalizing
     * the internals.
     */
    q = new Quaternion(-0.5, 0.5, 1.0 / 3.0, -Math.sqrt(7.0 / 18.0));

    assertEquals(-0.5, q.getScalar(), 0.0);

    vector = q.getVector(new VectorIJK());

    assertEquivalentVector(new VectorIJK(0.5, 1.0 / 3.0, -Math.sqrt(7.0 / 18.0)), vector);

  }

  @Test
  public void testQuaternionVectorIJKDouble() {

    Quaternion q = new Quaternion(new VectorIJK(1, 1, 1), Math.toRadians(15));

    assertEquals(Math.toRadians(15.0), q.getRotationAngle(), TOLERANCE);

    VectorIJK axis = q.getRotationAxis(new VectorIJK());

    assertEquivalentVector(new VectorIJK(1, 1, 1).unitize(), axis);

  }

  @Test
  public void testQuaternionQuaternion() {
    Quaternion q = new Quaternion(q1);

    assertEquals(q1.getScalar(), q.getScalar(), 0.0);
    assertEqualVector(q1.getVector(new VectorIJK()), q.getVector(new VectorIJK()));
  }

  @Test
  public void testQuaternionRotationMatrixIJK() {

    Quaternion q = new Quaternion(new RotationMatrixIJK(-0.6666666666666666, 0.6666666666666667,
        0.33333333333333326, 0.13333333333333341, -0.3333333333333333, 0.9333333333333335,
        0.7333333333333334, 0.6666666666666669, 0.13333333333333333));

    assertEquals(0.18257418583505536, q.getScalar(), TOLERANCE);
    assertComponentEquals(new VectorIJK(0.3651483716701107, 0.5477225575051661, 0.7302967433402214),
        q.getVector(new VectorIJK()), TOLERANCE);

  }

  @Test
  public void testGetScalar() {
    assertEquals(PicanteMath.cos(Math.toRadians(30.0) / 2.0), q1.getScalar(), 0.0);
    assertEquals(Math.cos(Math.toRadians(30.0) / 2.0), q1.getScalar(), TOLERANCE);
    assertEquals(PicanteMath.cos(Math.toRadians(62.0) / 2.0), q2.getScalar(), 0.0);
    assertEquals(Math.cos(Math.toRadians(62.0) / 2.0), q2.getScalar(), TOLERANCE);
    assertEquals(PicanteMath.cos(Math.toRadians(200.0) / 2.0), q3.getScalar(), 0.0);
    assertEquals(Math.cos(Math.toRadians(200.0) / 2.0), q3.getScalar(), TOLERANCE);
  }

  @Test
  public void testGetVector() {
    assertEquivalentVector(
        new VectorIJK(1, 2, 3).unitize().scale(Math.sin(Math.toRadians(30.0) / 2.0)),
        q1.getVector(new VectorIJK()));
    assertEquivalentVector(
        new VectorIJK(-1, 0, 1).unitize().scale(Math.sin(Math.toRadians(62.0) / 2.0)),
        q2.getVector(new VectorIJK()));
    assertEquivalentVector(
        new VectorIJK(-1, 4, 6).unitize().scale(Math.sin(Math.toRadians(200.0) / 2.0)),
        q3.getVector(new VectorIJK()));
  }

  @Test
  public void testGetRotationAxis() {
    assertComponentEquals(new VectorIJK(1, 2, 3).unitize(), q1.getRotationAxis(new VectorIJK()),
        TOLERANCE);
    assertComponentEquals(new VectorIJK(-1, 0, 1).unitize(), q2.getRotationAxis(new VectorIJK()),
        TOLERANCE);
    assertComponentEquals(new VectorIJK(-1, 4, 6).unitize().negate(),
        q3.getRotationAxis(new VectorIJK()), TOLERANCE);
  }

  @Test
  public void testGetRotationAngle() {
    assertEquals(Math.toRadians(30.0), q1.getRotationAngle(), TOLERANCE);
    assertEquals(Math.toRadians(62.0), q2.getRotationAngle(), TOLERANCE);
    assertEquals(Math.toRadians(160.0), q3.getRotationAngle(), TOLERANCE);
  }

  @Test
  public void testConjugate() {
    Quaternion returned = q1.conjugate();
    assertSame(returned, q1);
    assertEquals(PicanteMath.cos(Math.toRadians(30.0) / 2.0), q1.getScalar(), 0.0);
    assertEquals(Math.cos(Math.toRadians(30.0) / 2.0), q1.getScalar(), TOLERANCE);
    assertEquivalentVector(
        new VectorIJK(1, 2, 3).unitize().scale(Math.sin(Math.toRadians(30.0) / 2.0)).negate(),
        q1.getVector(new VectorIJK()));

    returned = q2.conjugate();
    assertSame(returned, q2);
    assertEquals(PicanteMath.cos(Math.toRadians(62.0) / 2.0), q2.getScalar(), 0.0);
    assertEquals(Math.cos(Math.toRadians(62.0) / 2.0), q2.getScalar(), TOLERANCE);
    assertEquivalentVector(
        new VectorIJK(-1, 0, 1).unitize().scale(Math.sin(Math.toRadians(62.0) / 2.0)).negate(),
        q2.getVector(new VectorIJK()));

    returned = q3.conjugate();
    assertSame(returned, q3);
    assertEquals(PicanteMath.cos(Math.toRadians(200.0) / 2.0), q3.getScalar(), 0.0);
    assertEquals(Math.cos(Math.toRadians(200.0) / 2.0), q3.getScalar(), TOLERANCE);
    assertEquivalentVector(
        new VectorIJK(-1, 4, 6).unitize().scale(Math.sin(Math.toRadians(200.0) / 2.0)).negate(),
        q3.getVector(new VectorIJK()));
  }

  @Test
  public void testNegate() {
    Quaternion returned = q1.negate();
    assertSame(returned, q1);
    assertEquals(-PicanteMath.cos(Math.toRadians(30.0) / 2.0), q1.getScalar(), 0.0);
    assertEquivalentDouble(-Math.cos(Math.toRadians(30.0) / 2.0), q1.getScalar());
    assertEquivalentVector(
        new VectorIJK(1, 2, 3).unitize().scale(Math.sin(Math.toRadians(30.0) / 2.0)).negate(),
        q1.getVector(new VectorIJK()));

    returned = q2.negate();
    assertSame(returned, q2);
    assertEquals(-PicanteMath.cos(Math.toRadians(62.0) / 2.0), q2.getScalar(), 0.0);
    assertEquivalentDouble(-Math.cos(Math.toRadians(62.0) / 2.0), q2.getScalar());
    assertEquivalentVector(
        new VectorIJK(-1, 0, 1).unitize().scale(Math.sin(Math.toRadians(62.0) / 2.0)).negate(),
        q2.getVector(new VectorIJK()));

    returned = q3.negate();
    assertSame(returned, q3);
    assertEquals(-PicanteMath.cos(Math.toRadians(200.0) / 2.0), q3.getScalar(), 0.0);
    assertEquals(-Math.cos(Math.toRadians(200.0) / 2.0), q3.getScalar(), TOLERANCE);
    assertEquivalentVector(
        new VectorIJK(-1, 4, 6).unitize().scale(Math.sin(Math.toRadians(200.0) / 2.0)).negate(),
        q3.getVector(new VectorIJK()));
  }

  @Test
  public void testSetToDoubleDoubleDoubleDouble() {

    Quaternion returned = q1.setTo(1.0 / 3.0, 0.5, -0.5, Math.sqrt(7.0 / 18.0));
    assertSame(returned, q1);

    assertEquals(1.0 / 3.0, q1.getScalar(), 0.0);

    VectorIJK vector = q1.getVector(new VectorIJK());
    assertEquivalentVector(new VectorIJK(0.5, -0.5, Math.sqrt(7.0 / 18.0)), vector);

    /*
     * Check that the change in sign is preserved through the component access API.
     */
    returned = q1.setTo(-1.0 / 3.0, 0.5, -0.5, Math.sqrt(7.0 / 18.0));
    assertSame(returned, q1);

    assertEquals(-1.0 / 3.0, q1.getScalar(), 0.0);
    q1.getVector(vector);
    assertEquivalentVector(new VectorIJK(0.5, -0.5, Math.sqrt(7.0 / 18.0)), vector);

  }

  @Test
  public void testSetToQuaternion() {
    Quaternion q = new Quaternion();
    Quaternion returned = q.setTo(q1);
    assertSame(returned, q);
    assertEquals(q1.getScalar(), q.getScalar(), 0.0);
    assertEqualVector(q1.getVector(new VectorIJK()), q.getVector(new VectorIJK()));
  }

  @Test
  public void testSetToVectorIJKDouble() {

    Quaternion returned = q1.setTo(new VectorIJK(1, 2, 3), Math.toRadians(12.0));
    assertSame(returned, q1);

    assertEquals(Math.toRadians(12.0), q1.getRotationAngle(), TOLERANCE);

    VectorIJK axis = q1.getRotationAxis(new VectorIJK());

    assertComponentEquals(new VectorIJK(1, 2, 3).unitize(), axis, TOLERANCE);

  }

  @Test
  public void testToString() {
    VectorIJK v = q1.getVector(new VectorIJK());
    String testString = "[" + String.valueOf(q1.getScalar()) + " " + String.valueOf(v.getI()) + " "
        + String.valueOf(v.getJ()) + " " + String.valueOf(v.getK()) + "]";
    assertEquals(testString, q1.toString());
  }

  @Test
  public void testSetToUnwritableRotationMatrixIJK() {

    Quaternion returned = q1.setTo(new RotationMatrixIJK(-0.6666666666666666, 0.6666666666666667,
        0.33333333333333326, 0.13333333333333341, -0.3333333333333333, 0.9333333333333335,
        0.7333333333333334, 0.6666666666666669, 0.13333333333333333));

    assertSame(returned, q1);

    assertEquals(0.18257418583505536, q1.getScalar(), TOLERANCE);

    assertComponentEquals(new VectorIJK(0.3651483716701107, 0.5477225575051661, 0.7302967433402214),
        q1.getVector(new VectorIJK()), TOLERANCE);

  }

  @Test
  public void testGetRotation() {

    Quaternion q = new Quaternion(1, 2, 3, 4);

    RotationMatrixIJK r = new RotationMatrixIJK();

    RotationMatrixIJK returned = q.getRotation(r);
    assertSame(returned, r);

    assertEquivalentMatrix(new RotationMatrixIJK(-0.6666666666666666, 0.6666666666666667,
        0.33333333333333326, 0.13333333333333341, -0.3333333333333333, 0.9333333333333335,
        0.7333333333333334, 0.6666666666666669, 0.13333333333333333), r);

  }

  @Test
  public void testEquals() {
    Quaternion q = new Quaternion(q1);

    assertNotSame(q, q1);
    assertTrue(q.equals(q1));
    assertTrue(q1.equals(q));

    assertFalse(q1.equals(q2));
    assertFalse(q2.equals(q1));
    assertFalse(q1.equals(null));

    assertFalse(q1.equals(Integer.valueOf(10)));

    Quaternion subClass = new Quaternion(q1) {};

    assertTrue(subClass.equals(q1));
    assertTrue(q1.equals(subClass));
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that objects that are not equal, but contain fields that are have precisely the
     * same hashcode. Just check one, as it should be necessary, but certainly not sufficient to
     * determine if this is the case.
     */
    Quaternion q = new Quaternion(q1);

    assertEquals(q.hashCode(), q1.hashCode());

  }

}
