package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualVector;
import static picante.junit.AssertTools.assertEquivalentVector;
import org.junit.Before;
import org.junit.Test;
import picante.junit.AssertTools;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

public class AxisAndAngleTest extends RotationTest {

  private static final double TOLERANCE = 1.0E-15;

  private AxisAndAngle a1;
  private AxisAndAngle a2;

  @Override
  public Rotation createRotation(RotationMatrixIJK matrix) {
    return new AxisAndAngle(matrix);
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

    a1 = new AxisAndAngle(new VectorIJK(1, 2, 3), 13 * Math.PI / 15);
    a2 = new AxisAndAngle(new VectorIJK(0, 1, -1), 3 * Math.PI / 4);

  }

  @Test
  public void testAxisAndAngle() {
    AxisAndAngle a = new AxisAndAngle();

    assertEquals(0.0, a.getAngle(), 0.0);

    VectorIJK vector = a.getAxis(new VectorIJK());
    assertEqualVector(VectorIJK.K, vector);
  }

  @Test
  public void testAxisAndAngleDoubleDoubleDoubleDouble() {
    AxisAndAngle a = new AxisAndAngle(12.0, 1.0, 1.0, Math.PI / 6);

    assertEquals(Math.PI / 6, a.getAngle(), 0.0);

    VectorIJK testVector = new VectorIJK(12, 1, 1).unitize();
    VectorIJK vector = a.getAxis(new VectorIJK());
    assertEqualVector(testVector, vector);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAxisAndAngleDoubleDoubleDoubleDoubleException() {
    new AxisAndAngle(0.0, 0.0, 0.0, 0.0);
  }

  @Test
  public void testAxisAndAngleUnwritableVectorIJKDouble() {
    VectorIJK v = new VectorIJK(2, 3, 4);
    AxisAndAngle a = new AxisAndAngle(v, Math.PI / 3.0);

    assertEquals(Math.PI / 3.0, a.getAngle(), 0.0);

    VectorIJK vector = a.getAxis(new VectorIJK());
    assertEqualVector(new VectorIJK(2, 3, 4).unitize(), vector);

    /*
     * Change v and make certain it has no impact on a.
     */
    v.setTo(1, 0, 3);
    assertEqualVector(new VectorIJK(2, 3, 4).unitize(), a.getAxis(vector));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAxisAndAngleUnwritableVectorIJKDoubleException() {
    new AxisAndAngle(new VectorIJK(0, 0, 0), 0.0);
  }

  @Test
  public void testAxisAndAngleUnwritableRotationMatrixIJK() {
    AxisAndAngle a = new AxisAndAngle(new RotationMatrixIJK(-0.6666666666666666, 0.6666666666666667,
        0.33333333333333326, 0.13333333333333341, -0.3333333333333333, 0.9333333333333335,
        0.7333333333333334, 0.6666666666666669, 0.13333333333333333));

    assertEquals(2.774384633031956, a.getAngle(), TOLERANCE);

    VectorIJK vector = a.getAxis(new VectorIJK());

    assertEquivalentVector(
        new VectorIJK(0.3713906763541037, 0.5570860145311556, 0.7427813527082073), vector);
  }

  @Test
  public void testAxisAndAngleUnwritableRotationMatrixIJKAsIdentity() {
    AxisAndAngle a = new AxisAndAngle(RotationMatrixIJK.IDENTITY);

    assertEquals(0.0, a.getAngle(), 0.0);

    VectorIJK vector = a.getAxis(new VectorIJK());
    assertEqualVector(VectorIJK.K, vector);
  }

  @Test
  public void testAxisAndAngleAxisAndAngle() {
    AxisAndAngle a = new AxisAndAngle(a1);

    double testAngle = a1.getAngle();

    assertEquals(testAngle, a.getAngle(), 0.0);

    VectorIJK testVector = a1.getAxis(new VectorIJK());
    VectorIJK vector = a.getAxis(new VectorIJK());

    assertEqualVector(testVector, vector);

    /*
     * Change the contents of a1, and verify that it has no impact on a.
     */
    a1.setTo(a2);

    assertEquals(testAngle, a.getAngle(), 0.0);
    a.getAxis(vector);
    assertEqualVector(testVector, vector);
  }

  @Test
  public void testGetAxis() {
    VectorIJK v = new VectorIJK(1, 2, 3);
    AxisAndAngle a = new AxisAndAngle(v, Math.toRadians(12.0));
    VectorIJK result = a.getAxis();
    assertNotSame(v, a);
    a.setAxis(new VectorIJK(-1, 2, 4));
    VectorIJK otherResult = a.getAxis();
    assertSame(result, otherResult);
    assertEquivalentVector(new VectorIJK(-1, 2, 4).unitize(), result);
    assertEquivalentVector(new VectorIJK(-1, 2, 4).unitize(), otherResult);
  }

  @Test
  public void testGetAxisVectorIJK() {
    VectorIJK v = new VectorIJK();
    VectorIJK returned = a1.getAxis(v);
    assertSame(returned, v);
    assertEquivalentVector(new VectorIJK(1, 2, 3).unitize(), returned);

    returned = a2.getAxis(v);
    assertSame(returned, v);
    assertEquivalentVector(new VectorIJK(0, 1, -1).unitize(), returned);
  }

  @Test
  public void testGetAngle() {
    assertEquals(13.0 * Math.PI / 15.0, a1.getAngle(), 0.0);
    assertEquals(3.0 * Math.PI / 4.0, a2.getAngle(), 0.0);
  }

  @Test
  public void testSetAxis() {
    a1.setAxis(new VectorIJK(-1, -2, 3));
    assertEquivalentVector(new VectorIJK(-1, -2, 3).unitize(), a1.getAxis(new VectorIJK()));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetAxisException() {
    a1.setAxis(new VectorIJK(0.0, 0.0, 0.0));
  }

  @Test
  public void testSetAngle() {
    a1.setAngle(12000.0);
    AssertTools.assertEqualDouble(12000.0, a1.getAngle());

    a2.setAngle(-1.0);
    AssertTools.assertEqualDouble(-1.0, a2.getAngle());
  }

  @Test
  public void testSetToUnwritableVectorIJKDouble() {
    VectorIJK v = new VectorIJK(3, 4, 5);
    AxisAndAngle returned = a1.setTo(v, 1.04);
    assertSame(returned, a1);

    AssertTools.assertEqualDouble(1.04, a1.getAngle());

    VectorIJK vector = a1.getAxis(new VectorIJK());
    assertEquivalentVector(new VectorIJK(3, 4, 5).unitize(), vector);

    /*
     * Change v and verify it has no effect on a1.
     */
    v.setTo(-3, 4, 2);
    assertEquivalentVector(new VectorIJK(3, 4, 5).unitize(), a1.getAxis(vector));

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetToUnwritableVectorIJKDoubleException() {
    a1.setTo(new VectorIJK(0.0, 0.0, 0.0), 0.0);
  }

  @Test
  public void testSetToAxisAndAngle() {
    AxisAndAngle returned = a1.setTo(a2);
    assertSame(returned, a1);

    double testAngle = a2.getAngle();
    AssertTools.assertEqualDouble(testAngle, a1.getAngle());

    VectorIJK testVector = a2.getAxis(new VectorIJK());
    VectorIJK vector = a1.getAxis(new VectorIJK());

    assertEquivalentVector(testVector, vector);

    /*
     * Change the contents of a1, and verify that it has no impact on a.
     */
    a2.setTo(new AxisAndAngle(VectorIJK.K, Math.PI / 3.0));

    assertEquals(testAngle, a1.getAngle(), 0.0);
    a1.getAxis(vector);
    assertEqualVector(testVector, vector);

  }

  @Test
  public void testSetToDoubleDoubleDoubleDouble() {
    AxisAndAngle returned = a1.setTo(2, 3, 4, 2.774384633031956);
    assertSame(returned, a1);

    AssertTools.assertEquivalentDouble(2.774384633031956, a1.getAngle());

    VectorIJK vector = a1.getAxis(new VectorIJK());
    assertEquivalentVector(new VectorIJK(2, 3, 4).unitize(), vector);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSetToDoubleDoubleDoubleDoubleException() {
    a1.setTo(0.0, 0.0, 0.0, 0.0);
  }

  @Test
  public void testSetToUnwritableRotationMatrixIJK() {
    AxisAndAngle returned = a1.setTo(new RotationMatrixIJK(-0.6666666666666666, 0.6666666666666667,
        0.33333333333333326, 0.13333333333333341, -0.3333333333333333, 0.9333333333333335,
        0.7333333333333334, 0.6666666666666669, 0.13333333333333333));
    assertSame(returned, a1);

    assertEquals(2.774384633031956, a1.getAngle(), 0.0);

    VectorIJK vector = a1.getAxis(new VectorIJK());
    assertEquivalentVector(
        new VectorIJK(0.3713906763541037, 0.5570860145311556, 0.7427813527082073), vector);
  }

  @Test
  public void testSetToUnwritableRotationMatrixIJKAsIdentity() {
    AxisAndAngle returned = a1.setTo(RotationMatrixIJK.IDENTITY);
    assertSame(returned, a1);

    assertEquals(0.0, a1.getAngle(), 0.0);

    VectorIJK vector = a1.getAxis(new VectorIJK());
    assertEqualVector(VectorIJK.K, vector);
  }

  @Test
  public void testGetRotation() {

    AxisAndAngle a = new AxisAndAngle(2, 3, 4, 2.774384633031956);

    RotationMatrixIJK r = new RotationMatrixIJK();

    RotationMatrixIJK returned = a.getRotation(r);
    assertSame(returned, r);

    double TOL = 1.67E-16;

    assertComponentEquals(new RotationMatrixIJK(-0.6666666666666667, 0.6666666666666666,
        0.33333333333333337, 0.13333333333333336, -0.3333333333333334, 0.9333333333333333,
        0.7333333333333333, 0.6666666666666667, 0.1333333333333333), r, TOL);
    // TODO, after switching to Crucible Math, we no longer get this level of agreement
    // assertEquivalentMatrix(new RotationMatrixIJK(-0.6666666666666667, 0.6666666666666666,
    // 0.33333333333333337, 0.13333333333333336, -0.3333333333333334, 0.9333333333333333,
    // 0.7333333333333333, 0.6666666666666667, 0.1333333333333333), r);
  }

  /**
   * This test exercises some of the internal logic in the setTo method that was a bit confusing.
   */
  @Test
  public void testSetToZeroScalar() {
    AxisAndAngle aa = new AxisAndAngle(VectorIJK.J, Math.toRadians(20.0));

    /*
     * Now create a quaternion that has a zero scalar component, and build the rotation from it
     * which should trip the bug.
     */
    Quaternion q = new Quaternion(0.0, 1.0, 1.0, 1.0);
    aa.setTo(q.getRotation(new RotationMatrixIJK()));

    assertEquals(Math.PI, aa.getAngle(), 0.0);

    /*
     * In this odd case, the quaternion code generates the negative axis.
     */
    assertEquivalentVector(new VectorIJK(1, 1, 1).negate().unitize(), aa.getAxis());

  }

  @Test
  public void testEquals() {
    AxisAndAngle a = new AxisAndAngle(a1);

    assertNotSame(a, a1);
    assertTrue(a.equals(a1));
    assertTrue(a1.equals(a));

    assertFalse(a1.equals(a2));
    assertFalse(a2.equals(a1));
    assertFalse(a1.equals(null));

    assertFalse(a1.equals(Integer.valueOf(24)));

    AxisAndAngle subClass = new AxisAndAngle(a1) {};

    assertTrue(subClass.equals(a1));
    assertTrue(a1.equals(subClass));
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that objects that are not equal, but contain fields that are have precisely the
     * same hashcode. Just check one, as it should be necessary, but certainly not sufficient to
     * determine if this is the case.
     */
    AxisAndAngle a = new AxisAndAngle(a1);
    assertEquals(a.hashCode(), a1.hashCode());

  }

  @Test
  public void testToString() {
    VectorIJK vector = a1.getAxis(new VectorIJK());
    String testString = "[" + String.valueOf(a1.getAngle()) + ", " + String.valueOf(vector) + "]";
    assertEquals(testString, a1.toString());
  }

}
