package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualVector;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;

/**
 * This class is generic, as such this test only exercise the generic portions of the code, which is
 * sufficient.
 */
public class WrapperRateTest extends DifferentiatedRotationTest {

  private static final double TOLERANCE = 1.0E-16;

  private WrapperWithRate<?> a1;
  private WrapperWithRate<?> a2;
  private MatrixWrapper w1;
  private MatrixWrapper w2;
  private RotationMatrixIJK m1;
  private RotationMatrixIJK m2;
  private VectorIJK v1;
  private VectorIJK v2;
  private StateTransform t1;
  private StateTransform t2;

  @Override
  @Before
  public void setUp() throws Exception {
    super.setUp();

    /*
     * Configure the tolerance against the default from the parent test. This is necessary to ensure
     * that the interface tests pass.
     */
    super.setTolerance(TOLERANCE);

    m1 = new RotationMatrixIJK(0.9990482215818578, -0.03437254630962194, 0.02685477637814414,
        0.04232370514336272, 0.6149337422478219, -0.7874421862130749, 0.010552484876318281,
        0.7878293093718222, 0.6158032350983981);
    v1 = new VectorIJK(0.1, 0.2, 0.05);
    w1 = new MatrixWrapper(new RotationMatrixIJK(m1));
    a1 = new WrapperWithRate<MatrixWrapper>(w1, v1);
    t1 = new StateTransform(m1,
        new MatrixIJK(-0.000005688281904479898, 0.12681917476197335, 0.16253275633033337,
            0.04889716259146107, -0.08050155825266332, -0.06023758469093261, -0.1955772738020353,
            0.06836788348670658, -0.08411517389693633));

    m2 = new RotationMatrixIJK(0.9512512425641977, -0.25488700224417876, 0.17364817766693033,
        0.3016166128991695, 0.6512328797187343, -0.6963642403200189, 0.06440879088526905,
        0.7147925240657044, 0.696364240320019);
    v2 = new VectorIJK(0.25, 0.1, 0.01);
    w2 = new MatrixWrapper(new RotationMatrixIJK(m2));
    a2 = new WrapperWithRate<MatrixWrapper>(w2, v2);
    t2 = new StateTransform(m2,
        new MatrixIJK(0.00342471295953521, 0.0649669236093831, 0.07660006643520209,
            -0.006589685295675284, -0.18124700103886787, -0.17235457830333545, -0.0197209710316274,
            0.18829692015410146, -0.19145587784669776));

  }

  @Override
  public DifferentiatedRotation createRotation(UnwritableStateTransform transform) {
    WrapperWithRate<MatrixWrapper> result = new WrapperWithRate<MatrixWrapper>(new MatrixWrapper());
    result.setTo(transform);
    return result;
  }

  @Test
  public void testGetRotation() {
    assertSame(w1, a1.getRotation());
    assertSame(w2, a2.getRotation());
  }

  @Test
  public void testGetRate() {
    VectorIJK rate = a1.getRate();
    assertNotSame(VectorIJK.ZERO, rate);
    rate.setTo(VectorIJK.ZERO);
    assertEquals(VectorIJK.ZERO, a1.getRate());

    rate = a2.getRate();
    assertNotSame(VectorIJK.MINUS_I, rate);
    rate.setTo(VectorIJK.MINUS_I);
    assertEquals(VectorIJK.MINUS_I, a2.getRate());
  }

  @Test
  public void testGetRateVectorIJK() {
    VectorIJK buffer = new VectorIJK();
    VectorIJK result = a1.getRate(buffer);
    assertEqualVector(v1, result);
    assertSame(result, buffer);
  }

  @Test
  public void testSetRate() {
    a1.setRate(VectorIJK.I);
    VectorIJK result = a1.getRate(new VectorIJK());
    assertEqualVector(VectorIJK.I, result);
  }

  @Test
  public void testHashCode() {

    WrapperWithRate<?> a =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(new RotationMatrixIJK(m1)), v1);

    assertNotSame(a, a1);
    assertEquals(a.hashCode(), a1.hashCode());

  }

  @Test
  public void testEqualsObject() {

    WrapperWithRate<MatrixWrapper> a =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(new RotationMatrixIJK(m1)), v1);

    assertNotSame(a, a1);
    assertTrue(a.equals(a1));
    assertTrue(a1.equals(a));

    assertFalse(a.equals(a2));
    assertFalse(a2.equals(a));
    assertFalse(a.equals(null));

    assertFalse(a.equals(Integer.valueOf(2)));

    /*
     * Construct an implementation of the Rotation interface on the fly. Then use this to verify
     * that will not result in equality with a rotation and rate of a different type.
     */
    Rotation r = new Rotation() {
      @Override
      public Rotation setTo(@SuppressWarnings("unused") UnwritableRotationMatrixIJK matrix) {
        return null;
      }

      @Override
      public RotationMatrixIJK getRotation(@SuppressWarnings("unused") RotationMatrixIJK buffer) {
        return m1;
      }
    };

    WrapperWithRate<?> otherRForm = new WrapperWithRate<Rotation>(r, v1);

    assertFalse(otherRForm.equals(a));
    assertFalse(a.equals(otherRForm));

    WrapperWithRate<?> subClass =
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(new RotationMatrixIJK(m1)), v1) {};

    assertTrue(subClass.equals(a));
    assertTrue(a.equals(subClass));

  }

  @Test
  public void testGetTransform() {

    /*
     * Simple consistency check to verify that a transform constructed from an angular rate vector
     * will produce the expected state transform.
     */
    StateTransform t = new StateTransform();
    StateTransform returned = a1.getTransform(t);
    assertSame(returned, t);
    assertComponentEquals(t1, t, TOLERANCE);

    returned = a2.getTransform(t);
    assertSame(returned, t);
    assertComponentEquals(t2, t, TOLERANCE);
  }

  @Test
  public void testToString() {

    RotationMatrixIJK m = a1.getRotation(new RotationMatrixIJK());
    VectorIJK v = a1.getRate(new VectorIJK());

    String testString = "[" + m + ";" + v + "]";
    assertEquals(testString, a1.toString());

  }

}
