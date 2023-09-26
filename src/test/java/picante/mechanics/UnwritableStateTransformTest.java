package picante.mechanics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualStateTransform;
import static picante.junit.AssertTools.assertEqualStateVector;
import static picante.junit.AssertTools.assertEquivalentStateVector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

public class UnwritableStateTransformTest {

  private UnwritableStateTransform transform;
  private UnwritableStateTransform copyCon;
  private UnwritableStateTransform otherRotation;
  private UnwritableStateTransform otherdRotation;
  private RotationMatrixIJK rotation;
  private MatrixIJK matrix;

  private StateVector a;
  private StateVector b;

  private UnwritableStateTransform r;

  private final UnwritableStateVector srcA =
      new UnwritableStateVector(new VectorIJK(1, 2, 3), new VectorIJK(4, 5, 6));
  private final UnwritableStateVector srcB =
      new UnwritableStateVector(new VectorIJK(1, -1, 2), new VectorIJK(5, -8, 13));

  private final UnwritableStateTransform srcR =
      new UnwritableStateTransform(new RotationMatrixIJK(0.30901699437494745, -0.930273649576356,
          0.19773576836617326, 0.9510565162951535, 0.30226423163382676, -0.06424824579191735, 0.,
          0.20791169081775934, 0.9781476007338057), new MatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9));

  @Before
  public void setUp() throws Exception {

    a = new StateVector(srcA);
    b = new StateVector(srcB);

    r = new UnwritableStateTransform(srcR);

    rotation = new RotationMatrixIJK(0.30901699437494745, -0.930273649576356, 0.19773576836617326,
        0.9510565162951535, 0.30226423163382676, -0.06424824579191735, 0., 0.20791169081775934,
        0.9781476007338057);

    matrix = new MatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9);

    transform = new UnwritableStateTransform(rotation, matrix);
    copyCon = new UnwritableStateTransform(transform);
    otherRotation = new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, matrix);
    otherdRotation = new UnwritableStateTransform(rotation, MatrixIJK.IDENTITY);
  }

  @After
  public void tearDown() throws Exception {
    StateTransformTest.checkStaticFinalMembers();
  }

  @Test
  public void testUnwritableStateTransformUnwritableRotationMatrixIJKUnwritableMatrixIJK() {
    assertEqualMatrix(rotation, transform.rotation);
    assertNotSame(rotation, transform.rotation);
    assertEqualMatrix(matrix, transform.dRotation);
    assertNotSame(matrix, transform.dRotation);
  }

  @Test
  public void testUnwritableStateTransformUnwritableStateTransform() {
    transform = new UnwritableStateTransform(StateTransform.IDENTITY);
    assertNotSame(StateTransform.IDENTITY.rotation, transform.rotation);
    assertNotSame(StateTransform.IDENTITY.dRotation, transform.dRotation);
    assertEqualStateTransform(
        new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, MatrixIJK.ZEROS),
        StateTransform.IDENTITY);
    assertEqualStateTransform(StateTransform.IDENTITY, transform);
  }

  @Test
  public void testCreateInverse() {
    UnwritableStateTransform inverse = r.createInverse();
    assertNotSame(r, inverse);
    assertEqualMatrix(r.getRotation().createTranspose(), inverse.rotation);
    assertEqualMatrix(r.getRotationDerivative().createTranspose(), inverse.dRotation);
  }

  @Test
  public void testGetRotation() {
    UnwritableRotationMatrixIJK result = transform.getRotation();

    /*
     * This test is questionable, as the API does not clearly indicate that this is the case;
     * however it is precisely how the current implementation functions.
     */
    assertSame(result, transform.rotation);

    assertEqualMatrix(rotation, result);
  }

  @Test
  public void testGetRotationDerivative() {
    UnwritableMatrixIJK result = transform.getRotationDerivative();

    /*
     * This test is questionable, as the API does not clearly indicate that this is the case;
     * however it is precisely how the current implementation functions.
     */
    assertSame(result, transform.dRotation);

    assertEqualMatrix(matrix, result);
  }

  @Test
  public void testMxv() {
    StateVector d = r.mxv(a, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertEqualStateTransform(srcR, r);

    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);
  }

  @Test
  public void testMxvOverA() {
    StateVector d = r.mxv(a, a);
    assertSame(d, a);
    assertEqualStateTransform(srcR, r);

    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);

  }

  @Test
  public void testMixv() {
    StateVector d = r.mixv(a, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertEqualStateTransform(srcR, r);

    r = r.createInverse();
    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);
  }

  @Test
  public void testMixvOverA() {
    StateVector d = r.mixv(a, a);
    assertSame(d, a);
    assertEqualStateTransform(srcR, r);

    r = r.createInverse();
    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);
  }

  @Test
  public void testCopyOf() {

    UnwritableStateTransform unwritable =
        new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, MatrixIJK.ONES);
    UnwritableStateTransform notUnwritable =
        new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, MatrixIJK.ONES) {};

    UnwritableStateTransform result = UnwritableStateTransform.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableStateTransform.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableStateTransform.class, result.getClass());

  }

  @Test
  public void testEqualsObject() {

    assertTrue(transform.equals(copyCon));
    assertNotSame(transform, copyCon);
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), copyCon);

    assertTrue(transform.equals(transform));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);

    assertFalse(transform.equals(null));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);

    assertFalse(transform.equals(""));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);

    assertFalse(transform.equals(otherRotation));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);
    assertEqualStateTransform(new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, matrix),
        otherRotation);

    assertFalse(transform.equals(otherdRotation));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);
    assertEqualStateTransform(new UnwritableStateTransform(rotation, MatrixIJK.IDENTITY),
        otherdRotation);

    UnwritableStateTransform subClass = new UnwritableStateTransform(rotation, matrix) {};

    assertTrue(subClass.equals(transform));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), subClass);

    assertTrue(transform.equals(subClass));
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), transform);
    assertEqualStateTransform(new UnwritableStateTransform(rotation, matrix), subClass);
  }

  @Test
  public void testHashCode() {

    /*
     * Simply check that state transforms are equal, but are different instances have equal
     * hashcodes.
     */
    assertEquals(transform.hashCode(), copyCon.hashCode());
    assertNotSame(transform, copyCon);

  }

}
