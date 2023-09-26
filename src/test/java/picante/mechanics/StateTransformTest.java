package picante.mechanics;

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertEqualMatrix;
import static picante.junit.AssertTools.assertEqualStateTransform;
import static picante.junit.AssertTools.assertEqualStateVector;
import static picante.junit.AssertTools.assertEquivalentStateTransform;
import static picante.junit.AssertTools.assertEquivalentStateVector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

public class StateTransformTest {

  private StateTransform transform;
  private RotationMatrixIJK rotation;
  private MatrixIJK matrix;

  private StateVector a;
  private StateVector b;

  private final UnwritableStateVector srcA =
      new UnwritableStateVector(new VectorIJK(1, 2, 3), new VectorIJK(4, 5, 6));
  private final UnwritableStateVector srcB =
      new UnwritableStateVector(new VectorIJK(1, -1, 2), new VectorIJK(5, -8, 13));

  private StateTransform r;
  private StateTransform s;
  private StateTransform t;

  private final UnwritableStateTransform srcR =
      new UnwritableStateTransform(new RotationMatrixIJK(0.30901699437494745, -0.930273649576356,
          0.19773576836617326, 0.9510565162951535, 0.30226423163382676, -0.06424824579191735, 0.,
          0.20791169081775934, 0.9781476007338057), new MatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9));
  private final UnwritableStateTransform srcS = new UnwritableStateTransform(
      new RotationMatrixIJK(0.281168780679703, -0.800294767620276, 0.5295964517353734,
          0.9470690358965772, 0.32049682733400686, -0.018493915614698535, -0.15493339864928052,
          0.5067643126626463, 0.848048096156426),
      new MatrixIJK(1, 1, 2, 3, 5, 8, 13, 21, 34));
  private final UnwritableStateTransform srcT = new UnwritableStateTransform(
      new RotationMatrixIJK(0.42297657009418227, -0.03501959013521177, 0.905463665454403,
          0.9059098452951042, 0.03889319506835513, -0.4216807697474185, -0.020449287243370747,
          0.9986295347545738, 0.04817550173170447),
      new MatrixIJK(-1, 0, 1, 1, 1, 1, 2, 12, 3));

  @Before
  public void setUp() throws Exception {

    rotation = new RotationMatrixIJK(0.30901699437494745, -0.930273649576356, 0.19773576836617326,
        0.9510565162951535, 0.30226423163382676, -0.06424824579191735, 0., 0.20791169081775934,
        0.9781476007338057);

    matrix = new MatrixIJK(1, 2, 3, 4, 5, 6, 7, 8, 9);

    transform = new StateTransform(rotation, matrix);

    a = new StateVector(srcA);
    b = new StateVector(srcB);

    r = new StateTransform(srcR);
    s = new StateTransform(srcS);
    t = new StateTransform(srcT);
  }

  @After
  public void tearDown() throws Exception {
    checkStaticFinalMembers();
  }

  @Test
  public void testStateTransform() {
    assertEqualStateTransform(StateTransform.IDENTITY, new StateTransform());
  }

  @Test
  public void testStateTransformUnwritableRotationMatrixIJKUnwritableMatrixIJK() {
    assertEqualMatrix(rotation, transform.rotation);
    assertNotSame(rotation, transform.rotation);
    assertEqualMatrix(matrix, transform.dRotation);
    assertNotSame(matrix, transform.dRotation);
  }

  @Test
  public void testStateTransformUnwritableStateTransform() {
    transform = new StateTransform(StateTransform.IDENTITY);
    assertNotSame(StateTransform.IDENTITY.rotation, transform.rotation);
    assertNotSame(StateTransform.IDENTITY.dRotation, transform.dRotation);
    assertEqualStateTransform(
        new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, MatrixIJK.ZEROS),
        StateTransform.IDENTITY);
    assertEqualStateTransform(StateTransform.IDENTITY, transform);
  }

  @Test
  public void testCreateInverse() {
    StateTransform inverse = r.createInverse();
    assertNotSame(r, inverse);
    assertEqualMatrix(r.getRotation().createTranspose(), inverse.rotation);
    assertEqualMatrix(r.getRotationDerivative().createTranspose(), inverse.dRotation);
  }

  @Test
  public void testGetRotation() {
    RotationMatrixIJK result = transform.getRotation();

    assertEqualMatrix(rotation, result);

    /*
     * Now change result and verify that the contents of transform's internals are synchronized.
     */
    result.setTo(RotationMatrixIJK.IDENTITY);
    RotationMatrixIJK newResult = transform.getRotation();
    assertEqualMatrix(MatrixIJK.IDENTITY, newResult);
  }

  @Test
  public void testGetRotationDerivative() {
    MatrixIJK result = transform.getRotationDerivative();

    assertEqualMatrix(matrix, result);

    /*
     * This test is questionable, as the API does not clearly indicate that this is the case;
     * however it is precisely how the current implementation functions.
     */
    result.setTo(MatrixIJK.ONES);
    MatrixIJK newResult = transform.getRotationDerivative();

    assertEqualMatrix(MatrixIJK.ONES, newResult);

  }

  @Test
  public void testSetRotation() {
    transform.setRotation(RotationMatrixIJK.IDENTITY);
    assertNotSame(transform.rotation, RotationMatrixIJK.IDENTITY);
    assertEqualMatrix(RotationMatrixIJK.IDENTITY, transform.rotation);
    assertEqualMatrix(matrix, transform.dRotation);
  }

  @Test
  public void testSetRotationDerivative() {
    transform.setRotationDerivative(MatrixIJK.ZEROS);
    assertNotSame(transform.dRotation, MatrixIJK.ZEROS);
    assertEqualMatrix(MatrixIJK.ZEROS, transform.dRotation);
    assertEqualMatrix(rotation, transform.rotation);

  }

  @Test
  public void testSetTo() {
    transform.setTo(StateTransform.IDENTITY);
    assertNotSame(transform.rotation, StateTransform.IDENTITY.rotation);
    assertNotSame(transform.dRotation, StateTransform.IDENTITY.dRotation);
    assertEqualStateTransform(StateTransform.IDENTITY, transform);
  }

  @Test
  public void testInvert() {
    /*
     * Since the test case isn't actually a properly constructed state transform, we can't expect
     * the usual test of inversion to go well. Just exercise the fact that both components were
     * transposed and be done with it.
     */
    transform.invert();
    assertEqualMatrix(rotation.createTranspose(), transform.rotation);
    assertEqualMatrix(matrix.createTranspose(), transform.dRotation);

  }

  @Test
  @SuppressWarnings("deprecation")
  public void testMxv() {
    StateVector d = StateTransform.mxv(r, a, b);
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
  @SuppressWarnings("deprecation")
  public void testMxvOverA() {
    StateVector d = StateTransform.mxv(r, a, a);
    assertSame(d, a);
    assertEqualStateTransform(srcR, r);

    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);

  }

  @Test
  @SuppressWarnings("deprecation")
  public void testMixv() {
    StateVector d = StateTransform.mixv(r, a, b);
    assertSame(d, b);
    assertEqualStateVector(srcA, a);
    assertEqualStateTransform(srcR, r);

    r.invert();
    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);
  }

  @Test
  @SuppressWarnings("deprecation")
  public void testMixvOverA() {
    StateVector d = StateTransform.mixv(r, a, a);
    assertSame(d, a);
    assertEqualStateTransform(srcR, r);

    r.invert();
    VectorIJK tmP = r.rotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV1 = r.rotation.mxv(srcA.velocity, new VectorIJK());
    VectorIJK tmV2 = r.dRotation.mxv(srcA.position, new VectorIJK());
    VectorIJK tmV = VectorIJK.add(tmV1, tmV2, new VectorIJK());

    assertEquivalentStateVector(new UnwritableStateVector(tmP, tmV), d);
  }

  @Test
  public void testMxm() {
    StateTransform u = StateTransform.mxm(r, s, t);
    assertSame(u, t);
    assertEqualStateTransform(srcR, r);
    assertEqualStateTransform(srcS, s);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxm(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxm(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxm(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMxmOverR() {
    StateTransform u = StateTransform.mxm(r, s, r);
    assertSame(u, r);
    assertEqualStateTransform(srcS, s);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxm(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxm(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxm(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMxmOverS() {
    StateTransform u = StateTransform.mxm(r, s, s);
    assertSame(u, s);
    assertEqualStateTransform(srcR, r);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxm(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxm(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxm(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMxmRROverR() {
    StateTransform u = StateTransform.mxm(r, r, r);
    assertSame(u, r);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxm(srcR.rotation, srcR.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxm(srcR.dRotation, srcR.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxm(srcR.rotation, srcR.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMixm() {
    StateTransform u = StateTransform.mixm(r, s, t);
    assertSame(u, t);
    assertEqualStateTransform(srcR, r);
    assertEqualStateTransform(srcS, s);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mtxm(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mtxm(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mtxm(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMixmOverR() {
    StateTransform u = StateTransform.mixm(r, s, r);
    assertSame(u, r);
    assertEqualStateTransform(srcS, s);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mtxm(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mtxm(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mtxm(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMixmOverS() {
    StateTransform u = StateTransform.mixm(r, s, s);
    assertSame(u, s);
    assertEqualStateTransform(srcR, r);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mtxm(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mtxm(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mtxm(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);

  }

  @Test
  public void testMixmRROverR() {
    StateTransform u = StateTransform.mixm(r, r, r);
    assertSame(u, r);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mtxm(srcR.rotation, srcR.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mtxm(srcR.dRotation, srcR.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mtxm(srcR.rotation, srcR.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);

  }

  @Test
  public void testMxmi() {
    StateTransform u = StateTransform.mxmi(r, s, t);
    assertSame(u, t);
    assertEqualStateTransform(srcR, r);
    assertEqualStateTransform(srcS, s);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxmt(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxmt(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxmt(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMxmiOverR() {
    StateTransform u = StateTransform.mxmi(r, s, r);
    assertSame(u, r);
    assertEqualStateTransform(srcS, s);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxmt(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxmt(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxmt(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMxmiOverS() {
    StateTransform u = StateTransform.mxmi(r, s, s);
    assertSame(u, s);
    assertEqualStateTransform(srcR, r);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxmt(srcR.rotation, srcS.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxmt(srcR.dRotation, srcS.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxmt(srcR.rotation, srcS.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);
  }

  @Test
  public void testMxmiRROverR() {
    StateTransform u = StateTransform.mxmi(r, r, r);
    assertSame(u, r);

    RotationMatrixIJK tmR =
        RotationMatrixIJK.mxmt(srcR.rotation, srcR.rotation, new RotationMatrixIJK());
    MatrixIJK tmD1 = MatrixIJK.mxmt(srcR.dRotation, srcR.rotation, new MatrixIJK());
    MatrixIJK tmD2 = MatrixIJK.mxmt(srcR.rotation, srcR.dRotation, new MatrixIJK());
    MatrixIJK tmD = MatrixIJK.add(tmD1, tmD2, new MatrixIJK());

    assertEquivalentStateTransform(new UnwritableStateTransform(tmR, tmD), u);

  }

  public static void checkStaticFinalMembers() {
    assertEqualStateTransform(
        new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, MatrixIJK.ZEROS),
        StateTransform.IDENTITY);
  }

}
