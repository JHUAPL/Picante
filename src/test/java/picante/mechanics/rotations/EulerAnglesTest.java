package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualDouble;
import static picante.units.FundamentalPhysicalConstants.TWOPI;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.MoreObjects;
import com.google.common.collect.Sets;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.EulerAngles.Axis;

/**
 * This class is abstract purely to allow the consolidation of code from its various concrete test
 * classes that exercise each of the types of Euler angle decompositions.
 * <p>
 * The abstract methods defined on this class have documentation that precede them which explain
 * their purpose, albeit tersely. The concrete test classes provide information to the abstract
 * parent class where all the tests are actually performed. If there were tests specific to a
 * particular type, they would be performed locally in that class. Given the general nature of the
 * Euler angle decompositions, all of the existing test cases were able to be represented as a
 * common set of tests regardless of specific implementation details.
 * </p>
 */
public abstract class EulerAnglesTest {

  private static final double THIRDPI = Math.PI / 3.0;
  private static final double SIXTHPI = Math.PI / 6.0;

  private static final UnwritableInterval CANONICAL_ANGLE_RANGE =
      new UnwritableInterval(-Math.PI, Math.PI);

  private static final double NORM_TOLERANCE = 1e-14;
  private static final double DET_TOLERANCE = 1e-14;
  private static final double MAT_TOLERANCE = 1e-14;
  private static final double ANGLE_TOLERANCE = 3e-13;

  private static final int NUM_RANDOM_CASES = 300;
  private static final long RANDOM_SEED = 0L;
  private static final Random RANDOM = new Random(RANDOM_SEED);

  private Quaternion left;
  private Quaternion center;
  private Quaternion right;

  private List<EulerAngles> testSamples;
  private List<Double> testAngles;
  private List<EulerAngles> defaultSamples;

  @Before
  public void setUp() throws Exception {

    left = new Quaternion();
    center = new Quaternion();
    right = new Quaternion();

    testAngles = new ArrayList<Double>(getNumberOfCases());

    for (int i = 0; i < 3 * getNumberOfCases(); i++) {
      testAngles.add(i * 0.01);
    }

    testSamples = new ArrayList<EulerAngles>(getNumberOfCases());
    defaultSamples = new ArrayList<EulerAngles>(getNumberOfCases());

    for (int i = 0; i < getNumberOfCases(); i++) {
      testSamples.add(createAngles(i, testAngles.get(3 * i), testAngles.get(3 * i + 1),
          testAngles.get(3 * i + 2)));
      defaultSamples.add(createAngles(i));
    }

  }

  /**
   * Retrieves the number of test cases the concrete subclass requires the standard tests to be run
   * over.
   * 
   * @return
   */
  public abstract int getNumberOfCases();

  /**
   * Creates an instance of EulerAngles for a particular test case
   * 
   * @param caseIndex an integer between 0 and {@link #getNumberOfCases()}-1 inclusive.
   * @param left the value of the left angle in radians
   * @param center the value of the center angle in radians
   * @param right the value of the right angle in radians
   * 
   * @return a newly constructed instance of the requested test case
   */
  public abstract EulerAngles createAngles(int caseIndex, double left, double center, double right);

  /**
   * Creates the default instance of EulerAngles for a particular test case. The implementation
   * should invoke the no-argument constructor when satisfying the request.
   * 
   * @param caseIndex an integer between 0 and {@link #getNumberOfCases()}-1 inclusive
   * 
   * @return a newly, default no-argument constructed instance of the requested test case
   */
  public abstract EulerAngles createAngles(int caseIndex);

  /**
   * Creates an instance of EulerAngles for a particular test case
   * 
   * @param caseIndex an integer between 0 and {@link #getNumberOfCases()}-1 inclusive.
   * @param matrix the matrix to convert to the EulerAngles representation
   * 
   * @return a newly constructed instance of the requested test case
   */
  public abstract EulerAngles createAngles(int caseIndex, UnwritableRotationMatrixIJK matrix);

  /**
   * Retrieves the acceptable range of the center angle.
   * 
   * @return
   */
  public abstract UnwritableInterval getCenterAngleRange();

  /**
   * Retrieves an instance of AxisTriple that describes the axis selection for the specific test
   * case.
   * 
   * @param caseIndex the index of the test case.
   * 
   * @return a newly created instance of AxisTriple capturing the axes of rotation for the test case
   */
  public abstract AxisTriple getTriple(int caseIndex);

  /**
   * Returns the expected value of the right angle when the degenerate center angle case is
   * exercised on the beginning of the angle range.
   * 
   * @param angles the degenerate instance
   * 
   * @return the expected value of the right most angle in the degenerate case.
   */
  public abstract double expectedDegenerateAngleBeginCase(EulerAngles angles);

  /**
   * Returns the expected value of the right angle when the degenerate center angle case is
   * exercised on the end of the angle range.
   * 
   * @param angles the degenerate instance
   * 
   * @return the expected value of the right most angle in the degenerate case
   */
  public abstract double expectedDegenerateAngleEndCase(EulerAngles angles);

  @Test
  public void testGetLeftAxis() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEquals(getTriple(i).left, testSamples.get(i).getLeftAxis());
      assertEquals(getTriple(i).left, defaultSamples.get(i).getLeftAxis());
    }
  }

  @Test
  public void testGetCenterAxis() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEquals(getTriple(i).center, testSamples.get(i).getCenterAxis());
      assertEquals(getTriple(i).center, defaultSamples.get(i).getCenterAxis());
    }
  }

  @Test
  public void testGetRightAxis() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEquals(getTriple(i).right, testSamples.get(i).getRightAxis());
      assertEquals(getTriple(i).right, defaultSamples.get(i).getRightAxis());
    }
  }

  @Test
  public void testDefaultConstructorZeroAngles() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEquals(0.0, defaultSamples.get(i).getLeftAngle(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getCenterAngle(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getRightAngle(), 0.0);
    }
  }

  @Test
  public void testMatrixConstructors() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      RotationMatrixIJK testMatrix =
          createAngles(i, THIRDPI, SIXTHPI, -Math.PI / 2.0).getRotation(new RotationMatrixIJK());
      EulerAngles angles = createAngles(i, testMatrix);
      assertEquals(THIRDPI, angles.getLeftAngle(), ANGLE_TOLERANCE);
      assertEquals(SIXTHPI, angles.getCenterAngle(), ANGLE_TOLERANCE);
      assertEquals(-Math.PI / 2.0, angles.getRightAngle(), ANGLE_TOLERANCE);
    }
  }

  @Test
  public void testGetLeftAngle() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAngles.get(3 * i), testSamples.get(i).getLeftAngle());
    }
  }

  @Test
  public void testSetLeftAngle() {
    for (EulerAngles angles : testSamples) {
      angles.setLeftAngle(-0.1);
      assertEqualDouble(-0.1, angles.getLeftAngle());
    }
  }

  @Test
  public void testGetCenterAngle() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAngles.get(3 * i + 1), testSamples.get(i).getCenterAngle());
    }
  }

  @Test
  public void testSetCenterAngle() {
    for (EulerAngles angles : testSamples) {
      angles.setCenterAngle(Math.PI / 4.0);
      assertEqualDouble(Math.PI / 4.0, angles.getCenterAngle());
    }
  }

  @Test
  public void testGetRightAngle() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAngles.get(3 * i + 2), testSamples.get(i).getRightAngle());
    }
  }

  @Test
  public void testSetRightAngle() {
    for (EulerAngles angles : testSamples) {
      angles.setRightAngle(-0.1);
      assertEqualDouble(-0.1, angles.getRightAngle());
    }
  }

  @Test
  public void testSet() {
    for (EulerAngles angles : testSamples) {
      angles.set(0.1, 0.2, 0.3);
      assertEqualDouble(0.1, angles.getLeftAngle());
      assertEquals(0.2, angles.getCenterAngle(), ANGLE_TOLERANCE);
      assertEqualDouble(0.3, angles.getRightAngle());
    }
  }

  @Test
  public void testCanonicalize() {
    for (EulerAngles angles : testSamples) {
      /*
       * Configure the angles to ranges outside the canonical ranges.
       */
      angles.setLeftAngle(angles.getLeftAngle() - Math.PI * 3.0);
      angles.setCenterAngle(angles.getCenterAngle() + TWOPI);
      angles.setRightAngle(angles.getRightAngle() + Math.PI * 4.0 + THIRDPI);

      RotationMatrixIJK expected = angles.getRotation(new RotationMatrixIJK());
      EulerAngles result = angles.canonicalize();

      assertSame(result, angles);

      RotationMatrixIJK actual = angles.getRotation(new RotationMatrixIJK());

      assertComponentEquals(expected, actual, MAT_TOLERANCE);
    }
  }

  @Test
  public void testRotationImplementation() {

    RotationMatrixIJK rot = new RotationMatrixIJK();
    RotationMatrixIJK test = new RotationMatrixIJK();

    for (EulerAngles angles : testSamples) {

      for (int c = 0; c < NUM_RANDOM_CASES; c++) {
        angles.setLeftAngle(getNextDouble(CANONICAL_ANGLE_RANGE));
        angles.setCenterAngle(getNextDouble(getCenterAngleRange()));
        angles.setRightAngle(getNextDouble(CANONICAL_ANGLE_RANGE));

        String label = buildIdentifier(angles);

        angles.getRotation(rot);

        /*
         * Check that this is a proper rotation.
         */
        assertTrue("Testing Is Rotation: " + label, rot.isRotation(NORM_TOLERANCE, DET_TOLERANCE));
        buildRotationFromAngles(angles, test);
        assertComponentEquals("Testing Rotation Content: " + label, test, rot, MAT_TOLERANCE);

        /*
         * Now verify the inversion from matrix back to angles works properly.
         */
        double left = angles.getLeftAngle();
        double center = angles.getCenterAngle();
        double right = angles.getRightAngle();

        angles.setTo(rot);

        assertEquals("Testing Left Recovery: " + label, left, angles.getLeftAngle(),
            ANGLE_TOLERANCE);
        assertEquals("Testing Center Recovery: " + label, center, angles.getCenterAngle(),
            ANGLE_TOLERANCE);
        assertEquals("Testing Right Recovery: " + label, right, angles.getRightAngle(),
            ANGLE_TOLERANCE);

      }
    }
  }

  @Test
  public void testRotationImplementationBeginCenterAngleDegenerateCase() {

    RotationMatrixIJK rot = new RotationMatrixIJK();
    RotationMatrixIJK test = new RotationMatrixIJK();

    for (EulerAngles angles : testSamples) {

      double left = SIXTHPI;
      double center = getCenterAngleRange().getBegin();
      double right = THIRDPI;

      angles.setLeftAngle(left);
      angles.setCenterAngle(center);
      angles.setRightAngle(right);

      String label = buildIdentifier(angles);

      angles.getRotation(rot);
      angles.setTo(rot);

      if (angles.getLeftAngle() == 0.0) {
        right = expectedDegenerateAngleBeginCase(angles);
        left = 0;

        assertEquals("Testing Left Recovery: " + label, left, angles.getLeftAngle(),
            ANGLE_TOLERANCE);
        assertEquals("Testing Center Recovery: " + label, center, angles.getCenterAngle(),
            ANGLE_TOLERANCE);
        assertEquals("Testing Right Recovery: " + label, right, angles.getRightAngle(),
            ANGLE_TOLERANCE);
      }

      /*
       * Regardless of what happened, verify that the recovered angles can be turned back into the
       * same rotation matrix.
       */
      angles.getRotation(test);

      assertComponentEquals("Testing Matrix: " + label, rot, test, MAT_TOLERANCE);
    }
  }

  @Test
  public void testRotationImplementationEndCenterAngleDegenerateCase() {

    RotationMatrixIJK rot = new RotationMatrixIJK();
    RotationMatrixIJK test = new RotationMatrixIJK();

    for (EulerAngles angles : testSamples) {

      double left = SIXTHPI;
      double center = getCenterAngleRange().getEnd();
      double right = THIRDPI;

      angles.setLeftAngle(left);
      angles.setCenterAngle(center);
      angles.setRightAngle(right);

      String label = buildIdentifier(angles);

      angles.getRotation(rot);
      angles.setTo(rot);

      if (angles.getLeftAngle() == 0.0) {
        right = expectedDegenerateAngleEndCase(angles);
        left = 0;

        assertEquals("Testing Left Recovery: " + label, left, angles.getLeftAngle(),
            ANGLE_TOLERANCE);
        assertEquals("Testing Center Recovery: " + label, center, angles.getCenterAngle(),
            ANGLE_TOLERANCE);
        assertEquals("Testing Right Recovery: " + label, right, angles.getRightAngle(),
            ANGLE_TOLERANCE);
      }

      /*
       * Regardless of what happened, verify that the recovered angles can be turned back into the
       * same rotation matrix.
       */
      angles.getRotation(test);

      assertComponentEquals("Testing Matrix: " + label, rot, test, MAT_TOLERANCE);
    }
  }

  @Test
  public void testEqualsAndHashcode() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      EulerAngles tester = createAngles(i, THIRDPI, SIXTHPI, -THIRDPI);
      EulerAngles equal = createAngles(i, THIRDPI, SIXTHPI, -THIRDPI);
      EulerAngles notEqual = createAngles(i, 0, 0, THIRDPI);

      assertTrue(tester.equals(equal));
      assertTrue(equal.equals(tester));
      assertEquals(tester.hashCode(), equal.hashCode());

      assertFalse(tester.equals(notEqual));
      assertFalse(notEqual.equals(tester));

      assertFalse(tester.equals("NOT EULER ANGLES"));

      EulerAngles alternateAxis =
          createAngles((i + 1) % getNumberOfCases(), THIRDPI, SIXTHPI, -THIRDPI);
      assertFalse(tester.equals(alternateAxis));
      assertFalse(alternateAxis.equals(tester));
    }
  }

  @Test
  public void testCreateAxisExceptions() {

    Set<AxisTriple> bogusTriples = Sets.newLinkedHashSet();
    bogusTriples.add(new AxisTriple(Axis.I, Axis.I, Axis.I));
    bogusTriples.add(new AxisTriple(Axis.J, Axis.J, Axis.J));
    bogusTriples.add(new AxisTriple(Axis.K, Axis.K, Axis.K));
    bogusTriples.add(new AxisTriple(Axis.I, Axis.I, Axis.J));
    bogusTriples.add(new AxisTriple(Axis.I, Axis.I, Axis.K));
    bogusTriples.add(new AxisTriple(Axis.J, Axis.J, Axis.I));
    bogusTriples.add(new AxisTriple(Axis.J, Axis.J, Axis.K));
    bogusTriples.add(new AxisTriple(Axis.K, Axis.K, Axis.I));
    bogusTriples.add(new AxisTriple(Axis.K, Axis.K, Axis.J));
    bogusTriples.add(new AxisTriple(Axis.J, Axis.I, Axis.I));
    bogusTriples.add(new AxisTriple(Axis.K, Axis.I, Axis.I));
    bogusTriples.add(new AxisTriple(Axis.I, Axis.J, Axis.J));
    bogusTriples.add(new AxisTriple(Axis.K, Axis.J, Axis.J));
    bogusTriples.add(new AxisTriple(Axis.I, Axis.K, Axis.K));
    bogusTriples.add(new AxisTriple(Axis.J, Axis.K, Axis.K));

    for (AxisTriple triple : bogusTriples) {
      try {
        EulerAngles.create(triple.left, triple.center, triple.right, THIRDPI, -SIXTHPI, SIXTHPI);
        fail("Expected exception not thrown: " + triple);
      } catch (IllegalArgumentException e) {
      }
    }

  }

  @Test
  public void testCreate() {

    Set<AxisTriple> allowedTriples = Sets.newLinkedHashSet();
    allowedTriples.add(new AxisTriple(Axis.I, Axis.J, Axis.I));
    allowedTriples.add(new AxisTriple(Axis.I, Axis.K, Axis.I));
    allowedTriples.add(new AxisTriple(Axis.J, Axis.I, Axis.J));
    allowedTriples.add(new AxisTriple(Axis.J, Axis.K, Axis.J));
    allowedTriples.add(new AxisTriple(Axis.K, Axis.I, Axis.K));
    allowedTriples.add(new AxisTriple(Axis.K, Axis.J, Axis.K));
    allowedTriples.add(new AxisTriple(Axis.I, Axis.J, Axis.K));
    allowedTriples.add(new AxisTriple(Axis.I, Axis.K, Axis.J));
    allowedTriples.add(new AxisTriple(Axis.J, Axis.I, Axis.K));
    allowedTriples.add(new AxisTriple(Axis.J, Axis.K, Axis.I));
    allowedTriples.add(new AxisTriple(Axis.K, Axis.I, Axis.J));
    allowedTriples.add(new AxisTriple(Axis.K, Axis.J, Axis.I));

    for (AxisTriple triple : allowedTriples) {
      EulerAngles angles =
          EulerAngles.create(triple.left, triple.center, triple.right, THIRDPI, SIXTHPI, -THIRDPI);

      assertEquals(triple.left, angles.getLeftAxis());
      assertEquals(triple.center, angles.getCenterAxis());
      assertEquals(triple.right, angles.getRightAxis());
      assertEquals(THIRDPI, angles.getLeftAngle(), 0.0);
      assertEquals(SIXTHPI, angles.getCenterAngle(), 0.0);
      assertEquals(-THIRDPI, angles.getRightAngle(), 0.0);
    }

  }

  @Test
  public void testCopyOf() {
    EulerAngles result;

    EulerAngles.IJI iji = new EulerAngles.IJI(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(iji);
    assertNotSame(result, iji);
    assertEquals(result, iji);

    EulerAngles.IKI iki = new EulerAngles.IKI(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(iki);
    assertNotSame(result, iki);
    assertEquals(result, iki);

    EulerAngles.JIJ jij = new EulerAngles.JIJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(jij);
    assertNotSame(result, jij);
    assertEquals(result, jij);

    EulerAngles.JKJ jkj = new EulerAngles.JKJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(jkj);
    assertNotSame(result, jkj);
    assertEquals(result, jkj);

    EulerAngles.KIK kik = new EulerAngles.KIK(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(kik);
    assertNotSame(result, kik);
    assertEquals(result, kik);

    EulerAngles.KJK kjk = new EulerAngles.KJK(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(kjk);
    assertNotSame(result, kjk);
    assertEquals(result, kjk);

    EulerAngles.IJK ijk = new EulerAngles.IJK(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(ijk);
    assertNotSame(result, ijk);
    assertEquals(result, ijk);

    EulerAngles.KJI KJI = new EulerAngles.KJI(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(KJI);
    assertNotSame(result, KJI);
    assertEquals(result, KJI);

    EulerAngles.JIK jik = new EulerAngles.JIK(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(jik);
    assertNotSame(result, jik);
    assertEquals(result, jik);

    EulerAngles.JKI jki = new EulerAngles.JKI(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(jki);
    assertNotSame(result, jki);
    assertEquals(result, jki);

    EulerAngles.KIJ kij = new EulerAngles.KIJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(kij);
    assertNotSame(result, kij);
    assertEquals(result, kij);

    EulerAngles.KJI kji = new EulerAngles.KJI(THIRDPI, SIXTHPI, -THIRDPI);
    result = EulerAngles.copyOf(kji);
    assertNotSame(result, kji);
    assertEquals(result, kji);

  }

  @Test
  public void testCreateAlternateDecomposition() {

    RotationMatrixIJK test = new AxisAndAngle(new VectorIJK(1, 2, 3), Math.toRadians(15.0))
        .getRotation(new RotationMatrixIJK());
    RotationMatrixIJK testTranspose = test.createTranspose();

    EulerAngles.IJI iji = new EulerAngles.IJI(test);
    EulerAngles result = iji.createAlternateDecomposition(testTranspose);
    assertNotSame(iji, result);
    assertTrue(iji.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(iji.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.IKI iki = new EulerAngles.IKI(test);
    result = iki.createAlternateDecomposition(testTranspose);
    assertNotSame(iki, result);
    assertTrue(iki.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(iki.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.JIJ jij = new EulerAngles.JIJ(test);
    result = jij.createAlternateDecomposition(testTranspose);
    assertNotSame(jij, result);
    assertTrue(jij.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(jij.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.JKJ jkj = new EulerAngles.JKJ(test);
    result = jkj.createAlternateDecomposition(testTranspose);
    assertNotSame(jkj, result);
    assertTrue(jkj.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(jkj.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.KIK kik = new EulerAngles.KIK(test);
    result = kik.createAlternateDecomposition(testTranspose);
    assertNotSame(kik, result);
    assertTrue(kik.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(kik.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.KJK kjk = new EulerAngles.KJK(test);
    result = kjk.createAlternateDecomposition(testTranspose);
    assertNotSame(kjk, result);
    assertTrue(kjk.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(kjk.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.IJK ijk = new EulerAngles.IJK(test);
    result = ijk.createAlternateDecomposition(testTranspose);
    assertNotSame(ijk, result);
    assertTrue(ijk.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(ijk.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.IKJ ikj = new EulerAngles.IKJ(test);
    result = ikj.createAlternateDecomposition(testTranspose);
    assertNotSame(ikj, result);
    assertTrue(ikj.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(ikj.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.JIK jik = new EulerAngles.JIK(test);
    result = jik.createAlternateDecomposition(testTranspose);
    assertNotSame(jik, result);
    assertTrue(jik.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(jik.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.JKI jki = new EulerAngles.JKI(test);
    result = jki.createAlternateDecomposition(testTranspose);
    assertNotSame(jki, result);
    assertTrue(jki.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(jki.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.KIJ kij = new EulerAngles.KIJ(test);
    result = kij.createAlternateDecomposition(testTranspose);
    assertNotSame(kij, result);
    assertTrue(kij.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(kij.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

    EulerAngles.KJI kji = new EulerAngles.KJI(test);
    result = kji.createAlternateDecomposition(testTranspose);
    assertNotSame(kji, result);
    assertTrue(kji.getClass().isAssignableFrom(result.getClass()));
    assertComponentEquals(kji.getRotation(new RotationMatrixIJK()),
        result.getRotation(new RotationMatrixIJK()), 1e-15);

  }

  private RotationMatrixIJK buildRotationFromAngles(EulerAngles angles, RotationMatrixIJK buffer) {

    left.setTo(Math.cos(-angles.getLeftAngle() / 2.0),
        computeQuaternionAxisComponent(1, angles.getLeftAxis(), angles.getLeftAngle()),
        computeQuaternionAxisComponent(2, angles.getLeftAxis(), angles.getLeftAngle()),
        computeQuaternionAxisComponent(3, angles.getLeftAxis(), angles.getLeftAngle()));

    center.setTo(Math.cos(-angles.getCenterAngle() / 2.0),
        computeQuaternionAxisComponent(1, angles.getCenterAxis(), angles.getCenterAngle()),
        computeQuaternionAxisComponent(2, angles.getCenterAxis(), angles.getCenterAngle()),
        computeQuaternionAxisComponent(3, angles.getCenterAxis(), angles.getCenterAngle()));

    right.setTo(Math.cos(-angles.getRightAngle() / 2.0),
        computeQuaternionAxisComponent(1, angles.getRightAxis(), angles.getRightAngle()),
        computeQuaternionAxisComponent(2, angles.getRightAxis(), angles.getRightAngle()),
        computeQuaternionAxisComponent(3, angles.getRightAxis(), angles.getRightAngle()));

    /*
     * Now that we have build quaternions capturing each of the Euler rotations, chain them together
     * appropriately to build the output product.
     */
    Quaternion.multiply(left, center, center);
    Quaternion.multiply(center, right, right);

    right.getRotation(buffer);

    return buffer;
  }

  private double computeQuaternionAxisComponent(int component, EulerAngles.Axis axis,
      double angle) {
    /*
     * This is a total hack, but it works since the elements of the axis are ordered according to
     * axis locations.
     */
    if (component == (axis.ordinal() + 1)) {
      return Math.sin(-angle / 2.0);
    }
    return 0.0;
  }

  private String buildIdentifier(EulerAngles angles) {
    return "[" + angles.getLeftAngle() + "]_" + angles.getLeftAxis() + " ["
        + angles.getCenterAngle() + "]_" + angles.getCenterAxis() + " [" + angles.getRightAngle()
        + "]_" + angles.getRightAxis();
  }

  private double getNextDouble(UnwritableInterval interval) {
    return RANDOM.nextDouble() * interval.getLength() + interval.getBegin();
  }

  /**
   * Class that captures the concrete subclasses intention for a specific test case with regards to
   * axes of rotation and order.
   */
  static class AxisTriple {

    final EulerAngles.Axis left;
    final EulerAngles.Axis center;
    final EulerAngles.Axis right;

    AxisTriple(Axis left, Axis center, Axis right) {
      this.left = left;
      this.center = center;
      this.right = right;
    }

    @Override
    public String toString() {
      return MoreObjects.toStringHelper(this).add("left", left).add("center", center)
          .add("right", right).toString();
    }

  }

}
