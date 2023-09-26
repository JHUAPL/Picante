package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEqualDouble;
import static picante.junit.AssertTools.assertEquivalentDouble;
import static picante.units.FundamentalPhysicalConstants.TWOPI;
import java.util.List;
import java.util.Random;
import java.util.Set;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.EulerAngles.Axis;
import picante.mechanics.rotations.EulerAnglesTest.AxisTriple;

/**
 * This class is abstract purely to allow the consolidation of code from its various concrete test
 * classes that exercise each of the types of Euler angle decompositions and their derivatives.
 * <p>
 * The abstract methods defined on this class have documentation that precede them which explain
 * their purpose, albeit tersely. The concrete test classes provide information to the abstract
 * parent class where all the tests are actually performed. If there were tests specific to a
 * particular type, they would be performed locally in that class. Given the general nature of the
 * Euler angle decompositions, all of the existing test cases were able to be represented as a
 * common set of tests regardless of specific implementation details.
 * </p>
 */
public abstract class DifferentiatedEulerAnglesTest {

  private static final UnwritableInterval CANONICAL_ANGLE_RANGE =
      new UnwritableInterval(-Math.PI, Math.PI);
  private static final UnwritableInterval DERIVATIVE_RANGE = new UnwritableInterval(-1, 1);

  static final double ANGLE_TOLERANCE = 3e-12;
  static final double TIGHT_TOLERANCE = 1e-15;
  static final double MATRIX_TOLERANCE = 2e-14;
  static final double DERIVATIVE_TOLERANCE = 2e-9;

  private static final double THIRDPI = Math.PI / 3.0;
  private static final double SIXTHPI = Math.PI / 6.0;

  private static final int NUM_RANDOM_CASES = 300;
  private static final long RANDOM_SEED = 0L;
  private static final Random RANDOM = new Random(RANDOM_SEED);

  private List<DifferentiatedEulerAngles> testSamples;
  private List<DifferentiatedEulerAngles> defaultSamples;

  private List<Double> testAnglesAndDerivatives;

  @Before
  public void setUp() throws Exception {

    testAnglesAndDerivatives = Lists.newArrayListWithCapacity(getNumberOfCases());

    for (int i = 0; i < 6 * getNumberOfCases(); i++) {
      testAnglesAndDerivatives.add(i * 0.01);
    }

    testSamples = Lists.newArrayListWithCapacity(getNumberOfCases());
    defaultSamples = Lists.newArrayListWithCapacity(getNumberOfCases());

    for (int i = 0; i < getNumberOfCases(); i++) {
      testSamples.add(createAngles(i, testAnglesAndDerivatives.get(6 * i),
          testAnglesAndDerivatives.get(6 * i + 1), testAnglesAndDerivatives.get(6 * i + 2),
          testAnglesAndDerivatives.get(6 * i + 3), testAnglesAndDerivatives.get(6 * i + 4),
          testAnglesAndDerivatives.get(6 * i + 5)));
      defaultSamples.add(createAngles(i));
    }

  }

  /**
   * @return the number of test cases the concrete subclass requires the standard tests to be run
   *         over.
   */
  public abstract int getNumberOfCases();

  /**
   * Creates an instance of DifferentiatedEulerAngles using the default, no argument constructor.
   * 
   * @param caseIndex an integer between 0 and {@link #getNumberOfCases()} -1 inclusive
   * 
   * @return newly created instance of the requested case
   */
  public abstract DifferentiatedEulerAngles createAngles(int caseIndex);

  /**
   * Creates an instance of {@link DifferentiatedEulerAngles} for a particular test case
   * 
   * @param caseIndex an integer between 0 and {@link #getNumberOfCases()} -1 inclusive
   * 
   * @param left
   * @param center
   * @param right
   * @param leftDerivative
   * @param centerDerivative
   * @param rightDerivative
   * @return
   */
  public abstract DifferentiatedEulerAngles createAngles(int caseIndex, double left, double center,
      double right, double leftDerivative, double centerDerivative, double rightDerivative);

  /**
   * Retrieve the class associated with the test case
   * 
   * @param caseIndex an integer between 0 and {@link #getNumberOfCases()} -1 inclusive
   * 
   * @return
   */
  public abstract Class<?> getClass(int caseIndex);

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
   * Retrieves the acceptable range of the center angle.
   * 
   * @return
   */
  public abstract UnwritableInterval getCenterAngleRange();

  /**
   * Computes the U parameter required in the conversion to and from state transforms
   * 
   * @param left
   * @param center
   * @param centerAngle
   * @return
   */
  public abstract double computeU(Axis left, Axis center, double centerAngle);

  /**
   * Computes the V parameter required in the conversion to and from state transforms
   * 
   * @param left
   * @param center
   * @param centerAngle
   * @return
   */
  public abstract double computeV(Axis left, Axis center, double centerAngle);

  /**
   * Required test case that must be generated by the subclass.
   */
  @Test
  public abstract void testDegenerateSetToCosAcceptedBranch();

  /**
   * Required test case that must be generated by the subclass.
   */
  @Test
  public abstract void testDegenerateSetToCosRejectedBranch();

  @Test
  public void testGetLeftAngle() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAnglesAndDerivatives.get(6 * i), testSamples.get(i).getLeftAngle());
    }
  }

  @Test
  public void testSetLeftAngle() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.setLeftAngle(-0.1);
      assertEqualDouble(-0.1, angles.getLeftAngle());
    }
  }

  @Test
  public void testGetCenterAngle() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAnglesAndDerivatives.get(6 * i + 1),
          testSamples.get(i).getCenterAngle());
    }
  }

  @Test
  public void testSetCenterAngle() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.setCenterAngle(Math.PI / 4.0);
      assertEqualDouble(Math.PI / 4.0, angles.getCenterAngle());
    }
  }

  @Test
  public void testGetRightAngle() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAnglesAndDerivatives.get(6 * i + 2),
          testSamples.get(i).getRightAngle());
    }
  }

  @Test
  public void testSetRightAngle() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.setRightAngle(-0.1);
      assertEqualDouble(-0.1, angles.getRightAngle());
    }
  }

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
      assertEquals(String.valueOf(i), getTriple(i).center, testSamples.get(i).getCenterAxis());
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
  public void testDefaultConstructorZeroAnglesAndDerivatives() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEquals(0.0, defaultSamples.get(i).getLeftAngle(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getCenterAngle(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getRightAngle(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getLeftAngleDerivative(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getCenterAngleDerivative(), 0.0);
      assertEquals(0.0, defaultSamples.get(i).getRightAngleDerivative(), 0.0);
    }
  }

  @Ignore
  @Test
  public void testOtherConstructors() {
    fail("Make up tests...");
  }

  @Test
  public void testGetLeftAngleDerivative() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAnglesAndDerivatives.get(6 * i + 3),
          testSamples.get(i).getLeftAngleDerivative());
    }
  }

  @Test
  public void testGetCenterAngleDerivative() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAnglesAndDerivatives.get(6 * i + 4),
          testSamples.get(i).getCenterAngleDerivative());
    }
  }

  @Test
  public void testGetRightAngleDerivative() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      assertEqualDouble(testAnglesAndDerivatives.get(6 * i + 5),
          testSamples.get(i).getRightAngleDerivative());
    }
  }

  @Test
  public void testSetLeftAngleDerivative() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.setLeftAngleDerivative(-0.1);
      assertEqualDouble(-0.1, angles.getLeftAngleDerivative());
    }
  }

  @Test
  public void testSetCenterAngleDerivative() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.setCenterAngleDerivative(-0.1);
      assertEqualDouble(-0.1, angles.getCenterAngleDerivative());
    }
  }

  @Test
  public void testSetRightAngleDerivative() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.setRightAngleDerivative(-0.1);
      assertEqualDouble(-0.1, angles.getRightAngleDerivative());
    }
  }

  @Test
  public void testSet() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      angles.set(0.1, 0.2, 0.3, -0.4, 0.5, -0.6);
      assertEqualDouble(0.1, angles.getLeftAngle());
      assertEquals(0.2, angles.getCenterAngle(), TIGHT_TOLERANCE);
      assertEqualDouble(0.3, angles.getRightAngle());
      assertEqualDouble(-0.4, angles.getLeftAngleDerivative());
      assertEqualDouble(0.5, angles.getCenterAngleDerivative());
      assertEqualDouble(-0.6, angles.getRightAngleDerivative());
    }
  }

  @Test
  public void testGetRotation() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      /*
       * Check to see if it is the class we are expecting.
       */
      assertEquals(getClass(i), defaultSamples.get(i).getClass());

      /*
       * Now verify that it is, in fact, a view of the internals.
       */
      EulerAngles view = defaultSamples.get(i).getRotation();

      view.setLeftAngle(0.5);
      assertEqualDouble(0.5, defaultSamples.get(i).getLeftAngle());

      view.setCenterAngle(0.6);
      assertEquals(0.6, defaultSamples.get(i).getCenterAngle(), TIGHT_TOLERANCE);

      view.setRightAngle(-0.1);
      assertEqualDouble(-0.1, defaultSamples.get(i).getRightAngle());
    }
  }

  @Test
  public void testComputeU() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      // TODO after switching to Crucible Math, we no longer get the exact equality
      assertEquivalentDouble(
          computeU(angles.getLeftAxis(), angles.getCenterAxis(), angles.getCenterAngle()),
          angles.computeU());
      // assertEqualDouble(
      // computeU(angles.getLeftAxis(), angles.getCenterAxis(), angles.getCenterAngle()),
      // angles.computeU());
    }
  }

  @Test
  public void testComputeV() {
    for (DifferentiatedEulerAngles angles : testSamples) {
      // TODO after switching to Crucible Math, we no longer get the exact equality
      assertEquals(computeV(angles.getLeftAxis(), angles.getCenterAxis(), angles.getCenterAngle()),
          angles.computeV(), 6.1E-17);
      // assertEqualDouble(
      // computeV(angles.getLeftAxis(), angles.getCenterAxis(), angles.getCenterAngle()),
      // angles.computeV());
    }
  }

  @Test
  public void testCanonicalize() {
    for (DifferentiatedEulerAngles angles : testSamples) {

      /*
       * Configure the angles to ranges outside the canonical ranges.
       */
      angles.setLeftAngle(angles.getLeftAngle() - Math.PI * 3.0);
      angles.setCenterAngle(angles.getCenterAngle() + TWOPI);
      angles.setRightAngle(angles.getRightAngle() + Math.PI * 4.0 + THIRDPI);

      StateTransform expected = angles.getTransform(new StateTransform());

      DifferentiatedEulerAngles result = angles.canonicalize();

      assertSame(angles, result);
      StateTransform actual = angles.getTransform(new StateTransform());

      assertComponentEquals(expected, actual, MATRIX_TOLERANCE);

    }
  }

  @Test
  public void testEqualsAndHashcode() {
    for (int i = 0; i < getNumberOfCases(); i++) {
      DifferentiatedEulerAngles tester = createAngles(i, THIRDPI, SIXTHPI, -THIRDPI, 1.0, 2.0, 3.0);
      DifferentiatedEulerAngles equal = createAngles(i, THIRDPI, SIXTHPI, -THIRDPI, 1.0, 2.0, 3.0);
      DifferentiatedEulerAngles notEqualAngles = createAngles(i, 0, 0, THIRDPI, 1.0, 2.0, 3.0);
      DifferentiatedEulerAngles notEqualDerivatives =
          createAngles(i, THIRDPI, SIXTHPI, -THIRDPI, 1.0, 2.0, -3.0);

      assertTrue(tester.equals(equal));
      assertTrue(equal.equals(tester));
      assertEquals(tester.hashCode(), equal.hashCode());

      assertFalse(tester.equals(notEqualAngles));
      assertFalse(notEqualAngles.equals(tester));

      assertFalse(tester.equals(notEqualDerivatives));
      assertFalse(notEqualDerivatives.equals(tester));

      assertFalse(tester.equals("NOT EULER ANGLES"));

      DifferentiatedEulerAngles alternateAxis =
          createAngles((i + 1) % getNumberOfCases(), THIRDPI, SIXTHPI, -THIRDPI, 1.0, 2.0, 3.0);
      assertFalse(tester.equals(alternateAxis));
      assertFalse(alternateAxis.equals(tester));
    }

  }

  @Test
  public void testTransformImplementation() {

    StateTransform transform = new StateTransform();
    StateTransform test = new StateTransform();

    for (DifferentiatedEulerAngles angles : testSamples) {

      int c = 0;
      while (c < NUM_RANDOM_CASES) {

        EulerAngles view = angles.getRotation();
        view.setLeftAngle(getNextDouble(CANONICAL_ANGLE_RANGE));
        view.setCenterAngle(getNextDouble(getCenterAngleRange()));
        view.setRightAngle(getNextDouble(CANONICAL_ANGLE_RANGE));
        angles.setLeftAngleDerivative(getNextDouble(DERIVATIVE_RANGE));
        angles.setCenterAngleDerivative(getNextDouble(DERIVATIVE_RANGE));
        angles.setRightAngleDerivative(getNextDouble(DERIVATIVE_RANGE));

        /*
         * Proceed, only if this is not a degenerate case.
         */
        if (angles.computeV() != 0) {
          String label = buildIdentifier(angles);

          StateTransform result = angles.getTransform(transform);

          /*
           * Test the transform matches expectations.
           */
          assertSame(result, transform);
          computeTransform(angles, test);
          assertComponentEquals("Testing Transform Content: " + label, test, transform,
              MATRIX_TOLERANCE);

          /*
           * Now verify that inversion from transform back to angles works properly.
           */
          double left = angles.getLeftAngle();
          double center = angles.getCenterAngle();
          double right = angles.getRightAngle();
          double leftDerivative = angles.getLeftAngleDerivative();
          double centerDerivative = angles.getCenterAngleDerivative();
          double rightDerivative = angles.getRightAngleDerivative();

          DifferentiatedEulerAngles deaResults = angles.setTo(transform);

          assertSame(deaResults, angles);

          assertEquals("Testing Left Recovery: " + label, left, angles.getLeftAngle(),
              ANGLE_TOLERANCE);
          assertEquals("Testing Center Recovery: " + label, center, angles.getCenterAngle(),
              ANGLE_TOLERANCE);
          assertEquals("Testing Right Recovery: " + label, right, angles.getRightAngle(),
              ANGLE_TOLERANCE);

          /*
           * The recovery of the derivatives seems to be relatively unstable numerically, hence the
           * larger tolerance.
           */
          assertEquals("Testing LeftDerivative Recovery: " + label, leftDerivative,
              angles.getLeftAngleDerivative(), DERIVATIVE_TOLERANCE);
          assertEquals("Testing CenterDerivative Recovery: " + label, centerDerivative,
              angles.getCenterAngleDerivative(), DERIVATIVE_TOLERANCE);
          assertEquals("Testing RightDerivative Recovery: " + label, rightDerivative,
              angles.getRightAngleDerivative(), DERIVATIVE_TOLERANCE);

        }

        c++;
      }

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
        DifferentiatedEulerAngles.create(triple.left, triple.center, triple.right, THIRDPI,
            -SIXTHPI, SIXTHPI, 0.1, 0.2, 0.3);
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
      DifferentiatedEulerAngles angles = DifferentiatedEulerAngles.create(triple.left,
          triple.center, triple.right, THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);

      assertEquals(triple.left, angles.getLeftAxis());
      assertEquals(triple.center, angles.getCenterAxis());
      assertEquals(triple.right, angles.getRightAxis());
      assertEquals(THIRDPI, angles.getLeftAngle(), 0.0);
      assertEquals(SIXTHPI, angles.getCenterAngle(), 0.0);
      assertEquals(-THIRDPI, angles.getRightAngle(), 0.0);
      assertEquals(0.1, angles.getLeftAngleDerivative(), 0.0);
      assertEquals(0.2, angles.getCenterAngleDerivative(), 0.0);
      assertEquals(0.3, angles.getRightAngleDerivative(), 0.0);
    }

  }

  @Test
  public void testCreateEulerAnglesDoubleDoubleDouble() {
    DifferentiatedEulerAngles result;
    EulerAngles.IJI iji = new EulerAngles.IJI(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(iji, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.IJI);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.IKI iki = new EulerAngles.IKI(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(iki, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.IKI);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.JIJ jij = new EulerAngles.JIJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(jij, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.JIJ);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.JKJ jkj = new EulerAngles.JKJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(jkj, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.JKJ);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.KIK kik = new EulerAngles.KIK(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(kik, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.KIK);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.KJK kjk = new EulerAngles.KJK(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(kjk, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.KJK);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.IJK ijk = new EulerAngles.IJK(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(ijk, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.IJK);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.IKJ ikj = new EulerAngles.IKJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(ikj, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.IKJ);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.JIK jik = new EulerAngles.JIK(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(jik, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.JIK);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.JKI jki = new EulerAngles.JKI(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(jki, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.JKI);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.KIJ kij = new EulerAngles.KIJ(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(kij, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.KIJ);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

    EulerAngles.KJI kji = new EulerAngles.KJI(THIRDPI, SIXTHPI, -THIRDPI);
    result = DifferentiatedEulerAngles.create(kji, 0.1, 0.2, 0.3);
    assertTrue(result instanceof DifferentiatedEulerAngles.KJI);
    assertEquals(THIRDPI, result.getLeftAngle(), 0.0);
    assertEquals(SIXTHPI, result.getCenterAngle(), 0.0);
    assertEquals(-THIRDPI, result.getRightAngle(), 0.0);
    assertEquals(0.1, result.getLeftAngleDerivative(), 0.0);
    assertEquals(0.2, result.getCenterAngleDerivative(), 0.0);
    assertEquals(0.3, result.getRightAngleDerivative(), 0.0);

  }

  @Test
  public void testCopyOf() {
    DifferentiatedEulerAngles result;

    DifferentiatedEulerAngles.IJI iji =
        new DifferentiatedEulerAngles.IJI(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(iji);
    assertNotSame(result, iji);
    assertEquals(result, iji);

    DifferentiatedEulerAngles.IKI iki =
        new DifferentiatedEulerAngles.IKI(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(iki);
    assertNotSame(result, iki);
    assertEquals(result, iki);

    DifferentiatedEulerAngles.JIJ jij =
        new DifferentiatedEulerAngles.JIJ(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(jij);
    assertNotSame(result, jij);
    assertEquals(result, jij);

    DifferentiatedEulerAngles.JKJ jkj =
        new DifferentiatedEulerAngles.JKJ(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(jkj);
    assertNotSame(result, jkj);
    assertEquals(result, jkj);

    DifferentiatedEulerAngles.KIK kik =
        new DifferentiatedEulerAngles.KIK(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(kik);
    assertNotSame(result, kik);
    assertEquals(result, kik);

    DifferentiatedEulerAngles.KJK kjk =
        new DifferentiatedEulerAngles.KJK(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(kjk);
    assertNotSame(result, kjk);
    assertEquals(result, kjk);

    DifferentiatedEulerAngles.IJK ijk =
        new DifferentiatedEulerAngles.IJK(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(ijk);
    assertNotSame(result, ijk);
    assertEquals(result, ijk);

    DifferentiatedEulerAngles.IKJ ikj =
        new DifferentiatedEulerAngles.IKJ(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(ikj);
    assertNotSame(result, ikj);
    assertEquals(result, ikj);

    DifferentiatedEulerAngles.JIK jik =
        new DifferentiatedEulerAngles.JIK(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(jik);
    assertNotSame(result, jik);
    assertEquals(result, jik);

    DifferentiatedEulerAngles.JKI jki =
        new DifferentiatedEulerAngles.JKI(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(jki);
    assertNotSame(result, jki);
    assertEquals(result, jki);

    DifferentiatedEulerAngles.KIJ kij =
        new DifferentiatedEulerAngles.KIJ(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(kij);
    assertNotSame(result, kij);
    assertEquals(result, kij);

    DifferentiatedEulerAngles.KJI kji =
        new DifferentiatedEulerAngles.KJI(THIRDPI, SIXTHPI, -THIRDPI, 0.1, 0.2, 0.3);
    result = DifferentiatedEulerAngles.copyOf(kji);
    assertNotSame(result, kji);
    assertEquals(result, kji);

  }

  private String buildIdentifier(DifferentiatedEulerAngles angles) {
    return "[" + angles.getLeftAngle() + ", " + angles.getLeftAngleDerivative() + "]_"
        + angles.getLeftAxis() + " [" + angles.getCenterAngle() + ", "
        + angles.getCenterAngleDerivative() + "]_" + angles.getCenterAxis() + " ["
        + angles.getRightAngle() + ", " + angles.getRightAngleDerivative() + "]_"
        + angles.getRightAxis();
  }

  private StateTransform computeTransform(DifferentiatedEulerAngles angles, StateTransform buffer) {

    RotationMatrixIJK left =
        angles.getLeftAxis().getRotation(angles.getLeftAngle(), new RotationMatrixIJK());
    RotationMatrixIJK center =
        angles.getCenterAxis().getRotation(angles.getCenterAngle(), new RotationMatrixIJK());
    RotationMatrixIJK right =
        angles.getRightAxis().getRotation(angles.getRightAngle(), new RotationMatrixIJK());

    MatrixWrapper leftWrapper = new MatrixWrapper(left);
    MatrixWrapper centerWrapper = new MatrixWrapper(center);
    MatrixWrapper rightWrapper = new MatrixWrapper(right);

    WrapperWithRate<MatrixWrapper> leftXf = new WrapperWithRate<MatrixWrapper>(leftWrapper,
        new VectorIJK(angles.getLeftAngleDerivative(), getBasisVector(angles.getLeftAxis())));
    WrapperWithRate<MatrixWrapper> centerXf = new WrapperWithRate<MatrixWrapper>(centerWrapper,
        new VectorIJK(angles.getCenterAngleDerivative(), getBasisVector(angles.getCenterAxis())));
    WrapperWithRate<MatrixWrapper> rightXf = new WrapperWithRate<MatrixWrapper>(rightWrapper,
        new VectorIJK(angles.getRightAngleDerivative(), getBasisVector(angles.getRightAxis())));

    /*
     * Build up the state transform by multiplying all of the individual transforms together.
     */
    StateTransform leftXform = leftXf.getTransform(new StateTransform());
    StateTransform centerXform = centerXf.getTransform(new StateTransform());
    StateTransform rightXform = rightXf.getTransform(new StateTransform());

    StateTransform.mxm(leftXform, centerXform, buffer);
    StateTransform.mxm(buffer, rightXform, buffer);

    return buffer;
  }

  private double getNextDouble(UnwritableInterval interval) {
    return RANDOM.nextDouble() * interval.getLength() + interval.getBegin();
  }

  private UnwritableVectorIJK getBasisVector(Axis axis) {
    switch (axis) {
      case I:
        return VectorIJK.I;
      case J:
        return VectorIJK.J;
      case K:
        return VectorIJK.K;
      default:
        throw new UnsupportedOperationException();
    }
  }

}
