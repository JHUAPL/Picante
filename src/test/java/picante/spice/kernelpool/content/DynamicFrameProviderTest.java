package picante.spice.kernelpool.content;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.fk.dynamic.DynamicEulerFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrozenFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicInertialFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicMeanEclipticFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicMeanEquatorFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicTrueEquatorFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicTwoVectorFrameFunction;

public class DynamicFrameProviderTest {

  private static MockSpice mockSpice;
  private static FrameInfo twoVec;
  private static FrameInfo meanEq;
  private static FrameInfo lackingMeanEq;
  private static FrameInfo trueEq;
  private static FrameInfo meanEc;
  private static FrameInfo euler;
  private static FrameInfo inertialMeanEc;
  private static FrameInfo frozenTwoVec;


  @BeforeClass
  public static void setUpMockSpice() {
    MockSpice.Builder mockSpiceBldr = new MockSpice.Builder();
    /*
     * Two Vector
     */
    twoVec = new FrameInfo("TWOVEC", 0, FrameType.DYNAMIC, 0, 0);
    mockSpiceBldr.addToPool(twoVec, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(twoVec, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(twoVec, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(twoVec, "PRI_VECTOR_DEF", "OBSERVER_TARGET_POSITION");
    mockSpiceBldr.addToPool(twoVec, "PRI_OBSERVER", "SATURN");
    mockSpiceBldr.addToPool(twoVec, "PRI_TARGET", "SUN");
    mockSpiceBldr.addToPool(twoVec, "PRI_ABCORR", "NONE");
    mockSpiceBldr.addToPool(twoVec, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(twoVec, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(twoVec, "SEC_VECTOR_DEF", "OBSERVER_TARGET_VELOCITY");
    mockSpiceBldr.addToPool(twoVec, "SEC_OBSERVER", "SUN");
    mockSpiceBldr.addToPool(twoVec, "SEC_TARGET", "SATURN");
    mockSpiceBldr.addToPool(twoVec, "SEC_ABCORR", "NONE");
    mockSpiceBldr.addToPool(twoVec, "SEC_FRAME", "J2000");

    /*
     * Mean Equator and Equinox
     */
    meanEq = new FrameInfo("MEANEQ", 1, FrameType.DYNAMIC, 1, 1);
    mockSpiceBldr.addToPool(meanEq, "FAMILY", "MEAN_EQUATOR_AND_EQUINOX_OF_DATE");
    mockSpiceBldr.addToPool(meanEq, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(meanEq, "PREC_MODEL", "EARTH_IAU_1976");
    mockSpiceBldr.addToPool(meanEq, "ROTATION_STATE", "ROTATING");
    mockSpiceBldr.addToPool(meanEq, "SEC_FRAME", "J2000");

    /*
     * True Equator and Equinox
     */
    trueEq = new FrameInfo("TRUEEQ", 2, FrameType.DYNAMIC, 2, 2);
    mockSpiceBldr.addToPool(trueEq, "FAMILY", "TRUE_EQUATOR_AND_EQUINOX_OF_DATE");
    mockSpiceBldr.addToPool(trueEq, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(trueEq, "PREC_MODEL", "EARTH_IAU_1976");
    mockSpiceBldr.addToPool(trueEq, "NUT_MODEL", "EARTH_IAU_1980");
    mockSpiceBldr.addToPool(trueEq, "ROTATION_STATE", "ROTATING");
    mockSpiceBldr.addToPool(trueEq, "SEC_FRAME", "J2000");

    /*
     * Mean Ecliptic and Equinox
     */
    meanEc = new FrameInfo("MEANEC", 3, FrameType.DYNAMIC, 3, 3);
    mockSpiceBldr.addToPool(meanEc, "FAMILY", "MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE");
    mockSpiceBldr.addToPool(meanEc, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(meanEc, "PREC_MODEL", "EARTH_IAU_1976");
    mockSpiceBldr.addToPool(meanEc, "OBLIQ_MODEL", "EARTH_IAU_1980");
    mockSpiceBldr.addToPool(meanEc, "ROTATION_STATE", "ROTATING");
    mockSpiceBldr.addToPool(meanEc, "SEC_FRAME", "J2000");

    /*
     * Lacking Mean Equator and Equinox
     */
    lackingMeanEq = new FrameInfo("LACKINGMEANEQ", 4, FrameType.DYNAMIC, 4, 4);
    mockSpiceBldr.addToPool(lackingMeanEq, "FAMILY", "MEAN_EQUATOR_AND_EQUINOX_OF_DATE");
    mockSpiceBldr.addToPool(lackingMeanEq, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(lackingMeanEq, "PREC_MODEL", "EARTH_IAU_1976");
    mockSpiceBldr.addToPool(lackingMeanEq, "SEC_FRAME", "J2000");

    /*
     * Full euler frame variables
     */
    euler = new FrameInfo("EULER", 5, FrameType.DYNAMIC, 5, 5);
    mockSpiceBldr.addToPool(euler, "FAMILY", "EULER");
    mockSpiceBldr.addToPool(euler, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(euler, "EPOCH", 2E8);
    mockSpiceBldr.addIntegersToPool(euler, "AXES", Lists.newArrayList(3, 1, 3));
    mockSpiceBldr.addToPool(euler, "UNITS", "DEGREES");
    mockSpiceBldr.addToPool(euler, "ANGLE_1_COEFFS", Lists.newArrayList(-1.2230000000000E+01,
        -9.2946839019223E-03, 4.0736989883402E-13, -1.2268739303111E-21));
    mockSpiceBldr.addToPool(euler, "ANGLE_2_COEFFS", 0);
    mockSpiceBldr.addToPool(euler, "ANGLE_3_COEFFS", 0);
    /*
     * Inertial Mean Ecliptic and Equinox
     */
    inertialMeanEc = new FrameInfo("INERTIALMEANEC", 6, FrameType.DYNAMIC, 6, 6);
    mockSpiceBldr.addToPool(inertialMeanEc, "FAMILY", "MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE");
    mockSpiceBldr.addToPool(inertialMeanEc, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(inertialMeanEc, "PREC_MODEL", "EARTH_IAU_1976");
    mockSpiceBldr.addToPool(inertialMeanEc, "OBLIQ_MODEL", "EARTH_IAU_1980");
    mockSpiceBldr.addToPool(inertialMeanEc, "ROTATION_STATE", "INERTIAL");
    mockSpiceBldr.addToPool(inertialMeanEc, "SEC_FRAME", "J2000");


    /*
     * Frozen Two Vector
     */
    frozenTwoVec = new FrameInfo("FROZENTWOVEC", 7, FrameType.DYNAMIC, 7, 7);
    mockSpiceBldr.addToPool(frozenTwoVec, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(frozenTwoVec, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(frozenTwoVec, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(frozenTwoVec, "PRI_VECTOR_DEF", "OBSERVER_TARGET_POSITION");
    mockSpiceBldr.addToPool(frozenTwoVec, "PRI_OBSERVER", "SATURN");
    mockSpiceBldr.addToPool(frozenTwoVec, "PRI_TARGET", "SUN");
    mockSpiceBldr.addToPool(frozenTwoVec, "PRI_ABCORR", "NONE");
    mockSpiceBldr.addToPool(frozenTwoVec, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(frozenTwoVec, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(frozenTwoVec, "SEC_VECTOR_DEF", "OBSERVER_TARGET_VELOCITY");
    mockSpiceBldr.addToPool(frozenTwoVec, "SEC_OBSERVER", "SUN");
    mockSpiceBldr.addToPool(frozenTwoVec, "SEC_TARGET", "SATURN");
    mockSpiceBldr.addToPool(frozenTwoVec, "SEC_ABCORR", "NONE");
    mockSpiceBldr.addToPool(frozenTwoVec, "SEC_FRAME", "J2000");
    mockSpiceBldr.addToPool(frozenTwoVec, "FREEZE_EPOCH", 2E8);

    mockSpice = mockSpiceBldr.build();
  }


  @Test
  public void twoVectorTest() throws Exception {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(twoVec, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicTwoVectorFrameFunction);
  }


  @Test
  public void testMeanEquator() throws FKInstantiationException, KernelPoolValidationException {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(meanEq, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicMeanEquatorFrameFunction);
  }

  @Test(expected = FKInstantiationException.class)
  public void testLackingMeanEquator()
      throws FKInstantiationException, KernelPoolValidationException {
    DynamicFrameProvider.createFunction(lackingMeanEq, mockSpice.getInfoHolder());
  }


  @Test
  public void testTrueEquator() throws FKInstantiationException, KernelPoolValidationException {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(trueEq, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicTrueEquatorFrameFunction);
  }

  @Test
  public void testMeanEcliptic() throws FKInstantiationException, KernelPoolValidationException {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(meanEc, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicMeanEclipticFrameFunction);
  }

  @Test
  public void eulerTest() throws Exception {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(euler, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicEulerFrameFunction);
  }

  @Test
  public void testInertialMeanEcliptic()
      throws FKInstantiationException, KernelPoolValidationException {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(inertialMeanEc, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicInertialFrameFunction);
  }

  @Test
  public void testFrozenTwoVec() throws FKInstantiationException, KernelPoolValidationException {
    DynamicFrameFunction function =
        DynamicFrameProvider.createFunction(frozenTwoVec, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicFrozenFrameFunction);
  }
}
