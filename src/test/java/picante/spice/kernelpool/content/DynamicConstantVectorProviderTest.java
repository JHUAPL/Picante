package picante.spice.kernelpool.content;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.fk.dynamic.DefinableStateVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicConstantVectorFunction;
import picante.spice.kernelpool.content.DynamicVectorProvider.VectorID;


public class DynamicConstantVectorProviderTest {

  private static MockSpice mockSpice;
  private static FrameInfo rectXlatArc;
  private static FrameInfo raDec_LatRad;
  private static FrameInfo badRect_LatDeg;

  @BeforeClass
  public static void setUpMockSpice() {
    MockSpice.Builder mockSpiceBldr = new MockSpice.Builder();
    /*
     * Constant rectangular by constant latitudinal in arcseconds
     */
    rectXlatArc = new FrameInfo("RECT_LAT_ARC", 0, FrameType.DYNAMIC, 0, 0);
    mockSpiceBldr.addToPool(rectXlatArc, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(rectXlatArc, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(rectXlatArc, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(rectXlatArc, "PRI_VECTOR_DEF", "CONSTANT");
    mockSpiceBldr.addToPool(rectXlatArc, "PRI_SPEC", "RECTANGULAR");
    mockSpiceBldr.addToPool(rectXlatArc, "PRI_VECTOR", Lists.newArrayList(1.0, 2.0, 3.0));
    mockSpiceBldr.addToPool(rectXlatArc, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_VECTOR_DEF", "CONSTANT");
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_SPEC", "LATITUDINAL");
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_UNITS", "ARCSECONDS");
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_LONGITUDE", 60);
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_LATITUDE", 60);
    mockSpiceBldr.addToPool(rectXlatArc, "SEC_FRAME", "J2000");

    /*
     * Constant ra dec by constant latitudinal in radians
     */
    raDec_LatRad = new FrameInfo("RADEC_LAT_RAD", 1, FrameType.DYNAMIC, 1, 1);
    mockSpiceBldr.addToPool(raDec_LatRad, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(raDec_LatRad, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_VECTOR_DEF", "CONSTANT");
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_SPEC", "RA/DEC");
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_RA", 30);
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_DEC", 20);
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_UNITS", "DEGREES");
    mockSpiceBldr.addToPool(raDec_LatRad, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_VECTOR_DEF", "CONSTANT");
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_SPEC", "LATITUDINAL");
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_UNITS", "RADIANS");
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_LONGITUDE", 1.0);
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_LATITUDE", 0.7);
    mockSpiceBldr.addToPool(raDec_LatRad, "SEC_FRAME", "J2000");

    /*
     * Lacking constant rectangular by constant latitudinal in degrees
     */
    badRect_LatDeg = new FrameInfo("BADRECT_LAT_DEG", 2, FrameType.DYNAMIC, 2, 2);
    mockSpiceBldr.addToPool(badRect_LatDeg, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(badRect_LatDeg, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(badRect_LatDeg, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(badRect_LatDeg, "PRI_VECTOR_DEF", "CONSTANT");
    // incorrect
    mockSpiceBldr.addToPool(badRect_LatDeg, "PRI_SPEC", "RECT");// "RECTANGULAR
    mockSpiceBldr.addToPool(badRect_LatDeg, "PRI_VECTOR", Lists.newArrayList(1.0, 2.0, 3.0));
    mockSpiceBldr.addToPool(badRect_LatDeg, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_VECTOR_DEF", "CONSTANT");
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_SPEC", "LATITUDINAL");
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_UNITS", "ARCSECONDS");
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_LONGITUDE", 60);
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_LATITUDE", 60);
    mockSpiceBldr.addToPool(badRect_LatDeg, "SEC_FRAME", "J2000");


    mockSpice = mockSpiceBldr.build();
  }

  @Test
  public void rectangularTest() throws Exception {
    DefinableStateVectorFunction function = DynamicVectorProvider.createFunction(VectorID.PRI,
        rectXlatArc, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicConstantVectorFunction);
  }

  @Test
  public void latitudinalArcsecondsTest() throws Exception {
    DefinableStateVectorFunction function = DynamicVectorProvider.createFunction(VectorID.SEC,
        rectXlatArc, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicConstantVectorFunction);
  }

  @Test
  public void latitudinalDegsecondsTest() throws Exception {
    DefinableStateVectorFunction function = DynamicVectorProvider.createFunction(VectorID.SEC,
        badRect_LatDeg, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicConstantVectorFunction);
  }

  @Test
  public void latitudinalRadsecondsTest() throws Exception {
    DefinableStateVectorFunction function = DynamicVectorProvider.createFunction(VectorID.SEC,
        raDec_LatRad, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicConstantVectorFunction);
  }

  @Test
  public void raDecTest() throws Exception {
    DefinableStateVectorFunction function = DynamicVectorProvider.createFunction(VectorID.PRI,
        raDec_LatRad, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicConstantVectorFunction);
  }



  @Test(expected = KernelPoolValidationException.class)
  public void lackingRectConstantParsingTest() throws Exception {
    DynamicVectorProvider.createFunction(VectorID.PRI, badRect_LatDeg, 1,
        mockSpice.getInfoHolder());
  }


}
