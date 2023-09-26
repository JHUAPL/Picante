package picante.spice.kernelpool.content;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.fk.dynamic.DefinableStateVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicConstantVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicPositionVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicVelocityVectorFunction;
import picante.spice.kernelpool.content.DynamicVectorProvider.VectorID;

public class DynamicVectorProviderTest {

  private static MockSpice mockSpice;
  private static FrameInfo posXVel;
  private static FrameInfo contsXnearPoint;


  @BeforeClass
  public static void setUpMockSpice() {
    MockSpice.Builder mockSpiceBldr = new MockSpice.Builder();
    /*
     * Position x velocity
     */
    posXVel = new FrameInfo("POS_X_VEL", 0, FrameType.DYNAMIC, 0, 0);
    mockSpiceBldr.addToPool(posXVel, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(posXVel, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(posXVel, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(posXVel, "PRI_VECTOR_DEF", "OBSERVER_TARGET_POSITION");
    mockSpiceBldr.addToPool(posXVel, "PRI_OBSERVER", "SATURN");
    mockSpiceBldr.addToPool(posXVel, "PRI_TARGET", "SUN");
    mockSpiceBldr.addToPool(posXVel, "PRI_ABCORR", "NONE");
    mockSpiceBldr.addToPool(posXVel, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(posXVel, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(posXVel, "SEC_VECTOR_DEF", "OBSERVER_TARGET_VELOCITY");
    mockSpiceBldr.addToPool(posXVel, "SEC_OBSERVER", "SUN");
    mockSpiceBldr.addToPool(posXVel, "SEC_TARGET", "SATURN");
    mockSpiceBldr.addToPool(posXVel, "SEC_ABCORR", "NONE");
    mockSpiceBldr.addToPool(posXVel, "SEC_FRAME", "J2000");
    /*
     * Constant x target near point
     */
    contsXnearPoint = new FrameInfo("CONST_X_NEARPT", 1, FrameType.DYNAMIC, 1, 1);
    mockSpiceBldr.addToPool(contsXnearPoint, "FAMILY", "TWO-VECTOR");
    mockSpiceBldr.addToPool(contsXnearPoint, "RELATIVE", "J2000");
    mockSpiceBldr.addToPool(contsXnearPoint, "PRI_AXIS", "X");
    mockSpiceBldr.addToPool(contsXnearPoint, "PRI_VECTOR_DEF", "CONSTANT");
    mockSpiceBldr.addToPool(contsXnearPoint, "PRI_SPEC", "RECTANGULAR");
    mockSpiceBldr.addToPool(contsXnearPoint, "PRI_VECTOR", Lists.newArrayList(1.0, 2.0, 3.0));
    mockSpiceBldr.addToPool(contsXnearPoint, "PRI_FRAME", "J2000");
    mockSpiceBldr.addToPool(contsXnearPoint, "SEC_AXIS", "Y");
    mockSpiceBldr.addToPool(contsXnearPoint, "SEC_VECTOR_DEF", "TARGET_NEAR_POINT");
    mockSpiceBldr.addToPool(contsXnearPoint, "SEC_OBSERVER", "SUN");
    mockSpiceBldr.addToPool(contsXnearPoint, "SEC_TARGET", "SATURN");
    mockSpiceBldr.addToPool(contsXnearPoint, "SEC_ABCORR", "NONE");
    mockSpiceBldr.addToPool(contsXnearPoint, "SEC_FRAME", "J2000");

    mockSpice = mockSpiceBldr.build();
  }

  @Test
  public void observerTargetPositionParsingTest() throws Exception {
    DefinableStateVectorFunction function =
        DynamicVectorProvider.createFunction(VectorID.PRI, posXVel, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicPositionVectorFunction);
  }


  @Test
  public void observerTargetVelocityParsingTest() throws Exception {
    DefinableStateVectorFunction function =
        DynamicVectorProvider.createFunction(VectorID.SEC, posXVel, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicVelocityVectorFunction);
  }


  @Test
  public void constantParsingTest() throws Exception {
    DefinableStateVectorFunction function = DynamicVectorProvider.createFunction(VectorID.PRI,
        contsXnearPoint, 1, mockSpice.getInfoHolder());
    Assert.assertTrue(function instanceof DynamicConstantVectorFunction);
  }


  @Test(expected = FKInstantiationException.class)
  public void nearPointParsingTest() throws Exception {
    DynamicVectorProvider.createFunction(VectorID.SEC, contsXnearPoint, 1,
        mockSpice.getInfoHolder());
  }

}
