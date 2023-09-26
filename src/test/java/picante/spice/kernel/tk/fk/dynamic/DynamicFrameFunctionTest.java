package picante.spice.kernel.tk.fk.dynamic;

import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.spice.kernel.tk.fk.FKInstantiationException;


public abstract class DynamicFrameFunctionTest {

  private DynamicFrameFunction undefinedFunction;
  private DynamicFrameFunction definedFunction;

  public abstract DynamicFrameFunction create()
      throws FKInstantiationException, DynamicFrameDefinitionException;

  @Before
  public void setUp() throws Exception {
    undefinedFunction = create();
    definedFunction = create();
    definedFunction.define(null, null, null);
  }

  @Test(expected = IllegalStateException.class)
  public void testUndefinedGetTransform() {
    undefinedFunction.getTransform(2E8, new RotationMatrixIJK());
  }

  @Test(expected = IllegalStateException.class)
  public void testUndefinedGetStateTransform() {
    undefinedFunction.getStateTransform(2E8, new StateTransform());
  }


  @Test
  public void testDefinedGetTransform() {
    definedFunction.getTransform(0.0, new RotationMatrixIJK());
  }


  @Test
  public void testDefineGetStateTransform() {
    definedFunction.getStateTransform(0.0, new StateTransform());
  }
}
