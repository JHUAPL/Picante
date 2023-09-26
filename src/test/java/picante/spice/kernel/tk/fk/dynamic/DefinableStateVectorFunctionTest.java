package picante.spice.kernel.tk.fk.dynamic;

import org.junit.Before;
import org.junit.Test;

public abstract class DefinableStateVectorFunctionTest {
  private DefinableStateVectorFunction undefinedFunction;
  private DefinableStateVectorFunction definedFunction;

  public abstract DefinableStateVectorFunction create();

  @Before
  public void setUp() throws Exception {
    undefinedFunction = create();
    definedFunction = create();
    definedFunction.define(null, null, null);
  }

  @Test(expected = IllegalStateException.class)
  public void testUndefinedGetVector() {
    undefinedFunction.getVector(0.0);
  }

  @Test(expected = IllegalStateException.class)
  public void testUndefinedGetStateVector() {
    undefinedFunction.getStateVector(0.0);
  }

  public void testDefinedGetVector() {
    definedFunction.getVector(0.0);
  }

  public void testDefineGetStateVector() {
    definedFunction.getStateVector(0.0);
  }
}
