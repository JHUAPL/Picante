package picante.mechanics;

import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;

public class MechanicsTest {

  @Test(expected = IllegalArgumentException.class)
  public void testCreateMarkerProxyNonInterfaceException() {
    Mechanics.createMarkerFunctionProxy(Number.class);
  }

  @Test
  public void testCreateMarkerProxy() {

    StateVectorFunction a = Mechanics.createMarkerFunctionProxy(StateVectorFunction.class);
    StateVectorFunction b = Mechanics.createMarkerFunctionProxy(StateVectorFunction.class);

    assertNotEquals(a, b);
    assertNotSame(a, b);

    /*
     * Can't assert behavior of hashCode, but the methods should invoke without generating
     * exceptions.
     */
    a.hashCode();
    b.hashCode();

    assertTrue(testException(a::getCoverage, UnsupportedOperationException.class));
    assertTrue(testException(a::getFrameID, UnsupportedOperationException.class));
    assertTrue(testException(a::getObserverID, UnsupportedOperationException.class));
    assertTrue(testException(a::getTargetID, UnsupportedOperationException.class));
    assertTrue(testException(() -> a.getPosition(0.0), UnsupportedOperationException.class));
    assertTrue(testException(() -> a.getPosition(0.0, new VectorIJK()),
        UnsupportedOperationException.class));
    assertTrue(testException(() -> a.getState(0.0), UnsupportedOperationException.class));
    assertTrue(testException(() -> a.getState(0.0, new StateVector()),
        UnsupportedOperationException.class));

  }

  /**
   * Method that verifies an exception assignable to the specified type is thrown.
   * 
   * @param r the runnable to execute, that is expected to generate an exception assignable to
   *        exceptionType.
   * @param exceptionType the type of expected exception
   * @return true, if an exception of the expected type was thrown; false otherwise
   */
  static boolean testException(Runnable r, Class<? extends Throwable> exceptionType) {
    try {
      r.run();
    } catch (Throwable t) {
      return exceptionType.isAssignableFrom(t.getClass());
    }
    return false;
  }

}
