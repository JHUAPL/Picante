package picante.mechanics.utilities;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

@SuppressWarnings("deprecation")
public class ScaledStateVectorFunctionTest {
  private static final double TOLERANCE = 1.e-13;
  private StateVectorFunction srcFunc;
  private final double X = 14;
  private final double Y = 16;
  private final double Z = 18;
  private final double VX = 140;
  private final double VY = 160;
  private final double VZ = 180;
  private double scaleFactor = 1.0 / 2.0;
  private ScaledStateVectorFunction scaledFunc;

  @Before
  public void setUp() throws Exception {

    final Coverage cvg = Coverage.ALL_TIME;

    final EphemerisID targetId = null;
    final EphemerisID obsId = null;
    final FrameID frameId = null;
    final VectorIJK posVec = new VectorIJK(X, Y, Z);
    final VectorIJK velVec = new VectorIJK(VX, VY, VZ);
    srcFunc = new StateVectorFunction() {

      @Override
      public EphemerisID getTargetID() {
        return targetId;
      }

      @Override
      public EphemerisID getObserverID() {
        return obsId;
      }

      @Override
      public FrameID getFrameID() {
        return frameId;
      }

      @Override
      public Coverage getCoverage() {
        return cvg;
      }

      @Override
      public VectorIJK getPosition(@SuppressWarnings("unused") double time, VectorIJK buffer) {
        return buffer.setTo(posVec);
      }

      @Override
      public StateVector getState(@SuppressWarnings("unused") double time, StateVector buffer) {
        buffer.setPosition(posVec);
        buffer.setVelocity(velVec);
        return buffer;
      }
    };
    scaledFunc = new ScaledStateVectorFunction(srcFunc, scaleFactor);
  }


  @Test
  public void testScaledStateVectorFunction() {
    assertEquals("identity of src function", srcFunc, scaledFunc.srcFunc);
    assertEquals("value of scale factor", scaleFactor, scaledFunc.scaleFactor, TOLERANCE);
  }

  @Test
  public void testGetState() {
    double delta = 0;
    double dummyEt = 0;
    StateVector buffer = new StateVector();
    scaledFunc.getState(dummyEt, buffer);
    double actualX = buffer.getPosition().getI();
    double actualY = buffer.getPosition().getJ();
    double actualZ = buffer.getPosition().getK();
    double actualVx = buffer.getVelocity().getI();
    double actualVy = buffer.getVelocity().getJ();
    double actualVz = buffer.getVelocity().getK();

    assertEquals("X componenet of position", X * scaleFactor, actualX, delta);
    assertEquals("Y componenet of position", Y * scaleFactor, actualY, delta);
    assertEquals("Z componenet of position", Z * scaleFactor, actualZ, delta);

    assertEquals("Vx componenet of position", VX * scaleFactor, actualVx, delta);
    assertEquals("Vy componenet of position", VY * scaleFactor, actualVy, delta);
    assertEquals("Vz componenet of position", VZ * scaleFactor, actualVz, delta);
  }

}
