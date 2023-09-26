package picante.mechanics.utilities;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;

@SuppressWarnings("deprecation")
public class ScaledPositionVectorFunctionTest {

  private static final double TOLERANCE = 1.e-13;
  private PositionVectorFunction srcFunc;
  private final double X = 14;
  private final double Y = 16;
  private final double Z = 18;
  static final String targetIdName = "fromName";
  static final String obsIdName = "obsName";
  static final String frameName = "frameName";
  private Coverage cvg;
  private boolean isInertial;
  private double scaleFactor = 1.0 / 2.0;
  private ScaledPositionVectorFunction scaledFunc;

  @Before
  public void setUp() throws Exception {

    cvg = Coverage.ALL_TIME;

    final EphemerisID targetId = new EphemerisID() {
      @Override
      public String getName() {
        return targetIdName;
      }
    };

    final EphemerisID obsId = new EphemerisID() {
      @Override
      public String getName() {
        return obsIdName;
      }
    };

    final FrameID frameId = new FrameID() {
      @Override
      public boolean isInertial() {
        return isInertial;
      }

      @Override
      public String getName() {
        return frameName;
      }
    };

    final VectorIJK posVec = new VectorIJK(X, Y, Z);

    srcFunc = new PositionVectorFunction() {

      @Override
      public EphemerisID getTargetID() {
        return targetId;
      }

      @Override
      public VectorIJK getPosition(@SuppressWarnings("unused") double time, VectorIJK buffer) {
        return buffer.setTo(posVec);
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
    };

    scaledFunc = new ScaledPositionVectorFunction(srcFunc, scaleFactor);
  }

  @Test
  public void testScaledPositionFunction() {
    assertEquals("source function assegned in constrcutor", srcFunc, scaledFunc.srcFunc);
    assertEquals("scale factor assigned in constructor", scaleFactor, scaledFunc.scaleFactor,
        TOLERANCE);
  }

  @Test
  public void testGetObserverID() {
    assertEquals("Observer ID", scaledFunc.getObserverID(), srcFunc.getObserverID());
  }

  @Test
  public void testGetTargetID() {
    assertEquals("target ID", targetIdName, srcFunc.getTargetID().getName());
  }

  @Test
  public void testGetFrameID() {
    assertEquals("frame ID", frameName, srcFunc.getFrameID().getName());
    assertEquals("frame 'inertialness'", isInertial, srcFunc.getFrameID().isInertial());
  }

  @Test
  public void testGetCoverage() {
    assertEquals("coverage", cvg, srcFunc.getCoverage());
  }

  @Test
  public void testGetPosition() {
    double delta = 0.0;
    double dummyEt = 0;
    VectorIJK buffer = new VectorIJK();
    scaledFunc.getPosition(dummyEt, buffer);
    double actualX = buffer.getI();
    assertEquals("X vector component", X * scaleFactor, actualX, delta);
    double actualY = buffer.getJ();
    assertEquals("Y vector component", Y * scaleFactor, actualY, delta);
    double actualZ = buffer.getK();
    assertEquals("Z vector component", Z * scaleFactor, actualZ, delta);
  }

}
