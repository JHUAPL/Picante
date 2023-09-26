package picante.spice.adapters;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.spice.provided.InertialFrames;

// TODO: Implement
@Ignore
public class FrameSourceFactoryTest {

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {}

  private Map<Integer, FrameID> frameBindings;
  // private CK ck;

  private FrameSourceFactory emptyFactory;
  private FrameSourceFactory factory;

  private SclkRegistry emptyNoAccessSclkRegistryMock;

  private final FrameID j2000 = new FrameID() {

    @Override
    public boolean isInertial() {
      return true;
    }

    @Override
    public String getName() {
      return "J2000";
    }
  };

  private final FrameID eclipB1950 = new FrameID() {

    @Override
    public boolean isInertial() {
      return true;
    }

    @Override
    public String getName() {
      return "ECLIPB1950";
    }
  };

  private SclkRegistry createEmptyNoAccessSclkRegistry() {
    SclkRegistry mock = createMock(SclkRegistry.class);
    replay(mock);
    return mock;
  }

  @Before
  public void setUp() throws Exception {

    emptyNoAccessSclkRegistryMock = createEmptyNoAccessSclkRegistry();

    // Map<Integer, FrameID> emptyIntFrameMap = new HashMap<Integer,
    // FrameID>();

    // emptyFactory = new FrameSourceFactory(emptyIntFrameMap,
    // emptyIntFrameMap, emptyIntFrameMap, emptyNoAccessSclkRegistryMock);

    frameBindings = new HashMap<Integer, FrameID>();
    frameBindings.put(1, j2000);
    frameBindings.put(18, eclipB1950);

    // factory = new FrameSourceFactory(frameBindings, emptyIntFrameMap,
    // emptyIntFrameMap,
    // emptyNoAccessSclkRegistryMock);
  }

  @Test
  public void testCreateFrameSource() {
    fail("Not yet implemented");
  }

  @Test
  public void testCreateBuiltInInertialFrameSourceAdapterList() throws Exception {

    for (FrameTransformFunction f : emptyFactory.createBuiltInInertialFrameSource()) {
      assertTrue(f.getFromID() instanceof SpiceFrameID);
      SpiceFrameID id = (SpiceFrameID) f.getFromID();
      int idCode = id.getIDCode();

      /*
       * Locate the corresponding inertial frame.
       */
      InertialFrames frame = getFrame(idCode);
      assertEquivalentMatrix(frame.getTransform(0.0, new RotationMatrixIJK()),
          f.getTransform(0.0, new RotationMatrixIJK()));

      /*
       * Lastly check the to ID matches.
       */
      id = (SpiceFrameID) f.getToID();
      assertEquals(frame.getToID(), id.getIDCode());
    }

    verify(emptyNoAccessSclkRegistryMock);

  }

  private InertialFrames getFrame(int idcode) {
    for (InertialFrames frame : InertialFrames.values()) {
      if (frame.getFromID() == idcode) {
        return frame;
      }
    }
    throw new IllegalArgumentException("Unable to locate inertial frame with ID code: " + idcode);
  }

  @Test
  public void testCreateBuiltInInertialFrameSourceCodeBindings() throws Exception {

    for (FrameTransformFunction f : factory.createBuiltInInertialFrameSource()) {

      if (f.getFromID() instanceof SpiceFrameID) {
        SpiceFrameID id = (SpiceFrameID) f.getFromID();
        assertFalse(frameBindings.containsKey(id.getIDCode()));
      } else {
        /*
         * This should only be the ECLIPB1950 frame.
         */
        assertEquivalentMatrix(f.getTransform(0.0, new RotationMatrixIJK()),
            InertialFrames.ECLIPB1950.getTransform(0.0, new RotationMatrixIJK()));
      }

      if (f.getToID() instanceof SpiceFrameID) {
        SpiceFrameID id = (SpiceFrameID) f.getToID();
        assertFalse(frameBindings.containsKey(id.getIDCode()));
      } else {
        assertEquals(f.getToID(), j2000);
      }

    }

    verify(emptyNoAccessSclkRegistryMock);

  }
}
