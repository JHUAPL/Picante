package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.SPKSegmentFactory;
import picante.spice.kernel.spk.SPK;

public class EphemerisSourceFactoryTest {

  private static SPK kernel;
  private EphemerisSourceFactory factory;
  private EphemerisSourceFactory emptyMapFactory;
  private Map<Integer, EphemerisID> ephemerisMap;
  private Map<Integer, FrameID> frameMap;
  private Map<Integer, EphemerisID> emptyEphemerisMap;
  private Map<Integer, FrameID> emptyFrameMap;

  private EphemerisID saturnID = new EphemerisID() {
    @Override
    public String getName() {
      return "SATURN";
    }
  };

  private EphemerisID earthBarycenterID = new EphemerisID() {
    @Override
    public String getName() {
      return "EARTH BARYCENTER";
    }
  };

  private FrameID j2000ID = new FrameID() {

    @Override
    public String getName() {
      return "J2000";
    }

    @Override
    public boolean isInertial() {
      return true;
    }

  };

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    kernel = new SPK("combined.bsp",
        new DAFBackedSPKContent(
            DAFFactory.createDAF(EphemerisSourceFactory.class.getResourceAsStream("combined.bsp")),
            SPKSegmentFactory.VALIDATING));
  }

  @Before
  public void setUp() throws Exception {
    emptyEphemerisMap = new HashMap<Integer, EphemerisID>();
    emptyFrameMap = new HashMap<Integer, FrameID>();
    emptyMapFactory = new EphemerisSourceFactory(emptyEphemerisMap, emptyFrameMap);

    ephemerisMap = new HashMap<Integer, EphemerisID>();
    ephemerisMap.put(699, saturnID);
    ephemerisMap.put(3, earthBarycenterID);
    frameMap = new HashMap<Integer, FrameID>();
    frameMap.put(1, j2000ID);
    factory = new EphemerisSourceFactory(ephemerisMap, frameMap);
  }

  @Test
  public void testMapSuppliedToConstructorIsUnmodified() throws Exception {
    emptyMapFactory.createEphemerisSource(kernel);
    assertEquals(0, emptyEphemerisMap.size());
    assertEquals(0, emptyFrameMap.size());
  }

  @Test
  public void testEphemerisMapChangesSuppliedToConstructorHasNoEffect() throws Exception {

    EphemerisID newID = new EphemerisID() {
      @Override
      public String getName() {
        return "TestID";
      }
    };
    emptyEphemerisMap.put(399, newID);

    Set<EphemerisID> kernelIDs = new HashSet<EphemerisID>();

    for (PositionVectorFunction f : emptyMapFactory.createEphemerisSource(kernel)) {
      kernelIDs.add(f.getObserverID());
      kernelIDs.add(f.getTargetID());
    }

    assertFalse(kernelIDs.contains(newID));

  }

  @Test
  public void testFrameMapChangesSuppliedToConstructorHasNoEffect() throws Exception {

    FrameID newID = new FrameID() {
      @Override
      public String getName() {
        return "TestID";
      }

      @Override
      public boolean isInertial() {
        return false;
      }
    };
    emptyFrameMap.put(1, newID);

    Set<FrameID> kernelIDs = new HashSet<FrameID>();

    for (PositionVectorFunction f : emptyMapFactory.createEphemerisSource(kernel)) {
      kernelIDs.add(f.getFrameID());
    }

    assertFalse(kernelIDs.contains(newID));
  }

  @Test
  public void testEmptyMapFactoryContentCreation() throws Exception {

    List<? extends PositionVectorFunction> list = emptyMapFactory.createEphemerisSource(kernel);

    assertEquals(4, list.size());

    PositionVectorFunction f = list.get(0);
    EphemerisID obs1 = f.getObserverID();
    EphemerisID tar1 = f.getTargetID();
    FrameID fr1 = f.getFrameID();

    assertEquals(new SpiceEphemerisID(3), obs1);
    assertEquals(new SpiceEphemerisID(399), tar1);
    assertEquals(new SpiceFrameID(1), fr1);

    f = list.get(1);
    EphemerisID obs2 = f.getObserverID();
    EphemerisID tar2 = f.getTargetID();

    assertEquals(new SpiceEphemerisID(6), obs2);
    assertEquals(new SpiceEphemerisID(699), tar2);
    assertSame(fr1, f.getFrameID());

    f = list.get(2);
    assertSame(obs1, f.getObserverID());
    assertSame(tar1, f.getTargetID());
    assertSame(fr1, f.getFrameID());

    f = list.get(3);
    assertSame(obs2, f.getObserverID());
    assertSame(tar2, f.getTargetID());
    assertSame(fr1, f.getFrameID());

  }

  @Test
  public void testFactoryContentCreation() throws Exception {

    List<? extends PositionVectorFunction> list = factory.createEphemerisSource(kernel);

    assertEquals(4, list.size());

    PositionVectorFunction f = list.get(0);
    EphemerisID tar1 = f.getTargetID();

    assertEquals(new SpiceEphemerisID(399), f.getTargetID());
    assertSame(earthBarycenterID, f.getObserverID());
    assertSame(j2000ID, f.getFrameID());

    f = list.get(1);
    EphemerisID obs1 = f.getObserverID();

    assertEquals(new SpiceEphemerisID(6), f.getObserverID());
    assertSame(saturnID, f.getTargetID());
    assertSame(j2000ID, f.getFrameID());

    f = list.get(2);
    assertSame(earthBarycenterID, f.getObserverID());
    assertSame(tar1, f.getTargetID());
    assertSame(j2000ID, f.getFrameID());

    f = list.get(3);
    assertSame(obs1, f.getObserverID());
    assertSame(saturnID, f.getTargetID());
    assertSame(j2000ID, f.getFrameID());

  }
}
