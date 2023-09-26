package picante.spice;

import static org.junit.Assert.fail;
import java.io.File;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.collect.Multimap;
import picante.mechanics.EphemerisID;

@Ignore
public class SpiceEnvironmentBuilderTest {

  @Before
  public void setUp() throws Exception {}

  @Test
  public void testSpiceEnvironmentBuilder() {
    fail("Not yet implemented");
  }

  @Test
  public void testSpiceEnvironmentBuilderKernelManagerKernelLoader() {
    fail("Not yet implemented");
  }

  @Test
  public void testBindEphemerisID() {
    fail("Not yet implemented");
  }

  @Test
  public void testBindAllEphemerisID() {
    fail("Not yet implemented");
  }

  @Test
  public void testUnbindEphemerisID() {
    fail("Not yet implemented");
  }

  @Test
  public void testUnbindAllEphemerisID() {
    fail("Not yet implemented");
  }

  @Test
  public void testBindFrameID() {
    fail("Not yet implemented");
  }

  @Test
  public void testBindAllFrameID() {
    fail("Not yet implemented");
  }

  @Test
  public void testUnbindFrameID() {
    fail("Not yet implemented");
  }

  @Test
  public void testUnbindAllFrameID() {
    fail("Not yet implemented");
  }

  @Test
  public void testUnload() {
    fail("Not yet implemented");
  }

  @Test
  public void testLoadStringInputStream() {
    fail("Not yet implemented");
  }

  @Test
  public void testLoadStringFile() {
    fail("Not yet implemented");
  }

  @Test
  public void testBuild() {
    fail("Not yet implemented");
  }

  @Test
  public void testGetTdtProvider() {
    fail("Not yet implemented");
  }

  @Test
  public void testBodyRadii() throws Exception {

    SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
    builder.load("naif0012.tls", new File("/local/opt/kernels/generic/lsk/naif0012.tls"));
    builder.load("pck00010.tpc", new File("/local/opt/kernels/generic/pck/pck00010.tpc"));

    SpiceEnvironment env = builder.build();

    Multimap<EphemerisID, Double> radii = env.getBodyRadii();

    for (EphemerisID id : radii.keySet()) {
      System.out.println(id + ": " + radii.get(id));
    }

  }

}
