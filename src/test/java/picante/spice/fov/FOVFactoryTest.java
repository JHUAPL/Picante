package picante.spice.fov;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.google.common.io.Resources;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.utilities.SimpleFrameID;
import picante.spice.SpiceEnvironmentBuilder;

/**
 * Compares the {@link FOVFactory} output to SPICE getfov() <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/getfov.html">example</a>.
 * 
 * @author nairah1
 *
 */
public class FOVFactoryTest {

  private static final double TOLERANCE = 1e-16;

  /**
   * Returns an FOV factory using the example instrument kernel in the resources folder
   * 
   * @return
   */
  private FOVFactory loadIK() {
    FOVFactory factory = null;
    /*-
    try (InputStream input =
        this.getClass().getResourceAsStream("/picante/spice/fov/example.ti")) {
      TextKernelParser parser = new TextKernelParser();
      KernelPool kernelPool = new KernelPool(parser.parse(new InputStreamReader(input)));
      factory = new FOVFactory(kernelPool);
    } catch (ParseException | IOException e) {
      System.err.println(e.getLocalizedMessage());
    }
    */

    try {
      SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
      builder.load("IK", Resources
          .asByteSource(this.getClass().getResource("/picante/spice/fov/example.ti")));
      builder.load("LSK", Resources
          .asByteSource(this.getClass().getResource("/picante/spice/naif0012.tls")));

      factory = new FOVFactory(builder.build().getPool());
    } catch (Exception e) {
      System.err.println(e.getLocalizedMessage());
    }

    return factory;
  }

  @Test
  public void testCircle() {
    FOVFactory factory = loadIK();
    int instrID = -999001;
    FOV fov = factory.create(instrID);
    FOVSpice fovSpice = fov.getFovSpice();
    assertTrue(fovSpice.getInstrumentID() == instrID);
    assertTrue(fovSpice.getShape() == FOVFactory.Shape.CIRCLE);
    assertTrue(fovSpice.getFrame().equals(new SimpleFrameID("SC999_INST001")));
    assertTrue(fovSpice.getBoresight().equals(VectorIJK.K));
    assertTrue(fovSpice.getBounds().size() == 1);
    assertTrue(
        fovSpice.getBounds().get(0).getSeparation(new UnwritableVectorIJK(8.7155742747658170e-02,
            0.0000000000000000e+00, 9.9619469809174550e-01)) < TOLERANCE);

    assertTrue(fov.getCone().getVertex().equals(VectorIJK.ZERO));
    assertTrue(fovSpice.getBounds().get(0).getSeparation(fov.getCone().getEdge(0)) < TOLERANCE);

  }

  @Test
  public void testEllipse() {
    FOVFactory factory = loadIK();
    int instrID = -999002;
    FOV fov = factory.create(instrID);
    FOVSpice fovSpice = fov.getFovSpice();
    assertTrue(fovSpice.getInstrumentID() == instrID);
    assertTrue(fovSpice.getShape() == FOVFactory.Shape.ELLIPSE);
    assertTrue(fovSpice.getFrame().equals(new SimpleFrameID("SC999_INST002")));
    assertTrue(fovSpice.getBoresight().equals(VectorIJK.I));
    assertTrue(fovSpice.getBounds().size() == 2);
    assertTrue(
        fovSpice.getBounds().get(0).getSeparation(new UnwritableVectorIJK(1.0000000000000000e+00,
            0.0000000000000000e+00, 1.7455060000000000e-02)) < TOLERANCE);
    assertTrue(
        fovSpice.getBounds().get(1).getSeparation(new UnwritableVectorIJK(1.0000000000000000e+00,
            3.4920770000000000e-02, 0.0000000000000000e+00)) < TOLERANCE);

    assertTrue(fov.getCone().getVertex().equals(VectorIJK.ZERO));
    // semiminor axis
    assertTrue(
        fovSpice.getBounds().get(0).getSeparation(fov.getCone().getEdge(Math.PI / 2)) < TOLERANCE);
    // semimajor axis
    assertTrue(fovSpice.getBounds().get(1).getSeparation(fov.getCone().getEdge(0)) < TOLERANCE);

  }

  @Test
  public void testRectangle() {
    FOVFactory factory = loadIK();
    int instrID = -999003;
    FOV fov = factory.create(instrID);
    FOVSpice fovSpice = fov.getFovSpice();
    assertTrue(fovSpice.getInstrumentID() == instrID);
    assertTrue(fovSpice.getShape() == FOVFactory.Shape.RECTANGLE);
    assertTrue(fovSpice.getFrame().equals(new SimpleFrameID("SC999_INST003")));
    assertTrue(fovSpice.getBoresight().equals(VectorIJK.K));
    assertTrue(fovSpice.getBounds().size() == 4);
    assertTrue(
        fovSpice.getBounds().get(0).getSeparation(new UnwritableVectorIJK(1.0471768168559534e-02,
            1.7452326687281040e-03, 9.9994364652932120e-01)) < TOLERANCE);
    assertTrue(
        fovSpice.getBounds().get(1).getSeparation(new UnwritableVectorIJK(-1.0471768168559534e-02,
            1.7452326687281040e-03, 9.9994364652932120e-01)) < TOLERANCE);
    assertTrue(
        fovSpice.getBounds().get(2).getSeparation(new UnwritableVectorIJK(-1.0471768168559534e-02,
            -1.7452326687281040e-03, 9.9994364652932120e-01)) < TOLERANCE);
    assertTrue(
        fovSpice.getBounds().get(3).getSeparation(new UnwritableVectorIJK(1.0471768168559534e-02,
            -1.7452326687281040e-03, 9.9994364652932120e-01)) < TOLERANCE);

    assertTrue(fov.getCone().getVertex().equals(VectorIJK.ZERO));
    for (int corner = 0; corner < fovSpice.getBounds().size(); corner++) {
      assertTrue(fovSpice.getBounds().get(corner)
          .getSeparation(fov.getCone().getEdge(corner)) < TOLERANCE);
    }

  }

  @Test
  public void testPolygon() {
    FOVFactory factory = loadIK();
    int instrID = -999004;
    FOV fov = factory.create(instrID);
    FOVSpice fovSpice = fov.getFovSpice();
    assertTrue(fovSpice.getInstrumentID() == instrID);
    assertTrue(fovSpice.getShape() == FOVFactory.Shape.POLYGON);
    assertTrue(fovSpice.getFrame().equals(new SimpleFrameID("SC999_INST004")));
    assertTrue(fovSpice.getBoresight().equals(VectorIJK.J));
    assertTrue(fovSpice.getBounds().size() == 3);
    assertTrue(fovSpice.getBounds().get(0)
        .getSeparation(new UnwritableVectorIJK(0, 0.8, 0.5)) < TOLERANCE);
    assertTrue(fovSpice.getBounds().get(1)
        .getSeparation(new UnwritableVectorIJK(0.4, 0.8, -0.2)) < TOLERANCE);
    assertTrue(fovSpice.getBounds().get(2)
        .getSeparation(new UnwritableVectorIJK(-0.4, 0.8, -0.2)) < TOLERANCE);

    assertTrue(fov.getCone().getVertex().equals(VectorIJK.ZERO));
    for (int corner = 0; corner < fovSpice.getBounds().size(); corner++) {
      assertTrue(fovSpice.getBounds().get(corner)
          .getSeparation(fov.getCone().getEdge(corner)) < TOLERANCE);
    }

  }
}
