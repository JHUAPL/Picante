package picante.math.coords;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.math.coords.AssertTools.assertComponentRelativeEquality;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Scanner;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * TODO, this is BROKEN!!!!!!! Fix me. I should probably limit the spread in values
 * 
 * @author stephgk1
 *
 */
public class CoordConvertersRandomStatesTest {

  private int num;

  private final static double TOL = 1.0E-8;

  private String cartInputName = "testRandomCartesianInput.txt";

  private String latStatesName = "testSpiceLat.txt";
  private String sphStatesName = "testSpiceSph.txt";
  private String cylStatesName = "testSpiceCyl.txt";

  private String fromLatToCartName = "testFromLatToCartOutput.txt";
  private String fromSphToCartName = "testFromSphToCartOutput.txt";
  private String fromCylToCartName = "testFromCylToCartOutput.txt";


  private List<CartesianState> randomStates;

  private List<LatitudinalState> fromCartToLats;
  private List<SphericalState> fromCartToSphs;
  private List<CylindricalState> fromCartToCyls;

  private List<CartesianState> fromLatToCarts;
  private List<CartesianState> fromSphToCarts;
  private List<CartesianState> fromCylToCarts;

  @Before
  public void setUp() throws Exception {

    randomStates = scanForStates(cartInputName);

    fromCartToLats = scanForLatStates(latStatesName);
    fromCartToSphs = scanForSphStates(sphStatesName);
    fromCartToCyls = scanForCylStates(cylStatesName);

    fromLatToCarts = scanForStates(fromLatToCartName);
    fromSphToCarts = scanForStates(fromSphToCartName);
    fromCylToCarts = scanForStates(fromCylToCartName);

    num = randomStates.size();

    // System.out.println(randomStates.size() + " " + fromCartToLats.size() + " "
    // + fromCartToSphs.size() + " " + fromCartToCyls.size() + " " + fromLatToCarts.size() + " "
    // + fromSphToCarts.size() + " " + fromCylToCarts.size());

  }

  /**
   * Test Lat->Cart
   * 
   * TODO: There are elements in the randomized input file that fail the tests due to floating point
   * fine points.
   */
  @Ignore
  @Test
  public void testConvertToLatitudinalCartesianState() {

    for (int i = 0; i < num; i++) {

      LatitudinalState state = fromCartToLats.get(i);
      CartesianState expected = fromLatToCarts.get(i);

      CartesianState lat = CoordConverters.convert(state);
      // System.out.println("LatState: " + state);
      // System.out.println("expected: " + expected);
      // System.out.println("actual: " + lat);
      assertComponentRelativeEquality(expected, lat, TOL);
    }
  }

  /**
   * Test Cart->Lat
   */
  @Test
  public void testConvertLatitudinalState() {

    for (int i = 0; i < num; i++) {

      CartesianState state = randomStates.get(i);
      LatitudinalState expected = fromCartToLats.get(i);

      // if this is NaN, it meant the Fortran code failed
      if (Double.isNaN(expected.getPosition().getI())) {

        // test to make sure that this value throws a Point on axis exception
        boolean exception = false;
        try {
          CoordConverters.convertToLatitudinal(state);
        } catch (PointOnAxisException e) {
          exception = true;
        }
        assertTrue(exception);
      }
      // normal case
      else {
        LatitudinalState lat = CoordConverters.convertToLatitudinal(state);
        assertComponentRelativeEquality(expected, lat, TOL);
      }

    }
  }

  /**
   * Test Sph->Cart
   * 
   * TODO: There are elements in the randomized input file that fail the tests due to floating point
   * fine points.
   */
  @Ignore
  @Test
  public void testConvertToSphericalCartesianState() {

    for (int i = 0; i < num; i++) {

      SphericalState state = fromCartToSphs.get(i);
      CartesianState expected = fromSphToCarts.get(i);

      CartesianState sph = CoordConverters.convert(state);
      // System.out.println("SphereState: " + state);
      // System.out.println("expected: " + expected);
      // System.out.println("actual: " + sph);
      assertComponentRelativeEquality(expected, sph, TOL);
    }
  }

  /**
   * Test Cart->Sph
   */
  @Test
  public void testConvertSphericalState() {

    for (int i = 0; i < num; i++) {

      CartesianState state = randomStates.get(i);
      SphericalState expected = fromCartToSphs.get(i);

      // if this is NaN, it meant the Fortran code failed
      if (Double.isNaN(expected.getPosition().getI())) {

        // test to make sure that this value throws a Point on axis exception
        boolean exception = false;
        try {
          CoordConverters.convertToSpherical(state);
        } catch (PointOnAxisException e) {
          exception = true;
        }
        assertTrue(exception);
      }
      // normal case
      else {
        SphericalState lat = CoordConverters.convertToSpherical(state);
        assertComponentRelativeEquality(expected, lat, TOL);
      }

    }
  }

  /**
   * Test Cyl->Cart
   * 
   * TODO: There are elements in the randomized input file that fail the tests due to floating point
   * fine points.
   */
  @Ignore
  @Test
  public void testConvertToCylindricalCartesianState() {

    for (int i = 0; i < num; i++) {

      CylindricalState state = fromCartToCyls.get(i);
      CartesianState expected = fromCylToCarts.get(i);

      CartesianState cyl = CoordConverters.convert(state);
      // System.out.println("CylindricalState: " + state);
      // System.out.println("expected: " + expected);
      // System.out.println("actual: " + cyl);
      assertComponentRelativeEquality(expected, cyl, TOL);
    }
  }

  /**
   * Test Cart->Cyl
   */
  @Test
  public void testConvertCylindricalState() {

    for (int i = 0; i < num; i++) {

      CartesianState state = randomStates.get(i);
      CylindricalState expected = fromCartToCyls.get(i);

      // if this is NaN, it meant the Fortran code failed
      if (Double.isNaN(expected.getPosition().getI())) {

        // test to make sure that this value throws a Point on axis exception
        boolean exception = false;
        try {
          CoordConverters.convertToCylindrical(state);
        } catch (PointOnAxisException e) {
          exception = true;
        }
        assertTrue(exception);
      }
      // normal case
      else {
        CylindricalState lat = CoordConverters.convertToCylindrical(state);
        assertComponentRelativeEquality(expected, lat, TOL);
      }

    }
  }

  @Ignore
  @Test
  public void testConvertToPolarCartesianStateIJ() {
    fail("Not yet implemented");
  }

  @Ignore
  @Test
  public void testConvertPolarState() {
    fail("Not yet implemented");
  }

  private static List<CartesianState> scanForStates(String fileName) {

    Scanner scanner =
        new Scanner(CoordConvertersRandomStatesTest.class.getResourceAsStream(fileName));

    List<CartesianState> states = Lists.newArrayList();

    while (scanner.hasNext()) {

      String next = scanner.next();

      double x = 0.0;
      double y = 0.0;
      double z = 0.0;
      double dx = 0.0;
      double dy = 0.0;
      double dz = 0.0;

      if (next.equals("FAILED")) {
        x = Double.NaN;
        y = Double.NaN;
        z = Double.NaN;
        dx = Double.NaN;
        dy = Double.NaN;
        dz = Double.NaN;
      } else {
        x = Double.parseDouble(next);
        y = scanner.nextDouble();
        z = scanner.nextDouble();
        dx = scanner.nextDouble();
        dy = scanner.nextDouble();
        dz = scanner.nextDouble();
      }

      states.add(new CartesianState(new UnwritableVectorIJK(x, y, z),
          new UnwritableVectorIJK(dx, dy, dz)));
    }

    scanner.close();

    return states;
  }

  private static List<LatitudinalState> scanForLatStates(String fileName) {

    Scanner scanner =
        new Scanner(CoordConvertersRandomStatesTest.class.getResourceAsStream(fileName));

    List<LatitudinalState> states = Lists.newArrayList();

    while (scanner.hasNext()) {

      String next = scanner.next();

      double x = 0.0;
      double y = 0.0;
      double z = 0.0;
      double dx = 0.0;
      double dy = 0.0;
      double dz = 0.0;

      if (next.equals("FAILED")) {
        x = Double.NaN;
        y = Double.NaN;
        z = Double.NaN;
        dx = Double.NaN;
        dy = Double.NaN;
        dz = Double.NaN;
      } else {
        x = Double.parseDouble(next);
        y = scanner.nextDouble();
        z = scanner.nextDouble();
        dx = scanner.nextDouble();
        dy = scanner.nextDouble();
        dz = scanner.nextDouble();
      }

      states.add(
          new LatitudinalState(new LatitudinalVector(x, z, y), new LatitudinalVector(dx, dz, dy)));
    }

    scanner.close();

    return states;
  }

  private static List<SphericalState> scanForSphStates(String fileName) {

    Scanner scanner =
        new Scanner(CoordConvertersRandomStatesTest.class.getResourceAsStream(fileName));

    List<SphericalState> states = Lists.newArrayList();

    while (scanner.hasNext()) {

      String next = scanner.next();

      double x = 0.0;
      double y = 0.0;
      double z = 0.0;
      double dx = 0.0;
      double dy = 0.0;
      double dz = 0.0;

      if (next.equals("FAILED")) {
        x = Double.NaN;
        y = Double.NaN;
        z = Double.NaN;
        dx = Double.NaN;
        dy = Double.NaN;
        dz = Double.NaN;
      } else {
        x = Double.parseDouble(next);
        y = scanner.nextDouble();
        z = scanner.nextDouble();
        dx = scanner.nextDouble();
        dy = scanner.nextDouble();
        dz = scanner.nextDouble();
      }

      states.add(new SphericalState(new SphericalVector(x, y, z), new SphericalVector(dx, dy, dz)));
    }

    scanner.close();

    return states;
  }

  private static List<CylindricalState> scanForCylStates(String fileName) {

    Scanner scanner =
        new Scanner(CoordConvertersRandomStatesTest.class.getResourceAsStream(fileName));

    List<CylindricalState> states = Lists.newArrayList();

    while (scanner.hasNext()) {

      String next = scanner.next();

      double x = 0.0;
      double y = 0.0;
      double z = 0.0;
      double dx = 0.0;
      double dy = 0.0;
      double dz = 0.0;

      if (next.equals("FAILED")) {
        x = Double.NaN;
        y = Double.NaN;
        z = Double.NaN;
        dx = Double.NaN;
        dy = Double.NaN;
        dz = Double.NaN;
      } else {
        x = Double.parseDouble(next);
        y = scanner.nextDouble();
        z = scanner.nextDouble();
        dx = scanner.nextDouble();
        dy = scanner.nextDouble();
        dz = scanner.nextDouble();
      }

      states.add(
          new CylindricalState(new CylindricalVector(x, y, z), new CylindricalVector(dx, dy, dz)));
    }

    scanner.close();

    return states;
  }

  /**
   * Randomly generates a bunch of Cartesian states, takes those states and prints out the proper
   * commands to run the Fortran SPICE code and writes the results of the spice code out to text
   * files.
   * 
   * @throws IOException
   */
  // @Test
  public void generateFromCartesianToCoordinate() throws IOException {

    int num = 50;

    double[] randomValues = createRandomValues(6 * num);

    randomStates = Lists.newArrayList();

    BufferedWriter writer = Files.newBufferedWriter(
        Paths.get("test", "picante", "math", "coords", cartInputName),
        Charset.defaultCharset(), StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.CREATE);

    for (int i = 0; i < num; i++) {

      double x = randomValues[i * 6 + 0];
      double y = randomValues[i * 6 + 1];
      double z = randomValues[i * 6 + 2];
      double dx = randomValues[i * 6 + 3];
      double dy = randomValues[i * 6 + 4];
      double dz = randomValues[i * 6 + 5];

      randomStates.add(new CartesianState(new UnwritableVectorIJK(x, y, z),
          new UnwritableVectorIJK(dx, dy, dz)));
      writer.write(String.format("%s %s %s %s %s %s\n", x, y, z, dx, dy, dz));
    }

    writer.flush();
    writer.close();

    // erases the contents of the files
    System.out.println("\" \" >! " + latStatesName);
    System.out.println("\" \" >! " + sphStatesName);
    System.out.println("\" \" >! " + cylStatesName);

    for (CartesianState state : randomStates) {

      UnwritableVectorIJK p = state.getPosition();
      UnwritableVectorIJK v = state.getVelocity();

      System.out.println(String.format("./testFromCartToCoord L %s %s %s %s %s %s >> %s", p.getI(),
          p.getJ(), p.getK(), v.getI(), v.getJ(), v.getK(), latStatesName));
      System.out.println(String.format("./testFromCartToCoord S %s %s %s %s %s %s >> %s", p.getI(),
          p.getJ(), p.getK(), v.getI(), v.getJ(), v.getK(), sphStatesName));
      System.out.println(String.format("./testFromCartToCoord C %s %s %s %s %s %s >> %s", p.getI(),
          p.getJ(), p.getK(), v.getI(), v.getJ(), v.getK(), cylStatesName));
    }

  }

  // @Test
  public void generateFromCoordToCart() throws IOException {

    List<LatitudinalState> latStates = scanForLatStates(latStatesName);
    List<SphericalState> sphStates = scanForSphStates(sphStatesName);
    List<CylindricalState> cylStates = scanForCylStates(cylStatesName);

    int num = latStates.size();

    System.out.println("\" \" >! " + fromLatToCartName);
    System.out.println("\" \" >! " + fromSphToCartName);
    System.out.println("\" \" >! " + fromCylToCartName);

    for (int i = 0; i < num; i++) {

      LatitudinalState latState = latStates.get(i);
      SphericalState sphState = sphStates.get(i);
      CylindricalState cylState = cylStates.get(i);

      LatitudinalVector lp = latState.getPosition();
      LatitudinalVector lv = latState.getVelocity();

      SphericalVector sp = sphState.getPosition();
      SphericalVector sv = sphState.getVelocity();

      CylindricalVector cp = cylState.getPosition();
      CylindricalVector cv = cylState.getVelocity();

      System.out.println(
          String.format("./testFromCoordToCart L %s %s %s %s %s %s >> " + fromLatToCartName,
              lp.getI(), lp.getK(), lp.getJ(), lv.getI(), lv.getK(), lv.getJ()));
      System.out.println(
          String.format("./testFromCoordToCart S %s %s %s %s %s %s >> " + fromSphToCartName,
              sp.getI(), sp.getJ(), sp.getK(), sv.getI(), sv.getJ(), sv.getK()));
      System.out.println(
          String.format("./testFromCoordToCart C %s %s %s %s %s %s >> " + fromCylToCartName,
              cp.getI(), cp.getJ(), cp.getK(), cv.getI(), cv.getJ(), cv.getK()));
    }

  }

  public double[] createRandomValues(int num) {


    double[] values = new double[num];

    for (int i = 0; i < num; i++) {
      values[i] = createRandom();
    }

    return values;
  }

  private double createRandom() {

    double r = Math.random();

    double d = 0.0;

    double l = 20.;


    if (r >= 5.0 / 6.0) {
      d = 0.0;
    } else if (r >= 4.0 / 6.0) {
      d = -0.0;
    } else if (r > 2.0 / 6.0) {
      d = l * Math.exp(Math.random() * l);
    } else {
      d = -l * Math.exp(Math.random() * l);
    }

    return d;
  }
}
