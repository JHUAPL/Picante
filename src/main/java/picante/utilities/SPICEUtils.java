package picante.utilities;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.spice.SpiceEnvironment;
import picante.spice.kernel.tk.sclk.SCLK;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.math3.util.FastMath;

public class SPICEUtils {

  /**
   * Converts geodetic coordinates to rectangular coordinates. Analogous to SPICE routine GEOREC.
   *
   * @param e Ellipsoid.  A reference spheroid will be created with axes [e.getA(), e.getA(),
   *        e.getC()]
   * @param lv geodetic lat, lon (radius is ignored)
   *        @param altitude altitude above reference spheroid, same units as Ellipsoid axes

   * @return rectangular coordinates
   */
  public static UnwritableVectorIJK GEOREC(Ellipsoid e, LatitudinalVector lv, double altitude) {
    // (https://en.wikipedia.org/wiki/Geographic_coordinate_conversion)
    double sinLat = Math.sin(lv.getLatitude());
    double sinLon = Math.sin(lv.getLongitude());
    double cosLat = Math.cos(lv.getLatitude());
    double cosLon = Math.cos(lv.getLongitude());

    double e2 = 1 - (e.getC() * e.getC()) / (e.getA() * e.getA());
    double N = e.getA() / Math.sqrt(1 - e2 * sinLat * sinLat);

    double x = (N + altitude) * cosLat * cosLon;
    double y = (N + altitude) * cosLat * sinLon;
    double z = ((1 - e2) * N + altitude) * sinLat;

    return new UnwritableVectorIJK(x, y, z);
  }

  /**
   * Converts rectangular coordinates to geodetic coordinates. Analogous to SPICE routine RECGEO.
   *
   * @param e Ellipsoid. A reference spheroid will be created with axes [e.getA(), e.getA(),
   *     e.getC()]
   * @param rec rectangular coordinates
   * @return LatitudinalVector where "radius" is the altitude above the reference spheroid and
   *     latitude is geodetic latitude
   */
  public static LatitudinalVector RECGEO(Ellipsoid e, UnwritableVectorIJK rec) {

    Ellipsoid ref = Surfaces.createEllipsoidalSurface(e.getA(), e.getA(), e.getC());
    Ellipsoid.PointAndDistance pointAndDistance = ref.computeNearPoint(new VectorIJK(rec));
    UnwritableVectorIJK surfacePt = pointAndDistance.getPoint();
    UnwritableVectorIJK normal = ref.computeOutwardNormal(surfacePt);

    LatitudinalVector lv = CoordConverters.convertToLatitudinal(normal);
    return new LatitudinalVector(
        pointAndDistance.getDistance(),
        lv.getLatitude(),
        normal.getI() == 0 && normal.getJ() == 0
            ? 0
            : FastMath.atan2(normal.getJ(), normal.getI()));
  }

  /**
   * Translates the name of a body or object to the corresponding SPICE integer ID code. Analogous
   * to NAIF routine BODN2C.
   *
   * @param env SpiceEnvironment
   * @return map where key is body name and value is ID code
   */
  public static Map<String, Integer> BODN2C(SpiceEnvironment env) {
    Map<String, Integer> bodn2c = new HashMap<>();

    UnwritableKernelPool envPool = env.getPool();

    // Search through the NAIF_BODY_NAME/NAIF_BODY_CODE pairs in the kernel pool for the instrument
    // name/code.
    List<String> names = envPool.getStrings("NAIF_BODY_NAME");
    List<Integer> codes = envPool.getIntegers("NAIF_BODY_CODE");
    assertNotNull(names);
    assertNotNull(codes);
    assertEquals(names.size(), codes.size());
    for (int i = 0; i < codes.size(); i++) {
      int code = codes.get(i);
      String name = names.get(i);
      bodn2c.put(name, code);
    }
    return Collections.unmodifiableMap(bodn2c);
  }

  public static SCLK fromString(String sclkString) {
    sclkString = sclkString.trim().replaceAll("\\s+", " ");

    int partition = 1;
    String[] parts = sclkString.split(Character.toString(SCLK.PARTITION_DELIMITER));
    if (parts.length > 1) partition = Integer.parseInt(parts[0].trim());
    String fieldString = parts[parts.length - 1].trim();

    // split on valid delimiters
    parts = fieldString.split("[-.,: ]");
    long[] fields = new long[parts.length];
    for (int i = 0; i < parts.length; i++) fields[i] = Long.parseLong(parts[i].trim());

    return new SCLK(partition, fields);
  }
}
