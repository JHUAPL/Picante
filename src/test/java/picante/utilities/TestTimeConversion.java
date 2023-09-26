package picante.utilities;

import static org.junit.Assert.assertEquals;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import org.junit.Test;
import picante.time.TimeConversion;

public class TestTimeConversion {

  @Test
  public void testTimeConversion() {

    Map<String, Map<Double, String>> testMaps = new LinkedHashMap<>();
    TimeConversion tc = TimeConversion.createUsingInternalConstants();

    Map<Double, String> map = new TreeMap<>();

    testMaps.put("ISOD", map);
    map.put(-0.5, "2000-001T11:58:55.316");
    map.put(0., "2000-001T11:58:55.816");
    map.put(3.12413e8, "2009-329T09:22:13.817");
    map.put(-126273538.81608607, "1995-365T23:59:60.000");
    map.put(-157809538.81607857, "1995-001T00:00:00.000");

    map = new TreeMap<>();
    testMaps.put("YYYY/Mon/DD HR:MN:SC ::RND", map);
    map.put(-0.5, "2000/Jan/01 11:58:55");
    map.put(0., "2000/Jan/01 11:58:56");
    map.put(3.12413e8, "2009/Nov/25 09:22:14");

    map = new TreeMap<>();
    testMaps.put("YYYY/Mon/DD HR:MN:SC.### (TDB) ::TDB ::RND", map);
    map.put(-0.5, "2000/Jan/01 11:59:59.500 (TDB) ");
    map.put(0., "2000/Jan/01 12:00:00.000 (TDB) ");
    map.put(3.12413e8, "2009/Nov/25 09:23:20.000 (TDB) ");

    map = new TreeMap<>();
    testMaps.put("YYYY DOY HR:MN:SC", map);
    map.put(-0.5, "2000 001 11:58:55");
    map.put(0., "2000 001 11:58:55");
    map.put(3.12413e8, "2009 329 09:22:13");

    map = new TreeMap<>();
    testMaps.put("JULIAND", map);
    map.put(0., "2451545");

    map = new TreeMap<>();
    testMaps.put("J", map);
    map.put(0., "JD 2451544.999");
    map.put(tc.utcStringToTDB("2000 JAN 01 12:00:00"), "JD 2451545.000");

    map = new TreeMap<>();
    testMaps.put("JULIAND.######", map);
    map.put(-527644192.54036528, "2445438.006415");
    map.put(0., "2451544.999257");
    map.put(tc.utcStringToTDB("2000 JAN 01 12:00:00"), "2451545.000000");

    for (String format : testMaps.keySet()) {
      Function<Double, String> func = tc.format(format);
      map = testMaps.get(format);
      // System.out.println(format);
      for (Double tdb : map.keySet()) {
        String utcString = func.apply(tdb);
        // System.out.printf("'%s' '%s'\n", utcString, map.get(tdb));
        assertEquals(utcString, map.get(tdb).trim());
      }
      // System.out.println();
    }

  }


}
