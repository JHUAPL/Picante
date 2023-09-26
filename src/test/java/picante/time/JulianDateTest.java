package picante.time;

import static org.junit.Assert.assertTrue;

import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.Test;

public class JulianDateTest {

  @Test
  public void test01() {
    Map<Double, UTCEpoch> dates = new LinkedHashMap<>();
    dates.put(28272.291, new UTCEpoch(-4635, 111, 18, 59, 02.400));
    dates.put(2451515.2981, new UTCEpoch(1999, 336, 19, 9, 15.840));
    dates.put(2436116.31, new UTCEpoch(1957, 277, 19, 26, 24.));

    TimeSystem<UTCEpoch> utcSystem = TimeAdapter.getInstance().getUTCTimeSys();

    for (double d : dates.keySet()) {
      JulianDate jd = new JulianDate(d);
      UTCEpoch utc = dates.get(d);
      //       System.out.printf("%f, %s\n", d, jd.toUTCEpoch().toString());
      assertTrue(Math.abs(utcSystem.difference(utc, jd.toUTCEpoch())) < 1e-3);

      jd = JulianDate.fromUTCEpoch(utc);
      assertTrue(Math.abs(jd.getDate() - d) < 1e-10);
    }
  }
}
