package picante.spice.provided;

import static org.junit.Assert.assertTrue;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import picante.mechanics.CelestialBodies;
import picante.mechanics.EphemerisID;

public class EphemerisNamesTest {

  private EphemerisNames names;

  @Before
  public void setUp() throws Exception {
    this.names = new EphemerisNames();
  }

  @Test
  public void testGetStandardBindings() {

    /*
     * Check that all of the bodies defined in CelestialBodies are present in the standard binding.
     * It's not worth fussing over the individual integer bindings, as software generated them in
     * the first place.
     */
    Map<Integer, EphemerisID> map = names.getStandardBindings();

    for (CelestialBodies body : CelestialBodies.values()) {
      assertTrue(body.name(), map.containsValue(body));
    }
  }

  @Test
  public void testGetMapAndInverse() {

    /*
     * Make certain that every integer in the forward map is present as a key in the inverse map.
     */
    for (Integer code : names.getMap().values()) {
      assertTrue(names.getInverseMap().containsKey(code));
    }

    /*
     * And make certain that every name in the inverse map is present in the forward map.
     */
    for (String name : names.getInverseMap().values()) {
      assertTrue(names.getMap().containsKey(name));
    }

  }

}
