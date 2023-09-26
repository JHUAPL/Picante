package picante.spice.provided;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.util.Map;
import java.util.Map.Entry;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Maps;
import picante.spice.Utilities;

public class CanonicalizedStringKeyMapTest {

  private CanonicalizedStringKeyMap<Double> map;
  private Map<String, Double> backer;

  @Before
  public void setUp() throws Exception {
    backer = createMock(Map.class);
    map = new CanonicalizedStringKeyMap<Double>(backer);
  }


  @Test
  public void testDelegate() {
    assertSame(backer, map.delegate());
  }

  @Test
  public void testPutStringV() {
    expect(backer.put("NAIF_STANDARD", 1.0)).andReturn(5.0);
    replay(backer);
    assertEquals(5.0, map.put("Naif_Standard", 1.0), 0.0);
    verify(backer);
  }

  @Test
  public void testPutAllMapOfQextendsStringQextendsV() {

    Map<String, Double> values = Maps.newLinkedHashMap();
    values.put("test", 2.0);
    values.put("another test", 3.0);
    values.put("yet _ _ another test", 4.0);

    for (Entry<String, Double> entry : values.entrySet()) {
      expect(backer.put(Utilities.canonicalizeSpiceName(entry.getKey()), entry.getValue()))
          .andReturn(null);
    }

    replay(backer);
    map.putAll(values);
    verify(backer);
  }

}
