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

public class CanonicalizedStringValueMapTest {

  private CanonicalizedStringValueMap<Integer> map;
  private Map<Integer, String> backer;

  @Before
  public void setUp() throws Exception {
    backer = createMock(Map.class);
    map = new CanonicalizedStringValueMap<Integer>(backer);
  }

  @Test
  public void testDelegate() {
    assertSame(backer, map.delegate());
  }

  @Test
  public void testPutKString() {
    expect(backer.put(1, "NAIF_STANDARD")).andReturn("TEST_VALUE");
    replay(backer);
    assertEquals("TEST_VALUE", map.put(1, "naif_StaNdArd"));
    verify(backer);
  }

  @Test
  public void testPutAllMapOfQextendsKQextendsString() {

    Map<Integer, String> values = Maps.newLinkedHashMap();
    values.put(2, "test");
    values.put(3, "another test");
    values.put(4, "yet _ another _ test");

    for (Entry<Integer, String> entry : values.entrySet()) {
      expect(backer.put(entry.getKey(), Utilities.canonicalizeSpiceName(entry.getValue())))
          .andReturn(null);
    }

    replay(backer);
    map.putAll(values);
    verify(backer);

  }

}
