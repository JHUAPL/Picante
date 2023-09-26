package picante.data.list;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SearcherTest {

  private Searcher basicSearcher;
  private GaugedRetrievable<?> basic;
  private Searcher repeatingSearcher;
  private GaugedRetrievable<?> repeating;

  @Before
  public void setUp() throws Exception {
    basic = createMock(GaugedRetrievable.class);
    expect(basic.getGauge(0)).andReturn(0.0).anyTimes();
    expect(basic.getGauge(1)).andReturn(1.0).anyTimes();
    expect(basic.getGauge(2)).andReturn(2.0).anyTimes();
    expect(basic.getGauge(3)).andReturn(3.0).anyTimes();
    expect(basic.size()).andReturn(4).anyTimes();
    replay(basic);
    basicSearcher = new Searcher(basic);

    repeating = createMock(GaugedRetrievable.class);
    expect(repeating.getGauge(0)).andReturn(0.0).anyTimes();
    expect(repeating.getGauge(1)).andReturn(0.0).anyTimes();
    expect(repeating.getGauge(2)).andReturn(1.0).anyTimes();
    expect(repeating.getGauge(3)).andReturn(1.0).anyTimes();
    expect(repeating.getGauge(4)).andReturn(2.0).anyTimes();
    expect(repeating.getGauge(5)).andReturn(2.0).anyTimes();
    expect(repeating.size()).andReturn(6).anyTimes();
    replay(repeating);
    repeatingSearcher = new Searcher(repeating);
  }

  @Test
  public void testIndexLastLessThanOrEqualToBasic() {
    assertEquals(-1, basicSearcher.indexLastLessThanOrEqualTo(-10.0));
    assertEquals(0, basicSearcher.indexLastLessThanOrEqualTo(0.0));
    assertEquals(0, basicSearcher.indexLastLessThanOrEqualTo(0.5));
    assertEquals(1, basicSearcher.indexLastLessThanOrEqualTo(1.0));
    assertEquals(1, basicSearcher.indexLastLessThanOrEqualTo(1.5));
    assertEquals(2, basicSearcher.indexLastLessThanOrEqualTo(2.0));
    assertEquals(2, basicSearcher.indexLastLessThanOrEqualTo(2.5));
    assertEquals(3, basicSearcher.indexLastLessThanOrEqualTo(3.0));
    assertEquals(3, basicSearcher.indexLastLessThanOrEqualTo(3.5));
  }

  @Test
  public void testIndexLastLessThanOrEqualToRepeating() {
    assertEquals(-1, repeatingSearcher.indexLastLessThanOrEqualTo(-10.0));
    assertEquals(1, repeatingSearcher.indexLastLessThanOrEqualTo(0.0));
    assertEquals(1, repeatingSearcher.indexLastLessThanOrEqualTo(0.5));
    assertEquals(3, repeatingSearcher.indexLastLessThanOrEqualTo(1.0));
    assertEquals(3, repeatingSearcher.indexLastLessThanOrEqualTo(1.5));
    assertEquals(5, repeatingSearcher.indexLastLessThanOrEqualTo(2.0));
    assertEquals(5, repeatingSearcher.indexLastLessThanOrEqualTo(2.5));
  }

  @Test
  public void testIndexLastLessThanBasic() {
    assertEquals(-1, basicSearcher.indexLastLessThan(-10.0));
    assertEquals(-1, basicSearcher.indexLastLessThan(0.0));
    assertEquals(0, basicSearcher.indexLastLessThan(0.5));
    assertEquals(0, basicSearcher.indexLastLessThan(1.0));
    assertEquals(1, basicSearcher.indexLastLessThan(1.5));
    assertEquals(1, basicSearcher.indexLastLessThan(2.0));
    assertEquals(2, basicSearcher.indexLastLessThan(2.5));
    assertEquals(2, basicSearcher.indexLastLessThan(3.0));
    assertEquals(3, basicSearcher.indexLastLessThan(3.5));
  }

  @Test
  public void testIndexLastLessThanRepeating() {
    assertEquals(-1, repeatingSearcher.indexLastLessThan(-10.0));
    assertEquals(-1, repeatingSearcher.indexLastLessThan(0.0));
    assertEquals(1, repeatingSearcher.indexLastLessThan(0.5));
    assertEquals(1, repeatingSearcher.indexLastLessThan(1.0));
    assertEquals(3, repeatingSearcher.indexLastLessThan(1.5));
    assertEquals(3, repeatingSearcher.indexLastLessThan(2.0));
    assertEquals(5, repeatingSearcher.indexLastLessThan(2.5));
  }

  @After
  public void tearDown() throws Exception {
    verify(basic);
    verify(repeating);
  }

}
