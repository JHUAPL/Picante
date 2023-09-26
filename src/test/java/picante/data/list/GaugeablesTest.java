package picante.data.list;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Function;

public class GaugeablesTest {

  @Before
  public void setUp() throws Exception {}

  @Test
  public void testFromFunction() {

    Function<String, Double> converter = createMock(Function.class);

    expect(converter.apply("TEST")).andReturn(12.0).once();
    replay(converter);

    Gaugeable<String> gauge = Gaugeables.from(converter);
    assertEquals(12.0, gauge.gauge("TEST"), 0.0);

    verify(converter);

  }

}
