package picante.math;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;

public class StatisticsTest {

  private Statistics stats;

  @Before
  public void setUp() throws Exception {
    stats = Statistics.createPopulationStatistics(ImmutableList.of(1., 2., 3., 4., 5., 6.));
  }

  @Test
  public void testGetSum() {
    assertEquals(21.0, stats.getSum(), 0.0);
  }

  @Test
  public void testGetMean() {
    assertEquals(21.0 / 6.0, stats.getMean(), 0.0);
  }

  @Test
  public void testGetVariance() {
    assertEquals(3.5 * 5.0 / 6.0, stats.getVariance(), 0.0);
  }

  @Test
  public void testGetStandardDeviation() {
    assertEquals(Math.sqrt(3.5 * 5.0 / 6.0), stats.getStandardDeviation(), 0.0);
  }

  @Test
  public void testGetSkewness() {
    assertEquals(0.0, stats.getSkewness(), 0.0);
  }

  @Test
  public void testGetExcessKurtosis() {
    assertEquals(303.0 / 175.0 - 3.0, stats.getExcessKurtosis(), 0.0);
  }

  @Test
  public void testGetMaximumValue() {
    assertEquals(6.0, stats.getMaximumValue(), 0.0);
  }

  @Test
  public void testGetMinimumValue() {
    assertEquals(1.0, stats.getMinimumValue(), 0.0);
  }

  @Test
  public void testGetSamples() {
    assertEquals(6, stats.getSamples());
  }

  @Test
  public void testGetMinimumIndices() {
    assertEquals(ImmutableList.of(0), stats.getMinimumIndices());
  }

  @Test
  public void testGetMaximumIndices() {
    assertEquals(ImmutableList.of(5), stats.getMaximumIndices());
  }

}
