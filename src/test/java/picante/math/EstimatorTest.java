package picante.math;

import static org.junit.Assert.assertEquals;
import static picante.math.Statistics.Estimator.POPULATION;
import static picante.math.Statistics.Estimator.SAMPLE;
import org.junit.Before;
import org.junit.Test;

public class EstimatorTest {

  private static final double TOLERANCE = 1e-13;

  private Statistics.Builder builder = new Statistics.Builder();

  @Before
  public void setUp() throws Exception {

    builder.n = 100;
    builder.m2 = 10;
    builder.m3 = 20;
    builder.m4 = 30;

  }

  @Test
  public void testSampleVariance() {
    assertEquals(builder.m2 / (builder.n - 1), SAMPLE.estimateVariance(builder), TOLERANCE);
  }

  @Test
  public void testSampleSkewness() {
    double g1 = builder.m3 / builder.n / Math.pow(builder.m2 / builder.n, 1.5);
    assertEquals(Math.sqrt(builder.n * (builder.n - 1)) / (builder.n - 2) * g1,
        SAMPLE.estimateSkewness(builder), TOLERANCE);
  }

  @Test
  public void testSampleExcessKurtosis() {
    double g2 = builder.m4 / builder.n / (builder.m2 / builder.n) / (builder.m2 / builder.n) - 3;
    assertEquals(
        (builder.n - 1.0) / (builder.n - 2.0) / (builder.n - 3.0) * ((builder.n + 1.0) * g2 + 6.0),
        SAMPLE.estimateExcessKurtosis(builder), TOLERANCE);
  }

  @Test
  public void testPopulationVariance() {
    assertEquals(builder.m2 / builder.n, POPULATION.estimateVariance(builder), TOLERANCE);
  }

  @Test
  public void testPopulationSkewness() {
    assertEquals(builder.m3 / builder.n / Math.pow(builder.m2 / builder.n, 1.5),
        POPULATION.estimateSkewness(builder), TOLERANCE);
  }

  @Test
  public void testPopulationKurtosis() {
    assertEquals(builder.m4 / builder.n / (builder.m2 / builder.n) / (builder.m2 / builder.n) - 3,
        POPULATION.estimateExcessKurtosis(builder), TOLERANCE);
  }
}
