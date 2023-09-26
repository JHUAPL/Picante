package picante.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import picante.designpatterns.BuildFailedException;
import picante.math.Statistics.Builder;
import picante.math.Statistics.Estimator;
import picante.math.Statistics.Tracker;

public class StatisticsBuilderTest {

  private final static double TOLERANCE = 1e-14;

  @Test
  public void testBuilder() {
    Builder builder = new Builder();
    assertEquals(builder.getTracker(), Tracker.FIRST);
    assertEquals(builder.getEstimator(), Estimator.SAMPLE);
  }

  @Test
  public void testBuilderTracker() {
    Builder builder = new Builder(Tracker.ALL);
    assertEquals(builder.getTracker(), Tracker.ALL);
    assertEquals(builder.getEstimator(), Estimator.SAMPLE);

    builder = new Builder(Tracker.LAST);
    assertEquals(builder.getTracker(), Tracker.LAST);
    assertEquals(builder.getEstimator(), Estimator.SAMPLE);
  }

  @Test
  public void testBuilderStatistics() {

    Builder source = new Builder(Tracker.ALL);
    source.accumulate(ImmutableList.of(1.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 1.0, 2.0));
    source.useEstimator(Estimator.POPULATION);
    Statistics stats = source.build();

    /*
     * Create a new builder from the derived statistics. Then accumulate both in the same fashion
     * without modifying the state and verify the resultant statistics are equal.
     */
    Builder builder = new Builder(stats);

    assertEquals(stats, builder.build());

    ImmutableList<Double> data = ImmutableList.of(1.0, 2.0, 3.0, 5.0, 5.0);

    builder.accumulate(data);
    source.accumulate(data);

    assertEquals(source.build(), builder.build());
  }

  @Test(expected = BuildFailedException.class)
  public void testEmptyBuildFailure() {
    new Builder().build();
  }

  @Test
  public void testAccumulateDouble() {

    Builder builder = new Builder();
    builder.useEstimator(Estimator.POPULATION);
    Builder returned = builder.accumulate(1.0);
    assertSame(builder, returned);
    returned = builder.accumulate(2.0);
    assertSame(builder, returned);
    returned = builder.accumulate(3.0);
    assertSame(builder, returned);
    returned = builder.accumulate(4.0);
    assertSame(builder, returned);
    returned = builder.accumulate(5.0);
    assertSame(builder, returned);

    Statistics result = builder.build();

    assertEquals(5, result.getSamples());
    assertEquals(3.0, result.getMean(), 0.0);
    assertEquals(2.5 * 4.0 / 5.0, result.getVariance(), 0.0);
    assertEquals(0.0, result.getSkewness(), 0.0);
    assertEquals(-1.3, result.getExcessKurtosis(), 0.0);
    assertEquals(1.0, result.getMinimumValue(), 0.0);
    assertEquals(5.0, result.getMaximumValue(), 0.0);
    assertEquals(ImmutableList.of(4), result.getMaximumIndices());
    assertEquals(ImmutableList.of(0), result.getMinimumIndices());
    assertEquals(Estimator.POPULATION, result.getEstimator());
    assertEquals(Tracker.FIRST, result.getTracker());
  }

  @Test
  public void testAccumulateIterableOfDouble() {
    Builder builder = new Builder();
    Builder returned = builder.accumulate(ImmutableList.of(1.0, 2.0, 3.0, 4.0, 5.0));
    assertSame(builder, returned);

    Statistics result = builder.build();

    assertEquals(5, result.getSamples());
    assertEquals(3.0, result.getMean(), 0.0);
    assertEquals(2.5, result.getVariance(), 0.0);
    assertEquals(0.0, result.getSkewness(), 0.0);
    assertEquals(-1.2, result.getExcessKurtosis(), TOLERANCE);
    assertEquals(1.0, result.getMinimumValue(), 0.0);
    assertEquals(5.0, result.getMaximumValue(), 0.0);
    assertEquals(ImmutableList.of(4), result.getMaximumIndices());
    assertEquals(ImmutableList.of(0), result.getMinimumIndices());
    assertEquals(Estimator.SAMPLE, result.getEstimator());
    assertEquals(Tracker.FIRST, result.getTracker());
  }

  @Test
  public void testAccumulateIteratorOfDouble() {
    Builder builder = new Builder();
    Builder returned = builder.accumulate(ImmutableList.of(1.0, 2.0, 3.0, 4.0, 5.0).iterator());
    assertSame(builder, returned);

    Statistics result = builder.build();

    assertEquals(5, result.getSamples());
    assertEquals(3.0, result.getMean(), 0.0);
    assertEquals(2.5, result.getVariance(), 0.0);
    assertEquals(0.0, result.getSkewness(), 0.0);
    assertEquals(-1.2, result.getExcessKurtosis(), TOLERANCE);
    assertEquals(1.0, result.getMinimumValue(), 0.0);
    assertEquals(5.0, result.getMaximumValue(), 0.0);
    assertEquals(ImmutableList.of(4), result.getMaximumIndices());
    assertEquals(ImmutableList.of(0), result.getMinimumIndices());
    assertEquals(Estimator.SAMPLE, result.getEstimator());
    assertEquals(Tracker.FIRST, result.getTracker());
  }

  @Test
  public void testGetTracker() {
    Builder builder = new Builder(Tracker.NONE);
    assertEquals(Tracker.NONE, builder.getTracker());
  }

  @Test
  public void testUseEstimator() {

    Builder builder = new Builder();
    Builder returned = builder.useEstimator(Estimator.POPULATION);
    assertSame(builder, returned);
    builder.accumulate(ImmutableList.of(1.0, 2.0, 3.0, 4.0, 5.0));

    Statistics population = builder.build();

    assertEquals(5, population.getSamples());
    assertEquals(3.0, population.getMean(), 0.0);
    assertEquals(2.5 * 4.0 / 5.0, population.getVariance(), 0.0);
    assertEquals(0.0, population.getSkewness(), 0.0);
    assertEquals(-1.3, population.getExcessKurtosis(), 0.0);
    assertEquals(1.0, population.getMinimumValue(), 0.0);
    assertEquals(5.0, population.getMaximumValue(), 0.0);
    assertEquals(ImmutableList.of(4), population.getMaximumIndices());
    assertEquals(ImmutableList.of(0), population.getMinimumIndices());
    assertEquals(Estimator.POPULATION, population.getEstimator());
    assertEquals(Tracker.FIRST, population.getTracker());

    returned = builder.useEstimator(Estimator.SAMPLE);
    assertSame(builder, returned);
    Statistics sample = builder.build();

    assertEquals(5, sample.getSamples());
    assertEquals(3.0, sample.getMean(), 0.0);
    assertEquals(2.5, sample.getVariance(), 0.0);
    assertEquals(0.0, sample.getSkewness(), 0.0);
    assertEquals(-1.2, sample.getExcessKurtosis(), TOLERANCE);
    assertEquals(1.0, sample.getMinimumValue(), 0.0);
    assertEquals(5.0, sample.getMaximumValue(), 0.0);
    assertEquals(ImmutableList.of(4), sample.getMaximumIndices());
    assertEquals(ImmutableList.of(0), sample.getMinimumIndices());
    assertEquals(Estimator.SAMPLE, sample.getEstimator());
    assertEquals(Tracker.FIRST, sample.getTracker());

  }

  @Test
  public void testGetEstimator() {
    Builder builder = new Builder();
    builder.useEstimator(Estimator.POPULATION);
    assertEquals(Estimator.POPULATION, builder.getEstimator());

    builder.useEstimator(Estimator.SAMPLE);
    assertEquals(Estimator.SAMPLE, builder.getEstimator());
  }

}
