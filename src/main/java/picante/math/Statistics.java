package picante.math;

import static picante.math.PicanteMath.pow;
import static picante.math.PicanteMath.sqrt;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import picante.designpatterns.BuildFailedException;

/**
 * Immutable class capturing statistics of a sequence of double precision numbers.
 * <p>
 * The constructors of this class are private and as such are unavailable for use. {@link Builder}
 * is used to create instances of this class, as are the static &quot;create&quot; convenience
 * methods present on this class.
 * </p>
 * <p>
 * The value of variance, standard deviation, skewness and kurtosis may be either computed with the
 * defining, biased operations or with the unbiased ones. See
 * {@link Builder#useEstimator(Estimator)} for details.
 * </p>
 * <p>
 * When a statistics instance is built, an estimator and a index tracker are utilized implicitly in
 * its construction. These enumerations are captured in the instance for reference purposes and
 * available via the methods {@link Statistics#getEstimator()} and {@link Statistics#getTracker()}.
 * </p>
 * 
 * @see <a href="doc-files/statistics.pdf">Statistics Reference Material</a>
 */
public final class Statistics {

  /**
   * Enumeration used to control how the builder tracks the indices indicating the location of the
   * extrema from the sequence of doubles. This exists largely as a memory optimization. If you are
   * analyzing large sequences of fixed values it may not make sense to capture the locations of the
   * extrema as it may require a large amount of memory.
   */
  public enum Tracker {

    /**
     * Provides absolutely no tracking of the minimum or maximum indices in the sequence of
     * accumulated doubles.
     */
    NONE {

      @SuppressWarnings("unused")
      @Override
      void record(List<Integer> indices, double oldValue, double newValue, int newIndex) {}

    },

    /**
     * Tracks only the first occurrence of the minimum or maximum value's location in the sequence
     * of accumulated doubles.
     */
    FIRST {

      @Override
      void record(List<Integer> indices, double oldValue, double newValue, int newIndex) {
        if (indices.size() == 0) {
          indices.add(0, newIndex);
          return;
        }
        if (oldValue != newValue) {
          indices.set(0, newIndex);
        }
      }

    },

    /**
     * Tracks only the final occurrence of the minimum or maximum value's location in the sequence
     * of accumulated doubles.
     */
    LAST {

      @Override
      void record(List<Integer> indices, @SuppressWarnings("unused") double oldValue,
          @SuppressWarnings("unused") double newValue, int newIndex) {
        if (indices.size() == 0) {
          indices.add(0, newIndex);
          return;
        }
        indices.set(0, newIndex);
      }

    },

    /**
     * Tracks the location of all minimum or maximum value's location in the sequence of accumulated
     * doubles.
     */
    ALL {

      @Override
      void record(List<Integer> indices, double oldValue, double newValue, int newIndex) {
        if (newValue != oldValue) {
          indices.clear();
        }
        indices.add(newIndex);
      }

    };

    /**
     * Record an extrema's location, if appropriate. This method should only be invoked when a new
     * or equivalent extrema is encountered.
     * 
     * @param indices the mutable list of indices of previously recorded extrema locations
     * @param oldValue the old extrema
     * @param newValue the new extrema
     * @param newIndex the index of the new extrema
     */
    abstract void record(List<Integer> indices, double oldValue, double newValue, int newIndex);

  }

  /**
   * Enumeration capturing the various options with regards to computing higher order statistics of
   * the sequence of doubles.
   */
  public enum Estimator {

    /**
     * Instructs the builder to compute &quot;unbiased&quot; statistics assuming the sequence of
     * doubles is a sample of a larger population.
     */
    SAMPLE {
      @Override
      double estimateVariance(Builder builder) {
        return builder.m2 / (builder.n - 1);
      }

      @Override
      double estimateSkewness(Builder builder) {
        return builder.n * sqrt(builder.n - 1) / (builder.n - 2) * builder.m3
            / pow(builder.m2, 1.5);
      }

      @Override
      double estimateExcessKurtosis(Builder builder) {
        return (builder.n - 1.0) / (builder.n - 2.0) / (builder.n - 3.0)
            * ((builder.n + 1.0) * (builder.n * builder.m4 / (builder.m2 * builder.m2) - 3.0)
                + 6.0);
      }
    },

    /**
     * Instructs the builder to compute statistics assuming the sequence of doubles is the entire
     * population. The values computed here are usually the standard definitions of the statistical
     * quantities.
     */
    POPULATION {
      @Override
      double estimateVariance(Builder builder) {
        return builder.m2 / builder.n;
      }

      @Override
      double estimateSkewness(Builder builder) {
        return sqrt(builder.n) * builder.m3 / pow(builder.m2, 1.5);
      }

      @Override
      double estimateExcessKurtosis(Builder builder) {
        return builder.n * builder.m4 / (builder.m2 * builder.m2) - 3;
      }
    };

    /**
     * Estimate the variance of the sequence
     * 
     * @param builder the builder from which to estimate
     * 
     * @return the variance of the sample
     */
    abstract double estimateVariance(Builder builder);

    /**
     * Estimate the skewness of the sequence
     * 
     * @param builder the builder from which to estimate
     * 
     * @return the skewness of the sample
     */
    abstract double estimateSkewness(Builder builder);

    /**
     * Estimate the excess (above normal) kurtosis of the sequence
     * 
     * @param builder the builder from which to estimate
     * 
     * @return the excess kurtosis
     */
    abstract double estimateExcessKurtosis(Builder builder);
  }

  private final double mean;
  private final double variance;
  private final double skewness;
  private final double excessKurtosis;
  private final double maximumValue;
  private final double minimumValue;
  private final int samples;
  private final ImmutableList<Integer> minimumIndices;
  private final ImmutableList<Integer> maximumIndices;
  private final Tracker tracker;
  private final Estimator estimator;

  /**
   * Values preserved purely for re-populating a builder.
   */
  private final double m2;
  private final double m3;
  private final double m4;

  /**
   * Creates a new instance of Statistics
   * 
   * @param samples the number of samples in the set
   * @param mean the mean of the sequence
   * @param variance the variance of the sequence
   * @param skewness the skewness of the sequence
   * @param excessKurtosis the excess (above normality) kurtosis
   * @param maximumValue the maximum value achieved by the sequence
   * @param minimumValue the minimum value achieved by the sequence
   * @param maximumIndices the tracked indices of the maximum values (may be empty)
   * @param minimumIndices the tracked indices of the minimum values (may be empty)
   * @param m2 the second central moment of the sequence
   * @param m3 the third central moment of the sequence
   * @param m4 the fourth central moment of the sequence
   * @param tracker the tracker used to capture extrema indices
   * @param estimator the estimator used to compute higher order statistics
   */
  private Statistics(int samples, double mean, double variance, double skewness,
      double excessKurtosis, double maximumValue, double minimumValue,
      ImmutableList<Integer> maximumIndices, ImmutableList<Integer> minimumIndices, double m2,
      double m3, double m4, Tracker tracker, Estimator estimator) {
    super();
    this.mean = mean;
    this.variance = variance;
    this.skewness = skewness;
    this.excessKurtosis = excessKurtosis;
    this.maximumValue = maximumValue;
    this.minimumValue = minimumValue;
    this.samples = samples;
    this.minimumIndices = minimumIndices;
    this.maximumIndices = maximumIndices;
    this.m2 = m2;
    this.m3 = m3;
    this.m4 = m4;
    this.tracker = tracker;
    this.estimator = estimator;
  }

  /**
   * Retrieves the sum of the sequence over which the statistics were computed.
   * 
   * @return the sum of the sequence
   */
  public double getSum() {
    return mean * samples;
  }

  /**
   * Retrieves the mean of the sequence over which the statistics were computed.
   * 
   * @return the mean of the sequence
   */
  public double getMean() {
    return mean;
  }

  /**
   * Retrieves the variance of the sequence over which the statistics were computed.
   * <p>
   * The value stored here is impacted by the estimator utilized to build the statistics. See
   * {@link Estimator} for details.
   * </p>
   * 
   * @return the variance of the sequence per {@link Statistics#getEstimator()}
   */
  public double getVariance() {
    return variance;
  }

  /**
   * Retrieves the standard deviation of the sequence over which the statistics were computed.
   * <p>
   * The value stored here is impacted by the estimator utilized to build the statistics. See
   * {@link Estimator} for details.
   * </p>
   * 
   * @return the standard deviation of the sequence per {@link Statistics#getEstimator()}
   */
  public double getStandardDeviation() {
    return sqrt(variance);
  }

  /**
   * Retrieves the skewness of the sequence over which the statistics were computed.
   * <p>
   * The value stored here is impacted by the estimator utilized to build the statistics. See
   * {@link Estimator} for details.
   * </p>
   * 
   * @return the skewness of the sequence per {@link Statistics#getEstimator()}
   */
  public double getSkewness() {
    return skewness;
  }

  /**
   * Retrieves the excess kurtosis of the sequence over which the statistics were computed.
   * <p>
   * The value stored here is impacted by the estimator utilized to build the statistics. See
   * {@link Estimator} for details.
   * </p>
   * <p>
   * The kurtosis of a normal distribution is 3, so this value here subtracts 3 from it to yield an
   * excess kurtosis of a normal distribution as 0.
   * </p>
   * 
   * @return the excess kurtosis of the sequence per {@link Statistics#getEstimator()}
   */
  public double getExcessKurtosis() {
    return excessKurtosis;
  }

  /**
   * Retrieves the maximum value achieved in the sequence over which the statistics were computed.
   * 
   * @return the maximum value
   */
  public double getMaximumValue() {
    return maximumValue;
  }

  /**
   * Retrieves the minimum value achieved in the sequence over which the statistics were computed.
   * 
   * @return the minimum value
   */
  public double getMinimumValue() {
    return minimumValue;
  }

  /**
   * Retrieves the number of samples used to estimate the statistics.
   * 
   * @return the number of samples
   */
  public int getSamples() {
    return samples;
  }

  /**
   * Retrieves an immutable list of integer indices into the total accumulated sequence used to
   * generate the statistics at which the minimum value occurred.
   * <p>
   * Depending on the index tracker utilized, this list may be empty, contain all of the indices at
   * which minimums occurred, or a subset of these indices. If multiple iterators or iterables were
   * used to accumulate the statistics, then the indices presented here start at 0 with the first
   * value supplied to the builder in the accumulation process and increase monotonically.
   * </p>
   * 
   * @return the list of indices
   */
  public ImmutableList<Integer> getMinimumIndices() {
    return minimumIndices;
  }

  /**
   * Retrieves an immutable list of integer indices into the total accumulated sequence used to
   * generate the statistics at which the maximum value occurred.
   * <p>
   * Depending on the index tracker utilized, this list may be empty, contain all of the indices at
   * which maximums occurred, or a subset of these indices. If multiple iterators or iterables were
   * used to accumulate the statistics, then the indices presented here start at 0 with the first
   * value supplied to the builder in the accumulation process and increase monotonically.
   * </p>
   * 
   * @return the list of indices
   */
  public ImmutableList<Integer> getMaximumIndices() {
    return maximumIndices;
  }

  /**
   * Retrieves the index tracker enumeration used to build the minimum and maximum indices lists
   * 
   * @return the tracker
   */
  public Tracker getTracker() {
    return tracker;
  }

  /**
   * Retrieves the estimator enumeration used to compute the higher order statistics captured in the
   * instance
   * 
   * @return the estimator
   */
  public Estimator getEstimator() {
    return estimator;
  }

  @Override
  public String toString() {
    return "Statistics [samples=" + samples + ", estimator=" + estimator + ", mean=" + mean
        + ", variance=" + variance + ", skewness=" + skewness + ", excessKurtosis=" + excessKurtosis
        + ", maximumValue=" + maximumValue + ", minimumValue=" + minimumValue + ", tracker="
        + tracker + ", minimumIndices=" + minimumIndices + ", maximumIndices=" + maximumIndices
        + "]";
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((estimator == null) ? 0 : estimator.hashCode());
    long temp;
    temp = Double.doubleToLongBits(excessKurtosis);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((maximumIndices == null) ? 0 : maximumIndices.hashCode());
    temp = Double.doubleToLongBits(maximumValue);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(mean);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((minimumIndices == null) ? 0 : minimumIndices.hashCode());
    temp = Double.doubleToLongBits(minimumValue);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + samples;
    temp = Double.doubleToLongBits(skewness);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((tracker == null) ? 0 : tracker.hashCode());
    temp = Double.doubleToLongBits(variance);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    Statistics other = (Statistics) obj;
    if (estimator != other.estimator) {
      return false;
    }
    if (Double.doubleToLongBits(excessKurtosis) != Double.doubleToLongBits(other.excessKurtosis)) {
      return false;
    }
    if (maximumIndices == null) {
      if (other.maximumIndices != null) {
        return false;
      }
    } else if (!maximumIndices.equals(other.maximumIndices)) {
      return false;
    }
    if (Double.doubleToLongBits(maximumValue) != Double.doubleToLongBits(other.maximumValue)) {
      return false;
    }
    if (Double.doubleToLongBits(mean) != Double.doubleToLongBits(other.mean)) {
      return false;
    }
    if (minimumIndices == null) {
      if (other.minimumIndices != null) {
        return false;
      }
    } else if (!minimumIndices.equals(other.minimumIndices)) {
      return false;
    }
    if (Double.doubleToLongBits(minimumValue) != Double.doubleToLongBits(other.minimumValue)) {
      return false;
    }
    if (samples != other.samples) {
      return false;
    }
    if (Double.doubleToLongBits(skewness) != Double.doubleToLongBits(other.skewness)) {
      return false;
    }
    if (tracker != other.tracker) {
      return false;
    }
    if (Double.doubleToLongBits(variance) != Double.doubleToLongBits(other.variance)) {
      return false;
    }
    return true;
  }

  /**
   * A builder that accumulates sequences of doubles and is capable of computing a statistical
   * snapshot via its build() method.
   * <p>
   * The builder has a few configuration options, namely specification of an index tracker
   * {@link Tracker} and a higher order estimator {@link Estimator}. The tracker must be supplied to
   * the constructor as it has a fundamental impact on the way the builder accumulates the indices
   * of the extrema, so this decision must be made at construction time. On the other hand, the
   * statistics estimation is performed just at build time, so it may be adjusted after the builder
   * is created. {@link Builder#Builder()} describes the default options for the builder.
   * </p>
   * <p>
   * If you are intending to accumulate statistics individually {@link Builder#accumulate(double)}
   * or via multiple invocations of {@link Builder#accumulate(Iterable)} or
   * {@link Builder#accumulate(Iterator)} the indices present in the Statistics class will be
   * referenced assuming the first element starts with index 0 and increases from there. So the
   * connection back to the indices of the various accumulation entry points is broken.
   * </p>
   */
  public static final class Builder
      implements picante.designpatterns.Builder<Statistics, RuntimeException> {

    /**
     * Creates the default builder. {@link Tracker#FIRST} is utilized to track indices, and
     * {@link Estimator#SAMPLE} is assumed initially.
     */
    public Builder() {
      this.tracker = Tracker.FIRST;
    }

    /**
     * Creates a builder with the supplied tracker. {@link Estimator#SAMPLE} is assumed initially.
     * 
     * @param tracker the tracker to utilize
     */
    public Builder(Tracker tracker) {
      this.tracker = tracker;
    }

    /**
     * Creates a builder from a previously accumulated statistic. The tracker utilized is precisely
     * the same as that used to create the supplied statistics. Further, the estimator is assigned
     * to the one used to produce the statistics, but may be changed.
     * 
     * @param statistics
     */
    public Builder(Statistics statistics) {
      this.n = statistics.samples;
      this.mean = statistics.mean;
      this.m2 = statistics.m2;
      this.m3 = statistics.m3;
      this.m4 = statistics.m4;
      this.min = statistics.minimumValue;
      this.max = statistics.maximumValue;
      this.minIndices.addAll(statistics.minimumIndices);
      this.maxIndices.addAll(statistics.maximumIndices);
      this.tracker = statistics.tracker;
      this.estimator = statistics.estimator;
    }

    /**
     * The estimator, by default {@link Estimator#SAMPLE} is utilized
     */
    private Estimator estimator = Estimator.SAMPLE;
    private final Tracker tracker;

    private final LinkedList<Integer> maxIndices = Lists.newLinkedList();
    private final LinkedList<Integer> minIndices = Lists.newLinkedList();

    private double max = -Double.MAX_VALUE;
    private double min = Double.MAX_VALUE;

    /*
     * These fields are package level access to allow the estimator enumeration direct access to
     * them. (Yes, this is a bit lazy coding, but it's all contained internally in the package.)
     */
    int n = 0;
    double mean = 0;
    double m2 = 0;
    double m3 = 0;
    double m4 = 0;

    /**
     * Creates a new statistical snapshot of the accumulated sequence so far.
     * 
     * @throws BuildFailedException if the builder has accumulated no data
     */
    @Override
    public Statistics build() {
      if (n == 0) {
        throw new BuildFailedException("Unable to build statistics, no samples provided.");
      }

      return new Statistics(n, mean, n < 2 ? 0 : estimator.estimateVariance(this),
          n < 3 ? 0 : estimator.estimateSkewness(this),
          n < 4 ? 0 : estimator.estimateExcessKurtosis(this), max, min,
          ImmutableList.copyOf(maxIndices), ImmutableList.copyOf(minIndices), m2, m3, m4, tracker,
          estimator);
    }

    /**
     * Accumulate an additional data element
     * 
     * @param x the element to add into the sequence
     * 
     * @return a reference to the builder instance for chaining convenience
     */
    public Builder accumulate(double x) {

      int n1 = n;
      n++;
      double delta = x - mean;
      double delta_n = delta / n;
      double delta_n2 = delta_n * delta_n;
      double term1 = delta * delta_n * n1;
      mean += delta_n;
      m4 += term1 * delta_n2 * (n * n - 3 * n + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3;
      m3 += term1 * delta_n * (n - 2) - 3 * delta_n * m2;
      m2 += term1;

      if (x >= max) {
        tracker.record(maxIndices, max, x, n1);
        max = x;
      }

      if (x <= min) {
        tracker.record(minIndices, min, x, n1);
        min = x;
      }

      return this;
    }

    /**
     * Accumulate all of the elements of a supplied iterable
     * 
     * @param data the iterable from which to accumulate
     * 
     * @return a reference to the builder instance for chaining convenience
     */
    public Builder accumulate(Iterable<Double> data) {

      for (double d : data) {
        accumulate(d);
      }

      return this;
    }

    /**
     * Accumulate all of the elements of a supplied iterator, consuming it
     * 
     * @param data the iterator which to consume and from which to accumulate
     * 
     * @return a reference to the builder instance for chaining convenience
     */
    public Builder accumulate(Iterator<Double> data) {

      while (data.hasNext()) {
        accumulate(data.next());
      }
      return this;
    }

    /**
     * Retrieves the index tracker specified at construction of the builder
     * 
     * @return the tracker utilized by the builder
     */
    public Tracker getTracker() {
      return tracker;
    }

    /**
     * Indicates the builder should utilize the supplied estimator for subsequent build operations
     * 
     * @param estimator the estimator to utilize
     * 
     * @return a reference to the builder instance for chaining convenience
     */
    public Builder useEstimator(Estimator estimator) {
      this.estimator = estimator;
      return this;
    }

    /**
     * Retrieves the estimator currently in use by the builder
     * 
     * @return the estimator
     */
    public Estimator getEstimator() {
      return this.estimator;
    }
  }

  /**
   * Creates a statistics builder.
   * 
   * @return newly created, default configured Statistics.Builder instance
   */
  public static Builder builder() {
    return new Builder();
  }

  /**
   * Creates a statistics instance from the supplied iterable assuming the {@link Estimator#SAMPLE}
   * estimator and {@link Tracker#FIRST} index tracker.
   * 
   * @param data the iterable of data to compute statistics
   * 
   * @return a newly created statistics instance
   */
  public static Statistics createSampleStatistics(Iterable<Double> data) {
    return new Builder().useEstimator(Estimator.SAMPLE).accumulate(data).build();
  }

  /**
   * Creates a statistics instance from the supplied iterable assuming the
   * {@link Estimator#POPULATION} estimator and {@link Tracker#FIRST} index tracker.
   * 
   * @param data the iterable of data to compute statistics
   * 
   * @return a newly created statistics instance
   */
  public static Statistics createPopulationStatistics(Iterable<Double> data) {
    return new Builder().useEstimator(Estimator.POPULATION).accumulate(data).build();
  }

}
