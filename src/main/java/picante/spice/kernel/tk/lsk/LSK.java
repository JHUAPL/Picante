package picante.spice.kernel.tk.lsk;

import java.util.NavigableSet;

import com.google.common.collect.ImmutableSortedSet;
import picante.designpatterns.Blueprint;
import picante.time.LeapsecondEntry;
import picante.time.TimeSystems;
import picante.time.TimeSystems.Builder;


/**
 * Implementation of the leapseconds kernel and the associated time conversion facilities provided
 * by the data contained within this text kernel.
 */
public class LSK implements Blueprint<TimeSystems.Builder> {

  private final ImmutableSortedSet<LeapsecondEntry> deltaAT;

  /**
   * The difference between TDT and TAI in seconds.
   */
  private final double deltaTa;

  /**
   * Amplitude of the TDB - TDT sinusoidal series.
   */
  private final double k;

  /**
   * Eccentricity of earth-moon barycenter orbit.
   */
  private final double eb;

  /**
   * Mean anomaly terms in the earth-moon barycenter orbit parameters.
   */
  private final double[] m = new double[2];

  private final TDTProvider tdtProvider;

  public LSK(double deltaTa, double k, double eb, double[] m,
      NavigableSet<LeapsecondEntry> deltaAT) {
    this.deltaTa = deltaTa;
    this.k = k;
    this.eb = eb;
    System.arraycopy(m, 0, this.m, 0, 2);
    this.tdtProvider = new TDTProvider(this.k, this.eb, this.m);
    this.deltaAT = ImmutableSortedSet.copyOf(deltaAT);
  }

  public UniformTimeProvider getTDTProvider() {
    return tdtProvider;
  }

  public double getDeltaTa() {
    return deltaTa;
  }

  public double getK() {
    return k;
  }

  public double getEB() {
    return eb;
  }

  public double[] getM(double[] buffer) {
    System.arraycopy(m, 0, buffer, 0, 2);
    return buffer;
  }

  public NavigableSet<LeapsecondEntry> getDeltaAT() {
    return deltaAT;
  }

  @Override
  public Builder configure(Builder builder) {

    builder.withDeltaT_A(deltaTa);
    builder.withEB(eb);
    builder.withK(k);
    builder.withM0(m[0]);
    builder.withM1(m[1]);
    builder.withLeapseconds(deltaAT);

    return builder;
  }

}
