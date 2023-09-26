package picante.spice.kernel.spk;

import picante.mechanics.StateVector;

/**
 * Data contents of an SPK type 1 record.
 * <p>
 * As the record contents are a relatively opaque output from the JPL orbit determination process,
 * this class is written in a manner that is convenient for populating the contents of the record
 * directly from a DAF based list of double precision numbers. The
 * {@link SPKType1Record#setRecord(double[])} method is written with this fact in mind, but if
 * necessary the class will evolve to provide additional setter methods as appropriate.
 * </p>
 * <p>
 * Further note that only a single evaluation method is provided as it is a direct parallel
 * implementation to SPKE01 from SPICE. If performance considerations are an issue, then a separate
 * method may be added in the future for position only evaluations.
 * </p>
 */
public class SPKType1Record {

  private double tl;
  private final double[] g = new double[15];
  private final double[] refpos = new double[3];
  private final double[] refvel = new double[3];
  private final double[][] dt = new double[3][15];
  private int kqmax1;
  private final int[] kq = new int[3];

  /*
   * Temporary buffers used in the evaluation of the record, that would otherwise need to be created
   * on demand at evaluation time.
   */
  private final double[] fc = new double[14];
  private final double[] wc = new double[13];
  private final double[] w = new double[17];
  private final double[] calcPos = new double[3];
  private final double[] calcVel = new double[3];

  /**
   * Creates a default, empty type 1 record.
   */
  public SPKType1Record() {
    fc[0] = 1;
  }

  /**
   * Sets the contents of the SPK type 1 record from an array of doubles, ordered as they would be
   * in a DAF.
   * 
   * @param record the DAF ordered record.
   */
  public void setRecord(double[] record) {

    int index = 0;
    tl = record[index++];

    System.arraycopy(record, index, g, 0, g.length);
    index += g.length;

    refpos[0] = record[index++];
    refvel[0] = record[index++];
    refpos[1] = record[index++];
    refvel[1] = record[index++];
    refpos[2] = record[index++];
    refvel[2] = record[index++];

    System.arraycopy(record, index, dt[0], 0, dt[0].length);
    index += dt[0].length;

    System.arraycopy(record, index, dt[1], 0, dt[1].length);
    index += dt[1].length;

    System.arraycopy(record, index, dt[2], 0, dt[2].length);
    index += dt[2].length;

    kqmax1 = (int) record[index++];
    kq[0] = (int) record[index++];
    kq[1] = (int) record[index++];
    kq[2] = (int) record[index++];

  }

  /**
   * Evaluates the data stored in the record to provide the position at the requested time. While
   * extrapolation is allowed by the API, it is strongly discouraged.
   * 
   * @param time the ephemeris time of interest (TDB seconds past J2000)
   * @param buffer the buffer to populate with the results.
   * 
   * @return a reference to buffer for convenience
   */
  public StateVector evaluate(double time, StateVector buffer) {

    double delta = time - tl;
    double tp = delta;
    int mq2 = kqmax1 - 2;
    int ks = kqmax1 - 1;

    for (int j = 0; j < mq2; j++) {
      fc[j + 1] = tp / g[j];
      wc[j] = delta / g[j];
      tp = delta + g[j];
    }

    for (int j = 0; j < kqmax1; j++) {
      w[j] = 1.0 / (j + 1);
    }

    int jx = 0;
    int ks1 = ks - 1;

    while (ks >= 2) {
      jx++;

      for (int j = 0; j < jx; j++) {
        w[j + ks] = fc[j + 1] * w[j + ks1] - wc[j] * w[j + ks];
      }

      ks = ks1;
      ks1--;
    }

    int kqq;
    double sum;

    for (int i = 0; i < 3; i++) {
      kqq = kq[i];
      sum = 0.0;

      for (int j = kqq - 1; j >= 0; j--) {
        sum += dt[i][j] * w[j + ks];
      }

      calcPos[i] = refpos[i] + delta * (refvel[i] + delta * sum);

    }

    /*
     * Position interpolation completed, copy the result to the outgoing state.
     */

    for (int j = 0; j < jx; j++) {
      w[j + ks] = fc[j + 1] * w[j + ks1] - wc[j] * w[j + ks];
    }

    ks--;

    for (int i = 0; i < 3; i++) {

      kqq = kq[i];
      sum = 0.0;

      for (int j = kqq - 1; j >= 0; j--) {
        sum += dt[i][j] * w[j + ks];
      }

      calcVel[i] = refvel[i] + delta * sum;
    }

    buffer.getPosition().setTo(calcPos[0], calcPos[1], calcPos[2]);
    buffer.getVelocity().setTo(calcVel[0], calcVel[1], calcVel[2]);

    return buffer;

  }

  public double getTl() {
    return tl;
  }

  public double[] getG(double[] buffer) {
    System.arraycopy(g, 0, buffer, 0, g.length);
    return buffer;
  }

  /**
   * 
   * @param buffer an array of at least length 3
   * @return
   */
  public double[] getRefpos(double[] buffer) {
    System.arraycopy(refpos, 0, buffer, 0, refpos.length);
    return buffer;
  }

  /**
   * 
   * @param buffer an array of at least length 3
   * @return
   */
  public double[] getRefvel(double[] buffer) {
    System.arraycopy(refvel, 0, buffer, 0, refpos.length);
    return buffer;
  }

  /**
   * 
   * @param buffer a two dimensional array of at least [3][15] in length.
   * @return
   */
  public double[][] getDt(double[][] buffer) {
    for (int i = 0; i < dt.length; i++) {
      System.arraycopy(dt[i], 0, buffer[i], 0, dt[i].length);
    }
    return buffer;
  }

  public int getKqmax1() {
    return kqmax1;
  }

  /**
   * 
   * @param buffer an array of at least length 3
   * @return
   */
  public int[] getKq(int[] buffer) {
    System.arraycopy(kq, 0, buffer, 0, kq.length);
    return buffer;
  }

}
