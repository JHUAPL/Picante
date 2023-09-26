package picante.time;

import static picante.math.PicanteMath.sin;

class TDTTimeSystem implements TimeSystem<Double> {

  private static int CMAP_ITERATIONS = 3;

  private final double k;
  private final double eb;
  private final double[] m;
  private TDBTimeSystem tdb;

  TDTTimeSystem(double k, double eb, double m0, double m1, TDBTimeSystem tdb) {
    super();
    this.k = k;
    this.eb = eb;
    this.m = new double[] {m0, m1};
    this.tdb = tdb;
  }

  @Override
  public TSEpoch getTSEpoch(Double time) {
    double tdbTime = time + k * sin(m[0] + m[1] * time + eb * sin(m[0] + m[1] * time));
    return tdb.getTSEpoch(tdbTime);
  }

  @Override
  public Double getTime(TSEpoch tsepoch) {
    double tdbTime = tdb.getTime(tsepoch);
    double tdt = tdbTime;

    for (int i = 0; i < CMAP_ITERATIONS; i++) {
      tdt = tdbTime - k * sin(m[0] + m[1] * tdt + eb * sin(m[0] + m[1] * tdt));
    }

    return tdt;
  }

  @Override
  public double difference(Double t0, Double t1) {
    return t0 - t1;
  }

  @Override
  public double difference(TSEpoch a, TSEpoch b) {
    return getTime(a) - getTime(b);
  }

  @Override
  public Double add(Double t0, double elapsedSeconds) {
    return t0 + elapsedSeconds;
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    return getTSEpoch(getTime(t0) + deltaT);
  }

}
