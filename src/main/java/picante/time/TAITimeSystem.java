package picante.time;

class TAITimeSystem implements TimeSystem<Double> {

  private final TDTTimeSystem tdt;
  private final double delta_t_a;

  TAITimeSystem(double delta_t_a, TDTTimeSystem tdt) {
    super();
    this.delta_t_a = delta_t_a;
    this.tdt = tdt;
  }

  /*
   * TDT = TAI + DELTA_T_A
   */

  @Override
  public TSEpoch getTSEpoch(Double time) {
    double tdtTime = time + delta_t_a;
    return tdt.getTSEpoch(tdtTime);
  }

  @Override
  public Double getTime(TSEpoch tsepoch) {
    double tdtTime = tdt.getTime(tsepoch);
    return tdtTime - delta_t_a;
  }

  @Override
  public double difference(Double t0, Double t1) {
    return tdt.difference(t0, t1);
  }

  @Override
  public double difference(TSEpoch a, TSEpoch b) {
    return tdt.difference(a, b);
  }

  @Override
  public Double add(Double t0, double elapsedSeconds) {
    return tdt.add(t0, elapsedSeconds);
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    return tdt.add(t0, deltaT);
  }

}
