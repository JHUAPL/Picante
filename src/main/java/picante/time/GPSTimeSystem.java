package picante.time;

class GPSTimeSystem implements TimeSystem<Double> {

  private final double gps_tai_dt;
  private final double delta_t_a;
  private final TDTTimeSystem tdt;

  /*
   * TAI = GPS + GPS_TAI_DT TDT = TAI + DELTA_T_A
   * 
   * Thus:
   * 
   * GPS = TDT - DELTA_T_A - GPS_TAI_DT TDT = GPS + DELTA_T_A + GPS_TAI_DT
   */

  GPSTimeSystem(double gps_tai_dt, double delta_t_a, TDTTimeSystem tdt) {
    super();
    this.gps_tai_dt = gps_tai_dt;
    this.delta_t_a = delta_t_a;
    this.tdt = tdt;
  }

  @Override
  public TSEpoch getTSEpoch(Double time) {
    double tdtTime = time + delta_t_a + gps_tai_dt;
    return tdt.getTSEpoch(tdtTime);
  }

  @Override
  public Double getTime(TSEpoch tsepoch) {
    double tdtTime = tdt.getTime(tsepoch);
    return tdtTime - delta_t_a - gps_tai_dt;
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
    return t0 + elapsedSeconds;
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    return tdt.add(t0, deltaT);
  }

}
