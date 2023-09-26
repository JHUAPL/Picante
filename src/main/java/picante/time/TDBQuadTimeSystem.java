package picante.time;



class TDBQuadTimeSystem implements TimeSystem<TDBQuadTime> {

  @Override
  public TDBQuadTime getTime(TSEpoch tstime) {
    return new TDBQuadTime(tstime.getSeconds(), tstime.getFractionalSecs());
  }

  @Override
  public TDBQuadTime add(TDBQuadTime t0, double deltaT) {
    return t0.add(deltaT);
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    TDBQuadTime et0 = getTime(t0);
    TDBQuadTime et1 = add(et0, deltaT);
    return getTSEpoch(et1);
  }

  @Override
  public double difference(TDBQuadTime t0, TDBQuadTime t1) {
    return t0.difference(t1);
  }

  @Override
  public double difference(TSEpoch t0, TSEpoch t1) {
    TDBQuadTime et0 = getTime(t0);
    TDBQuadTime et1 = getTime(t1);
    return difference(et0, et1);
  }

  @Override
  public TSEpoch getTSEpoch(TDBQuadTime time) {
    return new TSEpoch(time.seconds, time.fractionalSeconds);
  }


}
