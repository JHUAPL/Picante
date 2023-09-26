package picante.time;


class UTCTimeSystem implements TimeSystem<UTCEpoch> {

  private final UTCFactory utcProvider;
  private final TimeSystem<Double> tdbTS;

  UTCTimeSystem(UTCFactory utcProvider, TimeSystem<Double> tdbTS) {
    super();
    this.utcProvider = utcProvider;
    this.tdbTS = tdbTS;
  }

  @Override
  public TSEpoch getTSEpoch(UTCEpoch utc) {
    return tdbTS.getTSEpoch(utcProvider.getTDB(utc));
  }

  @Override
  public UTCEpoch getTime(TSEpoch tstime) {
    return utcProvider.getUTCfromTDB(tdbTS.getTime(tstime));
  }

  @Override
  public UTCEpoch add(UTCEpoch t0, double deltaT) {
    double tdb = utcProvider.getTDB(t0);
    double tai = utcProvider.convertTDBtoTAI(tdb) + deltaT;
    tdb = utcProvider.convertTAItoTDB(tai);
    return utcProvider.getUTCfromTDB(tdb);
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    UTCEpoch utc = add(getTime(t0), deltaT);
    return getTSEpoch(utc);
  }

  @Override
  public double difference(UTCEpoch t0, UTCEpoch t1) {
    double tdb0 = utcProvider.getTDB(t0);
    double tai0 = utcProvider.convertTDBtoTAI(tdb0);
    double tdb1 = utcProvider.getTDB(t1);
    double tai1 = utcProvider.convertTDBtoTAI(tdb1);
    return tai0 - tai1;
  }

  @Override
  public double difference(TSEpoch t0, TSEpoch t1) {
    return difference(getTime(t0), getTime(t1));
  }



}
