package picante.time;

import picante.spice.kernel.tk.sclk.SCLK;
import picante.spice.kernel.tk.sclk.SCLKKernel;

public class SCLKTimeSystem implements TimeSystem<SCLK> {


  private final SCLKKernel kernel;
  private final TimeSystem<Double> etTS;

  public SCLKTimeSystem(SCLKKernel kernel) {
    this.kernel = kernel;

    TimeSystems timeSystems = TimeSystems.createUsingInternalConstants();
    etTS = timeSystems.getTDB();
  }

  @Override
  public TSEpoch getTSEpoch(SCLK time) {
    double tdb = kernel.convertFromSclkToTDB(time);
    return etTS.getTSEpoch(tdb);
  }

  @Override
  public SCLK getTime(TSEpoch tsepoch) {
    Double tdb = etTS.getTime(tsepoch);
    return kernel.convertFromTDBToSclk(tdb, new SCLK());
  }

  @Override
  public double difference(SCLK t0, SCLK t1) {
    return kernel.convertToEncodedSclk(t0) - kernel.convertToEncodedSclk(t1);
  }

  @Override
  public double difference(TSEpoch t0, TSEpoch t1) {
    SCLK s0 = getTime(t0);
    SCLK s1 = getTime(t1);
    return difference(s0, s1);
  }

  @Override
  public SCLK add(SCLK t0, double elapsedSeconds) {
    double e0 = kernel.convertToEncodedSclk(t0);
    double e1 = e0 + elapsedSeconds;
    return kernel.convertToSclk(e1, new SCLK());
  }

  @Override
  public TSEpoch add(TSEpoch t0, double deltaT) {
    SCLK s0 = getTime(t0);
    SCLK s1 = add(s0, deltaT);
    return getTSEpoch(s1);
  }

  public double getMaxEncodedSCLK() {
    return kernel.getMaxEncodedSCLK();
  }

  public double convertToEncodedSCLK(SCLK sclk) {
    return kernel.convertToEncodedSclk(sclk);
  }

  public SCLK convertFromEncodedSCLK(double encodedSCLK) {
    return kernel.convertToSclk(encodedSCLK, new SCLK());
  }
}
