package picante.time;

public class TDBQuadTime {
  public final double seconds;
  public final double fractionalSeconds;

  public double getSeconds() {
    return seconds;
  }

  public double getFractionalSeconds() {
    return fractionalSeconds;
  }

  public TDBQuadTime(double seconds, double fractionalSeconds) {
    this.seconds = seconds;
    this.fractionalSeconds = fractionalSeconds;
  }

  public TDBQuadTime add(double deltaT) {
    double[] dts = TSEpoch.splitDouble(deltaT);
    double sec = seconds + dts[0];
    double frac = fractionalSeconds + dts[1];
    if (frac >= 1) {
      sec += 1.0;
      frac -= 1.0;
    }
    if (frac <= -1) {
      sec -= 1.0;
      frac -= 1.0;
    }
    return new TDBQuadTime(sec, frac);
  }

  public double difference(TDBQuadTime t1) {
    double sec = seconds - t1.seconds;
    double fracSec = fractionalSeconds - t1.fractionalSeconds;
    return sec + fracSec;
  }

  @Override
  public String toString() {
    return seconds + " + " + fractionalSeconds;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(fractionalSeconds);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(seconds);
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
    TDBQuadTime other = (TDBQuadTime) obj;
    if (Double.doubleToLongBits(fractionalSeconds) != Double
        .doubleToLongBits(other.fractionalSeconds)) {
      return false;
    }
    if (Double.doubleToLongBits(seconds) != Double.doubleToLongBits(other.seconds)) {
      return false;
    }
    return true;
  }


}
