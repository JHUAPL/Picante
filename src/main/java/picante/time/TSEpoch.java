/*
 * Filename: TSTime.java Author : vandejd1 Created : Mar 20, 2009
 * 
 * Copyright (C) 2009 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.floor;
import static picante.math.PicanteMath.signum;
import java.io.Serializable;

/**
 * an opaque, immutable, high precision time representation
 *
 * The details of how this works are still being hammered out...
 *
 * @author vandejd1
 */
public final class TSEpoch implements Comparable<TSEpoch>, Cloneable, Serializable {
  private static final long serialVersionUID = 298685072558729717L;
  public static final TSEpoch MIN_TIME = new TSEpoch(-Double.MAX_VALUE, -1.0);
  public static final TSEpoch MAX_TIME = new TSEpoch(Double.MAX_VALUE, 1.0);

  /**
   * just the integral portion, such that Math.abs(seconds) = Math.floor(Math.abs(seconds))
   */
  private final double seconds;

  /**
   * The fractional portion has the same sign as the integral portion
   */
  private final double fractionalSecs;

  TSEpoch(double[] val) {
    this(val[0], val[1]);
  }

  TSEpoch(double seconds, double fractionalSecs) {
    this.seconds = seconds;
    this.fractionalSecs = fractionalSecs;
  }

  double getSeconds() {
    return seconds;
  }

  double getFractionalSecs() {
    return fractionalSecs;
  }

  @Override
  public int compareTo(TSEpoch o) {
    if (seconds < o.seconds) {
      return -1;
    }
    if (seconds > o.seconds) {
      return 1;
    }

    if (seconds > 0) {
      if (fractionalSecs < o.fractionalSecs) {
        return -1;
      }
      if (fractionalSecs > o.fractionalSecs) {
        return 1;
      }
    } else {
      if (fractionalSecs > o.fractionalSecs) {
        return -1;
      }
      if (fractionalSecs < o.fractionalSecs) {
        return 1;
      }
    }
    return 0;
  }

  @Override
  public String toString() {
    return "[" + seconds + "," + fractionalSecs + "]";
  }



  /*
   * Returns an array of two doubles the 0th element is the integer part and the 1st element is the
   * fractional part as used by TSEpoch Can be used to call the appropriate TSEpoch constuctor
   * (assuming your times are low precision enough to be expressed as doubles)
   */
  public static double[] splitDouble(double d) {
    double absD = abs(d);
    double intD = floor(absD);
    double fracD = 0.0;
    if (intD != 0.0) {
      fracD = absD % intD;
    } else {
      fracD = absD;
    }
    double signD = signum(d);
    intD *= signD;
    fracD *= signD;
    return new double[] {intD, fracD};
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(fractionalSecs);
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
    TSEpoch other = (TSEpoch) obj;
    if (Double.doubleToLongBits(fractionalSecs) != Double.doubleToLongBits(other.fractionalSecs)) {
      return false;
    }
    if (Double.doubleToLongBits(seconds) != Double.doubleToLongBits(other.seconds)) {
      return false;
    }
    return true;
  }

  @Override
  public Object clone() {
    try {
      // no immutable fields to copy, so this is it:
      return super.clone();
    } catch (CloneNotSupportedException e) {
      // This should never happen because TSEpoch implements Clonable.
      throw new InternalError(e.toString());
    }
  }

}
