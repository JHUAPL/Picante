package picante.mechanics;

import com.google.common.base.Preconditions;

/**
 * represents an angle as a 24 hour clock angle
 * 
 * @author vandejd1
 */
public class HourAngle {
  private final int hours;
  private final int minutes;
  private final double seconds;

  public HourAngle(double fractionalHours) {
    Preconditions.checkArgument(fractionalHours >= 0, "fractional hours must be non-negative");
    hours = (int) fractionalHours;
    minutes = (int) ((fractionalHours - hours) * 60.0);
    seconds = (fractionalHours - hours - minutes / 60.0) * 3600.0;
  }

  public HourAngle(int hours, int minutes, double seconds) {
    this.hours = hours;
    this.minutes = minutes;
    this.seconds = seconds;
  }

  public double getFractionalHours() {
    return hours + (minutes + seconds / 60.0) / 60.0;
  }

  public int getHours() {
    return hours;
  }

  public int getMinutes() {
    return minutes;
  }

  public double getSeconds() {
    return seconds;
  }

}
