package picante.time;

class DaySecond {

  public int daysSince1AD;
  public double secondsOfDay;

  public DaySecond() {
    daysSince1AD = 0;
    secondsOfDay = 0.0;
  }

  public DaySecond(int daysSince1AD, double secondsOfDay) {
    this.daysSince1AD = daysSince1AD;
    this.secondsOfDay = secondsOfDay;
  }

  public void setTo(DaySecond ds) {
    this.daysSince1AD = ds.daysSince1AD;
    this.secondsOfDay = ds.secondsOfDay;
  }

}
