package picante.spice.jspice;

/**
 *
 * <p>
 * Static parameters used in the jspice package
 *
 * @author Douglas Rodgers <Douglas.Rodgers@jhuapl.edu>
 *
 */
final class Parameter {

  static final double SPICELIB_JYEAR = 31557600.0;
  static final double SPICELIB_PI = 3.141592653589793238462643383279502884197;
  static final double SPICELIB_RPD = 0.01745329251994329576923690768488612713443;
  static final double SPICELIB_SPD = 86400.0;
  static final double SPICELIB_TWOPI = 6.283185307179586476925286766559005768394;

  static final double M2EUL_DTOL = 0.1;
  static final double M2EUL_NTOL = 0.1;

  static final int XF2EUL_ALPHA = 0;
  static final int XF2EUL_BETA = 1;
  static final int XF2EUL_GAMMA = 2;
  static final int XF2EUL_DALPHA = 3;
  static final int XF2EUL_DBETA = 4;
  static final int XF2EUL_DGAMMA = 5;
  static final int[] XF2EUL_NEXT = {2, 3, 1};
  static final double[] XF2EUL_DELTA = {0.0, -1.0, 1.0, 1.0, 0.0, -1.0, -1.0, 1.0, 0.0};

  static final double ZZEPRC76_ZETA1 = 2306.2181;
  static final double ZZEPRC76_ZETA2 = 0.30188;
  static final double ZZEPRC76_ZETA3 = 0.017998;
  static final double ZZEPRC76_Z1 = 2306.2181;
  static final double ZZEPRC76_Z2 = 1.09468;
  static final double ZZEPRC76_Z3 = 0.018203;
  static final double ZZEPRC76_THETA1 = 2004.3109;
  static final double ZZEPRC76_THETA2 = -0.42665;
  static final double ZZEPRC76_THETA3 = -0.041833;

  static final double ZZMOBLIQ_C0 = 84381.448;
  static final double ZZMOBLIQ_C1 = -46.8150;
  static final double ZZMOBLIQ_C2 = -0.00059;
  static final double ZZMOBLIQ_C3 = 0.001813;

  static final int ZZWAHR_NTERM = 106;
  static final double ZZWAHR_REV = 360.0;
  static final double ZZWAHR_DEG = 1.0;
  static final double ZZWAHR_MIN = 1.0 / 60.0;
  static final double ZZWAHR_SEC = 1.0 / 3600.0;
  static final double ZZWAHR_CENT1 = 36525.0;
  static final double ZZWAHR_CENT2 = ZZWAHR_CENT1 * ZZWAHR_CENT1 * 1.0e-8;
  static final double ZZWAHR_CENT3 = ZZWAHR_CENT1 * ZZWAHR_CENT2 * 1.0e-4;
  /**
   * The following values are direct conversions to degrees from page 114 of the Explanatory
   * Supplement to the Astronomical Almanac.
   *
   * L0 through L3 are the coefficients for l---the mean longitude of the Moon minus the mean
   * longitude of the Moon's perigee. Units for the various terms:
   *
   * L0 degrees L1 degrees/day L2 degrees/(0.0001 days)**2 L3 degrees/(0.0001 days)**3
   */
  static final double ZZWAHR_L0 = 134.0 * ZZWAHR_DEG + 57.0 * ZZWAHR_MIN + 46.733 * ZZWAHR_SEC;
  static final double ZZWAHR_L1 =
      (1325.0 * ZZWAHR_REV + 198.0 * ZZWAHR_DEG + 52.0 * ZZWAHR_MIN + 2.633 * ZZWAHR_SEC)
          / ZZWAHR_CENT1;
  static final double ZZWAHR_L2 = 31.310 * ZZWAHR_SEC / ZZWAHR_CENT2;
  static final double ZZWAHR_L3 = 0.064 * ZZWAHR_SEC / ZZWAHR_CENT3;



  /**
   * LP0 through LP3 are the coefficients for l'---the mean longitude of the Sun minus the mean
   * longitude of the Sun's perigee. Units for the various terms:
   *
   * LP0 degrees LP1 degrees/day LP2 degrees/(0.0001 days)**2 LP3 degrees/(0.0001 days)**3
   */
  static final double ZZWAHR_LP0 = 357.0 * ZZWAHR_DEG + 31.0 * ZZWAHR_MIN + 39.804 * ZZWAHR_SEC;
  static final double ZZWAHR_LP1 =
      (99.0 * ZZWAHR_REV + 359.0 * ZZWAHR_DEG + 3.0 * ZZWAHR_MIN + 1.224 * ZZWAHR_SEC)
          / ZZWAHR_CENT1;
  static final double ZZWAHR_LP2 = -0.577 * ZZWAHR_SEC / ZZWAHR_CENT2;
  static final double ZZWAHR_LP3 = -0.012 * ZZWAHR_SEC / ZZWAHR_CENT3;


  /**
   * F0 through F3 are the coefficients for F---the mean longitude of the Moon minus the mean
   * longitude of the Moon's node. Units for the various terms:
   *
   * F0 degrees F1 degrees/day F2 degrees/(0.0001 days)**2 F3 degrees/(0.0001 days)**3
   */
  static final double ZZWAHR_F0 = 93.0 * ZZWAHR_DEG + 16.0 * ZZWAHR_MIN + 18.877 * ZZWAHR_SEC;
  static final double ZZWAHR_F1 =
      (1342.0 * ZZWAHR_REV + 82.0 * ZZWAHR_DEG + 1.0 * ZZWAHR_MIN + 3.137 * ZZWAHR_SEC)
          / ZZWAHR_CENT1;
  static final double ZZWAHR_F2 = -13.257 * ZZWAHR_SEC / ZZWAHR_CENT2;
  static final double ZZWAHR_F3 = 0.011 * ZZWAHR_SEC / ZZWAHR_CENT3;

  /**
   * D0 through D3 are the coefficients for D---the mean longitude of the Moon minus the mean
   * longitude of the Sun. Units for the various terms:
   *
   * D0 degrees D1 degrees/day D2 degrees/(0.0001 days)**2 D3 degrees/(0.0001 days)**3
   */
  static final double ZZWAHR_D0 = 297.0 * ZZWAHR_DEG + 51.0 * ZZWAHR_MIN + 1.307 * ZZWAHR_SEC;
  static final double ZZWAHR_D1 =
      (1236.0 * ZZWAHR_REV + 307.0 * ZZWAHR_DEG + 6.0 * ZZWAHR_MIN + 41.328 * ZZWAHR_SEC)
          / ZZWAHR_CENT1;
  static final double ZZWAHR_D2 = -6.891 * ZZWAHR_SEC / ZZWAHR_CENT2;
  static final double ZZWAHR_D3 = 0.019 * ZZWAHR_SEC / ZZWAHR_CENT3;

  /**
   * MG0 through MG3 are the coefficients for Omega---the longitude of the mean ascending node of
   * the lunar orbit on the ecliptic measured from the mean equinox of date. NOTE: The constant term
   * MG0 is correct. The value o 135 02' 40".280
   *
   * given in the Explanatory Supplement page 114 has a typo. The correct value is the one used
   * here:
   *
   * o 125 02' 40".280
   *
   * MG0 degrees MG1 degrees/day MG2 degrees/(0.0001 days)**2 MG3 degrees/(0.0001 days)**3
   */
  static final double ZZWAHR_MG0 = 125.0 * ZZWAHR_DEG + 2.0 * ZZWAHR_MIN + 40.280 * ZZWAHR_SEC;
  static final double ZZWAHR_MG1 =
      -(5.0 * ZZWAHR_REV + 134.0 * ZZWAHR_DEG + 8.0 * ZZWAHR_MIN + 10.539 * ZZWAHR_SEC)
          / ZZWAHR_CENT1;
  static final double ZZWAHR_MG2 = 7.455 * ZZWAHR_SEC / ZZWAHR_CENT2;
  static final double ZZWAHR_MG3 = 0.008 * ZZWAHR_SEC / ZZWAHR_CENT3;
  /**
   * Below are the coefficients for the various periods of the nutation model. There does not appear
   * to be any particular reason for the ordering selected. The n'th row corresponds to the n'th
   * period listed above each data statement.
   */

  static final int[] ZZWAHR_MATRIX = {
      /**
       * Periods: 6798.4, 3399.2, 1305.5, 1095.2, 1615.7, 3232.9, 6786.3, 943.2, 182.6, 365.3,
       * 121.7, 365.2, 177.8, 205.9, 173.3, 182.6, 386.0, 91.3, 346.6
       */
      0, 0, 0, 0, 1, -171996, -1742, 92025, 89, 0, 0, 0, 0, 2, 2062, 2, -895, 5, -2, 0, 2, 0, 1, 46,
      0, -24, 0, 2, 0, -2, 0, 0, 11, 0, 0, 0, -2, 0, 2, 0, 2, -3, 0, 1, 0, 1, -1, 0, -1, 0, -3, 0,
      0, 0, 0, -2, 2, -2, 1, -2, 0, 1, 0, 2, 0, -2, 0, 1, 1, 0, 0, 0, 0, 0, 2, -2, 2, -13187, -16,
      5736, -31, 0, 1, 0, 0, 0, 1426, -34, 54, -1, 0, 1, 2, -2, 2, -517, 12, 224, -6, 0, -1, 2, -2,
      2, 217, -5, -95, 3, 0, 0, 2, -2, 1, 129, 1, -70, 0, 2, 0, 0, -2, 0, 48, 0, 1, 0, 0, 0, 2, -2,
      0, -22, 0, 0, 0, 0, 2, 0, 0, 0, 17, -1, 0, 0, 0, 1, 0, 0, 1, -15, 0, 9, 0, 0, 2, 2, -2, 2,
      -16, 1, 7, 0, 0, -1, 0, 0, 1, -12, 0, 6, 0,

      /**
       * Periods: 199.8, 346.6, 212.3, 119.6, 411.8, 131.7, 169.0, 329.8, 409.2, 388.3, 117.5, 13.7,
       * 27.6, 13.6, 9.1, 31.8, 27.1, 14.8, 27.7
       */
      -2, 0, 0, 2, 1, -6, 0, 3, 0, 0, -1, 2, -2, 1, -5, 0, 3, 0, 2, 0, 0, -2, 1, 4, 0, -2, 0, 0, 1,
      2, -2, 1, 4, 0, -2, 0, 1, 0, 0, -1, 0, -4, 0, 0, 0, 2, 1, 0, -2, 0, 1, 0, 0, 0, 0, 0, -2, 2,
      1, 1, 0, 0, 0, 0, 1, -2, 2, 0, -1, 0, 0, 0, 0, 1, 0, 0, 2, 1, 0, 0, 0, -1, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 1, 2, -2, 0, -1, 0, 0, 0, 0, 0, 2, 0, 2, -2274, -2, 977, -5, 1, 0, 0, 0, 0, 712, 1,
      -7, 0, 0, 0, 2, 0, 1, -386, -4, 200, 0, 1, 0, 2, 0, 2, -301, 0, 129, -1, 1, 0, 0, -2, 0, -158,
      0, -1, 0, -1, 0, 2, 0, 2, 123, 0, -53, 0, 0, 0, 0, 2, 0, 63, 0, -2, 0, 1, 0, 0, 0, 1, 63, 1,
      -33, 0,
      /**
       * Periods: 27.4, 9.6, 9.1, 7.1, 13.8, 23.9, 6.9, 13.6, 27.0, 32.0, 31.7, 9.5, 34.8, 13.2,
       * 14.2, 5.6, 9.6, 12.8, 14.8
       */
      -1, 0, 0, 0, 1, -58, -1, 32, 0, -1, 0, 2, 2, 2, -59, 0, 26, 0, 1, 0, 2, 0, 1, -51, 0, 27, 0,
      0, 0, 2, 2, 2, -38, 0, 16, 0, 2, 0, 0, 0, 0, 29, 0, -1, 0, 1, 0, 2, -2, 2, 29, 0, -12, 0, 2,
      0, 2, 0, 2, -31, 0, 13, 0, 0, 0, 2, 0, 0, 26, 0, -1, 0, -1, 0, 2, 0, 1, 21, 0, -10, 0, -1, 0,
      0, 2, 1, 16, 0, -8, 0, 1, 0, 0, -2, 1, -13, 0, 7, 0, -1, 0, 2, 2, 1, -10, 0, 5, 0, 1, 1, 0,
      -2, 0, -7, 0, 0, 0, 0, 1, 2, 0, 2, 7, 0, -3, 0, 0, -1, 2, 0, 2, -7, 0, 3, 0, 1, 0, 2, 2, 2,
      -8, 0, 3, 0, 1, 0, 0, 2, 0, 6, 0, 0, 0, 2, 0, 2, -2, 2, 6, 0, -3, 0, 0, 0, 0, 2, 1, -6, 0, 3,
      0,
      /**
       * Periods: 7.1, 23.9, 14.7, 29.8, 6.9, 15.4, 26.9, 29.5, 25.6, 9.1, 9.4, 9.8, 13.7, 5.5, 7.2,
       * 8.9, 32.6, 13.8, 27.8
       */
      0, 0, 2, 2, 1, -7, 0, 3, 0, 1, 0, 2, -2, 1, 6, 0, -3, 0, 0, 0, 0, -2, 1, -5, 0, 3, 0, 1, -1,
      0, 0, 0, 5, 0, 0, 0, 2, 0, 2, 0, 1, -5, 0, 3, 0, 0, 1, 0, -2, 0, -4, 0, 0, 0, 1, 0, -2, 0, 0,
      4, 0, 0, 0, 0, 0, 0, 1, 0, -4, 0, 0, 0, 1, 1, 0, 0, 0, -3, 0, 0, 0, 1, 0, 2, 0, 0, 3, 0, 0, 0,
      1, -1, 2, 0, 2, -3, 0, 1, 0, -1, -1, 2, 2, 2, -3, 0, 1, 0, -2, 0, 0, 0, 1, -2, 0, 1, 0, 3, 0,
      2, 0, 2, -3, 0, 1, 0, 0, -1, 2, 2, 2, -3, 0, 1, 0, 1, 1, 2, 0, 2, 2, 0, -1, 0, -1, 0, 2, -2,
      1, -2, 0, 1, 0, 2, 0, 0, 0, 1, 2, 0, -1, 0, 1, 0, 0, 0, 2, -2, 0, 1, 0,

      /**
       * Periods: 9.2, 9.3, 27.3, 10.1, 14.6, 5.8, 15.9, 22.5, 5.6, 7.3, 9.1, 29.3, 12.8, 4.7, 9.6,
       * 12.7, 8.7, 23.8, 13.1
       */
      3, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 1, 2, 2, 0, -1, 0, -1, 0, 0, 0, 2, 1, 0, -1, 0, 1, 0, 0,
      -4, 0, -1, 0, 0, 0, -2, 0, 2, 2, 2, 1, 0, -1, 0, -1, 0, 2, 4, 2, -2, 0, 1, 0, 2, 0, 0, -4, 0,
      -1, 0, 0, 0, 1, 1, 2, -2, 2, 1, 0, -1, 0, 1, 0, 2, 2, 1, -1, 0, 1, 0, -2, 0, 2, 4, 2, -1, 0,
      1, 0, -1, 0, 4, 0, 2, 1, 0, 0, 0, 1, -1, 0, -2, 0, 1, 0, 0, 0, 2, 0, 2, -2, 1, 1, 0, -1, 0, 2,
      0, 2, 2, 2, -1, 0, 0, 0, 1, 0, 0, 2, 1, -1, 0, 0, 0, 0, 0, 4, -2, 2, 1, 0, 0, 0, 3, 0, 2, -2,
      2, 1, 0, 0, 0, 1, 0, 2, -2, 0, -1, 0, 0, 0, 0, 1, 2, 0, 1, 1, 0, 0, 0,
      /**
       * Periods: 35.0, 13.6, 25.4, 14.2, 9.5, 14.2, 34.7, 32.8, 7.1, 4.8, 27.3
       */
      -1, -1, 0, 2, 1, 1, 0, 0, 0, 0, 0, -2, 0, 1, -1, 0, 0, 0, 0, 0, 2, -1, 2, -1, 0, 0, 0, 0, 1,
      0, 2, 0, -1, 0, 0, 0, 1, 0, -2, -2, 0, -1, 0, 0, 0, 0, -1, 2, 0, 1, -1, 0, 0, 0, 1, 1, 0, -2,
      1, -1, 0, 0, 0, 1, 0, -2, 2, 0, -1, 0, 0, 0, 2, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 2, 4, 2, -1, 0,
      0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0};

}
