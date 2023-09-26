package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import picante.exceptions.BugException;
import picante.units.FundamentalPhysicalConstants;

/**
 * Enumeration describing the various times of aberration corrections supported by the
 * {@link AberratedEphemerisProvider}.
 * <p>
 * In general when working with aberration corrected vector functions, it is rarely correct to
 * request a light time only correction. In practice one should choose from:
 * <ul>
 * <li>{@link AberrationCorrection#LT_S}</li>
 * <li>{@link AberrationCorrection#NONE}</li>
 * <li>{@link AberrationCorrection#XLT_S}</li>
 * </ul>
 * </p>
 * <p>
 * The other options are provided for completeness, and are useful under specific circumstances. The
 * stellar aberration correction has no impact on the computed value of the light time or its time
 * derivative.
 * </p>
 */
public enum AberrationCorrection {

  /**
   * Apply both light time and stellar aberration corrections in the receipt case.
   */
  LT_S(false, true, false),

  /**
   * Apply only light time corrections in the receipt case.
   */
  LT(false, false, false),

  /**
   * Apply no corrections. Sometimes this is referred to as geometric.
   */
  NONE(true, false, false),

  /**
   * Apply only light time corrections in the transmission case.
   */
  XLT(false, false, true),

  /**
   * Apply both light time and stellar aberration corrections in the transmission case.
   */
  XLT_S(false, true, true);

  /**
   * Since the {@link FundamentalPhysicalConstants#SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC} is not
   * provided in units of km/sec, convert it here once for use within this package.
   */
  static final double SPEED_OF_LIGHT =
      FundamentalPhysicalConstants.SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC / 1000.0;

  private final boolean isGeometric;
  private final boolean useStellarAberration;
  private final boolean isTransmission;

  private AberrationCorrection(boolean isGeometric, boolean useStellarAberration,
      boolean isTransmission) {
    this.isGeometric = isGeometric;
    this.useStellarAberration = useStellarAberration;
    this.isTransmission = isTransmission;
  }

  /**
   * Indicates whether the correction is geometric.
   */
  public boolean isGeometric() {
    return isGeometric;
  }

  /**
   * Indicates whether the correction includes stellar aberration.
   */
  public boolean useStellarAberration() {
    return useStellarAberration;
  }

  /**
   * Indicates whether the correction is for the transmission case (photons traveling from the
   * observer to the target).
   */
  public boolean isTransmission() {
    return isTransmission;
  }

  /**
   * Indicates whether the correction is for the receipt case (photons traveling from the target to
   * the observer).
   */
  public boolean isReceipt() {
    return !isTransmission;
  }

  /**
   * Strips the stellar aberration correction from the supplied correction.
   * 
   * @param correction
   * 
   * @return LT if input was LT_S, XLT if input was XLT_S
   * 
   * @throws IllegalArgumentException if the supplied correction does not include stellar aberration
   *         {@link AberrationCorrection#useStellarAberration()}
   */
  static AberrationCorrection stripStellarAberration(AberrationCorrection correction) {
    checkArgument(correction.useStellarAberration, "Correction must have stellar aberration.");
    switch (correction) {
      case LT_S:
        return LT;
      case XLT_S:
        return XLT;
      default:
        throw new BugException("A correction that has stellar aberration has "
            + "been defined that isn't handled by this method. " + "This is clearly a bug.");
    }

  }
}
