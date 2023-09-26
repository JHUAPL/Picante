/**
 * Filename: FundamentalPhysicalConstants.java Author : vandejd1 Created : Feb 9, 2009
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.units;

import static picante.math.PicanteMath.PI;

/**
 * This class offers access to constants in two ways: a raw form, where you can get the value of the
 * constant through the public final static fields of the class. The units of these values will be
 * whatever they were in the source material from which the value was obtained.
 * 
 * But in striving to present a uniform way of thinking about units within picante, it is
 * beneficial to present things with an agreed upon "strong preference" for units. See the table in
 * the package-info.java for this package for the preferred units within picante.
 * 
 * To support this approach, this class can also be instantiated and the constants obtained through
 * methods which ensure that the values obtained for the constants conform to the standard, strong
 * preference for units use within picante. This instance is available as a singleton, since you
 * really only ever need one of these, and it further emphasizes that you should not sub-class of
 * this class.
 * 
 * So - within picante, you should use the singleton (so that your use of units is "typical").
 * Outside of picante, you can use the constants however you want.
 * 
 * @author vandejd1
 */
public final class FundamentalPhysicalConstants {

  /**
   * units: m/s
   * <p>
   * This is a defined value.
   * <p>
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC = 299792458.0;

  /**
   * units: J/ ( mol * K )
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double MOLAR_GAS_CONST_R = 8.314472;

  /**
   * <b>The accepted value of Avogadro's number (as of 2019) is that of
   * {@link #AVOGADRO_CONSTANT_NA}.</b>
   * <p>
   * units: 1/mol
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double AVAGADROs_NUMBER_NA = 6.02214179e23;

  /**
   * units: 1/mol
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double AVOGADRO_CONSTANT_NA = 6.02214076e23;

  /**
   * units: J K-1
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double BOLTZMANS_CONSTANT_k = 1.3806504e-23;

  /**
   * units: J Hz-1
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double PLANCK_CONSTANT_h = 6.62607015e-34;

  /**
   * Reduced Planck constant: h-bar = h / (2 PI)
   * 
   * units: J s
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double PLANCK_CONSTANT_hbar = PLANCK_CONSTANT_h / (2 * PI);

  /**
   * Planck constant (h) times speed of light in vacuum (c)
   * 
   * units: J m
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double PLANCK_CONSTANT_h_c =
      PLANCK_CONSTANT_h * SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC;

  /**
   * Reduced Planck constant (hbar) times speed of light in vacuum (c)
   * 
   * units: J m
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double PLANCK_CONSTANT_hbar_c =
      PLANCK_CONSTANT_hbar * SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC;

  /**
   * units: C (Coulombs)
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double ELECTRON_CHARGE = 1.602176487e-19;

  /**
   * units: kg
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double ATOMIC_MASS_UNIT_KG = 1.660538782e-27;

  public final static double ELECTRON_MASS_KG = 9.10938291e-31;

  /**
   * units: u (same as AMU)
   * 
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double ELECTRON_MASS_AMU = 5.4857990943e-4;

  public final static double ELECTRON_MASS_ENERGY_EQUIVALENT_MEV = 0.510998928;

  public final static double PROTON_MASS_AMU = 1.007276466812;

  public final static double PROTON_MASS_KG = 1.67262177710e-27;

  public final static double PROTON_MASS_ENERGY_EQUIVALENT_MEV = 938.272046;

  /**
   * units: N*A<sup>2</sup>
   * <p>
   * Also known as the permeability of free space. This is an exact value.
   * <p>
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double MAGNETIC_CONSTANT = 4.0 * PI * 1.0E-7;

  /**
   * units: F*M<sup>-1</sup>
   * <p>
   * Also known as the permittivity of free space. Since C is defined, this is an exact value.
   * <p>
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double ELECTRIC_CONSTANT = 1.0 / (MAGNETIC_CONSTANT
      * SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC * SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC);

  /**
   * units: m<sup>3</sup>*kg<sup>-1</sup>*s<sup>-2</sup>
   * <p>
   * The Newtonian constant of Gravitation (G)
   * <p>
   * copied from NIST: http://physics.nist.gov/cuu/Constants/
   */
  public final static double NEWTONIAN_CONSTANT_OF_GRAVITATION = 6.67384;

  /**
   * units: m
   * <p>
   * The official definition of the AU provided by the IAU.
   * </p>
   * 
   * @see <a href="http://www.iau.org/static/resolutions/IAU2012_English.pdf">http://www.iau.org/
   *      static/resolutions/IAU2012_English.pdf</a>
   */
  public final static double AU_IN_METERS = 149597870700.0;

  /**
   * Return the Julian Date of 2000 JAN 01 12:00:00 (2000 JAN 1.5).
   * 
   * units: days
   */
  public final static double JULIAN_DATE_OF_J2000 = 2451545.0;

  public final static double SECONDS_PER_DAY = 86400.0;

  public final static double TWOPI = 2.0 * PI;

  public final static double HALFPI = PI / 2.0;

  private static FundamentalPhysicalConstants instance = new FundamentalPhysicalConstants();

  public static FundamentalPhysicalConstants getInstance() {
    return instance;
  }

  private FundamentalPhysicalConstants() {}

  public double getAUinKm() {
    return AU_IN_METERS / 1000.0;
  }

  public double getSpeedOfLightInKmPerSec() {
    return SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC / 1000.0;
  }

  public double getElectronMassInU() {
    return ELECTRON_MASS_AMU;
  }

  public double getAMUInKg() {
    return ATOMIC_MASS_UNIT_KG;
  }

  public double getMolarGasConstantR() {
    return MOLAR_GAS_CONST_R;
  }

  public double getAvagadrosNumber() {
    return AVAGADROs_NUMBER_NA;
  }

}
