package picante.mechanics;

/**
 * Enumeration implementing the <code>EphemerisID</code> interface to support ephemeris provider
 * tests.
 */
public enum EphemerisTestCodes implements EphemerisID {
  E_R, E_AA, E_AB, E_AC, E_BA, E_BB, E_BC, E_XA, E_XB, E_UNK;

  @Override
  public String getName() {
    return name();
  }

}
