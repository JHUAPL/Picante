package picante.mechanics;

/**
 * Enumeration implementing the <code>FrameID</code> interface to support frame provider tests.
 */
public enum FrameTestCodes implements FrameID {
  R, AA, AB, AC, BA, BB, BC, XA, XB, UNK;

  @Override
  public String getName() {
    return name();
  }

  @Override
  public boolean isInertial() {
    return false;
  }

}
