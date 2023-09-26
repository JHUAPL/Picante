package picante.spice.kernel.tk.fk;

/**
 * Enumeration capturing the different classes of frames supported with in SPICE.
 */
public enum FrameType {

  INERTIAL(1), PCK(2), CK(3), TK(4), DYNAMIC(5);

  private final int typeID;

  private FrameType(int typeID) {
    this.typeID = typeID;
  }

  /**
   * Retrieves the integer class type ID for this type.
   * 
   * @return the integer found in _CLASS field in the kernel pool
   */
  public int getTypeID() {
    return typeID;
  }

  public static FrameType getTypeForClassInteger(int integer) {
    /*
     * This is a horrible implementation, as it relies up on the ordering of the enumeration
     * elements defined above. But, it's quick and dirty, and it works for now.
     */
    return FrameType.values()[integer - 1];
  }
}
