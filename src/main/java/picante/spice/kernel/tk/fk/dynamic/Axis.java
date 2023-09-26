package picante.spice.kernel.tk.fk.dynamic;

import java.util.HashMap;
import java.util.Map;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

public enum Axis {
  I("X", "I", VectorIJK.I, false), MINUS_I("-X", "-I", VectorIJK.MINUS_I, true), J("Y", "J",
      VectorIJK.J, false), MINUS_J("-Y", "-J", VectorIJK.MINUS_J,
          true), K("Z", "K", VectorIJK.K, false), MINUS_K("-Z", "-K", VectorIJK.MINUS_K, true);

  private final String XYZ;
  private final String IJK;
  private final UnwritableVectorIJK vector;
  private final boolean negative;

  Axis(String XYZ, String IJK, UnwritableVectorIJK vector, boolean negative) {
    this.XYZ = XYZ;
    this.IJK = IJK;
    this.vector = vector;
    this.negative = negative;
  }


  public String getXYZ() {
    return XYZ;
  }

  public boolean isNegative() {
    return negative;
  }

  public UnwritableVectorIJK getVector() {
    return vector;
  }

  public String getIJK() {
    return IJK;
  }

  private static Map<String, Axis> stringToAxisMap = new HashMap<>();
  static {
    for (Axis axis : Axis.values()) {
      stringToAxisMap.put(axis.XYZ, axis);
      stringToAxisMap.put(axis.IJK, axis);
    }
  }

  public static Axis fromString(String str) {
    if (stringToAxisMap.containsKey(str)) {
      return stringToAxisMap.get(str);
    } else {
      throw new IllegalArgumentException("Illegal argument. " + str + " not contained.");
    }
  }

  public Axis getOpposite() {
    String axisStr = this.getXYZ();
    if (axisStr.length() == 1) {
      return fromString("-" + axisStr);
    } else {
      return fromString(axisStr.substring(1));
    }
  }

  public static void main(String[] args) {
    for (Axis axis : Axis.values()) {
      System.out.println(axis.getXYZ() + " " + axis.getVector());
    }
  }
}
