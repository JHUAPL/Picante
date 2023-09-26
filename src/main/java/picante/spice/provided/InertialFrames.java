package picante.spice.provided;

import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.StateTransform;

/**
 * Implementation of SPICE supported, built-in inertial frames. This code is derived from the setup
 * code in the SPICE routine CHGIRF. The documentation and code within this routine borrows heavily
 * from that module, with conversion to Java style indexing.
 */
public enum InertialFrames {

  /**
   * The B1950 reference frame is obtained by precessing the J2000 frame backwards from Julian year
   * 2000 to Besselian year 1950, using the 1976 IAU precession model.
   */
  B1950("B1950", 2, null, 1152.84248596724, 2, -1002.26108439117, 1, 1153.04066200330, 2),

  /**
   * The FK4 reference frame is derived from the B1950 frame by applying the equinox offset
   * determined by Fricke. This is just the rotation outlined in the constructor.
   */
  FK4("FK4", 3, B1950, 0.525, 2),

  /**
   * The DE-118 reference frame is nearly identical to FK4. It is also derived from the B1950 frame.
   * Only the offset is different.
   */
  DE118("DE-118", 4, B1950, 0.53155, 2),

  /**
   * The DE-96 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE96("DE-96", 5, B1950, 0.4107, 2),

  /**
   * The DE-102 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE102("DE-102", 6, B1950, 0.1359, 2),

  /**
   * The DE-108 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE108("DE-108", 7, B1950, 0.4775, 2),

  /**
   * The DE-111 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE111("DE-111", 8, B1950, 0.5880, 2),

  /**
   * The DE-114 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE114("DE-114", 9, B1950, 0.5529, 2),

  /**
   * The DE-122 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE122("DE-122", 10, B1950, 0.5316, 2),

  /**
   * The DE-125 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE125("DE-125", 11, B1950, 0.5754, 2),

  /**
   * The DE-130 reference frame is nearly identical to FK4. It is also derived from the B1950 frame,
   * only the offset is different.
   */
  DE130("DE-130", 12, B1950, 0.5247, 2),

  /**
   * The Galactic System II reference frame is defined by the following rotations:
   * 
   * <pre>
   *        o          o            o
   *   [ 327  ]  [ 62.6  ]  [ 282.25  ]
   *           2          0            2
   * </pre>
   * 
   * In the absence of better information, we will assume that it is derived from the FK4 frame.
   */
  GALACTIC("GALACTIC", 13, FK4, 1177200.0, 2, 225360.0, 0, 1016100.0, 2),

  /**
   * The various DE-200 frames are identical with J2000, because the ephemerides are rotated prior
   * to releasing them.
   */
  DE200("DE-200", 14, null),

  /**
   * The various DE-200 frames are identical with J2000, because the ephemerides are rotated prior
   * to releasing them.
   */
  DE202("DE-202", 15, null),

  /**
   * The values for the transformation from J2000 to MARSIAU_MO are derived from the constants given
   * for the pole of Mars.
   */
  MARSIAU("MARSIAU", 16, null, 324000.0, 2, 133610.4, 1, -152348.4, 2),

  /**
   * Defines the ecliptic frame evaluated at J2000.
   */
  ECLIPJ2000("ECLIPJ2000", 17, null, 84381.448, 0),

  /**
   * Defines the ecliptic frame evaluated at B1950.
   */
  ECLIPB1950("ECLIPB1950", 18, B1950, 84404.836, 0),

  /**
   * The frame for DE-140 is simply DE-400 rotated by the rotation:
   * 
   * <pre>
   *  0.9999256765384668  0.0111817701197967  0.0048589521583895
   * -0.0111817701797229  0.9999374816848701 -0.0000271545195858
   * -0.0048589520204830 -0.0000271791849815  0.9999881948535965
   * </pre>
   * 
   * Note that the DE-400 frame is simply J2000.
   */
  DE140("DE-140", 19, null, 1152.71013777252, 2, -1002.25042010533, 1, 1153.75719544491, 2),

  /**
   * The frame for DE-142 is simply DE-402 rotated by the rotation:
   * 
   * <pre>
   *  0.9999256765402605  0.0111817697320531  0.0048589526815484
   * -0.0111817697907755  0.9999374816892126 -0.0000271547693170
   * -0.0048589525464121 -0.0000271789392288  0.9999881948510477
   * </pre>
   * 
   * Note that the DE-402 frame is J2000.
   */
  DE142("DE-142", 20, null, 1152.72061453864, 2, -1002.25052830351, 1, 1153.74663857521, 2),

  /**
   * The frame for DE-143 is simply DE-403 rotated by the rotation:
   * 
   * <pre>
   *  0.9999256765435852  0.0111817743077255  0.0048589414674762
   * -0.0111817743300355  0.9999374816382505 -0.0000271622115251
   * -0.0048589414161348 -0.0000271713942366  0.9999881949053349
   * </pre>
   * 
   * Note that the DE-403 frame is J2000.
   */
  DE143("DE-143", 21, null, 1153.03919093833, 2, -1002.24822382286, 1, 1153.42900222357, 2);

  private final String name;
  private final int fromID;
  private final int toID;
  private final RotationMatrixIJK matrix;
  private final Coverage coverage = Coverage.ALL_TIME;

  /**
   * Constructs a built-in inertial frame transform.
   * 
   * @param name a string indicating the NAIF name for the frame
   * 
   * @param fromID an integer indicating the SPICE frame ID code for the defined frame
   * @param relativeTo another element of the enumeration if the frame is defined relative to
   *        something other than J2000, or null to indicate it is directly tied to J2000
   * @param values an alternating sequence of angles in arcseconds and axes {0,1,2} specifications
   *        indicating the matrix order of multiplication to build the desired frame
   */
  InertialFrames(String name, int fromID, InertialFrames relativeTo, double... values) {
    this.name = name;
    this.fromID = fromID;
    this.toID = 1;
    this.matrix = createMatrix(relativeTo, values);
  }

  private RotationMatrixIJK createMatrix(InertialFrames relativeTo, double... values) {
    /*
     * Start by constructing the matrix that moves vectors from relativeTo to the defined frame.
     */
    RotationMatrixIJK matrix = new RotationMatrixIJK(RotationMatrixIJK.IDENTITY);

    for (int i = values.length - 1; i >= 0; i -= 2) {
      rotate(matrix, values[i - 1], (int) values[i]);
    }

    /*
     * If there is a basic frame that this frame is relative to, then apply its conversion back to
     * J2000. Otherwise, assume that it is directly relative to J2000 inherently.
     */
    if (relativeTo != null) {
      RotationMatrixIJK.mxmt(matrix, relativeTo.matrix, matrix);
    }

    /*
     * Transpose the matrix, because SPICE captures the matrix that takes vectors from J2000 to the
     * defined frame. As the frame transform function interface takes vectors from the defined frame
     * to J2000, we need to revert this.
     */
    return matrix.transpose();
  }

  /**
   * Rotates the supplied matrix about an axis by an angle.
   * 
   * @param matrix the matrix to rotate
   * @param angle the rotation angle, in arcseconds
   * @param axis an integer index indicating which of the principal axes to rotate about {0=i, 1=j,
   *        2=k}
   */
  private void rotate(RotationMatrixIJK matrix, double angle, int axis) {

    final int[] indices = {0, 1, 2, 0, 1};
    final double arcsecondsToDegrees = 1.0 / 3600.0;

    /*
     * Convert the angle to radians.
     */
    angle = Math.toRadians(angle * arcsecondsToDegrees);

    double[][] buffer = new double[3][3];

    double s = Math.sin(angle);
    double c = Math.cos(angle);

    /*
     * Compute a positive modulo with 3.
     */
    int temp = ((axis % 3) + 3) % 3;

    int i1 = indices[temp];
    int i2 = indices[temp + 1];
    int i3 = indices[temp + 2];

    for (int i = 0; i < 3; i++) {
      buffer[i1][i] = matrix.get(i1, i);
      buffer[i2][i] = c * matrix.get(i2, i) + s * matrix.get(i3, i);
      buffer[i3][i] = -s * matrix.get(i2, i) + c * matrix.get(i3, i);
    }

    /*
     * Copy the resultant rotated matrix back into matrix.
     */
    matrix.setTo(buffer);

  }

  public StateTransform getStateTransform(@SuppressWarnings("unused") double time,
      StateTransform buffer) {
    buffer.setRotation(matrix);
    buffer.setRotationDerivative(MatrixIJK.ZEROS);
    return buffer;
  }

  public Coverage getCoverage() {
    return coverage;
  }

  public int getFromID() {
    return fromID;
  }

  public int getToID() {
    return toID;
  }

  public String getFrameName() {
    return name;
  }

  public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
      RotationMatrixIJK buffer) {
    return buffer.setTo(matrix);
  }

}
