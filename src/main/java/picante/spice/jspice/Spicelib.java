package picante.spice.jspice;

import static picante.spice.jspice.Parameter.M2EUL_DTOL;
import static picante.spice.jspice.Parameter.M2EUL_NTOL;
import static picante.spice.jspice.Parameter.SPICELIB_JYEAR;
import static picante.spice.jspice.Parameter.SPICELIB_PI;
import static picante.spice.jspice.Parameter.SPICELIB_RPD;
import static picante.spice.jspice.Parameter.SPICELIB_SPD;
import static picante.spice.jspice.Parameter.SPICELIB_TWOPI;
import static picante.spice.jspice.Parameter.XF2EUL_ALPHA;
import static picante.spice.jspice.Parameter.XF2EUL_BETA;
import static picante.spice.jspice.Parameter.XF2EUL_DALPHA;
import static picante.spice.jspice.Parameter.XF2EUL_DBETA;
import static picante.spice.jspice.Parameter.XF2EUL_DELTA;
import static picante.spice.jspice.Parameter.XF2EUL_DGAMMA;
import static picante.spice.jspice.Parameter.XF2EUL_GAMMA;
import static picante.spice.jspice.Parameter.XF2EUL_NEXT;
import static picante.spice.jspice.Parameter.ZZEPRC76_THETA1;
import static picante.spice.jspice.Parameter.ZZEPRC76_THETA2;
import static picante.spice.jspice.Parameter.ZZEPRC76_THETA3;
import static picante.spice.jspice.Parameter.ZZEPRC76_Z1;
import static picante.spice.jspice.Parameter.ZZEPRC76_Z2;
import static picante.spice.jspice.Parameter.ZZEPRC76_Z3;
import static picante.spice.jspice.Parameter.ZZEPRC76_ZETA1;
import static picante.spice.jspice.Parameter.ZZEPRC76_ZETA2;
import static picante.spice.jspice.Parameter.ZZEPRC76_ZETA3;
import static picante.spice.jspice.Parameter.ZZMOBLIQ_C0;
import static picante.spice.jspice.Parameter.ZZMOBLIQ_C1;
import static picante.spice.jspice.Parameter.ZZMOBLIQ_C2;
import static picante.spice.jspice.Parameter.ZZMOBLIQ_C3;
import static picante.spice.jspice.Parameter.ZZWAHR_D0;
import static picante.spice.jspice.Parameter.ZZWAHR_D1;
import static picante.spice.jspice.Parameter.ZZWAHR_D2;
import static picante.spice.jspice.Parameter.ZZWAHR_D3;
import static picante.spice.jspice.Parameter.ZZWAHR_F0;
import static picante.spice.jspice.Parameter.ZZWAHR_F1;
import static picante.spice.jspice.Parameter.ZZWAHR_F2;
import static picante.spice.jspice.Parameter.ZZWAHR_F3;
import static picante.spice.jspice.Parameter.ZZWAHR_L0;
import static picante.spice.jspice.Parameter.ZZWAHR_L1;
import static picante.spice.jspice.Parameter.ZZWAHR_L2;
import static picante.spice.jspice.Parameter.ZZWAHR_L3;
import static picante.spice.jspice.Parameter.ZZWAHR_LP0;
import static picante.spice.jspice.Parameter.ZZWAHR_LP1;
import static picante.spice.jspice.Parameter.ZZWAHR_LP2;
import static picante.spice.jspice.Parameter.ZZWAHR_LP3;
import static picante.spice.jspice.Parameter.ZZWAHR_MATRIX;
import static picante.spice.jspice.Parameter.ZZWAHR_MG0;
import static picante.spice.jspice.Parameter.ZZWAHR_MG1;
import static picante.spice.jspice.Parameter.ZZWAHR_MG2;
import static picante.spice.jspice.Parameter.ZZWAHR_MG3;
import static picante.spice.jspice.Parameter.ZZWAHR_NTERM;
import java.util.Arrays;

import com.google.common.base.Preconditions;
import com.google.common.primitives.ImmutableDoubleArray;

/**
 * <pre>
 *$ Disclaimer
 *
 *     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
 *     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
 *     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
 *     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
 *     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
 *     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
 *     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
 *     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
 *     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
 *     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
 *
 *     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
 *     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
 *     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 *     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
 *     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
 *     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
 *
 *     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
 *     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
 *     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
 *     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
 * </pre>
 */
public final class Spicelib {

  /**
   * ############################################################################# PUBLIC METHODS
   * ##############################################################################
   */

  /**
   * <pre>
   *$Procedure BRCKTD (Bracket a double precision value within an interval)
   *
   *      DOUBLE PRECISION FUNCTION BRCKTD ( NUMBER, END1, END2 )
   *
   *$ Abstract
   *
   *      Bracket a number. That is, given a number and an acceptable
   *      interval, make sure that the number is contained in the
   *      interval. (If the number is already in the interval, leave it
   *      alone. If not, set it to the nearest endpoint of the interval.)
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *      INTERVALS,  NUMBERS,  UTILITY
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION   NUMBER
   *      DOUBLE PRECISION   END1
   *      DOUBLE PRECISION   END2
   *
   *$ Brief_I/O
   *
   *      VARIABLE  I/O  DESCRIPTION
   *      --------  ---  --------------------------------------------------
   *      NUMBER     I   Number to be bracketed.
   *      END1       I   One of the bracketing endpoints for NUMBER.
   *      END2       I   The other bracketing endpoint for NUMBER.
   *      BRCKTD     O   Bracketed number.
   *
   *$ Detailed_Input
   *
   *      NUMBER      is the number to be bracketed. That is, the
   *                  value of NUMBER is constrained to lie in the
   *                  interval bounded by END1 and END2.
   *
   *      END1,
   *      END2        are the lower and upper bounds for NUMBER. The
   *                  order is not important.
   *
   *$ Detailed_Output
   *
   *      BRCKTD      is NUMBER, if it was already in the interval
   *                  provided. Otherwise it is the value of the nearest
   *                  bound of the interval.
   *
   *$ Parameters
   *
   *      None.
   *
   *$ Particulars
   *
   *      This routine provides a shorthand notation for code fragments
   *      like the following
   *
   *            IF      ( NUMBER .LT. END1 ) THEN
   *                                              NUMBER = END1
   *            ELSE IF ( NUMBER .GT. END2 ) THEN
   *                                              NUMBER = END2
   *            END IF
   *
   *      which occur frequently during the processing of program inputs.
   *
   *$ Examples
   *
   *      The following illustrate the operation of BRCKTD.
   *
   *            BRCKTD (  -1.D0,   1.D0,  10.D0 )  =  1.D0
   *            BRCKTD (  29.D0,   1.D0,  10.D0 )  = 10.D0
   *            BRCKTD (   3.D0, -10.D0,  10.D0 )  =  3.D0
   *            BRCKTD (   3.D0, -10.D0,  -1.D0 )  = -1.D0
   *
   *      The following code fragment illustrates a typical use for BRCKTD.
   *
   *            C
   *            C     Star magnitude limit must be in the range 0-10.
   *            C
   *                  READ (5,*) MAGLIM
   *                  MAGLIM = BRCKTD ( MAGLIM, 0.D0, 10.D0 )
   *
   *$ Restrictions
   *
   *      None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *      None.
   *
   *$ Author_and_Institution
   *
   *      W.L. Taber      (JPL)
   *      I.M. Underwood  (JPL)
   *
   *$ Literature_References
   *
   *      None.
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     bracket a d.p. value within an interval
   *
   *-&
   *
   *
   *$ Revisions
   *
   *-     Beta Version 1.1.0, 30-DEC-1988 (WLT)
   *
   *      The routine was modified so that the order of the endpoints
   *      of the bracketing interval is not needed.  The routine now
   *      determines which is the left endpoint and which is the
   *      right and acts appropriately.
   *
   *-&
   * </pre>
   */
  public static final double BRCKTD(final double NUMBER, final double END1, final double END2) {
    double BRCKTD;
    if (END1 < END2) {
      BRCKTD = Math.max(END1, Math.min(END2, NUMBER));
    } else {
      BRCKTD = Math.max(END2, Math.min(END1, NUMBER));
    }
    return BRCKTD;
  }

  /**
   * <pre>
   *$Procedure  DET  ( Determinant of a double precision 3x3 matrix )
   *
   *      DOUBLE PRECISION FUNCTION  DET ( M1 )
   *
   *$ Abstract
   *
   *      Compute the determinant of a double precision 3x3 matrix.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *      MATRIX,  MATH
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION    M1 ( 3,3 )
   *
   *$ Brief_I/O
   *
   *      VARIABLE  I/O  DESCRIPTION
   *      --------  ---  --------------------------------------------------
   *       M1        I     Matrix whose determinant is to be found.
   *
   *$ Detailed_Input
   *
   *      M1      This variable may be any double precision, 3x3 matrix.
   *
   *$ Detailed_Output
   *
   *      DET   This is the value of the determinant found by direct
   *            application of the definition of the determinant.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Particulars
   *
   *      DET calculates the determinant of M1 in a single arithmetic
   *      expression which is, effectively, the expansion of M1 about its
   *      first row.  Since the calculation of the determinant involves
   *      the multiplication of numbers whose magnitudes are unrestricted,
   *      there is the possibility of floating point overflow or underflow.
   *      NO error checking or recovery is implemented in this routine.
   *
   *$ Examples
   *
   *           | 1  2  3 |
   *      M1 = | 4  5  6 |   ---->   DET(M1) = 0
   *           | 7  8  9 |
   *
   *           | 1  2  3 |
   *      M1 = | 0  5  6 |   ---->   DET(M1) = 45
   *           | 0  0  9 |
   *
   *$ Restrictions
   *
   *      No checking is implemented to determine whether M1 will cause
   *      overflow or underflow in the process of calculating the
   *      determinant.  In most cases, this will not pose a problem.
   *      The user is required to determine if M1 is suitable matrix
   *      for DET to operate on.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *      None
   *
   *$ Author_and_Institution
   *
   *      W.M. Owen       (JPL)
   *
   *$ Literature_References
   *
   *      None
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     determinant of a d.p. 3x3_matrix
   *
   *-&
   * </pre>
   */
  public static final double DET(final ImmutableDoubleArray M1) {
    return _DET(M1.toArray());
  }

  /**
   * <pre>
   *$Procedure      EUL2M ( Euler angles to matrix )
   *
   *      SUBROUTINE EUL2M (  ANGLE3,   ANGLE2,   ANGLE1,
   *     .                    AXIS3,    AXIS2,    AXIS1,   R  )
   *
   *$ Abstract
   *
   *     Construct a rotation matrix from a set of Euler angles.
   *
   *$ Required_Reading
   *
   *     ROTATION
   *
   *$ Keywords
   *
   *     MATRIX
   *     ROTATION
   *     TRANSFORMATION
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION      ANGLE3
   *      DOUBLE PRECISION      ANGLE2
   *      DOUBLE PRECISION      ANGLE1
   *
   *      INTEGER               AXIS3
   *      INTEGER               AXIS2
   *      INTEGER               AXIS1
   *
   *      DOUBLE PRECISION      R ( 3, 3 )
   *
   *$ Brief_I/O
   *
   *     Variable  I/O  Description
   *     --------  ---  --------------------------------------------------
   *     ANGLE3,
   *     ANGLE2,
   *     ANGLE1     I   Rotation angles about third, second, and first
   *                    rotation axes (radians).
   *     AXIS3,
   *     AXIS2,
   *     AXIS1      I   Axis numbers of third, second, and first rotation
   *                    axes.
   *
   *     R          O   Product of the 3 rotations.
   *
   *$ Detailed_Input
   *
   *     ANGLE3,
   *     ANGLE2,
   *     ANGLE1,
   *
   *     AXIS3,
   *     AXIS2,
   *     AXIS1          are, respectively, a set of three angles and three
   *                    coordinate axis numbers; each pair ANGLEx and
   *                    AXISx specifies a coordinate transformation
   *                    consisting of a rotation by ANGLEx radians about
   *                    the coordinate axis indexed by AXISx.  These
   *                    coordinate transformations are typically
   *                    symbolized by
   *
   *                       [ ANGLEx ]     .
   *                                 AXISx
   *
   *                    See the $ Particulars section below for details
   *                    concerning this notation.
   *
   *                    Note that these coordinate transformations rotate
   *                    vectors by
   *
   *                       -ANGLEx
   *
   *                    radians about the axis indexed by AXISx.
   *
   *                    The values of AXISx may be 1, 2, or 3, indicating
   *                    the x, y, and z axes respectively.
   *
   *$ Detailed_Output
   *
   *     R              is a rotation matrix representing the composition
   *                    of the rotations defined by the input angle-axis
   *                    pairs.  Together, the three pairs specify a
   *                    composite transformation that is the result of
   *                    performing the rotations about the axes indexed
   *                    by AXIS1, AXIS2, and AXIS3, in that order.  So,
   *
   *                       R = [ ANGLE3 ]    [ ANGLE2 ]      [ ANGLE1 ]
   *                                    AXIS3          AXIS2          AXIS1
   *
   *                    See the $ Particulars section below for details
   *                    concerning this notation.
   *
   *                    The resulting matrix R may be thought of as a
   *                    coordinate transformation; applying it to a vector
   *                    yields the vector's coordinates in the rotated
   *                    system.
   *
   *                    Viewing R as a coordinate transformation matrix,
   *                    the basis that R transforms vectors to is created
   *                    by rotating the original coordinate axes first by
   *                    ANGLE1 radians about the coordinate axis indexed
   *                    by AXIS1, next by ANGLE2 radians about the
   *                    coordinate axis indexed by AXIS2, and finally by
   *                    ANGLE3 radians about coordinate axis indexed by
   *                    AXIS3.  At the second and third steps of this
   *                    process, the coordinate axes about which rotations
   *                    are performed belong to the bases resulting from
   *                    the previous rotations.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     1)   If any of AXIS3, AXIS2, or AXIS1 do not have values in
   *
   *             { 1, 2, 3 },
   *
   *          the error SPICE(BADAXISNUMBERS) is signalled.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     A word about notation:  the symbol
   *
   *        [ x ]
   *             i
   *
   *     indicates a rotation of x radians about the ith coordinate axis.
   *     To be specific, the symbol
   *
   *        [ x ]
   *             1
   *
   *     indicates a coordinate system rotation of x radians about the
   *     first, or x-, axis; the corresponding matrix is
   *
   *        +-                    -+
   *        |  1      0       0    |
   *        |                      |
   *        |  0    cos(x)  sin(x) |.
   *        |                      |
   *        |  0   -sin(x)  cos(x) |
   *        +-                    -+
   *
   *     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
   *     matrix, when applied to a vector, rotates the vector by -x
   *     radians, not x radians.  Applying the matrix to a vector yields
   *     the vector's representation relative to the rotated coordinate
   *     system.
   *
   *     The analogous rotation about the second, or y-, axis is
   *     represented by
   *
   *        [ x ]
   *             2
   *
   *     which symbolizes the matrix
   *
   *        +-                    -+
   *        | cos(x)   0   -sin(x) |
   *        |                      |
   *        |  0       1      0    |,
   *        |                      |
   *        | sin(x)   0    cos(x) |
   *        +-                    -+
   *
   *     and the analogous rotation about the third, or z-, axis is
   *     represented by
   *
   *        [ x ]
   *             3
   *
   *     which symbolizes the matrix
   *
   *        +-                    -+
   *        |  cos(x)  sin(x)   0  |
   *        |                      |
   *        | -sin(x)  cos(x)   0  |.
   *        |                      |
   *        |  0        0       1  |
   *        +-                    -+
   *
   *     From time to time, (depending on one's line of work, perhaps) one
   *     may happen upon a pair of coordinate systems related by a
   *     sequence of rotations.  For example, the coordinate system
   *     defined by an instrument such as a camera is sometime specified
   *     by RA, DEC, and twist; if alpha, delta, and phi are the rotation
   *     angles, then the series of rotations
   *
   *        [ phi ]     [ pi/2  -  delta ]     [ alpha ]
   *               3                      2             3
   *
   *     produces a transformation from inertial to camera coordinates.
   *
   *     This routine is related to the SPICELIB routine M2EUL, which
   *     produces a sequence of Euler angles, given a rotation matrix.
   *     This routine is a `left inverse' of M2EUL:  the sequence of
   *     calls
   *
   *        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
   *       .                 ANGLE3,  ANGLE2,  ANGLE1     )
   *
   *        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
   *       .                 AXIS3,   AXIS2,   AXIS1,   R )
   *
   *     preserves R, except for round-off error.
   *
   *
   *     On the other hand, the sequence of calls
   *
   *        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
   *       .                 AXIS3,   AXIS2,   AXIS1,   R )
   *
   *        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
   *       .                 ANGLE3,  ANGLE2,  ANGLE1     )
   *
   *     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
   *     out in the ranges that M2EUL's outputs are restricted to.
   *
   *$ Examples
   *
   *     1)  Create a coordinate transformation matrix by rotating
   *         the original coordinate axes first by 30 degrees about
   *         the z axis, next by 60 degrees about the y axis resulting
   *         from the first rotation, and finally by -50 degrees about
   *         the z axis resulting from the first two rotations.
   *
   *
   *            C
   *            C     Create the coordinate transformation matrix
   *            C
   *            C                   o          o          o
   *            C        R  =  [ -50  ]   [  60  ]   [  30  ]
   *            C                      3          2          3
   *            C
   *            C     All angles in radians, please.   The SPICELIB
   *            C     function RPD (radians per degree) gives the
   *            C     conversion factor.
   *            C
   *            C     The z axis is `axis 3'; the y axis is `axis 2'.
   *            C
   *                  ANGLE1 = RPD() *  30.D0
   *                  ANGLE2 = RPD() *  60.D0
   *                  ANGLE3 = RPD() * -50.D0
   *
   *                  AXIS1  = 3
   *                  AXIS2  = 2
   *                  AXIS3  = 3
   *
   *                  CALL EUL2M (  ANGLE3, ANGLE2, ANGLE1,
   *                 .              AXIS3,  AXIS2,  AXIS1,   R  )
   *
   *
   *     2)  A trivial example using actual numbers.
   *
   *         The code fragment
   *
   *            CALL EUL2M (  0.D0,     0.D0,     HALFPI(),
   *           .                 1,        1,            3,      R  )
   *
   *         sets R equal to the matrix
   *
   *            +-                  -+
   *            |  0      1       0  |
   *            |                    |
   *            | -1      0       0  |.
   *            |                    |
   *            |  0      0       1  |
   *            +-                  -+
   *
   *
   *     3)  Finding the rotation matrix specified by a set of `clock,
   *         cone, and twist' angles, as defined on the Voyager 2 project:
   *
   *            Voyager 2 narrow angle camera pointing, relative to the
   *            Sun-Canopus coordinate system, was frequently specified
   *            by a set of Euler angles called `clock, cone, and twist'.
   *            These defined a 3-2-3 coordinate transformation matrix
   *            TSCTV as the product
   *
   *               [ twist ]  [ cone ]   [ clock ] .
   *                        3         2           3
   *
   *            Given the angles CLOCK, CONE, and TWIST (in units of
   *            radians), we can compute TSCTV with the code fragment
   *
   *               CALL EUL2M (  TWIST,  CONE,  CLOCK,
   *              .              3,      2,     3,      TSCTV  )
   *
   *
   *     4)  Finding the rotation matrix specified by a set of `right
   *         ascension, declination, and twist' angles, as defined on the
   *         Galileo project:
   *
   *            Galileo scan platform pointing, relative to an inertial
   *            reference frame, (EME50 variety) is frequently specified
   *            by a set of Euler angles called `right ascension (RA),
   *            declination (Dec), and twist'. These define a 3-2-3
   *            coordinate transformation matrix TISP as the product
   *
   *               [ Twist ]  [ pi/2 - Dec ]   [ RA ] .
   *                        3               2        3
   *
   *            Given the angles RA, DEC, and TWIST (in units of radians),
   *            we can compute TISP with the code fragment
   *
   *               CALL EUL2M (  TWIST,   HALFPI()-DEC,   RA,
   *              .              3,       2,              3,   TISP  )
   *
   *
   *$ Restrictions
   *
   *     Beware:  more than one definition of "RA, DEC and twist" exists.
   *
   *$ Literature_References
   *
   *     [1]  `Galileo Attitude and Camera Models', JPL IOM 314-323,
   *           W. M. Owen, Jr.,  Nov. 11, 1983.  NAIF document number
   *           204.0.
   *
   *$ Author_and_Institution
   *
   *     N.J. Bachman   (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.2.1, 26-DEC-2006 (NJB)
   *
   *        Corrected header typo.
   *
   *-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
   *
   *        Updated to remove non-standard use of duplicate arguments
   *        in ROTMAT calls.
   *
   *-    SPICELIB Version 1.1.2, 14-OCT-2004 (LSE)
   *
   *        Corrected a typo in the header.
   *
   *-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
   *
   *        Names of input arguments changed to reflect the order in
   *        which the rotations are applied when their product is
   *        computed.  The header was upgraded to describe notation in
   *        more detail.  Examples were added.
   *
   *-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
   *
   *-&
   *$ Index_Entries
   *
   *     euler angles to matrix
   *
   *-&
   *$ Revisions
   *
   *-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
   *
   *        Updated to remove non-standard use of duplicate arguments
   *        in ROTMAT calls.
   *
   *-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
   *
   *        Argument names were changed to describe the use of the
   *        arguments more accurately.  The axis and angle numbers
   *        now decrease, rather than increase, from left to right.
   *        The current names reflect the order of operator application
   *        when the Euler angle rotations are applied to a vector:  the
   *        rightmost matrix
   *
   *           [ ANGLE1 ]
   *                     AXIS1
   *
   *        is applied to the vector first, followed by
   *
   *           [ ANGLE2 ]
   *                     AXIS2
   *
   *        and then
   *
   *           [ ANGLE3 ]
   *                     AXIS3
   *
   *        Previously, the names reflected the order in which the Euler
   *        angle matrices appear on the page, from left to right.  This
   *        naming convention was found to be a bit obtuse by a various
   *        users.
   *
   *        No change in functionality was made; the operation of the
   *        routine is identical to that of the previous version.
   *
   *        Two new examples were added to assist users in verifying
   *        their understanding of the routine.
   *
   *        Also, the header was upgraded to describe the notation in more
   *        detail.  The symbol
   *
   *           [ x ]
   *                i
   *
   *        is explained at mind-numbing length.  An example was added
   *        that shows a specific set of inputs and the resulting output
   *        matrix.
   *-&
   *
   *     SPICELIB functions
   *
   *      LOGICAL               RETURN
   *
   *
   * </pre>
   */
  public static final ImmutableDoubleArray EUL2M(final double ANGLE3, final double ANGLE2,
      final double ANGLE1, final int AXIS3, final int AXIS2, final int AXIS1) {
    return ImmutableDoubleArray.copyOf(_EUL2M(ANGLE3, ANGLE2, ANGLE1, AXIS3, AXIS2, AXIS1));
  }

  /**
   * <pre>
   *$Procedure      EUL2XF ( Euler angles and derivative to transformation)
   *
   *     ENTRY      EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM )
   *
   *$ Abstract
   *
   *     This routine computes a state transformation from an Euler angle
   *     factorization of a rotation and the derivatives of those Euler
   *     angles.
   *
   *$ Required_Reading
   *
   *     ROTATION
   *
   *$ Keywords
   *
   *     ANGLES
   *     STATE
   *     DERIVATIVES
   *
   *$ Declarations
   *
   *     DOUBLE PRECISION      EULANG ( 6 )
   *     INTEGER               AXISA
   *     INTEGER               AXISB
   *     INTEGER               AXISC
   *     DOUBLE PRECISION      XFORM  ( 6, 6 )
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     EULANG     I   An array of Euler angles and their derivatives.
   *     AXISA      I   Axis A of the Euler angle factorization.
   *     AXISB      I   Axis B of the Euler angle factorization.
   *     AXISC      I   Axis C of the Euler angle factorization.
   *     XFORM      O   A state transformation matrix.
   *
   *$ Detailed_Input
   *
   *
   *     EULANG      is the set of Euler angles corresponding to the
   *                 specified factorization.
   *
   *                 If we represent R as shown here:
   *
   *                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
   *                                   AXISA        AXISB         AXISC
   *
   *                 then
   *
   *
   *                    EULANG(1) = ALPHA
   *                    EULANG(2) = BETA
   *                    EULANG(3) = GAMMA
   *                    EULANG(4) = dALPHA/dt
   *                    EULANG(5) = dBETA/dt
   *                    EULANG(6) = dGAMMA/dt
   *
   *
   *     AXISA       are the axes desired for the factorization of R.
   *     AXISB       All must be in the range from 1 to 3.  Moreover
   *     AXIS*       it must be the case that AXISA and AXISB are distinct
   *                 and that AXISB and AXISC are distinct.
   *
   *                 Every rotation matrix can be represented as a product
   *                 of three rotation matrices about the principal axes
   *                 of a reference frame.
   *
   *                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
   *                                   AXISA        AXISB         AXISC
   *
   *                 The value 1 corresponds to the X axis.
   *                 The value 2 corresponds to the Y axis.
   *                 The value 3 corresponds to the Z axis.
   *
   *$ Detailed_Output
   *
   *     XFORM       is the state transformation corresponding R and dR/dt
   *                 as described above.  Pictorially,
   *
   *                      [       |        ]
   *                      |  R    |    0   |
   *                      |       |        |
   *                      |-------+--------|
   *                      |       |        |
   *                      | dR/dt |    R   |
   *                      [       |        ]
   *
   *                 where R is a rotation that varies with respect to time
   *                 and dR/dt is its time derivative.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     All erroneous inputs are diagnosed by routines in the call
   *     tree to this routine.  These include
   *
   *     1)   If any of AXISA, AXISB, or AXISC do not have values in
   *
   *             { 1, 2, 3 },
   *
   *          then the error SPICE(INPUTOUTOFRANGE) is signaled.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     This entry point is intended to provide an inverse for the
   *     entry point XF2EUL.  See that entry point for a discussion
   *     of notation.
   *
   *$ Examples
   *
   *     Suppose you have a set of Euler angles and their derivatives
   *     for a 3 1 3 rotation, and that you would like to determine
   *     the equivalent angles and derivatives for a 1 2 3 rotation.
   *
   *         R = [ALPHA]  [BETA]  [GAMMA]
   *                    3       1        3
   *
   *         R = [ROLL]  [PITCH]  [YAW]
   *                   1        2      3
   *
   *     The following pair of subroutine calls will perform the
   *     desired computation.
   *
   *        ABGANG(1) = ALPHA
   *        ABGANG(2) = BETA
   *        ABGANG(3) = GAMMA
   *        ABGANG(4) = DALPHA
   *        ABGANG(5) = DBETA
   *        ABGANG(6) = DGAMMA
   *
   *        CALL EUL2XF ( ABGANG, 3, 1, 3, XFORM  )
   *        CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
   *
   *        ROLL     = RPYANG(1)
   *        PITCH    = RPYANG(2)
   *        YAW      = RPYANG(3)
   *        DROLL    = RPYANG(4)
   *        DPITCH   = RPYANG(5)
   *        DYAW     = RPYANG(6)
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
   *
   *      Corrected code in Examples section, example showed
   *      a XF2EUL call:
   *
   *            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
   *
   *      The proper form of the call:
   *
   *            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
   *
   *-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
   *
   *        Restriction that second axis must differ from both the first
   *        and third axes was removed.
   *
   *-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     State transformation from Euler angles and derivatives
   *
   *-&
   * </pre>
   */
  public static final ImmutableDoubleArray EUL2XF(final ImmutableDoubleArray EULANG,
      final int AXISA, final int AXISB, final int AXISC) {
    return ImmutableDoubleArray.copyOf(_EUL2XF(EULANG.toArray(), AXISA, AXISB, AXISC));
  }

  /**
   * <pre>
   *$Procedure     ISROT ( Indicate whether a matrix is a rotation matrix )
   *
   *      LOGICAL FUNCTION ISROT ( M, NTOL, DTOL )
   *
   *$ Abstract
   *
   *     Indicate whether a 3x3 matrix is a rotation matrix.
   *
   *$ Required_Reading
   *
   *     ROTATION
   *
   *$ Keywords
   *
   *     ERROR
   *     MATRIX
   *     ROTATION
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION      M    ( 3, 3 )
   *      DOUBLE PRECISION      NTOL
   *      DOUBLE PRECISION      DTOL
   *
   *$ Brief_I/O
   *
   *     Variable  I/O  Description
   *     --------  ---  --------------------------------------------------
   *     M          I   A matrix to be tested.
   *     NTOL       I   Tolerance for the norms of the columns of M.
   *     DTOL       I   Tolerance for the determinant of a matrix whose
   *                    columns are the unitized columns of M.
   *
   *     The function returns the value .TRUE. if and only if M is
   *     a rotation matrix.
   *
   *$ Detailed_Input
   *
   *     M              is a 3x3 matrix to be tested.
   *
   *     NTOL           is the tolerance for the norms of the columns
   *                    of M.
   *
   *     DTOL           is the tolerance for the determinant of a matrix
   *                    whose columns are the unitized columns of M.
   *
   *$ Detailed_Output
   *
   *     The function returns the value .TRUE. if and only if M is found
   *     to be a rotation matrix.  The criteria that M must meet are:
   *
   *
   *        1) The norm of each column of M must satisfy the relation
   *
   *              1.D0 - NTOL  <   || column ||   <  1.D0 + NTOL.
   *                           -                  -
   *
   *        2) The determinant of the matrix whose columns are the
   *           unitized columns of M must satisfy
   *
   *              1.D0 - DTOL  <   determinant   <  1.D0 + DTOL.
   *                           -                 -
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     1)  If either of NTOL or DTOL is negative, the error
   *         SPICE(VALUEOUTOFRANGE) is signalled.  ISROT returns the
   *         value .FALSE. in this case.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     This routine is an error checking `filter'; its purpose is to
   *     detect gross errors, such as uninitialized matrices.  Matrices
   *     that do not pass the tests used by this routine hardly qualify as
   *     rotation matrices.  The test criteria can be adjusted by varying
   *     the parameters NTOL and DTOL.
   *
   *     A property of rotation matrices is that their columns form a
   *     right-handed, orthonormal basis in 3-dimensional space.  The
   *     converse is true:  all 3x3 matrices with this property are
   *     rotation matrices.
   *
   *     An ordered set of three vectors V1, V2, V3 forms a right-handed,
   *     orthonormal basis if and only if
   *
   *        1)   || V1 ||  =  || V2 ||  =  || V3 ||  =  1
   *
   *        2)   V3 = V1 x V2.  Since V1, V2, and V3 are unit vectors,
   *             we also have
   *
   *             < V3, V1 x V2 > = 1.
   *
   *             This quantity is the determinant of the matrix whose
   *             colums are V1, V2 and V3.
   *
   *     When finite precision numbers are used, rotation matrices will
   *     usually fail to satisfy these criteria exactly.  We must use
   *     criteria that indicate approximate conformance to the criteria
   *     listed above.  We choose
   *
   *        1)   |   || Vi ||  -  1   |   <   NTOL,  i = 1, 2, 3.
   *                                      -
   *
   *        2)   Let
   *
   *                       Vi
   *                Ui = ------ ,   i = 1, 2, 3.
   *                     ||Vi||
   *
   *             Then we require
   *
   *                | < U3, U1 x U2 > - 1 |  <  DTOL;
   *                                         -
   *
   *             equivalently, letting U be the matrix whose columns
   *             are U1, U2, and U3, we insist on
   *
   *                | det(U) - 1 |  <  DTOL.
   *                                _
   *$ Examples
   *
   *     1)  We have obtained an instrument pointing matrix C from a
   *         C-kernel, and we wish to test whether it is in fact a
   *         rotation matrix.  We can use ISROT to check this:
   *
   *            C
   *            C    Obtain pointing matrix:
   *            C
   *                 CALL CKGP ( INST, TIMEIN, TOL, REF, C, TIMOUT, FOUND )
   *
   *            C
   *            C    Verify that C is a rotation:
   *            C
   *                 IF ( .NOT. ISROT ( C )  ) THEN
   *
   *                    [ perform exception handling ]
   *
   *                 ELSE
   *
   *                    [ code for the normal case goes here ]
   *
   *                 END IF
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     N.J. Bachman   (JPL)
   *     H.A. Neilan    (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
   *
   *       If the value of the function RETURN is TRUE upon execution of
   *       this module, this function is assigned a default value of
   *       either 0, 0.0D0, .FALSE., or blank depending on the type of the
   *       function.
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 06-SEP-1990  (NJB)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     indicate whether a matrix is a rotation matrix
   *
   *-&
   *
   *
   *     SPICELIB functions
   *
   *      DOUBLE PRECISION      BRCKTD
   *      DOUBLE PRECISION      DET
   *
   *      LOGICAL               RETURN
   *
   * </pre>
   */
  public static final boolean ISROT(final ImmutableDoubleArray M, final double NTOL,
      final double DTOL) {
    return _ISROT(M.toArray(), NTOL, DTOL);
  }

  /**
   * <pre>
   *$Procedure      JYEAR ( Seconds per julian year )
   *
   *     DOUBLE PRECISION FUNCTION JYEAR ()
   *
   *$ Abstract
   *
   *     Return the number of seconds in a julian year.
   *
   *$ Required_Reading
   *
   *  None.
   *
   *$ Keywords
   *
   *      CONSTANTS
   *
   *$ Declarations
   *
   *     None.
   *
   *$ Brief_I/O
   *
   *      VARIABLE  I/O              DESCRIPTION
   *      --------  ---  --------------------------------------------------
   *      JYEAR       O   The number of seconds/julian year
   *
   *$ Detailed_Input
   *
   *     None.
   *
   *$ Detailed_Output
   *
   *     The function returns the number of seconds per julian
   *     year.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Particulars
   *
   *     The julian year is often used as a fundamental unit
   *     of time when dealing with ephemeris data.  For this
   *     reason its value in terms of ephemeris seconds is
   *     recorded in this function.
   *
   *$ Examples
   *
   *     Suppose you wish to compute the number of julian centuries
   *     that have elapsed since the ephemeris epoch J1950 (beginning
   *     of the julian year 1950) at a particular ET epoch.  The
   *     following line of code will do the trick.
   *
   *
   *        CENTRY = ( ET - UNITIM ( J1950(), 'JED', 'ET' ) )
   *       .       / ( 100.0D0 * JYEAR() )
   *
   *
   *$ Restrictions
   *
   *      None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *      None.
   *
   *$ Author_and_Institution
   *
   *      W.L. Taber      (JPL)
   *
   *$ Literature_References
   *
   *      Explanatory Supplement to the Astronomical Almanac.
   *      Page 8. University Science Books, 20 Edgehill Road,
   *      Mill Valley, CA 94941
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     Number of seconds per julian year
   *
   *-&
   * </pre>
   */
  public static final double JYEAR() {
    return SPICELIB_JYEAR;
  }

  /**
   *
   * <p>
   * Output from {@link Spicelib#M2EUL}
   * <p>
   * public final double ANGLE3;
   * <p>
   * public final double ANGLE2;
   * <p>
   * public final double ANGLE1;
   *
   * @author Douglas Rodgers <Douglas.Rodgers@jhuapl.edu>
   *
   */
  public static class M2EUL_OUT {
    public final double ANGLE3, ANGLE2, ANGLE1;

    public M2EUL_OUT(final double ANGLE3, final double ANGLE2, final double ANGLE1) {
      this.ANGLE3 = ANGLE3;
      this.ANGLE2 = ANGLE2;
      this.ANGLE1 = ANGLE1;
    }
  }

  /**
   * <pre>
   *$Procedure      M2EUL ( Matrix to Euler angles )
   *
   *      SUBROUTINE M2EUL (  R,  AXIS3,   AXIS2,   AXIS1,
   *     .                        ANGLE3,  ANGLE2,  ANGLE1  )
   *
   *$ Abstract
   *
   *     Factor a rotation matrix as a product of three rotations about
   *     specified coordinate axes.
   *
   *$ Required_Reading
   *
   *     ROTATION
   *
   *$ Keywords
   *
   *     ANGLE
   *     MATRIX
   *     ROTATION
   *     TRANSFORMATION
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION      R      ( 3, 3 )
   *
   *      INTEGER               AXIS3
   *      INTEGER               AXIS2
   *      INTEGER               AXIS1
   *
   *      DOUBLE PRECISION      ANGLE3
   *      DOUBLE PRECISION      ANGLE2
   *      DOUBLE PRECISION      ANGLE1
   *
   *$ Brief_I/O
   *
   *     Variable  I/O  Description
   *     --------  ---  --------------------------------------------------
   *     R          I   A rotation matrix to be factored.
   *     AXIS3,
   *     AXIS2,
   *     AXIS1      I   Numbers of third, second, and first rotation axes.
   *     ANGLE3,
   *     ANGLE2,
   *     ANGLE1     O   Third, second, and first Euler angles, in radians.
   *
   *$ Detailed_Input
   *
   *     R              is a 3x3 rotation matrix that is to be factored as
   *                    a product of three rotations about a specified
   *                    coordinate axes.  The angles of these rotations are
   *                    called `Euler angles'.
   *
   *     AXIS3,
   *     AXIS2,
   *     AXIS1          are the indices of the rotation axes of the
   *                    `factor' rotations, whose product is R.  R is
   *                    factored as
   *
   *                       R = [ ANGLE3 ]     [ ANGLE2 ]     [ ANGLE1 ]   .
   *                                    AXIS3          AXIS2         AXIS1
   *
   *                    The axis numbers must belong to the set {1, 2, 3}.
   *                    The second axis number MUST differ from the first
   *                    and third axis numbers.
   *
   *                    See the $ Particulars section below for details
   *                    concerning this notation.
   *
   *$ Detailed_Output
   *
   *     ANGLE3,
   *     ANGLE2,
   *     ANGLE1         are the Euler angles corresponding to the matrix
   *                    R and the axes specified by AXIS3, AXIS2, and
   *                    AXIS1.  These angles satisfy the equality
   *
   *                       R = [ ANGLE3 ]     [ ANGLE2 ]     [ ANGLE1 ]
   *                                   AXIS3          AXIS2          AXIS1
   *
   *
   *                    See the $ Particulars section below for details
   *                    concerning this notation.
   *
   *                    The range of ANGLE3 and ANGLE1 is (-pi, pi].
   *
   *                    The range of ANGLE2 depends on the exact set of
   *                    axes used for the factorization.  For
   *                    factorizations in which the first and third axes
   *                    are the same,
   *
   *                       R = [r]  [s]  [t] ,
   *                              a    b    a
   *
   *                    the range of ANGLE2 is [0, pi].
   *
   *
   *                    For factorizations in which the first and third
   *                    axes are different,
   *
   *                       R = [r]  [s]  [t] ,
   *                              a    b    c
   *
   *                    the range of ANGLE2 is [-pi/2, pi/2].
   *
   *                    For rotations such that ANGLE3 and ANGLE1 are not
   *                    uniquely determined, ANGLE3 will always be set to
   *                    zero; ANGLE1 is then uniquely determined.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     1)   If any of AXIS3, AXIS2, or AXIS1 do not have values in
   *
   *             { 1, 2, 3 },
   *
   *          then the error SPICE(BADAXISNUMBERS) is signaled.
   *
   *     2)   An arbitrary rotation matrix cannot be expressed using
   *          a sequence of Euler angles unless the second rotation axis
   *          differs from the other two.  If AXIS2 is equal to AXIS3 or
   *          AXIS1, then then error SPICE(BADAXISNUMBERS) is signaled.
   *
   *     3)   If the input matrix R is not a rotation matrix, the error
   *          SPICE(NOTAROTATION) is signaled.
   *
   *     4)   If ANGLE3 and ANGLE1 are not uniquely determined, ANGLE3
   *          is set to zero, and ANGLE1 is determined.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     A word about notation:  the symbol
   *
   *        [ x ]
   *             i
   *
   *     indicates a coordinate system rotation of x radians about the
   *     ith coordinate axis.  To be specific, the symbol
   *
   *        [ x ]
   *             1
   *
   *     indicates a coordinate system rotation of x radians about the
   *     first, or x-, axis; the corresponding matrix is
   *
   *        +-                    -+
   *        |  1      0       0    |
   *        |                      |
   *        |  0    cos(x)  sin(x) |.
   *        |                      |
   *        |  0   -sin(x)  cos(x) |
   *        +-                    -+
   *
   *     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
   *     matrix, when applied to a vector, rotates the vector by -x
   *     radians, not x radians.  Applying the matrix to a vector yields
   *     the vector's representation relative to the rotated coordinate
   *     system.
   *
   *     The analogous rotation about the second, or y-, axis is
   *     represented by
   *
   *        [ x ]
   *             2
   *
   *     which symbolizes the matrix
   *
   *        +-                    -+
   *        | cos(x)   0   -sin(x) |
   *        |                      |
   *        |  0       1      0    |,
   *        |                      |
   *        | sin(x)   0    cos(x) |
   *        +-                    -+
   *
   *     and the analogous rotation about the third, or z-, axis is
   *     represented by
   *
   *        [ x ]
   *             3
   *
   *     which symbolizes the matrix
   *
   *        +-                    -+
   *        |  cos(x)  sin(x)   0  |
   *        |                      |
   *        | -sin(x)  cos(x)   0  |.
   *        |                      |
   *        |  0        0       1  |
   *        +-                    -+
   *
   *
   *     The input matrix is assumed to be the product of three
   *     rotation matrices, each one of the form
   *
   *        +-                    -+
   *        |  1      0       0    |
   *        |                      |
   *        |  0    cos(r)  sin(r) |     (rotation of r radians about the
   *        |                      |      x-axis),
   *        |  0   -sin(r)  cos(r) |
   *        +-                    -+
   *
   *
   *        +-                    -+
   *        | cos(s)   0   -sin(s) |
   *        |                      |
   *        |  0       1      0    |     (rotation of s radians about the
   *        |                      |      y-axis),
   *        | sin(s)   0    cos(s) |
   *        +-                    -+
   *
   *     or
   *
   *        +-                    -+
   *        |  cos(t)  sin(t)   0  |
   *        |                      |
   *        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
   *        |                      |      z-axis),
   *        |  0        0       1  |
   *        +-                    -+
   *
   *     where the second rotation axis is not equal to the first or
   *     third.  Any rotation matrix can be factored as a sequence of
   *     three such rotations, provided that this last criterion is met.
   *
   *     This routine is related to the SPICELIB routine EUL2M, which
   *     produces a rotation matrix, given a sequence of Euler angles.
   *     This routine is a `right inverse' of EUL2M:  the sequence of
   *     calls
   *
   *        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
   *       .                 ANGLE3,  ANGLE2,  ANGLE1     )
   *
   *        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
   *       .                 AXIS3,   AXIS2,   AXIS1,   R )
   *
   *     preserves R, except for round-off error.
   *
   *
   *     On the other hand, the sequence of calls
   *
   *        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
   *       .                 AXIS3,   AXIS2,   AXIS1,   R )
   *
   *        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
   *       .                 ANGLE3,  ANGLE2,  ANGLE1     )
   *
   *
   *     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
   *     out in the ranges that M2EUL's outputs are restricted to.
   *
   *$ Examples
   *
   *     1)  Conversion of instrument pointing from a matrix representation
   *         to Euler angles:
   *
   *         Suppose we want to find camera pointing in alpha, delta, and
   *         kappa, given the inertial-to-camera coordinate transformation
   *
   *
   *    +-                                                               -+
   *    |  0.49127379678135830  0.50872620321864170  0.70699908539882417  |
   *    |                                                                 |
   *    | -0.50872620321864193 -0.49127379678135802  0.70699908539882428  |
   *    |                                                                 |
   *    |  0.70699908539882406 -0.70699908539882439  0.01745240643728360  |
   *    +-                                                               -+
   *
   *
   *         We want to find angles alpha, delta, kappa such that
   *
   *            TICAM  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
   *                               3                 1                 3
   *
   *         We can use the following small program to do this computation:
   *
   *
   *            PROGRAM EX1
   *            IMPLICIT NONE
   *
   *            DOUBLE PRECISION      DPR
   *            DOUBLE PRECISION      HALFPI
   *            DOUBLE PRECISION      TWOPI
   *
   *            DOUBLE PRECISION      ALPHA
   *            DOUBLE PRECISION      ANG1
   *            DOUBLE PRECISION      ANG2
   *            DOUBLE PRECISION      DELTA
   *            DOUBLE PRECISION      KAPPA
   *            DOUBLE PRECISION      TICAM  ( 3, 3 )
   *
   *
   *            DATA TICAM /  0.49127379678135830D0,
   *           .             -0.50872620321864193D0,
   *           .              0.70699908539882406D0,
   *           .              0.50872620321864170D0,
   *           .             -0.49127379678135802D0,
   *           .             -0.70699908539882439D0,
   *           .              0.70699908539882417D0,
   *           .              0.70699908539882428D0,
   *           .              0.01745240643728360D0  /
   *
   *
   *            CALL M2EUL ( TICAM, 3, 1, 3, KAPPA, ANG2, ANG1 )
   *
   *            DELTA = HALFPI() - ANG2
   *            ALPHA = ANG1     - HALFPI()
   *
   *            IF ( KAPPA .LT. 0.D0 ) THEN
   *               KAPPA = KAPPA + TWOPI()
   *            END IF
   *
   *            IF ( ALPHA .LT. 0.D0 ) THEN
   *               ALPHA = ALPHA + TWOPI()
   *            END IF
   *
   *            WRITE (*,'(1X,A,F24.14)') 'Alpha (deg): ', DPR() * ALPHA
   *            WRITE (*,'(1X,A,F24.14)') 'Delta (deg): ', DPR() * DELTA
   *            WRITE (*,'(1X,A,F24.14)') 'Kappa (deg): ', DPR() * KAPPA
   *
   *            END
   *
   *
   *         The program's output should be something like
   *
   *            Alpha (deg):       315.00000000000000
   *            Delta (deg):         1.00000000000000
   *            Kappa (deg):        45.00000000000000
   *
   *         possibly formatted a little differently, or degraded slightly
   *         by round-off.
   *
   *
   *     2)  Conversion of instrument pointing angles from a non-J2000,
   *         not necessarily inertial frame to J2000-relative RA, Dec,
   *         and Twist.
   *
   *         Suppose that we have pointing for some instrument expressed as
   *
   *            [ gamma ]  [ beta ]  [ alpha ]
   *                     3         2          3
   *
   *         with respect to some coordinate system S.  For example, S
   *         could be a spacecraft-fixed system.
   *
   *         We will suppose that the transformation from J2000
   *         coordinates to system S coordinates is given by the rotation
   *         matrix J2S.
   *
   *         The rows of J2S are the unit basis vectors of system S, given
   *         in J2000 coordinates.
   *
   *         We want to express the pointing with respect to the J2000
   *         system as the sequence of rotations
   *
   *            [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
   *                     3                 1                 3
   *
   *         First, we use subroutine EUL2M to form the transformation
   *         from system S to instrument coordinates S2INST.
   *
   *            CALL EUL2M ( GAMMA, BETA, ALPHA, 3, 2, 3, S2INST )
   *
   *         Next, we form the transformation from J2000 to instrument
   *         coordinates J2INST.
   *
   *            CALL MXM ( S2INST, J2S, J2INST )
   *
   *         Finally, we express J2INST using the desired Euler angles, as
   *         in the first example:
   *
   *            CALL M2EUL ( J2INST, 3, 1, 3, TWIST, ANG2, ANG3 )
   *
   *            RA   =  ANG3 - HALFPI()
   *            DEC  =  HALFPI() - ANG2
   *
   *         If we wish to make sure that RA, DEC, and TWIST are in
   *         the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi)
   *         respectively, we may add the code
   *
   *            IF ( RA .LT. 0.D0 ) THEN
   *               RA = RA + TWOPI()
   *            END IF
   *
   *            IF ( TWIST .LT. 0.D0 ) THEN
   *               TWIST = TWIST + TWOPI()
   *            END IF
   *
   *         Note that DEC is already in the correct range, since ANG2
   *         is in the range [0, pi] when the first and third input axes
   *         are equal.
   *
   *         Now RA, DEC, and TWIST express the instrument pointing
   *         as RA, Dec, and Twist, relative to the J2000 system.
   *
   *         A warning note:  more than one definition of RA, Dec, and
   *         Twist is extant.  Before using this example in an application,
   *         check that the definition given here is consistent with that
   *         used in your application.
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     N.J. Bachman   (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.2.1, 21-DEC-2006 (NJB)
   *
   *        Error corrected in header example:  input matrix
   *        previously did not match shown outputs.  Offending
   *        example now includes complete program.
   *
   *-    SPICELIB Version 1.2.0, 15-OCT-2005 (NJB)
   *
   *        Updated to remove non-standard use of duplicate arguments
   *        in MXM and MTXM calls.  A short error message cited in
   *        the Exceptions section of the header failed to match
   *        the actual short message used; this has been corrected.
   *
   *-    SPICELIB Version 1.1.2, 13-OCT-2004 (NJB)
   *
   *        Fixed header typo.
   *
   *-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
   *
   *        Header upgraded to describe notation in more detail.  Argument
   *        names were changed to describe the use of the arguments more
   *        accurately.  No change in functionality was made; the operation
   *        of the routine is identical to that of the previous version.
   *
   *-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     matrix to euler angles
   *
   *-&
   *
   *
   *
   *$ Revisions
   *
   *-    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB)
   *
   *        Updated to remove non-standard use of duplicate arguments
   *        in MXM and MTXM calls.  A short error message cited in
   *        the Exceptions section of the header  failed to match
   *        the actual short message used; this has been corrected.
   *
   *-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
   *
   *        Argument names were changed to describe the use of the
   *        arguments more accurately.  The axis and angle numbers
   *        now decrease, rather than increase, from left to right.
   *        The current names reflect the order of operator application
   *        when the Euler angle rotations are applied to a vector:  the
   *        rightmost matrix
   *
   *           [ ANGLE1 ]
   *                     AXIS1
   *
   *        is applied to the vector first, followed by
   *
   *           [ ANGLE2 ]
   *                     AXIS2
   *
   *        and then
   *
   *           [ ANGLE3 ]
   *                     AXIS3
   *
   *        Previously, the names reflected the order in which the Euler
   *        angle matrices appear on the page, from left to right.  This
   *        naming convention was found to be a bit obtuse by a various
   *        users.
   *
   *        No change in functionality was made; the operation of the
   *        routine is identical to that of the previous version.
   *
   *        Also, the header was upgraded to describe the notation in more
   *        detail.  The symbol
   *
   *           [ x ]
   *                i
   *
   *        is explained at mind-numbing length.  An example was added
   *        that shows a specific set of inputs and the resulting output
   *        matrix.
   *
   *        The angle sequence notation was changed to be consistent with
   *        Rotations required reading.
   *
   *          1-2-3  and  a-b-c
   *
   *        have been changed to
   *
   *          3-2-1  and  c-b-a.
   *
   *       Also, one `)' was changed to a `}'.
   *
   *       The phrase `first and third' was changed to `first or third'
   *       in the $ Particulars section, where the criterion for the
   *       existence of an Euler angle factorization is stated.
   *
   *-&
   *
   *     SPICELIB functions
   *
   *      LOGICAL               ISROT
   *      LOGICAL               RETURN
   * </pre>
   */
  public static final M2EUL_OUT M2EUL(final ImmutableDoubleArray R, final int AXIS3,
      final int AXIS2, final int AXIS1) {
    // ANGLE[0] = ANGLE3
    // ANGLE[1] = ANGLE2
    // ANGLE[2] = ANGLE1
    double[] ANGLE = _M2EUL(R.toArray(), AXIS3, AXIS2, AXIS1);
    return new M2EUL_OUT(ANGLE[0], ANGLE[1], ANGLE[2]);
  }

  /**
   * <pre>
   *$Procedure      MTXM  ( Matrix transpose times matrix, 3x3 )
   *
   *      SUBROUTINE MTXM ( M1, M2, MOUT )
   *
   *$ Abstract
   *
   *     Multiply the transpose of a 3x3 matrix and a 3x3 matrix.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     MATRIX
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION   M1   ( 3,3 )
   *      DOUBLE PRECISION   M2   ( 3,3 )
   *      DOUBLE PRECISION   MOUT ( 3,3 )
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     M1         I   3x3 double precision matrix.
   *     M2         I   3x3 double precision matrix.
   *     MOUT       O   3x3 double precision matrix which is the product
   *                    (M1**T) * M2.
   *
   *$ Detailed_Input
   *
   *     M1         is any 3x3 double precision matrix. Typically,
   *                M1 will be a rotation matrix since then its
   *                transpose is its inverse (but this is NOT a
   *                requirement).
   *
   *     M2         is any 3x3 double precision matrix.
   *
   *$ Detailed_Output
   *
   *     MOUT       is s 3x3 double precision matrix. MOUT is the
   *                product MOUT = (M1**T) x M2.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The code reflects precisely the following mathematical expression
   *
   *        For each value of the subscripts I and J from 1 to 3:
   *
   *        MOUT(I,J) = Summation from K=1 to 3 of  ( M1(K,I) * M2(K,J) )
   *
   *     Note that the reversal of the K and I subscripts in the left-hand
   *     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
   *     and not simply of M1 itself.
   *
   *$ Examples
   *
   *     Let M1  = | 1.0D0  2.0D0  3.0D0 |
   *               |                     |
   *               | 4.0D0  5.0D0  6.0D0 |
   *               |                     |
   *               | 7.0D0  8.0D0  9.0D0 |
   *
   *
   *         M2  = |  1.0D0   1.0D0  0.0D0 |
   *               |                       |
   *               | -1.0D0   1.0D0  0.0D0 |
   *               |                       |
   *               |  0.0D0   0.0D0  1.0D0 |
   *
   *     then the call
   *
   *        CALL MTXM ( M1, M2, MOUT )
   *
   *     produces the matrix
   *
   *
   *        MOUT = | -3.0D0   5.0D0  7.0D0 |
   *               |                       |
   *               | -3.0D0   7.0D0  8.0D0 |
   *               |                       |
   *               | -3.0D0   9.0D0  9.0D0 |
   *
   *
   *$ Restrictions
   *
   *     The user is responsible for checking the magnitudes of the
   *     elements of M1 and M2 so that a floating point overflow does
   *     not occur.  (In the typical use where M1 and M2 are rotation
   *     matrices, this not a risk at all.)
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.M. Owen       (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
   *
   *        Header correction: assertions that the output
   *        can overwrite the input have been removed.
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     matrix_transpose times matrix 3x3_case
   *
   *-&
   * </pre>
   */
  public static final ImmutableDoubleArray MTXM(final ImmutableDoubleArray M1,
      final ImmutableDoubleArray M2) {
    return ImmutableDoubleArray.copyOf(_MTXM(M1.toArray(), M2.toArray()));
  }

  /**
   * <pre>
   *$Procedure      MXM  ( Matrix times matrix, 3x3 )
   *
   *      SUBROUTINE MXM ( M1, M2, MOUT )
   *
   *$ Abstract
   *
   *     Multiply two 3x3 matrices.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     MATRIX
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION   M1   ( 3,3 )
   *      DOUBLE PRECISION   M2   ( 3,3 )
   *      DOUBLE PRECISION   MOUT ( 3,3 )
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O              DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     M1         I   3x3 double precision matrix.
   *     M2         I   3x3 double prercision matrix.
   *     MOUT       O   3x3 double precision matrix. MOUT is the product
   *                    M1*M2.
   *
   *$ Detailed_Input
   *
   *     M1         is an arbitrary 3x3 double precision matrix.
   *
   *     M2         is an arbitrary 3x3 double precision matrix.
   *
   *$ Detailed_Output
   *
   *     MOUT       is a 3x3 double precision matrix. MOUT is the product
   *                M1*M2.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The code reflects precisely the following mathematical expression
   *
   *        For each value of the subscripts I and J from 1 to 3:
   *
   *        MOUT(I,J) = Summation from K=1 to 3 of  ( M1(I,K) * M2(K,J) )
   *
   *$ Examples
   *
   *     Let M1 = |  1.0D0  1.0D0  0.0D0 |
   *              |                      |
   *              | -1.0D0  1.0D0  0.0D0 |
   *              |                      |
   *              |  0.0D0  0.0D0  1.0D0 |
   *
   *
   *     and M2 = |  1.0D0  0.0D0  0.0D0 |
   *              |                      |
   *              |  0.0D0  1.0D0  1.0D0 |
   *              |                      |
   *              |  0.0D0 -1.0D0  1.0D0 |
   *
   *     then the call
   *
   *        CALL MXM ( M1, M2, MOUT )
   *
   *     produces the matrix
   *
   *        MOUT = |  1.0D0  1.0D0  1.0D0 |
   *               |                      |
   *               | -1.0D0  1.0D0  1.0D0 |
   *               |                      |
   *               |  0.0D0 -1.0D0  1.0D0 |
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.M. Owen       (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
   *
   *        Header correction: assertions that the output
   *        can overwrite the input have been removed.
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     matrix times matrix 3x3_case
   *
   *-&
   *
   *
   *     Local variables
   *
   *      DOUBLE PRECISION      PRODM( 3, 3 )
   *
   *      INTEGER               I
   *      INTEGER               J
   *
   * </pre>
   */
  public static final ImmutableDoubleArray MXM(final ImmutableDoubleArray M1,
      final ImmutableDoubleArray M2) {
    return ImmutableDoubleArray.copyOf(_MXM(M1.toArray(), M2.toArray()));
  }

  /**
   * <pre>
   *$Procedure      MXMT ( Matrix times matrix transpose, 3x3 )
   *
   *      SUBROUTINE MXMT ( M1, M2, MOUT )
   *
   *$ Abstract
   *
   *      Multiply a 3x3 matrix and the transpose of another 3x3 matrix.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     MATRIX
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION   M1   ( 3,3 )
   *      DOUBLE PRECISION   M2   ( 3,3 )
   *      DOUBLE PRECISION   MOUT ( 3,3 )
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     M1         I   3x3 double precision matrix.
   *     M2         I   3x3 double precision matrix.
   *     MOUT       O   3x3 double precision matrix. MOUT is the
   *                    product M1 * M2**T.
   *
   *$ Detailed_Input
   *
   *     M1         is an arbitrary 3x3 double precision matrix.
   *
   *     M2         is an arbitrary 3x3 double precision matrix.
   *                Typically, M2 will be a rotation matrix since
   *                then its transpose is its inverse (but this is
   *                NOT a requirement).
   *
   *$ Detailed_Output
   *
   *     MOUT       is a 3x3 double precision matrix. MOUT is the
   *                product (M1) x (M2**T).
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The code reflects precisely the following mathematical expression
   *
   *        For each value of the subscripts I and J from 1 to 3:
   *
   *        MOUT(I,J) = Summation from K=1 to 3 of  ( M1(I,K) * M2(J,K) )
   *
   *     Note that the reversal of the K and J subscripts in the right-
   *     hand matrix M2 is what makes MOUT the product of the TRANSPOSE of
   *     M2 and not simply of M2 itself.
   *
   *$ Examples
   *
   *     Let M1 = |  0.0D0  1.0D0  0.0D0 |
   *              |                      |
   *              | -1.0D0  0.0D0  0.0D0 |
   *              |                      |
   *              |  0.0D0  0.0D0  1.0D0 |
   *
   *
   *         M2 = |  0.0D0  1.0D0  0.0D0 |
   *              |                      |
   *              | -1.0D0  0.0D0  0.0D0 |
   *              |                      |
   *              |  0.0D0  0.0D0  1.0D0 |
   *
   *     then the call
   *
   *        CALL MXMT ( M1, M2, MOUT )
   *
   *     produces the matrix
   *
   *
   *        MOUT = | 1.0D0  0.0D0  0.0D0 |
   *               |                     |
   *               | 0.0D0  1.0D0  0.0D0 |
   *               |                     |
   *               | 0.0D0  0.0D0  1.0D0 |
   *
   *
   *$ Restrictions
   *
   *     The user is responsible for checking the magnitudes of the
   *     elements of M1 and M2 so that a floating point overflow does
   *     not occur.  (In the typical use where M1 and M2 are rotation
   *     matrices, this not a risk at all.)
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.M. Owen       (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
   *
   *        Header correction: assertions that the output
   *        can overwrite the input have been removed.
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     matrix times matrix_transpose 3x3_case
   *
   *-&
   * 
   * </pre>
   */
  public static final ImmutableDoubleArray MXMT(final ImmutableDoubleArray M1,
      final ImmutableDoubleArray M2) {
    return ImmutableDoubleArray.copyOf(_MXMT(M1.toArray(), M2.toArray()));
  }

  /**
   * $Procedure MXV ( Matrix times vector, 3x3 )
   *
   * SUBROUTINE MXV ( MATRIX, VIN, VOUT )
   *
   * $ Abstract
   *
   * Multiply a 3x3 double precision matrix with a 3-dimensional double precision vector.
   *
   * $ Required_Reading
   *
   * None.
   *
   * $ Keywords
   *
   * MATRIX VECTOR
   *
   * $ Declarations
   *
   * DOUBLE PRECISION MATRIX ( 3,3 ) DOUBLE PRECISION VIN ( 3 ) DOUBLE PRECISION VOUT ( 3 )
   *
   * $ Brief_I/O
   *
   * VARIABLE I/O DESCRIPTION -------- --- -------------------------------------------------- MATRIX
   * I 3x3 double precision matrix. VIN I 3-dimensional double precision vector. VOUT O
   * 3-dimensinoal double precision vector. VOUT is the product MATRIX*VIN.
   *
   * $ Detailed_Input
   *
   * MATRIX is an arbitrary 3x3 double precision matrix.
   *
   * VIN is an arbitrary 3-dimensional double precision vector.
   *
   * $ Detailed_Output
   *
   * VOUT is a 3-dimensional double precision vector. VOUT is the product MATRIX * V.
   *
   * $ Parameters
   *
   * None.
   *
   * $ Exceptions
   *
   * Error free.
   *
   * $ Files
   *
   * None.
   *
   * $ Particulars
   *
   * The code reflects precisely the following mathematical expression
   *
   * For each value of the subscript I from 1 to 3:
   *
   * VOUT(I) = Summation from K=1 to 3 of ( MATRIX(I,K) * VIN(K) )
   *
   * $ Examples
   *
   * Let
   *
   * MATRIX = | 0.0D0 1.0D0 0.0D0 | and VIN = | 1.0D0 | | | | | | -1.0D0 0.0D0 0.0D0 | | 2.0D0 | | |
   * | | | 0.0D0 0.0D0 1.0D0 | | 3.0D0 |
   *
   * Then the call,
   *
   * CALL MXV ( MATRIX, VIN, VOUT )
   *
   * produces the vector
   *
   * VOUT = | 2.0D0 | | | | -1.0D0 | | | | 3.0D0 |
   *
   *
   * $ Restrictions
   *
   * The user is responsible for checking the magnitudes of the elements of MATRIX and VIN so that a
   * floating point overflow does not occur.
   *
   * $ Literature_References
   *
   * None.
   *
   * $ Author_and_Institution
   *
   * W.M. Owen (JPL)
   *
   * $ Version
   *
   * - SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
   *
   * Header correction: assertions that the output can overwrite the input have been removed.
   *
   * - SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   * Comment section for permuted index source lines was added following the header.
   *
   * - SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   * -&
   *
   * $ Index_Entries
   *
   * matrix times 3-dimensional vector
   *
   * -&
   *
   *
   * DOUBLE PRECISION PRODV(3) INTEGER I
   *
   *
   * </pre>
   */
  public static final ImmutableDoubleArray MXV(final ImmutableDoubleArray MATRIX,
      final ImmutableDoubleArray VIN) {
    return ImmutableDoubleArray.copyOf(_MXV(MATRIX.toArray(), VIN.toArray()));
  }

  /**
   * <pre>
   *$Procedure                     PI ( Value of pi )
   *
   *      DOUBLE PRECISION FUNCTION PI ( )
   *
   *$ Abstract
   *
   *     Return the value of pi (the ratio of the circumference of
   *     a circle to its diameter).
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     CONSTANTS
   *
   *$ Declarations
   *
   *     None.
   *
   *$ Brief_I/O
   *
   *     The function returns the value of pi.
   *
   *$ Detailed_Input
   *
   *     None.
   *
   *$ Detailed_Output
   *
   *     The function returns the value of pi (the ratio of a circle's
   *     circumference to its diameter), determined by the ACOS function.
   *     That is,
   *
   *           PI = ACOS ( -1.D0 )
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The first time the function is referenced, the value is computed
   *     as shown above. The value is saved, and returned directly upon
   *     subsequent reference.
   *
   *$ Examples
   *
   *     The code fragment below illustrates the use of PI.
   *
   *        C
   *        C     Compute the polar radius,
   *        C
   *        C                 p
   *        C          ----------------
   *        C          1 + e cos(theta)
   *        C
   *        C     at evenly spaced values of the polar angle, theta.
   *        C
   *              DELTA = PI() / N
   *
   *              DO I = 0, N
   *                 R(I) = P / (1.D0 + ECC * COS(I*DELTA))
   *              END DO
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *     I.M. Underwood  (JPL)
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
   *
   *-&
  
   *$ Index_Entries
   *
   *     value of pi
   *
   *-&
   * 
   * </pre>
   */
  public static final double PI() {
    return SPICELIB_PI;
  }

  /**
   * <pre>
   *$Procedure      ROTATE ( Generate a rotation matrix )
   *
   *     SUBROUTINE ROTATE ( ANGILE, IAXIS, MOUT )
   *
   *$ Abstract
   *
   *      Calculate the 3x3 rotation matrix generated by a rotation
   *      of a specified angle about a specified axis. This rotation
   *      is thought of as rotating the coordinate system.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *      MATRIX,  ROTATION
   *
   *$ Declarations
   *
   *     DOUBLE PRECISION   ANGLE
   *     INTEGER            IAXIS
   *     DOUBLE PRECISION   MOUT  ( 3,3 )
   *
   *$ Brief_I/O
   *
   *      VARIABLE  I/O              DESCRIPTION
   *      --------  ---  --------------------------------------------------
   *       ANGLE     I     Angle of rotation (radians).
   *       IAXIS     I     Axis of rotation (X=1, Y=2, Z=3).
   *       MOUT      O     Resulting rotation matrix [ANGLE]
   *                                                       IAXIS
   *$ Detailed_Input
   *
   *      ANGLE   The angle given in radians, through which the rotation
   *              is performed.
   *
   *      IAXIS   The index of the axis of rotation.  The X, Y, and Z
   *              axes have indices 1, 2 and 3 respectively.
   *
   *$ Detailed_Output
   *
   *      MOUT    Rotation matrix which describes the rotation of the
   *               COORDINATE system through ANGLE radians about the
   *               axis whose index is IAXIS.
   *
   *$ Parameters
   *
   *      None.
   *
   *$ Particulars
   *
   *      A rotation about the first, i.e. x-axis, is described by
   *
   *      |  1        0          0      |
   *      |  0   cos(theta) sin(theta)  |
   *      |  0  -sin(theta) cos(theta)  |
   *
   *      A rotation about the second, i.e. y-axis, is described by
   *
   *      |  cos(theta)  0  -sin(theta)  |
   *      |      0       1        0      |
   *      |  sin(theta)  0   cos(theta)  |
   *
   *      A rotation about the third, i.e. z-axis, is described by
   *
   *      |  cos(theta) sin(theta)   0   |
   *      | -sin(theta) cos(theta)   0   |
   *      |       0          0       1   |
   *
   *      ROTATE decides which form is appropriate according to the value
   *      of IAXIS.
   *
   *$ Examples
   *
   *      If ROTATE is called from a FORTRAN program as follows:
   *
   *            CALL ROTATE (PI/4, 3, MOUT)
   *
   *      then MOUT will be given by
   *
   *                   | SQRT(2)/2   SQRT(2)/2   0  |
   *            MOUT = |-SQRT(2)/2   SQRT(2)/2   0  |
   *                   |     0           0       1  |
   *
   *$ Restrictions
   *
   *      None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *     1) If the axis index is not in the range 1 to 3 it will be
   *        treated the same as that integer 1, 2, or 3 that is congruent
   *        to it mod 3.
   *
   *$ Files
   *
   *      None.
   *
   *$ Author_and_Institution
   *
   *      W.M. Owen       (JPL)
   *      W.L. Taber      (JPL)
   *
   *$ Literature_References
   *
   *      None.
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     generate a rotation matrix
   *
   *-&
   *
   *
   *$ Revisions
   *
   *-    Beta Version 1.1.0, 3-JAN-1989 (WLT)
   *
   *     Upgrade the routine to work with negative axis indexes.  Also take
   *     care of the funky way the indices (other than the input) were
   *     obtained via the MOD function.  It works but isn't as clear
   *     (or fast) as just reading the axes from data.
   *
   *-&
   *
   *
   *      DOUBLE PRECISION      S
   *      DOUBLE PRECISION      C
   *
   *      INTEGER               TEMP
   *      INTEGER               I1
   *      INTEGER               I2
   *      INTEGER               I3
   *      INTEGER               INDEXS ( 5 )
   *      SAVE                  INDEXS
   *
   *      DATA                  INDEXS  / 3, 1, 2, 3, 1 /
   * </pre>
   */
  public static final ImmutableDoubleArray ROTATE(final double ANGLE, final int IAXIS) {
    return ImmutableDoubleArray.copyOf(_ROTATE(ANGLE, IAXIS));
  }

  /**
   * <pre>
   *$Procedure      ROTMAT ( Rotate a matrix )
   *
   *     SUBROUTINE ROTMAT ( M1, ANGLE, IAXIS, MOUT )
   *
   *$ Abstract
   *
   *     ROTMAT applies a rotation of ANGLE radians about axis IAXIS to a
   *     matrix.  This rotation is thought of as rotating the coordinate
   *     system.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     MATRIX
   *     ROTATION
   *
   *$ Declarations
   *
   *     DOUBLE PRECISION   M1    ( 3,3 )
   *     DOUBLE PRECISION   ANGLE
   *     INTEGER            IAXIS
   *     DOUBLE PRECISION   MOUT  ( 3,3 )
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     M1         I   Matrix to be rotated.
   *     ANGLE      I   Angle of rotation (radians).
   *     IAXIS      I   Axis of rotation (X=1, Y=2, Z=3).
   *     MOUT       O   Resulting rotated matrix [ANGLE]      * M1
   *                                                   IAXIS
   *
   *$ Detailed_Input
   *
   *     M1       This is a matrix to which a rotation is to be applied.
   *              In matrix algebra, the components of the matrix are
   *              relevant in one particular coordinate system. Applying
   *              ROTMAT changes the components of M1 so that they are
   *              relevant to a rotated coordinate system.
   *
   *     ANGLE    The angle in radians through which the original
   *              coordinate system is to be rotated.
   *
   *     IAXIS    An index for the axis of the original coordinate system
   *              about which the rotation by ANGLE is to be performed.
   *              IAXIS = 1,2 or 3 designates the x-, y- or z-axis,
   *              respectively.
   *
   *$ Detailed_Output
   *
   *     MOUT     The matrix resulting from the application of the
   *              specified rotation to the input matrix M1.  If
   *              [ANGLE]        denotes the rotation matrix by ANGLE
   *                     IAXIS
   *              radians about IAXIS, (refer to the routine ROTATE) then
   *              MOUT is given by the following matrix equation:
   *
   *                 MOUT = [ANGLE]      * M1
   *                               IAXIS
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *     1) If the axis index is not in the range 1 to 3 it will be
   *        treated the same as that integer 1, 2, or 3 that is congruent
   *        to it mod 3.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     None.
   *
   *$ Examples
   *
   *     Suppose that to rotate a set of inertial axes to body fixed
   *     axes, one must first roll the coordinate axes about the x-axis by
   *     angle R to get x', y', z'.  From this one must pitch about the y'
   *     axis by angle P to get x'', y'', z''.  And finally yaw the x'',
   *     y'', z'' about the z'' axis by angle Y to obtain the
   *     transformation to bodyfixed coordinates.  If ID is the identity
   *     matrix, then the following code fragment generates the
   *     transformation from inertial to body fixed.
   *
   *        CALL ROTMAT ( ID, R,     1,     M1   )
   *        CALL ROTMAT ( M1, P,     2,     M2   )
   *        CALL ROTMAT ( M2, Y,     3,     TIBF )
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.M. Owen       (JPL)
   *     W.L. Taber      (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
   *
   *        Header correction: assertions that the output
   *        can overwrite the input have been removed.
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     rotate a matrix
   *
   *-&
   *
   *
   *$ Revisions
   *
   *-    Beta Version 1.1.0, 3-JAN-1989 (WLT)
   *
   *     Upgrade the routine to work with negative axis indexes.  Also take
   *     care of the funky way the indices (other than the input) were
   *     obtained via the MOD function.  It works but isn't as clear
   *     (or fast) as just reading the axes from data.
   *
   *-&
   *
   *     DOUBLE PRECISION      S
   *     DOUBLE PRECISION      C
   *
   *     INTEGER               TEMP
   *     INTEGER               I1
   *     INTEGER               I2
   *     INTEGER               I3
   *     INTEGER               I
   *
   *     DOUBLE PRECISION      PRODM   (3,3)
   *
   *     INTEGER               INDEXS  (5)
   *     SAVE                  INDEXS
   *
   *     DATA                  INDEXS  / 3, 1, 2, 3, 1 /
   * </pre>
   */
  public static final ImmutableDoubleArray ROTMAT(final ImmutableDoubleArray M1, final double ANGLE,
      final int IAXIS) {
    return ImmutableDoubleArray.copyOf(_ROTMAT(M1.toArray(), ANGLE, IAXIS));
  }

  /**
   * <pre>
   * $Procedure                     RPD ( Radians per degree )
   *
   *      DOUBLE PRECISION FUNCTION RPD ( )
   *
   *$ Abstract
   *
   *     Return the number of radians per degree.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     CONSTANTS
   *
   *$ Declarations
   *
   *     None.
   *
   *$ Brief_I/O
   *
   *     The function returns the number of radians per degree.
   *
   *$ Detailed_Input
   *
   *     None.
   *
   *$ Detailed_Output
   *
   *     The function returns the number of radians per degree: pi/180.
   *     The value of pi is determined by the ACOS function. That is,
   *
   *           RPD = ACOS ( -1.D0 ) / 180.D0
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The first time the function is referenced, the value is computed
   *     as shown above. The value is saved, and returned directly upon
   *     subsequent reference.
   *
   *$ Examples
   *
   *     The code fragment below illustrates the use of RPD.
   *
   *        C
   *        C     Convert all input angles to radians.
   *        C
   *              CLOCK = CLOCK * RPD()
   *              CONE  = CONE  * RPD()
   *              TWIST = TWIST * RPD()
   *
   *     or equivalently,
   *
   *        C
   *        C     Convert all input angles to radians.
   *        C
   *              CALL VPACK  ( CLOCK, CONE, CCTWIST, ALBTGAM )
   *              CALL VSCL   ( RPD(), ALBTGAM, ALBTGAM )
   *              CALL VUPACK ( ALBTGAM, CLOCK, CONE, CCTWIST )
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *     I.M. Underwood  (JPL)
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     radians per degree
   *
   *-&
   * </pre>
   */
  public static final double RPD() {
    return SPICELIB_RPD;
  }

  /**
   * <pre>
   *$Procedure                     SPD ( Seconds per day )
   *
   *      DOUBLE PRECISION FUNCTION SPD ( )
   *
   *$ Abstract
   *
   *     Return the number of seconds in a day.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     CONSTANTS
   *
   *$ Declarations
   *
   *     None.
   *
   *$ Brief_I/O
   *
   *     The function returns the number of seconds in a day.
   *
   *$ Detailed_Input
   *
   *     None.
   *
   *$ Detailed_Output
   *
   *     The function returns the number of seconds in a day: 86400.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The function always returns the constant value shown above.
   *
   *$ Examples
   *
   *     The following code fragment illustrates the use of SPD.
   *
   *        C
   *        C     Convert Julian Date to UTC seconds past the reference
   *        C     epoch (J2000).
   *        C
   *              SPREF = ( JD - J2000() ) * SPD()
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *     I.M. Underwood  (JPL)
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     seconds per day
   *
   *-&
   * </pre>
   */
  public static final double SPD() {
    return SPICELIB_SPD;
  }

  /**
   * <pre>
   *$Procedure                     TWOPI ( Twice the value of pi )
   *
   *      DOUBLE PRECISION FUNCTION TWOPI ( )
   *
   *$ Abstract
   *
   *     Return twice the value of pi (the ratio of the circumference of
   *     a circle to its diameter).
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     CONSTANTS
   *
   *$ Declarations
   *
   *     None.
   *
   *$ Brief_I/O
   *
   *     The function returns twice the value of pi.
   *
   *$ Detailed_Input
   *
   *     None.
   *
   *$ Detailed_Output
   *
   *     The function returns twice the value of pi (the ratio of
   *     a circle's circumference to its diameter), determined by
   *     the ACOS function. That is,
   *
   *           TWOPI = ACOS ( -1.D0 ) * 2.D0
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The first time the function is referenced, the value is computed
   *     as shown above. The value is saved, and returned directly upon
   *     subsequent reference.
   *
   *$ Examples
   *
   *     The code fragment below illustrates the use of TWOPI.
   *
   *        C
   *        C      The longitude of the ascending node is the angle
   *        C      between the x-axis and the node vector, n.
   *        C                                              -
   *        C
   *               NODE = ACOS ( N(1) / VNORM(N) )
   *
   *               IF ( NODE .LT. 0.D0 ) THEN
   *                  NODE = NODE + TWOPI()
   *               END IF
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *     I.M. Underwood  (JPL)
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.2, 28-AUG-1997 (WLT)
   *
   *         Fixed the description in the detailed output section
   *         of the header.
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     twice the value of pi
   *
   *-&
   * </pre>
   */
  public static final double TWOPI() {
    return SPICELIB_TWOPI;
  }

  /**
   *
   * <p>
   * Output from {@link Spicelib#UNORM}
   * <p>
   * public final ImmutableDoubleArray VOUT;
   * <p>
   * public final double VMAG;
   *
   * @author Douglas Rodgers <Douglas.Rodgers@jhuapl.edu>
   *
   */
  public static class UNORM_OUT {
    public final ImmutableDoubleArray VOUT;
    public final double VMAG;

    public UNORM_OUT(final ImmutableDoubleArray VOUT, final double VMAG) {
      this.VOUT = VOUT;
      this.VMAG = VMAG;
    }
  }

  /**
   * <pre>
   *$Procedure      UNORM ( Unit vector and norm, 3 dimensional )
   *
   *      SUBROUTINE UNORM ( V1, VOUT, VMAG )
   *
   *$ Abstract
   *
   *     Normalize a double precision 3-vector and return its magnitude.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     VECTOR
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION  V1   ( 3 )
   *      DOUBLE PRECISION  VOUT ( 3 )
   *      DOUBLE PRECISION  VMAG
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     V1         I   Vector to be normalized.
   *     VOUT       O   Unit vector V1 / |V1|.
   *                    If V1 is the zero vector, then VOUT will also
   *                    be zero.
   *     VMAG       O   Magnitude of V1, i.e. |V1|.
   *
   *$ Detailed_Input
   *
   *     V1      This variable may contain any 3-vector, including the
   *             zero vector.
   *
   *$ Detailed_Output
   *
   *     VOUT    This variable contains the unit vector in the direction
   *             of V1.  If V1 is the zero vector, then VOUT will also be
   *             the zero vector.
   *
   *     VMAG    This is the magnitude of V1.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     UNORM references a function called VNORM (which itself is
   *     numerically stable) to calculate the norm of the input vector V1.
   *     If the norm is equal to zero, then each component of the output
   *     vector VOUT is set to zero.  Otherwise, VOUT is calculated by
   *     dividing V1 by the norm.
   *
   *$ Examples
   *
   *     The following table shows how selected V1 implies VOUT and MAG.
   *
   *        V1                    VOUT                   MAG
   *        ------------------    ------------------     ----
   *        (5, 12, 0)            (5/13, 12/13, 0)       13
   *        (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)        3D-7
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.M. Owen       (JPL)
   *     W.L. Taber      (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
   *
   *        Header correction: assertions that the output
   *        can overwrite the input have been removed.
   *
   *-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *        Comment section for permuted index source lines was added
   *        following the header.
   *
   *-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     3-dimensional unit vector and norm
   *
   *-&
   *
   *$ Revisions
   *
   *-    Beta Version 1.0.1, 10-JAN-1989 (WLT)
   *
   *     Error free specification added.
   *
   *-&
   *
   *
   *     SPICELIB functions
   *
   *      DOUBLE PRECISION   VNORM
   *
   * </pre>
   */
  public static final UNORM_OUT UNORM(final ImmutableDoubleArray V1) {
    double[] OUT = _UNORM(V1.toArray());
    return new UNORM_OUT(ImmutableDoubleArray.of(OUT[0], OUT[1], OUT[2]), OUT[3]);
  }

  /**
   * <pre>
   *$Procedure      VHAT ( "V-Hat", unit vector along V, 3 dimensions )
   *
   *      SUBROUTINE VHAT ( V1, VOUT )
   *
   *$ Abstract
   *
   *      Find the unit vector along a double precision 3-dimensional
   *      vector.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *      VECTOR
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION   V1   ( 3 )
   *      DOUBLE PRECISION   VOUT ( 3 )
   *
   *$ Brief_I/O
   *
   *      VARIABLE  I/O  DESCRIPTION
   *      --------  ---  --------------------------------------------------
   *       V1        I     Vector to be normalized.
   *       VOUT      O     Unit vector V1 / |V1|.
   *                       If V1 = 0, VOUT will also be zero.
   *                       VOUT can overwrite V1.
   *
   *$ Detailed_Input
   *
   *      V1      This is any double precision, 3-dimensional vector.  If
   *              this vector is the zero vector, this routine will detect
   *              it, and will not attempt to divide by zero.
   *
   *$ Detailed_Output
   *
   *      VOUT    VOUT contains the unit vector in the direction of V1. If
   *              V1 represents the zero vector, then VOUT will also be the
   *              zero vector.  VOUT may overwrite V1.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Particulars
   *
   *      VHAT determines the magnitude of V1 and then divides each
   *      component of V1 by the magnitude.  This process is highly stable
   *      over the whole range of 3-dimensional vectors.
   *
   *$ Examples
   *
   *      The following table shows how selected V1 implies VOUT.
   *
   *      V1                    VOUT
   *      ------------------    ------------------
   *      (5, 12, 0)            (5/13, 12/13, 0)
   *      (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)
   *
   *
   *$ Restrictions
   *
   *      There is no known case whereby floating point overflow may occur.
   *      Thus, no error recovery or reporting scheme is incorporated
   *      into this subroutine.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *      None.
   *
   *$ Author_and_Institution
   *
   *      N.J. Bachman    (JPL)
   *      H.A. Neilan     (JPL)
   *      W.M. Owen       (JPL)
   *
   *$ Literature_References
   *
   *      None.
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     unitize a 3-dimensional vector
   *
   *-&
   *
   *
   *$ Revisions
   *
   *-     Beta Version 1.1.0, 10-FEB-1989 (HAN) (NJB)
   *
   *         Contents of the Exceptions section was changed
   *         to "error free" to reflect the decision that the
   *         module will never participate in error handling.
   *         Also, the declaration of the unused variable I was
   *         removed.
   *-&
   * 
   * </pre>
   */
  public static final ImmutableDoubleArray VHAT(final ImmutableDoubleArray V1) {
    return ImmutableDoubleArray.copyOf(_VHAT(V1.toArray()));
  }

  /**
   * <pre>
   *$Procedure      VNORM ( Vector norm, 3 dimensions )
   *
   *      DOUBLE PRECISION FUNCTION VNORM ( V1 )
   *
   *$ Abstract
   *
   *      Compute the magnitude of a double precision, 3-dimensional
   *      vector.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *      VECTOR
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION  V1 ( 3 )
   *
   *$ Brief_I/O
   *
   *      VARIABLE  I/O  DESCRIPTION
   *      --------  ---  --------------------------------------------------
   *       V1        I     Vector whose magnitude is to be found.
   *
   *$ Detailed_Input
   *
   *      V1      This may be any 3-dimensional, double precision vector.
   *
   *$ Detailed_Output
   *
   *      VNORM is the magnitude of V1 calculated in a numerically stable
   *      way.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Particulars
   *
   *      VNORM finds the component of V1 whose magnitude is the largest.
   *      If the absolute magnitude of that component indicates that a
   *      numeric overflow would occur when it is squared, or if it
   *      indicates that an underflow would occur when square (giving a
   *      magnitude of zero) then the following expression is used:
   *
   *      VNORM = V1MAX * MAGNITUDE OF [ (1/V1MAX)*V1 ]
   *
   *      Otherwise a simpler expression is used:
   *
   *      VNORM = MAGNITUDE OF [ V1 ]
   *
   *      Beyond the logic described above, no further checking of the
   *      validity of the input is performed.
   *
   *$ Examples
   *
   *      The following table show the correlation between various input
   *      vectors V1 and VNORM:
   *
   *      V1                                    VNORM
   *      -----------------------------------------------------------------
   *      (1.D0, 2.D0, 2.D0)                     3.D0
   *      (5.D0, 12.D0, 0.D0)                   13.D0
   *      (-5.D-17, 0.0D0, 12.D-17)             13.D-17
   *
   *$ Restrictions
   *
   *      None.
   *
   *$ Exceptions
   *
   *      Error free.
   *
   *$ Files
   *
   *      None.
   *
   *$ Author_and_Institution
   *
   *      W.M. Owen       (JPL)
   *
   *$ Literature_References
   *
   *      None.
   *
   *$ Version
   *
   *-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
   *
   *         Comment section for permuted index source lines was added
   *         following the header.
   *
   *-     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     norm of 3-dimensional vector
   *
   *-&
   * </pre>
   */
  public static final double VNORM(final ImmutableDoubleArray V1) {
    return _VNORM(V1.toArray());
  }

  /**
   *
   * <p>
   * Output from {@link Spicelib#XF2EUL}
   * <p>
   * public final ImmutableDoubleArray EULANG;
   * <p>
   * public final boolean UNIQUE;
   *
   * @author Douglas Rodgers <Douglas.Rodgers@jhuapl.edu>
   *
   */
  public static class XF2EUL_OUT {
    public final ImmutableDoubleArray EULANG;
    public final boolean UNIQUE;

    public XF2EUL_OUT(final ImmutableDoubleArray EULANG, final boolean UNIQUE) {
      this.EULANG = EULANG;
      this.UNIQUE = UNIQUE;
    }
  }


  /**
   * <pre>
   *$Procedure      XF2EUL ( State transformation to Euler angles )
   *
   *      SUBROUTINE XF2EUL ( XFORM, AXISA, AXISB, AXISC, EULANG, UNIQUE )
   *
   *$ Abstract
   *
   *     Convert a state transformation matrix to Euler angles and their
   *     derivatives with respect to a specified set of axes.
   *
   *     The companion entry point EUL2XF converts Euler angles and their
   *     derivatives with respect to a specified set of axes to a state
   *     transformation matrix.
   *
   *$ Required_Reading
   *
   *     ROTATION
   *     PCK
   *
   *$ Keywords
   *
   *     ANGLES
   *     STATE
   *     DERIVATIVES
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION      XFORM  ( 6, 6 )
   *      INTEGER               AXISA
   *      INTEGER               AXISB
   *      INTEGER               AXISC
   *      DOUBLE PRECISION      EULANG ( 6 )
   *      LOGICAL               UNIQUE
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     XFORM      I   A state transformation matrix.
   *     AXISA      I   Axis A of the Euler angle factorization.
   *     AXISB      I   Axis B of the Euler angle factorization.
   *     AXISC      I   Axis C of the Euler angle factorization.
   *     EULANG     O   An array of Euler angles and their derivatives.
   *     UNIQUE     O   Indicates if EULANG is a unique representation.
   *
   *$ Detailed_Input
   *
   *     XFORM       is a state transformation from some frame FRAME1 to
   *                 another frame FRAME2.  Pictorially, XFORM has the
   *                 structure shown here.
   *
   *                      [       |        ]
   *                      |  R    |    0   |
   *                      |       |        |
   *                      |-------+--------|
   *                      |       |        |
   *                      | dR/dt |    R   |
   *                      [       |        ]
   *
   *                 where R is a rotation that varies with respect to time
   *                 and dR/dt is its time derivative.
   *
   *                 More specifically, if S1 is the state of some object
   *                 in FRAME1, then S2, the state of the same object
   *                 relative to FRAME2 is given by
   *
   *                    S2 = XFORM*S1
   *
   *                 where '*' denotes the matrix vector product.
   *
   *     AXISA       are the axes desired for the factorization of R.
   *     AXISB       All must be in the range from 1 to 3.  Moreover
   *     AXISC       it must be the case that AXISA and AXISB are distinct
   *                 and that AXISB and AXISC are distinct.
   *
   *                 Every rotation matrix can be represented as a product
   *                 of three rotation matrices about the principal axes
   *                 of a reference frame.
   *
   *                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
   *                                   AXISA        AXISB         AXISC
   *
   *                 The value 1 corresponds to the X axis.
   *                 The value 2 corresponds to the Y axis.
   *                 The value 3 corresponds to the Z axis.
   *
   *$ Detailed_Output
   *
   *     EULANG      is the set of Euler angles corresponding to the
   *                 specified factorization.
   *
   *                 If we represent R as shown here:
   *
   *                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
   *                                   AXISA        AXISB         AXISC
   *
   *                 then
   *
   *
   *                    EULANG(1) = ALPHA
   *                    EULANG(2) = BETA
   *                    EULANG(3) = GAMMA
   *                    EULANG(4) = dALPHA/dt
   *                    EULANG(5) = dBETA/dt
   *                    EULANG(6) = dGAMMA/dt
   *
   *                 The range of ALPHA and GAMMA is (-pi, pi].
   *
   *                 The range of BETA depends on the exact set of
   *                 axes used for the factorization.  For
   *                 factorizations in which the first and third axes
   *                 are the same, the range of BETA is [0, pi].
   *
   *                 For factorizations in which the first and third
   *                 axes are different, the range of BETA is
   *                 [-pi/2, pi/2].
   *
   *                 For rotations such that ALPHA and GAMMA are not
   *                 uniquely determined, ALPHA and dALPHA/dt will
   *                 always be set to zero; GAMMA and dGAMMA/dt are
   *                 then uniquely determined.
   *
   *     UNIQUE      is a logical that indicates whether or not the
   *                 values in EULANG are uniquely determined.  If
   *                 the values are unique then UNIQUE will be set to
   *                 TRUE.  If the values are not unique and some
   *                 components ( EULANG(1) and EULANG(4) ) have been set
   *                 to zero, then UNIQUE will have the value FALSE.
   *
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     All erroneous inputs are diagnosed by routines in the call
   *     tree to this routines.  These include
   *
   *     1)   If any of AXISA, AXISB, or AXISC do not have values in
   *
   *             { 1, 2, 3 },
   *
   *          then the error SPICE(INPUTOUTOFRANGE) is signaled.
   *
   *     2)   An arbitrary rotation matrix cannot be expressed using
   *          a sequence of Euler angles unless the second rotation axis
   *          differs from the other two.  If AXISB is equal to AXISC or
   *          AXISA, then the error SPICE(BADAXISNUMBERS) is signaled.
   *
   *     3)   If the input matrix R is not a rotation matrix, the error
   *          SPICE(NOTAROTATION) is signaled.
   *
   *     4)   If EULANG(1) and EULANG(3) are not uniquely determined,
   *          EULANG(1) is set to zero, and EULANG(3) is determined.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     A word about notation:  the symbol
   *
   *        [ x ]
   *             i
   *
   *     indicates a coordinate system rotation of x radians about the
   *     ith coordinate axis.  To be specific, the symbol
   *
   *        [ x ]
   *             1
   *
   *     indicates a coordinate system rotation of x radians about the
   *     first, or x-, axis; the corresponding matrix is
   *
   *        +-                    -+
   *        |  1      0       0    |
   *        |                      |
   *        |  0    cos(x)  sin(x) |.
   *        |                      |
   *        |  0   -sin(x)  cos(x) |
   *        +-                    -+
   *
   *     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
   *     matrix, when applied to a vector, rotates the vector by -x
   *     radians, not x radians.  Applying the matrix to a vector yields
   *     the vector's representation relative to the rotated coordinate
   *     system.
   *
   *     The analogous rotation about the second, or y-, axis is
   *     represented by
   *
   *        [ x ]
   *             2
   *
   *     which symbolizes the matrix
   *
   *        +-                    -+
   *        | cos(x)   0   -sin(x) |
   *        |                      |
   *        |  0       1      0    |,
   *        |                      |
   *        | sin(x)   0    cos(x) |
   *        +-                    -+
   *
   *     and the analogous rotation about the third, or z-, axis is
   *     represented by
   *
   *        [ x ]
   *             3
   *
   *     which symbolizes the matrix
   *
   *        +-                    -+
   *        |  cos(x)  sin(x)   0  |
   *        |                      |
   *        | -sin(x)  cos(x)   0  |.
   *        |                      |
   *        |  0        0       1  |
   *        +-                    -+
   *
   *
   *     The input matrix is assumed to be the product of three
   *     rotation matrices, each one of the form
   *
   *        +-                    -+
   *        |  1      0       0    |
   *        |                      |
   *        |  0    cos(r)  sin(r) |     (rotation of r radians about the
   *        |                      |      x-axis),
   *        |  0   -sin(r)  cos(r) |
   *        +-                    -+
   *
   *
   *        +-                    -+
   *        | cos(s)   0   -sin(s) |
   *        |                      |
   *        |  0       1      0    |     (rotation of s radians about the
   *        |                      |      y-axis),
   *        | sin(s)   0    cos(s) |
   *        +-                    -+
   *
   *     or
   *
   *        +-                    -+
   *        |  cos(t)  sin(t)   0  |
   *        |                      |
   *        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
   *        |                      |      z-axis),
   *        |  0        0       1  |
   *        +-                    -+
   *
   *     where the second rotation axis is not equal to the first or
   *     third.  Any rotation matrix can be factored as a sequence of
   *     three such rotations, provided that this last criterion is met.
   *
   *     This routine is related to the routine EUL2XF which produces
   *     a state transformation from an input set of axes, Euler angles
   *     and derivatives.
   *
   *     The two subroutine calls shown here will not change
   *     XFORM except for round off errors.
   *
   *     CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
   *     CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
   *
   *     On the other hand the two calls
   *
   *     CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
   *     CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
   *
   *     will leave EULANG unchanged only if the components of EULANG
   *     are in the range produced by EUL2XF and the Euler representation
   *     of the rotation component of XFORM is unique within that range.
   *
   *
   *$ Examples
   *
   *     Suppose that you wish to determine the rate of change of
   *     the right ascension and declination of the pole of an object,
   *     from the state transformation matrix that transforms J2000
   *     states to object fixed states.
   *
   *     Using this routine with the routine TISBOD you can determine
   *     these instanteous rates.
   *
   *     Recall that the rotation component of TSIPM is given by
   *
   *                   [W] [HALFPI-DEC] [RA+HALFPI]
   *                      3            1           3
   *
   *
   *     Thus the calls:
   *
   *     CALL TISBOD ( 'J2000', BODY, ET, TSIPM )
   *     CALL XF2EUL (  TSIPM,  3, 1, 3,  EULANG, UNIQUE )
   *
   *     yield the following:
   *
   *        EULANG(1) is  W
   *        EULANG(2) is  HALFPI - DEC
   *        EULANG(3) is  RA     + HALFPI
   *        EULANG(4) is  dW/dt
   *        EULANG(5) is -dDEC/dt
   *        EULANG(6) is  dRA/dt
   *
   *     Hence:
   *
   *        dDEC/dt = -EULANG(5)
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Literature_References
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
   *
   *      Corrected code in EUL2EF entry point Examples section, example
   *      showed a XF2EUL call:
   *
   *            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
   *
   *      The proper form of the call:
   *
   *            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
   *
   *-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
   *
   *        Entry point EUL2XF was updated to allow axis sequences
   *        in which the second angle is not distinct from the first
   *        or third.
   *
   *-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
   *
   *
   *-&
  
   *$ Index_Entries
   *
   *     Euler angles and derivatives from state transformation
   *
   *-&
   *
   *     Spicelib Functions.
   *
   *      LOGICAL               FAILED
   *      LOGICAL               RETURN
   *
   *
   *     The computation of the non-derivative terms EULANG is handled
   *     by the SPICE routine M2EUL.  This routine contributes by
   *     determining the derivative components of EULANG.
   *
   *     To understand the code below a rather lengthy derivation is
   *     required.  If you're not interested in the details of this
   *     derivation skip down to the  IF ( RETURN() ) THEN line of
   *     code below.
   *
   *     First we note that if b is one of the basis vectors i,j, or k
   *     or the opposite of one of these (-i, -j, or -k) then
   *
   *       [ ANGLE ]  * b  = COS( {1 - |<e_n,b>|}*ANGLE )b
   *                n
   *                       - SIN( ANGLE ) e_n x b
   *
   *     where <,> denotes the dot product, and x is used to denote the
   *     cross product operation and e_1, e_2, and e_3 are the standard
   *     basis vectors i, j, and k respectively.
   *
   *     Using M2EUL we can readily determine the values of ALPHA, BETA
   *     and GAMMA such that
   *
   *
   *        R   = [ ALPHA ]  [ BETA ]  [ GAMMA ]
   *                       A         B          C
   *
   *
   *    From this equation we have:
   *
   *        dR/dt =   dALPHA/dt OMEGA [ ALPHA ]  [ BETA ]  [ GAMMA ]
   *                                 A         A         B          C
   *
   *              +   dBETA/dt  [ ALPHA ] OMEGA  [ BETA ]  [ GAMMA ]
   *                                     A     B         B          C
   *
   *              +   dGAMMA/dt [ ALPHA ] [ BETA ]  OMEGA [ GAMMA ]
   *                                     A        B      C         C
   *
   *     where OMEGA   is the cross product matrix.
   *                n
   *
   *
   *         [   0      D_3n    -D_2n  ]
   *         |  -D_3n    0       D_1n  |
   *         [   D_2n  -D_1n      0    ]
   *
   *
   *     (D_ij   denotes the Kronecker delta.)  Note that OMEGA * v
   *                                                           n
   *     yields -e  x  v  for all vectors v.
   *              n
   *
   *     Multiplying both sides of the equation for dR/dt by the transpose
   *     of R yields:
   *
   *            T
   *     dR/dt*R  = dALPHA/dt OMEGA
   *                                A
   *
   *              + dBETA/dt  [ ALPHA ] OMEGA  [ -ALPHA ]
   *                                   A     B           A
   *
   *              + dGAMMA/dt [ ALPHA ] [ BETA ] OMEGA [ -BETA ]  [-ALPHA]
   *                                   A        B     C         B         A
   *                        T
   *     The product dR/dt*R  is a skew symmetric matrix and hence can
   *     be represented as a cross product,
   *               T
   *        dR/dt*R  V  = W x V
   *
   *     for all vectors V, provided that
   *
   *                       T
   *        W(1) =  dR/dt*R  (3,2)
   *
   *                       T
   *        W(2) =  dR/dt*R  (1,3)
   *
   *                       T
   *        W(3) =  dR/dt*R  (2,1)
   *
   *     For any vector V, there is a corresponding skew symmetric
   *     matrix CROSS{V}  such that CROSS{V} * W  = V x W for all vectors
   *     W.  Moreover, if ROT is any rotation, then
   *
   *                                           T
   *           CROSS{ROT(V)} = ROT CROSS{V} ROT
   *
   *     This can easily be verified by noting that
   *
   *        ROT(VxU) = ROT(V) X ROT(U)
   *
   *     From these observations it follows that
   *
   *
   *        W =   -dALPHA/dt e_A
   *
   *
   *          -    dBETA/dt [ALPHA]  e_B
   *                               A
   *
   *          -    dGAMMA/dt [ ALPHA ] [ BETA ] e_C
   *                                  A        B
   *
   *
   *        W =   -dALPHA/dt e_A
   *
   *
   *          -    dBETA/dt {    COS ( ALPHA (1 - |<e_A,e_B>|)) e_B
   *
   *                          -  SIN ( ALPHA ) e_A x e_B }
   *
   *
   *          -    dGAMMA/dt [ ALPHA ] {    COS(BETA(1 - |<e_B,e_C>|)) e_C
   *                                  A
   *                                     -  SIN (BETA) e_B x e_C }
   *
   *     But <e_A,e_B> = 0 = <e_B,e_C> so that the above expression
   *     simplifies to
   *
   *        W =   -dALPHA/dt e_A
   *
   *
   *          -    dBETA/dt {COS(ALPHA)e_B -  SIN(ALPHA) e_A x e_B}
   *
   *
   *          -    dGAMMA/dt [ ALPHA ] {COS(BETA)e_C - SIN(BETA)e_B x e_C}
   *                                  A
   *
   *     If we let L = 6 - A - B, then by construction e_L is the third
   *     vector needed to complete the basis containing e_A and e_B.
   *     Let D be +1 or -1, so that D*e_L = e_A x e_B
   *     (note D = <e_L,e_A x e_B> )
   *
   *     Then applying our rotation formula again and simplifying we have
   *
   *     W =   -dALPHA/dt e_A
   *
   *
   *       -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
   *
   *
   *       -  dGAMMA/dt COS(BETA){ COS(ALPHA(1-<e_A , e_C>))e_C
   *                              -SIN(ALPHA)   e_A x e_C }
   *
   *       +  dGAMMA/dt SIN(BETA){ COS(ALPHA(1-|<e_A,e_B x e_C>|))e_B x e_C
   *                              -SIN(ALPHA) e_A x (e_B x e_C )
   *
   *
   *     Now we have two cases: 1) e_A = e_C or 2)  e_C = e_L
   *
   *     Case 1. e_A = e_C
   *     ====================
   *
   *        W =   -dALPHA/dt e_A
   *
   *
   *          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
   *
   *
   *          -  dGAMMA/dt COS(BETA)e_A
   *
   *          -  dGAMMA/dt D*SIN(BETA)COS(ALPHA)e_L
   *
   *          -  dGAMMA/dt SIN(BETA)SIN(ALPHA)e_B
   *
   *
   *        W = e_A{-dALPHA/dt - COS(BETA)dGAMMA/dt}
   *          + e_B{ -COS(ALPHA)dBETA/dt -   SIN(ALPHA)SIN(BETA)dGAMMA/dt}
   *          + e_L{D*SIN(ALPHA)dBETA/dt - D*COS(ALPHA)SIN(BETA)dGAMMA/dt}
   *
   *
   *        let U =    COS(BETA)
   *            V =  D*SIN(BETA)
   *
   *        then
   *
   *        W = e_A{-dALPHA/dt                                -U*dGAMMA/dt}
   *          + e_B{         -COS(ALPHA)dBETA/dt -D*SIN(ALPHA)*V*dGAMMA/dt}
   *          + e_L{        D*SIN(ALPHA)dBETA/dt   -COS(ALPHA)*V*dGAMMA/dt}
   *
   *
   *     Case 2. e_L = e_C
   *     ====================
   *
   *        W =   -dALPHA/dt e_A
   *
   *
   *          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
   *
   *
   *          -  dGAMMA/dt COS(BETA){ COS(ALPHA)e_L
   *                                 -D*SIN(ALPHA)e_B }
   *
   *          +  dGAMMA/dt SIN(BETA) D*e_A
   *
   *
   *       W  = e_A{-dALPHA/dt + D*SIN(BETA)dGAMMA/dt}
   *          + e_B{-COS(ALPHA)dBETA/dt  - D*SIN(ALPHA)COS(BETA)dGAMMA/dt}
   *          + e_L{D*SIN(ALPHA)dBETA/dt -   COS(ALPHA)COS(BETA)dGAMMA/dt}
   *
   *
   *       Let U = -D*SIN(BETA)
   *           V =    COS(BETA)
   *
   *       then
   *
   *       W  = e_A{-dALPHA/dt                  -              U*dGAMMA/dt}
   *          + e_B{       -COS(ALPHA)*dBETA/dt - D*SIN(ALPHA)*V*dGAMMA/dt}
   *          + e_L{      D*SIN(ALPHA)dBETA/dt  -   COS(ALPHA)*V*dGAMMA/dt}
   *
   *     As we can see from the above, by choosing appropriate assignments
   *     for U and V, the two cases can be unified in a single expression.
   *
   *     Substituting CA and SA for COS(ALPHA) and SIN(ALPHA) and
   *     re-writing the last expression in matrix form we have:
   *
   *
   *                          [ -1     0      0 ][ 1  0  U ] [dALPHA/dt]
   *      W  = {e_A  e_B  e_L}|  0   -CA  -D*SA || 0  1  0 | |dBETA /dt|
   *                          [  0  D*SA    -CA ][ 0  0  V ] [dGAMMA/dt]
   *
   *
   *     If we let E_n stand for the transpose of e_n, then solving for
   *     the derivative vector we have:
   *
   *     [dALPHA/dt]   [ 1 0 -U/V ] [ -1     0     0] [ E_A ]
   *     |dBETA /dt| = | 0 1   0  | |  0   -CA  D*SA| | E_B | W
   *     [dGAMMA/dt]   [ 0 0  1/V ] [  0 -D*SA   -CA] [ E_L ]
   *
   *
   *     But since the matrix product E_n W is <e_n,W> = W(n) this can
   *     be rewritten as
   *
   *     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ W(A) ]
   *     |dBETA /dt| = |  0   -CA      D*SA   | [ W(B) |
   *     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ W(L) ]
   *
   *
   *     Thus we see that there is a relatively elementary computation
   *     required to determine the derivatives of the three Euler angles
   *     returned by M2EUL.
   *
   *
   * </pre>
   */
  public static final XF2EUL_OUT XF2EUL(final ImmutableDoubleArray XFORM, final int AXISA,
      final int AXISB, final int AXISC) {
    ImmutableDoubleArray OUT =
        ImmutableDoubleArray.copyOf(_XF2EUL(XFORM.toArray(), AXISA, AXISB, AXISC));
    return new XF2EUL_OUT(OUT.subArray(0, 5).trimmed(), OUT.get(6) != 0);
  }

  /**
   * <pre>
   * *$Procedure ZZENUT80 ( Earth nutation transformation, IAU 1980 model )
   *
   *      SUBROUTINE ZZENUT80 ( ET, NUTXF )
   *      IMPLICIT NONE
   *
   *$ Abstract
   *
   *     SPICE Private routine intended solely for the support of SPICE
   *     routines.  Users should not call this routine directly due
   *     to the volatile nature of this routine.
   *
   *     Compute the state transformation matrix implementing the IAU 1980
   *     nutation model.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     FRAMES
   *     MATRIX
   *     PRIVATE
   *     TRANSFORMATION
   *     UTILITY
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION      ET
   *      DOUBLE PRECISION      NUTXF  ( 6, 6 )
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  -------------------------------------------------
   *     ET         I   Ephemeris time, seconds past J2000.
   *     NUTXF      O   Nutation transformation matrix.
   *
   *$ Detailed_Input
   *
   *     ET             is an epoch, expressed as seconds past J2000 TDB.
   *
   *$ Detailed_Output
   *
   *     NUTXF          is a state transformation matrix that maps states
   *                    from the earth mean equator and equinox of date
   *                    frame (based on the 1976 IAU precession model) to
   *                    the earth true equator and equinox frame of date
   *                    (based on the 1980 IAU nutation model).
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     See the private SPICELIB routine ZZWAHR for a discussion
   *     of the implementation of the 1980 IAU nutation model.
   *
   *     See the private SPICELIB routine ZZMOBLIQ for a discussion
   *     of the implementation of the 1980 IAU earth mean obliquity
   *     of date model.
   *
   *$ Examples
   *
   *     See ZZDYNFRM.
   *
   *$ Restrictions
   *
   *     1) This is a SPICE private routine; the routine is subject
   *        to change without notice.  User applications should not
   *        call this routine.
   *
   *$ Author_and_Institution
   *
   *     N.J. Bachman    (JPL)
   *
   *$ Literature_References
   *
   *     [1] "Explanatory Supplement to the Astronomical Almanac"
   *          edited by P. Kenneth Seidelmann. University Science
   *          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
   *
   *     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
   *         Acceleration Vectors of Tracking Station" by T. D. Moyer.
   *         Draft of JPL Publication documenting the JPL navigation
   *         program "Regres."
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
   *
   *-&
   * </pre>
   */
  public static final ImmutableDoubleArray ZZENUT80(final double ET) {
    return ImmutableDoubleArray.copyOf(_ZZENUT80(ET));
  }

  /**
   * <pre>
   *$Procedure   ZZEPRC76   ( Earth precession, 1976 IAU model )
   *
   *      SUBROUTINE ZZEPRC76 ( ET, PRECXF )
   *      IMPLICIT NONE
   *
   *$ Abstract
   *
   *     SPICE Private routine intended solely for the support of SPICE
   *     routines.  Users should not call this routine directly due
   *     to the volatile nature of this routine.
   *
   *     Compute the state transformation matrix implementing the IAU 1976
   *     precession model.
   *
   *$ Required_Reading
   *
   *     ROTATION
   *
   *$ Keywords
   *
   *     FRAMES
   *     GEOMETRY
   *     MATRIX
   *     PRIVATE
   *     TRANSFORMATION
   *
   *$ Declarations
   *
   *      DOUBLE PRECISION      ET
   *      DOUBLE PRECISION      PRECXF ( 6, 6 )
   *
   *$ Brief_I/O
   *
   *     Variable  I/O  Description
   *     --------  ---  --------------------------------------------------
   *     ET         I   Ephemeris time, in seconds past J2000 TDB.
   *     PRECXF     O   Precession state transformation matrix at ET.
   *
   *$ Detailed_Input
   *
   *     ET             is the epoch at which the precession matrix is
   *                    to be computed.  ET is barycentric dynamical time,
   *                    expressed as seconds past J2000.
   *
   *$ Detailed_Output
   *
   *     PRECXF         is a 6x6 matrix that transforms states from the
   *                    J2000 frame to the mean equator and equinox frame
   *                    of the earth at the epoch ET.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     According to reference [2], the precession model used in this
   *     routine is that used in the JPL navigation program "Regres."
   *
   *     The precession matrix is defined using the Euler angles
   *
   *        zeta ,   z ,  and theta
   *            A     A            A
   *
   *
   *     Equation (5-147) of [2] gives the matrix determined by these
   *     angles as
   *
   *        A  =  [ -z   ]   [ theta  ]   [ -zeta  ]
   *                  A   3         A  2         A  3
   *
   *
   *     Formulas for the Euler angles are from [2], equation
   *     (5-143):
   *                                              2                3
   *         zeta   =  2306".2181*T  +  0".30188*T   +  0".017998*T
   *             A
   *
   *
   *                                              2                3
   *         z      =  2306".2181*T  +  1".09468*T   +  0".018203*T
   *          A
   *
   *
   *                                              2                3
   *         theta  =  2004".3109*T  -  0".42665*T   -  0".041833*T
   *              A
   *
   *$ Examples
   *
   *     1) Convert a state vector S from J2000 to Earth Mean equator and
   *        equinox of date coordinates at epoch ET.  Call the resulting
   *        vector SMOD.
   *
   *           CALL ZZEPRC76 ( ET,     PRECXF        )
   *           CALL MXVG     ( PRECXF, S, 6, 6, SMOD )
   *
   *$ Restrictions
   *
   *     1) This is a SPICE private routine; the routine is subject to
   *        change without notice.  User applications should not call this
   *        routine.
   *
   *     2) Though reference [1] does not specify limitations on the range
   *        of valid time inputs for this precession model, the fact that
   *        the rotation angles used in the model are defined by
   *        polynomials implies that the model is not valid for all time.
   *
   *$ Literature_References
   *
   *     [1] "Explanatory Supplement to the Astronomical Almanac"
   *          edited by P. Kenneth Seidelmann. University Science
   *          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
   *
   *     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
   *         Acceleration Vectors of Tracking Station" by T. D. Moyer.
   *         Draft of JPL Publication documenting the JPL navigation
   *         program "Regres."
   *
   *
   *$ Author_and_Institution
   *
   *     N.J. Bachman       (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 2.0.1, 18-APR-2016 (NJB)
   *
   *        Corrected typo in header abstract: changed "1876"
   *        to "1976."
   *
   *-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     IAU 1976 earth precession transformation
   *
   *		 *     SPICELIB functions
   *
   *     DOUBLE PRECISION      JYEAR
   *     DOUBLE PRECISION      RPD
   *-&
   * </pre>
   */
  public static final ImmutableDoubleArray ZZEPRC76(final double ET) {
    return ImmutableDoubleArray.copyOf(_ZZEPRC76(ET));
  }

  /**
   *
   * <p>
   * Output from {@link Spicelib#ZZMOBLIQ}
   * <p>
   * public final double MOB;
   * <p>
   * public final double DMOB;
   *
   * @author Douglas Rodgers <Douglas.Rodgers@jhuapl.edu>
   *
   */
  public static class ZZMOBLIQ_OUT {
    public final double MOB, DMOB;

    public ZZMOBLIQ_OUT(final double MOB, final double DMOB) {
      this.MOB = MOB;
      this.DMOB = DMOB;
    }
  }

  /**
   * <pre>
   *$Procedure   ZZMOBLIQ   ( Mean obliquity of date )
   *
   *      SUBROUTINE ZZMOBLIQ ( ET, MOB, DMOB )
   *
   *$ Abstract
   *
   *     Return the mean obliquity of the ecliptic, and its time
   *     derivative, at a specified epoch.
   *
   *     SPICE Private routine intended solely for the support of SPICE
   *     routines.  Users should not call this routine directly due
   *     to the volatile nature of this routine.
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     GEOMETRY
   *
   *$ Declarations
   *
   *      IMPLICIT NONE
   *      DOUBLE PRECISION      ET
   *      DOUBLE PRECISION      MOB
   *      DOUBLE PRECISION      DMOB
   *
   *$ Brief_I/O
   *
   *     Variable  I/O  Description
   *     --------  ---  --------------------------------------------------
   *     ET         I   Ephemeris time, in seconds past J2000.
   *     MOB        O   Mean obliquity of the ecliptic at ET.
   *     DMOB       O   Time derivative of the mean obliquity.
   *
   *$ Detailed_Input
   *
   *     ET             is the epoch at which the obliquity of the ecliptic
   *                    is to be computed.  ET is barycentric dynamical
   *                    time, expressed as seconds past J2000.
   *
   *$ Detailed_Output
   *
   *     MOB            is the mean obliquity of the ecliptic at epoch ET.
   *                    The mean obliquity of the ecliptic is the
   *                    inclination of the ecliptic of date to the mean
   *                    Earth equator of date.  Output units are radians.
   *
   *     DMOB           is the time derivative of MOB at ET, expressed in
   *                    radians per second.
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Files
   *
   *     None.
   *
   *$ Particulars
   *
   *     The expression for mean is obliquity is
   *
   *                          ''        ''            ''         2
   *        MOBLIQ   =   84381 .448 - 46 .8150 * T - 0 .00059 * T
   *
   *                      ''          3
   *                   + 0 .001813 * T
   *
   *     where T indicates Julian centuries past J2000.  This is from
   *     equation 5-153 of reference [2].
   *
   *$ Examples
   *
   *     See the routine ENUTAT for an example of usage.
   *
   *$ Restrictions
   *
   *     1)  This is a preliminary version of the routine.
   *
   *$ Literature_References
   *
   *     [1] "Explanatory Supplement to the Astronomical Almanac"
   *          edited by P. Kenneth Seidelmann. University Science
   *          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
   *
   *     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
   *         Acceleration Vectors of Tracking Station" by T. D. Moyer.
   *         Draft of JPL Publication documenting the JPL navigation
   *         program "Regres."
   *
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber         (JPL)
   *     N.J. Bachman       (JPL)
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.0 18-JUL-1997 (WLT)
   *
   *        Adapted Nat'routine to private version making output
   *        rate be radians/sec.
   *
   *-    Beta Version 1.0.0, 29-SEP-1996 (NJB)
   *
   *-&
   *
   *$ Index_Entries
   *
   *     compute mean obliquity of date of the ecliptic
   *
   *-&
   *
   *     SPICELIB functions
   *
   *      DOUBLE PRECISION      JYEAR
   *      DOUBLE PRECISION      RPD
   * </pre>
   */
  public static final ZZMOBLIQ_OUT ZZMOBLIQ(final double ET) {
    /**
     *
     *
     * Local variables
     *
     */
    double RAD = RPD();
    double YEAR = JYEAR();
    double PERSEC = 1.0 / (YEAR * 100.0);

    /**
     *
     * Convert the input epoch to Julian centuries past J2000:
     *
     */
    double T = (ET / YEAR) / 100.0;

    /**
     *
     * Compute the obliquity at epoch. The polynomial yields arcseconds; convert the units to
     * radians.
     *
     */
    double MOB =
        (RAD / 3.6e3) * (ZZMOBLIQ_C0 + T * (ZZMOBLIQ_C1 + T * (ZZMOBLIQ_C2 + T * ZZMOBLIQ_C3)));
    double DMOB =
        (RAD / 3.6e3) * (ZZMOBLIQ_C1 + T * (2 * ZZMOBLIQ_C2 + T * 3 * ZZMOBLIQ_C3)) * PERSEC;

    return new ZZMOBLIQ_OUT(MOB, DMOB);

  }

  /**
   * <pre>
   *$Procedure      ZZWAHR ( SPICELIB private version of Newhalls' WAHR )
   *
   *      SUBROUTINE ZZWAHR ( ET, DVNUT )
   *
   *$ Abstract
   *
   *     Calculates nutation angles delta psi and delta epsilon,  and
   *     their rates of change, referred to the ecliptic of date, from
   *     the wahr series (Table 1,'Proposal to the IAU Working Group
   *     on Nutation', John M. Wahr and Martin L. Smith 1979)
   *
   *$ Required_Reading
   *
   *     None.
   *
   *$ Keywords
   *
   *     NUTATIONS
   *
   *$ Declarations
   *
   *     IMPLICIT NONE
   *     DOUBLE PRECISION      ET
   *     DOUBLE PRECISION      DVNUT(4)
   *
   *$ Brief_I/O
   *
   *     VARIABLE  I/O  DESCRIPTION
   *     --------  ---  --------------------------------------------------
   *     ET         I   Ephemeris Time for which nutations are sought
   *     DVNUT      O   Nutation angles and their rates.
   *
   *$ Detailed_Input
   *
   *     ET         is the epoch for which nutation angles are being
   *                requested expressed in TDB seconds past the epoch
   *                of J2000.
   *
   *$ Detailed_Output
   *
   *     DVNUT      are the nutation angles and their derivatives.
   *                Following the notation on page 112 of the
   *                Explanatory Supplement to the Astronomical
   *                Almanac we have
   *
   *                DVNUT(1) = Psi------nutation in longitude (radians)
   *                DVNUT(2) = Epsilon--nutation in obliquity (radians)
   *                DVNUT(3) = dPsi/dt     (radians/second)
   *                DVNUT(4) = dEpsilon/dt (radians/second)
   *
   *$ Parameters
   *
   *     None.
   *
   *$ Files
   *
   *     None.
   *
   *$ Exceptions
   *
   *     Error free.
   *
   *$ Particulars
   *
   *     This routine computes the angles required for computing the
   *     transformation from the mean of date frame for the earth
   *     to the true of date frame of the earth.
   *
   *$ Examples
   *
   *     None.
   *
   *$ Restrictions
   *
   *     None.
   *
   *$ Author_and_Institution
   *
   *     W.L. Taber      (JPL)
   *
   *$ Literature_References
   *
   *     Explanatory Supplement to the Astronomical Almanac edited
   *     by P. Kenneth Siedelmann. (1992) (University Science
   *     Books, Mill Valley CA) pp. 111-116
   *
   *$ Version
   *
   *-    SPICELIB Version 1.0.0, 15-JUL-1997 (WLT)
   *
   *        This routine was adapted from a routine provided by
   *        Skip Newhall.  Skip's notes indicate that he obtained this
   *        from Jay Lieske and Mylse Standish.  The actual notes
   *        from the original routine WAHR are given here.
   *
   *           Lieske 3/91.  NUTATION in the IAU J2000 system.  Univac
   *           version obtained from Myles Standish, (subroutine WAHR)
   *           who had obtained it from USNO.  Re-ordered terms to match
   *           Astronomical Almanac 1984 table S23-S25 and corrected
   *           the rate for dPsi in the 0 0 2 -2 2 term.  Eliminated
   *           the equivalences, common block and added necessary SAVEs.
   *           Corrected the fundamental angles (L, L', F, D, Node) to
   *           match Almanac.
   *
   *        In the current routine the various angles L, L', F, D, and
   *        Node (MG) are computed using the actual values given
   *        in the Explanatory Supplement.
   *
   *        Note that there is an error in the Explanatory supplement
   *        for the Node term.  The Explanatory Supplement (page 114) has
   *
   *          OMEGA = 135 degrees 2 minutes 40.280 seconds
   *                +  etc.
   *
   *        The correct formulation should be:
   *
   *          OMEGA = 125 degrees 2 minutes 40.280 seconds
   *                +  etc.
   *
   *        This is the value used in this routine.  The verification of
   *        this error is courtesy of Myles Standish.
   *
   *
   *-&
   *
   *     SPICELIB Functions
   *
   *     DOUBLE PRECISION      PI
   *     DOUBLE PRECISION      TWOPI
   *     DOUBLE PRECISION      SPD
   * </pre>
   */
  public static final ImmutableDoubleArray ZZWAHR(final double ET) {
    return ImmutableDoubleArray.copyOf(_ZZWAHR(ET));
  }

  /**
   * ############################################################################# PRIVATE METHODS
   * ##############################################################################
   */

  private static final double _DET(final double[] M1) {
    return M1[0] * (M1[4] * M1[8] - M1[7] * M1[5]) - M1[3] * (M1[1] * M1[8] - M1[7] * M1[2])
        + M1[6] * (M1[1] * M1[5] - M1[4] * M1[2]);
  }

  private static final double[] _EUL2M(final double ANGLE3, final double ANGLE2,
      final double ANGLE1, final int AXIS3, final int AXIS2, final int AXIS1) {
    /**
     *
     * Make sure the axis numbers are all right: They must belong to the set {1, 2, 3}.
     *
     *
     * IF ( BADAX ) THEN
     *
     * CALL SETMSG ( 'Axis numbers are #, #, #. ' ) CALL ERRINT ( '#', AXIS3 ) CALL ERRINT ( '#',
     * AXIS2 ) CALL ERRINT ( '#', AXIS1 ) CALL SIGERR ( 'SPICE(BADAXISNUMBERS)' ) CALL CHKOUT (
     * 'EUL2M' ) RETURN
     *
     * END IF
     */
    boolean BADAX = ((AXIS3 < 1) || (AXIS3 > 3)) || ((AXIS2 < 1) || (AXIS2 > 3))
        || ((AXIS1 < 1) || (AXIS1 > 3));

    Preconditions.checkArgument(!BADAX, "Bad axes in EUL2M");

    /**
     *
     * Just do it.
     *
     * CALL ROTATE ( ANGLE1, AXIS1, R ) CALL ROTMAT ( R, ANGLE2, AXIS2, R1 ) CALL ROTMAT ( R1,
     * ANGLE3, AXIS3, R )
     */
    double[] R = _ROTATE(ANGLE1, AXIS1);
    double[] R1 = _ROTMAT(R, ANGLE2, AXIS2);
    double[] ROUT = _ROTMAT(R1, ANGLE3, AXIS3);

    return ROUT;

  }

  private static final double[] _EUL2XF(final double[] EULANG, final int AXISA, final int AXISB,
      final int AXISC) {

    /**
     * Standard SPICE error handling.
     *
     * IF ( RETURN() ) THEN RETURN END IF
     *
     * CALL CHKIN ( 'EUL2XF' )
     *
     *
     * We're going to work with a local copy LOCANG of the euler angle state vector EULANG. We'll
     * also use a local set of axis numbers.
     *
     *
     * CALL MOVED ( EULANG, 6, LOCANG );
     */

    double[] LOCANG = Arrays.copyOf(EULANG, 6);

    int LOCAXA = AXISA;
    int LOCAXB = AXISB;
    int LOCAXC = AXISC;

    /**
     *
     * Parts of the following algorithm depend on the central axis being different from the first
     * and third axes. We'll adjust the axes and angles to make this so, if necessary.
     *
     */
    if ((AXISB == AXISA) || (AXISB == AXISC)) {

      int I = 0;

      if (AXISB == AXISA) {
        /**
         *
         * The first angle will "absorb" the second, and the second will be set to zero. All we do
         * here is select the first angle.
         *
         */
        I = 0;
      } else {
        I = 2;
      }

      /**
       *
       * Absorb the second angle into the selected angle and set the second angle to zero. The same
       * goes for the angular rates.
       *
       */
      LOCANG[I] = LOCANG[I] + LOCANG[1];
      LOCANG[1] = 0.0;

      LOCANG[I + 3] = LOCANG[I + 3] + LOCANG[4];
      LOCANG[4] = 0.0;

      /**
       *
       * Pick a second axis that doesn't match the others. Since the rotation angle about the second
       * axis is zero, all that matters here is picking a distinct axis.
       *
       */
      if (AXISC == XF2EUL_NEXT[AXISA - 1]) {

        /**
         *
         * The first axis is the predecessor of the third, so we pick the successor of the third.
         *
         */
        LOCAXB = XF2EUL_NEXT[AXISC - 1];

      } else {

        /**
         *
         * Either the third axis is the predecessor of the first or matches the first, so the
         * successor of the first is our choice.
         *
         */
        LOCAXB = XF2EUL_NEXT[AXISA - 1];

      }

    }

    /**
     *
     * The following local variables are set:
     *
     * LOCANG(*), LOCAXA, LOCAXB, LOCAXC
     *
     * These variables describe the input rotation, but the second axis is now guaranteed to differ
     * from the first and third.
     *
     * The derivation for everything that is about to happen here is included in the previous entry
     * point.
     *
     */
    double[] R = _EUL2M(LOCANG[XF2EUL_ALPHA], LOCANG[XF2EUL_BETA], LOCANG[XF2EUL_GAMMA], LOCAXA,
        LOCAXB, LOCAXC);

    /**
     * IF ( FAILED() ) THEN CALL CHKOUT ( 'EUL2XF' ) RETURN END IF
     */

    /**
     *
     * Construct local copies of the axes, determine L and D from the derivation above.
     *
     */
    int A = LOCAXA - 1;
    int B = LOCAXB - 1;
    int L = 3 - A - B;
    double D = XF2EUL_DELTA[A + 3 * B];

    /**
     *
     * Compute the various sines and cosines that we need.
     *
     */
    double CA = Math.cos(LOCANG[XF2EUL_ALPHA]);
    double SA = Math.sin(LOCANG[XF2EUL_ALPHA]);

    double U, V;
    if (LOCAXA == LOCAXC) {
      U = Math.cos(LOCANG[XF2EUL_BETA]);
      V = D * Math.sin(LOCANG[XF2EUL_BETA]);
    } else {
      U = -D * Math.sin(LOCANG[XF2EUL_BETA]);
      V = Math.cos(LOCANG[XF2EUL_BETA]);
    }

    /**
     *
     * t Next we compute dR/dt R. Recall from the derivation above that
     *
     *
     * [ W(A) ] [ -1 0 -U ][dALPHA/dt] | W(B) | = | 0 -CA -D*SA*V ||dBETA /dt| [ W(L) ] [ 0 D*SA
     * -CA*V ][dGAMMA/dt]
     *
     * In the previous entry point we used OMEGA for the vector of rearranged components of W.
     *
     * OMEGA(1) = W(A) = D*DRDTRT(L,B) OMEGA(2) = W(B) = D*DRDTRT(A,L) OMEGA(3) = W(L) =
     * D*DRDTRT(B,A)
     *
     * DRDTRT(L,B) = D*OMEGA(1) DRDTRT(A,L) = D*OMEGA(2) DRDTRT(B,A) = D*OMEGA(3)
     *
     * [ DRDTRT(L,B) ] [ -D 0 -D*U ][dALPHA/dt] | DRDTRT(A,L) | = | 0 -D*CA -SA*V ||dBETA /dt| [
     * DRDTRT(B,A) ] [ 0 SA -D*CA*V ][dGAMMA/dt]
     *
     * We set up the matrix of this equation in SOLUTN below and compute D*OMEGA which we denote by
     * the variable DOMEGA.
     *
     */
    double[] SOLUTN = {-D, 0.0, 0.0, 0.0, -D * CA, SA, -D * U, -SA * V, -D * CA * V};

    double[] DOMEGA = _MXV(SOLUTN, new double[] {LOCANG[3], LOCANG[4], LOCANG[5]});

    double[] DRDTRT = new double[9];

    DRDTRT[L + 3 * B] = DOMEGA[0];
    DRDTRT[B + 3 * L] = -DOMEGA[0];

    DRDTRT[A + 3 * L] = DOMEGA[1];
    DRDTRT[L + 3 * A] = -DOMEGA[1];

    DRDTRT[B + 3 * A] = DOMEGA[2];
    DRDTRT[A + 3 * B] = -DOMEGA[2];

    DRDTRT[0] = 0.0;
    DRDTRT[4] = 0.0;
    DRDTRT[8] = 0.0;

    double[] DRDT = _MXM(DRDTRT, R);

    // DO J = 1,3
    // DO I = 1,3
    // XFORM(I, J ) = R (I,J)
    // XFORM(I+3,J+3) = R (I,J)
    // XFORM(I+3,J ) = DRDT(I,J)
    // XFORM(I, J+3) = 0.0D0
    // END DO
    // END DO

    double[] XFORM = new double[36];
    for (int J = 0; J < 3; J++) {
      int K = 3 * J;
      int KK = 2 * K;
      for (int I = 0; I < 3; I++, K++, KK++) {
        XFORM[KK] = R[K];
        XFORM[KK + 21] = R[K];
        XFORM[KK + 3] = DRDT[K];
        XFORM[KK + 18] = 0.0;
      }
    }

    return XFORM;

  }

  private static final boolean _ISROT(final double[] M, final double NTOL, final double DTOL) {
    /**
     *
     * Tolerances must be non-negative.
     *
     */
    Preconditions.checkArgument(NTOL >= 0.0, "Must have NTOL >= 0.0");
    Preconditions.checkArgument(DTOL >= 0.0, "Must have DTOL >= 0.0");

    /**
     *
     * The columns of M must resemble unit vectors. If the norms are outside of the allowed range, M
     * is not a rotation matrix.
     *
     * Also, the columns of M are required to be pretty nearly orthogonal. The discrepancy is gauged
     * by taking the determinant of the matrix UNIT, computed below, whose columns are the unitized
     * columns of M.
     *
     */
    double[] C1 = _UNORM(new double[] {M[0], M[1], M[2]});
    double[] C2 = _UNORM(new double[] {M[3], M[4], M[5]});
    double[] C3 = _UNORM(new double[] {M[6], M[7], M[8]});
    double[] UNIT = {C1[0], C1[1], C1[2], C2[0], C2[1], C2[2], C3[0], C3[1], C3[2]};
    double N1 = C1[3];
    double N2 = C2[3];
    double N3 = C3[3];

    double D = _DET(UNIT);

    boolean NORMOK = (N1 == BRCKTD(N1, 1.0 - NTOL, 1.0 + NTOL))
        && (N2 == BRCKTD(N2, 1.0 - NTOL, 1.0 + NTOL)) && (N3 == BRCKTD(N3, 1.0 - NTOL, 1.0 + NTOL));

    boolean DETOK = D == BRCKTD(D, 1.0 - DTOL, 1.0 + DTOL);

    if (NORMOK && DETOK) {
      return true;
    }

    return false;

  }

  private static final double[] _M2EUL(final double[] R, final int AXIS3, final int AXIS2,
      final int AXIS1) {

    // Output: note reverse order
    // ANGLE[0] = ANGLE3
    // ANGLE[1] = ANGLE2
    // ANGLE[2] = ANGLE1
    double[] ANGLE = new double[3];

    int[] NEXT = {2, 3, 1};

    /**
     *
     * The first order of business is to screen out the goofy cases.
     *
     * Make sure the axis numbers are all right: They must belong to the set {1, 2, 3}...
     *
     */
    boolean BADAX = ((AXIS3 < 1) || (AXIS3 > 3)) || ((AXIS2 < 1) || (AXIS2 > 3))
        || ((AXIS1 < 1) || (AXIS1 > 3));
    Preconditions.checkArgument(!BADAX, "Bad axes in M2EUL");

    /**
     *
     * ...and the second axis number must differ from its neighbors.
     *
     */
    BADAX = (AXIS3 == AXIS2) || (AXIS1 == AXIS2);
    Preconditions.checkArgument(!BADAX, "Middle axis matches neighbor in M2EUL");

    /**
     *
     * R must be a rotation matrix, or we may as well forget it.
     *
     */
    boolean GOODROT = _ISROT(R, M2EUL_NTOL, M2EUL_DTOL);
    Preconditions.checkArgument(GOODROT, "Input matrix is not a rotation");

    /**
     *
     * AXIS3, AXIS2, AXIS1 and R have passed their tests at this point. We take the liberty of
     * working with TMPROT, a version of R that has unitized columns.
     *
     */
    double[] TMPROTC1 = _VHAT(new double[] {R[0], R[3], R[6]});
    double[] TMPROTC2 = _VHAT(new double[] {R[1], R[4], R[7]});
    double[] TMPROTC3 = _VHAT(new double[] {R[2], R[5], R[8]});
    double[] TMPROT = new double[] {TMPROTC1[0], TMPROTC2[0], TMPROTC3[0], TMPROTC1[1], TMPROTC2[1],
        TMPROTC3[1], TMPROTC1[2], TMPROTC2[2], TMPROTC3[2],};

    /**
     *
     * We now proceed to recover the promised Euler angles from TMPROT.
     *
     * The ideas behind our method are explained in excruciating detail in the ROTATION required
     * reading, so we'll be terse. Nonetheless, a word of explanation is in order.
     *
     * The sequence of rotation axes used for the factorization belongs to one of two categories:
     * a-b-a or c-b-a. We wish to handle each of these cases in one shot, rather than using
     * different formulas for each sequence to recover the Euler angles.
     *
     * What we're going to do is use the Euler angle formula for the 3-1-3 factorization for all of
     * the a-b-a sequences, and the formula for the 3-2-1 factorization for all of the c-b-a
     * sequences.
     *
     * How can we get away with this? The Euler angle formulas for each factorization are different!
     *
     * Our trick is to apply a change-of-basis transformation to the input matrix R. For the a-b-a
     * factorizations, we choose a new basis such that a rotation of ANGLE3 radians about the basis
     * vector indexed by AXIS3 becomes a rotation of ANGLE3 radians about the third coordinate axis,
     * and such that a rotation of ANGLE2 radians about the basis vector indexed by AXIS2 becomes a
     * rotation of ANGLE2 radians about the first coordinate axis. So R can be factored as a 3-1-3
     * rotation relative to the new basis, and the Euler angles we obtain are the exact ones we
     * require.
     *
     * The c-b-a factorizations can be handled in an analogous fashion. We transform R to a basis
     * where the original axis sequence becomes a 3-2-1 sequence. In some cases, the angles we
     * obtain will be the negatives of the angles we require. This will happen if and only if the
     * ordered basis (here the e's are the standard basis vectors)
     *
     * { e e e } AXIS3 AXIS2 AXIS1
     *
     * is not right-handed. An easy test for this condition is that AXIS2 is not the successor of
     * AXIS3, where the ordering is cyclic.
     *
     */
    if (AXIS3 == AXIS1) {

      /**
       *
       * The axis order is a-b-a. We're going to find a matrix CHANGE such that
       *
       * T CHANGE R CHANGE
       *
       * gives us R in the a useful basis, that is, a basis in which our original a-b-a rotation is
       * a 3-1-3 rotation, but where the rotation angles are unchanged. To achieve this pleasant
       * simplification, we set column 3 of CHANGE to to e(AXIS3), column 1 of CHANGE to e(AXIS2),
       * and column 2 of CHANGE to
       *
       * (+/-) e(C),
       *
       * (C is the remaining index) depending on whether AXIS3-AXIS2-C is a right-handed sequence of
       * axes: if it is, the sign is positive. (Here e(1), e(2), e(3) are the standard basis
       * vectors.)
       *
       * Determine the sign of our third basis vector, so that we can ensure that our new basis is
       * right-handed. The variable NEXT is just a little mapping that takes 1 to 2, 2 to 3, and 3
       * to 1.
       *
       */
      double SIGN = -1.0;
      if (AXIS2 == NEXT[AXIS3 - 1]) {
        SIGN = 1.0;
      }

      /**
       *
       * Since the axis indices sum to 6,
       *
       */
      int C = 5 - AXIS3 - AXIS2;

      /**
       *
       * Set up the entries of CHANGE:
       *
       * CALL CLEARD ( 9, CHANGE )
       *
       * CHANGE ( AXIS3, 3 ) = 1.D0 CHANGE ( AXIS2, 1 ) = 1.D0 CHANGE ( C, 2 ) = SIGN * 1.D0
       */
      double[] CHANGE = new double[6];
      CHANGE[AXIS3 + 5] = 1.0;
      CHANGE[AXIS2 - 1] = 1.0;
      CHANGE[C + 3] = SIGN * 1.0;

      /**
       *
       * Transform TMPROT.
       *
       */
      TMPROT = _MTXM(CHANGE, _MXM(TMPROT, CHANGE));

      /**
       *
       * Now we're ready to find the Euler angles, using a 3-1-3 factorization. In general, the
       * matrix product
       *
       * [ a1 ] [ a2 ] [ a3 ] 3 1 3
       *
       * has the form
       *
       * +- -+ | cos(a1)cos(a3) cos(a1)sin(a3) sin(a1)sin(a2) | | -sin(a1)cos(a2)sin(a3)
       * +sin(a1)cos(a2)cos(a3) | | | | -sin(a1)cos(a3) -sin(a1)sin(a3) cos(a1)sin(a2) | |
       * -cos(a1)cos(a2)sin(a3) +cos(a1)cos(a2)cos(a3) | | | | sin(a2)sin(a3) -sin(a2)cos(a3)
       * cos(a2) | +- -+
       *
       *
       * but if a2 is 0 or pi, the product matrix reduces to
       *
       *
       * +- -+ | cos(a1)cos(a3) cos(a1)sin(a3) 0 | | -sin(a1)cos(a2)sin(a3) +sin(a1)cos(a2)cos(a3) |
       * | | | -sin(a1)cos(a3) -sin(a1)sin(a3) 0 | | -cos(a1)cos(a2)sin(a3) +cos(a1)cos(a2)cos(a3) |
       * | | | 0 0 cos(a2) | +- -+
       *
       *
       * In this case, a1 and a3 are not uniquely determined. If we arbitrarily set a1 to zero, we
       * arrive at the matrix
       *
       * +- -+ | cos(a3) sin(a3) 0 | | -cos(a2)sin(a3) cos(a2)cos(a3) 0 | | 0 0 cos(a2) | +- -+
       *
       * We take care of this case first. We test three conditions that are mathematically
       * equivalent, but may not be satisfied simultaneously because of round-off:
       *
       */
      boolean DEGEN = ((TMPROT[6] == 0.0) && (TMPROT[7] == 0.0))
          || ((TMPROT[2] == 0.0) && (TMPROT[5] == 0.0)) || (Math.abs(TMPROT[8]) == 1.0);

      /**
       *
       * In the following block of code, we make use of the fact that
       *
       * SIN ( ANGLE2 ) > 0 - in choosing the signs of the ATAN2 arguments correctly. Note that
       * ATAN2(x,y) = -ATAN2(-x,-y).
       *
       */
      if (DEGEN) {

        ANGLE[0] = 0.0;
        ANGLE[1] = Math.acos(TMPROT[8]);
        ANGLE[2] = Math.atan2(TMPROT[3], TMPROT[0]);

      } else {

        /**
         *
         * The normal case.
         *
         */

        ANGLE[0] = Math.atan2(TMPROT[6], TMPROT[7]);
        ANGLE[1] = Math.acos(TMPROT[8]);
        ANGLE[2] = Math.atan2(TMPROT[2], -TMPROT[5]);

      }



    } else {

      /**
       *
       * The axis order is c-b-a. We're going to find a matrix CHANGE such that
       *
       * T CHANGE R CHANGE
       *
       * gives us R in the a useful basis, that is, a basis in which our original c-b-a rotation is
       * a 3-2-1 rotation, but where the rotation angles are unchanged, or at worst negated. To
       * achieve this pleasant simplification, we set column 1 of CHANGE to to e(AXIS3), column 2 of
       * CHANGE to e(AXIS2), and column 3 of CHANGE to
       *
       * (+/-) e(AXIS1),
       *
       * depending on whether AXIS3-AXIS2-AXIS1 is a right-handed sequence of axes: if it is, the
       * sign is positive. (Here e(1), e(2), e(3) are the standard basis vectors.)
       *
       * We must be cautious here, because if AXIS3-AXIS2-AXIS1 is a right-handed sequence of axes,
       * all of the rotation angles will be the same in our new basis, but if it's a left-handed
       * sequence, the third angle will be negated. Let's get this straightened out right now. The
       * variable NEXT is just a little mapping that takes 1 to 2, 2 to 3, and 3 to 1.
       *
       */
      double SIGN;
      if (AXIS2 == NEXT[AXIS3 - 1]) {
        SIGN = 1.0;
      } else {
        SIGN = -1.0;
      }

      /**
       *
       * Set up the entries of CHANGE:
       *
       * CALL CLEARD ( 9, CHANGE )
       *
       * CHANGE ( AXIS3, 1 ) = 1.D0 CHANGE ( AXIS2, 2 ) = 1.D0 CHANGE ( AXIS1, 3 ) = SIGN * 1.D0
       *
       */
      double[] CHANGE = new double[6];
      CHANGE[AXIS3 - 1] = 1.0;
      CHANGE[AXIS2 + 2] = 1.0;
      CHANGE[AXIS1 + 5] = SIGN * 1.0;

      /**
       *
       * Transform TMPROT.
       *
       */
      TMPROT = _MTXM(CHANGE, _MXM(TMPROT, CHANGE));

      /**
       *
       * Now we're ready to find the Euler angles, using a 3-2-1 factorization. In general, the
       * matrix product
       *
       * [ a1 ] [ a2 ] [ a3 ] 1 2 3
       *
       * has the form
       *
       *
       * +- -+ | cos(a2)cos(a3) cos(a2)sin(a3) -sin(a2) | | | | -cos(a1)sin(a3) cos(a1)cos(a3)
       * sin(a1)cos(a2) | | +sin(a1)sin(a2)cos(a3) +sin(a1)sin(a2)sin(a3) | | | | sin(a1)sin(a3)
       * -sin(a1)cos(a3) cos(a1)cos(a2) | | +cos(a1)sin(a2)cos(a3) +cos(a1)sin(a2)sin(a3) | +- -+
       *
       *
       * but if a2 is -pi/2 or pi/2, the product matrix reduces to
       *
       *
       * +- -+ | 0 0 -sin(a2) | | | | -cos(a1)sin(a3) cos(a1)cos(a3) 0 | | +sin(a1)sin(a2)cos(a3)
       * +sin(a1)sin(a2)sin(a3) | | | | sin(a1)sin(a3) -sin(a1)cos(a3) 0 | | +cos(a1)sin(a2)cos(a3)
       * +cos(a1)sin(a2)sin(a3) | +- -+
       *
       *
       * In this case, a1 and a3 are not uniquely determined. If we arbitrarily set a1 to zero, we
       * arrive at the matrix
       *
       * +- -+ | 0 0 -sin(a2) | | -sin(a3) cos(a3) 0 |, | sin(a2)cos(a3) sin(a2)sin(a3) 0 | +- -+
       *
       *
       * We take care of this case first. We test three conditions that are mathematically
       * equivalent, but may not be satisfied simultaneously because of round-off:
       *
       *
       */
      boolean DEGEN = ((TMPROT[0] == 0.0) && (TMPROT[3] == 0.0))
          || ((TMPROT[7] == 0.0) && (TMPROT[8] == 0.0)) || (Math.abs(TMPROT[6]) == 1.0);

      /**
       *
       * In the following block of code, we make use of the fact that
       *
       * COS ( ANGLE2 ) > 0 - in choosing the signs of the ATAN2 arguments correctly. Note that
       * ATAN2(x,y) = -ATAN2(-x,-y).
       *
       */
      if (DEGEN) {

        ANGLE[0] = 0.0;
        ANGLE[1] = Math.asin(-TMPROT[6]);
        ANGLE[2] = SIGN * Math.atan2(-TMPROT[1], TMPROT[4]);

      } else {

        /**
         *
         * The normal case.
         *
         */
        ANGLE[0] = Math.atan2(TMPROT[7], TMPROT[8]);
        ANGLE[1] = Math.asin(-TMPROT[6]);
        ANGLE[2] = SIGN * Math.atan2(TMPROT[3], TMPROT[0]);

      }

    }

    return ANGLE;

  }

  private static final double[] _MTXM(final double[] M1, final double[] M2) {
    double[] MOUT = new double[9];
    for (int J = 0, K = 0, KK = 0; J < 3; J++, K += 3) {
      for (int I = 0, L = 0; I < 3; I++, KK++, L += 3) {
        MOUT[KK] = M1[L] * M2[K] + M1[L + 1] * M2[K + 1] + M1[L + 2] * M2[K + 2];
      }
    }
    return MOUT;
  }

  private static final double[] _MXM(final double[] M1, final double[] M2) {
    double[] MOUT = new double[9];
    for (int J = 0, K = 0, KK = 0; J < 3; J++, K += 3) {
      for (int I = 0; I < 3; I++, KK++) {
        MOUT[KK] = M1[I] * M2[K] + M1[I + 3] * M2[K + 1] + M1[I + 6] * M2[K + 2];
      }
    }
    return MOUT;
  }

  private static final double[] _MXMT(final double[] M1, final double[] M2) {
    double[] MOUT = new double[9];
    for (int J = 0, KK = 0; J < 3; J++) {
      for (int I = 0; I < 3; I++, KK++) {
        MOUT[KK] = M1[I] * M2[J] + M1[I + 3] * M2[J + 3] + M1[I + 6] * M2[J + 6];
      }
    }
    return MOUT;
  }

  private static final double[] _MXV(final double[] MATRIX, final double[] VIN) {
    double[] VOUT = new double[3];
    for (int I = 0; I < 3; I++) {
      VOUT[I] = MATRIX[I] * VIN[0] + MATRIX[I + 3] * VIN[1] + MATRIX[I + 6] * VIN[2];
    }
    return VOUT;
  }

  private static final double[] _ROTATE(final double ANGLE, final int IAXIS) {
    /**
     *
     * Get the sine and cosine of ANGLE
     *
     */
    int[] INDEXS = {2, 0, 1, 2, 0};
    double S = Math.sin(ANGLE);
    double C = Math.cos(ANGLE);

    /**
     *
     * Get indices for axes. The first index is for the axis of rotation. The next two axes follow
     * in right hand order (XYZ). First get the non-negative value of IAXIS mod 3 .
     *
     */
    int TEMP = ((IAXIS % 3) + 3) % 3;

    int I1 = INDEXS[TEMP];
    int I2 = INDEXS[TEMP + 1];
    int I3 = INDEXS[TEMP + 2];
    int J1 = 3 * I1;
    int J2 = 3 * I2;
    int J3 = 3 * I3;

    /**
     *
     * Construct the rotation matrix
     *
     */
    double[] MOUT = new double[9];
    MOUT[I1 + J1] = 1.0;
    MOUT[I2 + J1] = 0.0;
    MOUT[I3 + J1] = 0.0;
    MOUT[I1 + J2] = 0.0;
    MOUT[I2 + J2] = C;
    MOUT[I3 + J2] = -S;
    MOUT[I1 + J3] = 0.0;
    MOUT[I2 + J3] = S;
    MOUT[I3 + J3] = C;

    return MOUT;

  }

  private static final double[] _ROTMAT(final double[] M1, final double ANGLE, final int IAXIS) {
    /**
     *
     * Get the sine and cosine of ANGLE
     *
     */
    int[] INDEXS = {2, 0, 1, 2, 0};
    double S = Math.sin(ANGLE);
    double C = Math.cos(ANGLE);

    /**
     *
     * Get indices for axes. The first index is for the axis of rotation. The next two axes follow
     * in right hand order (XYZ). First get the non-negative value of IAXIS mod 3 .
     *
     */
    int TEMP = ((IAXIS % 3) + 3) % 3;

    int I1 = INDEXS[TEMP];
    int I2 = INDEXS[TEMP + 1];
    int I3 = INDEXS[TEMP + 2];

    /**
     *
     * Calculate the output matrix column by column
     *
     */
    double[] MOUT = new double[9];
    for (int I = 0; I < 9; I += 3) {
      MOUT[I1 + I] = M1[I1 + I];
      MOUT[I2 + I] = C * M1[I2 + I] + S * M1[I3 + I];
      MOUT[I3 + I] = -S * M1[I2 + I] + C * M1[I3 + I];
    }

    /**
     *
     * Move the buffered matrix into MOUT.
     *
     * CALL MOVED (PRODM, 9, MOUT)
     */

    return MOUT;

  }


  private static final double[] _UNORM(final double[] V1) {

    /**
     *
     * Obtain the magnitude of V1
     *
     */
    double VMAG = _VNORM(V1);

    /**
     *
     * If VMAG is nonzero, then normalize. Note that this process is numerically stable: overflow
     * could only happen if VMAG were small, but this could only happen if each component of V1 were
     * small. In fact, the magnitude of any vector is never less than the magnitude of any
     * component.
     *
     */
    double V10 = 0.0, V11 = 0.0, V12 = 0.0;
    if (VMAG > 0.0) {
      V10 = V1[0] / VMAG;
      V11 = V1[1] / VMAG;
      V12 = V1[2] / VMAG;
    }

    return new double[] {V10, V11, V12, VMAG};

  }

  private static final double[] _VHAT(final double[] V1) {

    /**
     *
     * Obtain the magnitude of V1
     *
     */
    double VMAG = _VNORM(V1);

    /**
     *
     * If VMAG is nonzero, then normalize. Note that this process is numerically stable: overflow
     * could only happen if VMAG were small, but this could only happen if each component of V1 were
     * small. In fact, the magnitude of any vector is never less than the magnitude of any
     * component.
     *
     */
    double V10 = 0.0, V11 = 0.0, V12 = 0.0;
    if (VMAG > 0.0) {
      V10 = V1[0] / VMAG;
      V11 = V1[1] / VMAG;
      V12 = V1[2] / VMAG;
    }

    return new double[] {V10, V11, V12};

  }


  private static final double _VNORM(final double[] V1) {
    /**
     *
     * Determine the maximum component of the vector.
     *
     */
    double V1MAX = Math.abs(V1[0]);
    V1MAX = Math.max(V1MAX, Math.abs(V1[1]));
    V1MAX = Math.max(V1MAX, Math.abs(V1[2]));

    /**
     *
     * If the vector is zero, return zero; otherwise normalize first. Normalizing helps in the cases
     * where squaring would cause overflow or underflow. In the cases where such is not a problem it
     * not worth it to optimize further.
     *
     */
    double VNORM = 0;
    if (V1MAX > 0) {
      double V10 = V1[0] / V1MAX;
      double V11 = V1[1] / V1MAX;
      double V12 = V1[2] / V1MAX;
      VNORM = V1MAX * Math.sqrt(V10 * V10 + V11 * V11 + V12 * V12);
    }

    return VNORM;
  }



  private static final double[] _XF2EUL(final double[] XFORM, final int AXISA, final int AXISB,
      final int AXISC) {

    /**
     *
     * Get the rotation and derivative of the rotation separately.
     *
     */
    double[] R = new double[6];
    double[] DRDT = new double[6];
    for (int J = 0; J < 3; J++) {
      int K = 3 * J;
      int KK = 2 * K;
      for (int I = 0; I < 3; I++, K++, KK++) {
        R[K] = XFORM[KK];
        DRDT[K] = XFORM[KK + 3];
      }
    }

    /**
     *
     * We have to do it sooner or later so we take care of getting the various Euler angles now.
     * This will take care of all the bad axis cases too so we don't have to check here.
     *
     */
    double[] TMPANG = _M2EUL(R, AXISA, AXISB, AXISC);
    double[] EULANG = new double[6];
    EULANG[XF2EUL_ALPHA] = TMPANG[0];
    EULANG[XF2EUL_BETA] = TMPANG[1];
    EULANG[XF2EUL_GAMMA] = TMPANG[2];

    /**
     * IF ( FAILED() ) THEN CALL CHKOUT ( 'XF2EUL' ) RETURN END IF
     */

    /**
     *
     * Construct local copies of the axes, determine L and D from the derivation above.
     *
     */
    int A = AXISA - 1;
    int B = AXISB - 1;
    int L = 6 - A - B;
    double D = XF2EUL_DELTA[A + 3 * B];

    /**
     *
     * t Compute DR/DT * R and extract OMEGA
     *
     */
    double[] DRDTRT = _MXMT(DRDT, R);

    /**
     *
     * The vector corresponding to DRDTRT is computed as shown below.
     *
     * w(1) = drdtrt (3,2) w(2) = drdtrt (1,3) w(3) = drdtrt (2,1)
     *
     * However, we need the 3-vector
     *
     * w(A) w(B) w(L)
     *
     * We'll call this vector omega. It's computed as shown here.
     *
     * omega(1) = w(A) = d*drdtrt(L,B) omega(2) = w(B) = d*drdtrt(A,L) omega(3) = w(L) =
     * d*drdtrt(B,A)
     *
     */
    double[] OMEGA = new double[3];
    OMEGA[0] = D * DRDTRT[L + 3 * B];
    OMEGA[1] = D * DRDTRT[A + 3 * L];
    OMEGA[2] = D * DRDTRT[B + 3 * A];

    /**
     *
     * Compute the various sines and cosines that we need.
     *
     */

    double CA = Math.cos(EULANG[XF2EUL_ALPHA]);
    double SA = Math.sin(EULANG[XF2EUL_ALPHA]);

    double U, V;
    if (AXISA == AXISC) {
      U = Math.cos(EULANG[XF2EUL_BETA]);
      V = D * Math.sin(EULANG[XF2EUL_BETA]);
    } else {
      U = -D * Math.sin(EULANG[XF2EUL_BETA]);
      V = Math.cos(EULANG[XF2EUL_BETA]);
    }

    /**
     *
     * To avoid floating point overflows we make sure that we can perform a division by V. We do
     * this by looking at U. If it has absolute value 1, then we set V equal to zero. After all U*U
     * + V*V = 1 if SIN and COS and various arithmetic operations work perfectly.
     *
     */
    if (Math.abs(U) == 1.0) {
      V = 0.0;
    }

    /**
     *
     * We have to look at the singular case first. Recall from above that
     *
     * [ W(A) ] [ -1 0 -U ][dALPHA/dt] | W(B) | = | 0 -CA -D*SA*V ||dBETA /dt| [ W(C) ] [ 0 D*SA
     * -CA*V ][dGAMMA/dt]
     *
     * The singularity arises if V = 0. In this case the equation becomes: ( Note that U is plus or
     * minus 1 so that division by U is the same as multiplication by U. )
     *
     * [ OMEGA(1) ] [ -1 0 -U ][dALPHA/dt] | OMEGA(2) | = | 0 -CA 0 ||dBETA /dt| [ OMEGA(3) ] [ 0
     * D*SA 0 ][dGAMMA/dt]
     *
     */
    boolean UNIQUE;
    if (V == 0) {

      UNIQUE = false;
      EULANG[XF2EUL_DALPHA] = 0.0;
      EULANG[XF2EUL_DGAMMA] = -U * OMEGA[0];

      /**
       *
       * We solve for EULANG(DBETA) by selecting the more stable of the two available equations.
       *
       */
      if (Math.abs(CA) > Math.abs(SA)) {
        EULANG[XF2EUL_DBETA] = -OMEGA[1] / CA;
      } else {
        EULANG[XF2EUL_DBETA] = D * OMEGA[2] / SA;
      }

      /**
       * CALL CHKOUT ( 'XF2EUL' )
       */

    } else {

      UNIQUE = true;

      /**
       *
       * The matrix needed to compute the derivatives uniquely exists. Construct it and carry out
       * the multiplication.
       *
       * [dALPHA/dt] [ -1 U*D*SA/V U*CA/V ] [ OMEGA(1) ] |dBETA /dt| = | 0 -CA D*SA | [ OMEGA(2) |
       * [dGAMMA/dt] [ 0 -D*SA/V -CA/V ] [ OMEGA(3) ]
       *
       */

      double[] SOLUTN = new double[9];

      SOLUTN[0] = -1.0;
      SOLUTN[1] = 0.0;
      SOLUTN[2] = 0.0;

      SOLUTN[3] = U * D * SA / V;
      SOLUTN[4] = -CA;
      SOLUTN[5] = -D * SA / V;

      SOLUTN[6] = U * CA / V;
      SOLUTN[7] = D * SA;
      SOLUTN[8] = -CA / V;

      TMPANG = _MXV(SOLUTN, OMEGA);
      EULANG[XF2EUL_DALPHA] = TMPANG[0];
      EULANG[XF2EUL_DBETA] = TMPANG[1];
      EULANG[XF2EUL_DGAMMA] = TMPANG[2];

      /**
       * CALL CHKOUT ( 'XF2EUL' )
       */

    }

    return new double[] {EULANG[XF2EUL_ALPHA], EULANG[XF2EUL_BETA], EULANG[XF2EUL_GAMMA],
        EULANG[XF2EUL_DALPHA], EULANG[XF2EUL_DBETA], EULANG[XF2EUL_DGAMMA], UNIQUE ? 1 : 0};

  }

  private static final double[] _ZZENUT80(final double ET) {
    /**
     * SPICELIB functions
     *
     * LOGICAL RETURN
     *
     *
     * Local variables
     *
     * DOUBLE PRECISION DMOB DOUBLE PRECISION DVNUT ( 4 ) DOUBLE PRECISION EULANG ( 6 ) DOUBLE
     * PRECISION MOB
     *
     *
     * IF ( RETURN() ) THEN RETURN END IF
     *
     * CALL CHKIN ( 'ZZENUT80' )
     *
     *
     * Get nutation angles and their rates. We're expecting
     *
     * DVNUT(1) = Psi------nutation in longitude (radians) DVNUT(2) = Epsilon--nutation in obliquity
     * (radians) DVNUT(3) = dPsi/dt (radians/second) DVNUT(4) = dEpsilon/dt (radians/second)
     *
     * CALL ZZWAHR ( ET, DVNUT )
     */
    double[] DVNUT = _ZZWAHR(ET);

    /**
     *
     * Get the mean obliquity of date.
     *
     * We're expecting the outputs to be as follows:
     *
     * MOB is the mean obliquity of the ecliptic at epoch ET. The mean obliquity of the ecliptic is
     * the inclination of the ecliptic of date to the mean Earth equator of date. Output units are
     * radians.
     *
     * DMOB is the time derivative of MOB at ET, expressed in radians per second.
     *
     * CALL ZZMOBLIQ ( ET, MOB, DMOB )
     */
    ZZMOBLIQ_OUT ZZMOB_OUT = ZZMOBLIQ(ET);

    /**
     *
     * The nutation rotation N is defined by
     *
     *
     * N = [ -MOB - NUOBL ] [ -NULON ] [ MOB ] 1 3 1
     *
     * where MOBLIQ is the mean obliquity of the earth's ecliptic at epoch, NUOB is nutation in
     * obliquity at epoch, and NULONG is nutation in longitude at epoch. Using our variable names,
     * the Euler angle sequence is
     *
     * [ -MOB - DVNUT(2) ] [ -DVNUT(1) ] [ MOB ] 1 3 1
     *
     * The rates corresponding to these angles are:
     *
     * -DMOB - DVNUT(4), -DVNUT(3), DMOB
     *
     * We can use EUL2XF to form the state transformation from the nutation base frame to the
     * nutation frame.
     *
     * EULANG(1) = -MOB - DVNUT(2) EULANG(2) = - DVNUT(1) EULANG(3) = MOB; EULANG(4) = -DMOB -
     * DVNUT(4) EULANG(5) = - DVNUT(3) EULANG(6) = DMOB
     *
     */
    double[] EULANG = {-ZZMOB_OUT.MOB - DVNUT[1], -DVNUT[0], ZZMOB_OUT.MOB,
        -ZZMOB_OUT.DMOB - DVNUT[3], -DVNUT[2], ZZMOB_OUT.DMOB};

    /**
     * CALL EUL2XF ( EULANG, 1, 3, 1, NUTXF )
     *
     * CALL CHKOUT ( 'ZZENUT80' ) RETURN END
     */

    return _EUL2XF(EULANG, 1, 3, 1);

  }

  private static final double[] _ZZEPRC76(final double ET) {
    /**
     *
     *
     *
     * Local parameters
     *
     *
     * Local variables
     *
     * DOUBLE PRECISION CENT DOUBLE PRECISION DTHETA DOUBLE PRECISION DZ DOUBLE PRECISION DZETA
     * DOUBLE PRECISION EULANG ( 6 ) DOUBLE PRECISION SCALE DOUBLE PRECISION T DOUBLE PRECISION
     * THETA DOUBLE PRECISION TS DOUBLE PRECISION Z DOUBLE PRECISION ZETA
     *
     * According to reference [2], the precession model used in this routine is that used in the JPL
     * navigation program "Regres."
     *
     * The precession matrix is defined using the Euler angles
     *
     * zeta , z , and theta A A A
     *
     *
     * Equation (5-147) of [2] gives the matrix determined by these angles as
     *
     * A = [ -z ] [ theta ] [ -zeta ] A 3 A 2 A 3
     *
     *
     * Formulas for the Euler angles are from [2], equation (5-143): 2 3 zeta = 2306".2181*T +
     * 0".30188*T + 0".017998*T A
     *
     *
     * 2 3 z = 2306".2181*T + 1".09468*T + 0".018203*T A
     *
     *
     * 2 3 theta = 2004".3109*T - 0".42665*T - 0".041833*T A
     *
     * No check-in required; this routine does not participate in SPICELIB error handling.
     *
     *
     * Compute the precession angles first. The time argument has units of Julian centuries. The
     * polynomial expressions yield angles in units of arcseconds prior to scaling. After scaling,
     * the angles are in units of radians.
     *
     */
    double CENT = JYEAR() * 100.0;
    double T = ET / CENT;
    double SCALE = RPD() / 3600.0;

    double ZETA = T * (ZZEPRC76_ZETA1 + T * (ZZEPRC76_ZETA2 + T * ZZEPRC76_ZETA3)) * SCALE;
    double Z = T * (ZZEPRC76_Z1 + T * (ZZEPRC76_Z2 + T * ZZEPRC76_Z3)) * SCALE;
    double THETA = T * (ZZEPRC76_THETA1 + T * (ZZEPRC76_THETA2 + T * ZZEPRC76_THETA3)) * SCALE;

    double TS = 1.0 / CENT;
    double DZETA =
        TS * (ZZEPRC76_ZETA1 + T * (2 * ZZEPRC76_ZETA2 + 3 * T * ZZEPRC76_ZETA3)) * SCALE;
    double DZ = TS * (ZZEPRC76_Z1 + T * (2 * ZZEPRC76_Z2 + 3 * T * ZZEPRC76_Z3)) * SCALE;
    double DTHETA =
        TS * (ZZEPRC76_THETA1 + T * (2 * ZZEPRC76_THETA2 + 3 * T * ZZEPRC76_THETA3)) * SCALE;

    /**
     *
     * Now compute the precession matrix.
     *
     * EULANG(1) = -Z EULANG(2) = THETA EULANG(3) = -ZETA EULANG(4) = -DZ EULANG(5) = DTHETA
     * EULANG(6) = -DZETA
     *
     * CALL EUL2XF ( EULANG, 3, 2, 3, PRECXF )
     */
    double[] EULANG = {-Z, THETA, -ZETA, -DZ, DTHETA, -DZETA};

    return _EUL2XF(EULANG, 3, 2, 3);

  }


  private static final double[] _ZZWAHR(final double ET) {
    /**
     *
     * Local Variables.
     *
     */
    double DPI = PI();
    double DTWOPI = TWOPI();
    double RADIAN = 180.0 / DPI;
    double RASEC = 3600.0 * RADIAN;
    double FACTR = 1.0e4 * RASEC;
    double ONEDAY = SPD();

    /**
     *
     * The next set of parameters is an enumeration of the various angles needed in the computation
     * of nutations.
     *
     */
    int L = 0; // corrected for zero-based arrays
    int LP = L + 1;
    int F = LP + 1;
    int D = F + 1;
    int MG = D + 1;

    /**
     *
     * Compute all of the various time components. DJ is the delta in the Julian date from the J2000
     * epoch.
     *
     */
    double DJ = ET / ONEDAY;
    double DD = DJ / 1.0e4;
    double DDDJ = DD / 1.0e4;
    double DD2 = DD * DD;
    double T = DJ / 365250.0;

    /**
     *
     * Now compute all of the various angles and their rates at the current epoch
     *
     */
    double[] ANGLE = new double[5];
    ANGLE[L] = ZZWAHR_L0 + DJ * ZZWAHR_L1 + (ZZWAHR_L2 + DD * ZZWAHR_L3) * DD2;
    ANGLE[LP] = ZZWAHR_LP0 + DJ * ZZWAHR_LP1 + (ZZWAHR_LP2 + DD * ZZWAHR_LP3) * DD2;
    ANGLE[F] = ZZWAHR_F0 + DJ * ZZWAHR_F1 + (ZZWAHR_F2 + DD * ZZWAHR_F3) * DD2;
    ANGLE[D] = ZZWAHR_D0 + DJ * ZZWAHR_D1 + (ZZWAHR_D2 + DD * ZZWAHR_D3) * DD2;
    ANGLE[MG] = ZZWAHR_MG0 + DJ * ZZWAHR_MG1 + (ZZWAHR_MG2 + DD * ZZWAHR_MG3) * DD2;
    double[] ANGRT = new double[5];
    ANGRT[L] = ZZWAHR_L1 + DDDJ * (2.0 * ZZWAHR_L2 + 3.0 * DD * ZZWAHR_L3);
    ANGRT[LP] = ZZWAHR_LP1 + DDDJ * (2.0 * ZZWAHR_LP2 + 3.0 * DD * ZZWAHR_LP3);
    ANGRT[F] = ZZWAHR_F1 + DDDJ * (2.0 * ZZWAHR_F2 + 3.0 * DD * ZZWAHR_F3);
    ANGRT[D] = ZZWAHR_D1 + DDDJ * (2.0 * ZZWAHR_D2 + 3.0 * DD * ZZWAHR_D3);
    ANGRT[MG] = ZZWAHR_MG1 + DDDJ * (2.0 * ZZWAHR_MG2 + 3.0 * DD * ZZWAHR_MG3);

    /**
     *
     * Wrap all of the angles and rates to range from 0 to 360, then convert to radians.
     *
     */
    for (int J = 0; J < 5; J++) {
      ANGLE[J] %= 360.0;
      ANGRT[J] %= 360.0;
      ANGLE[J] /= RADIAN;
      ANGRT[J] /= RADIAN;
    }

    /**
     *
     * Zero out the components of the nutation array
     *
     */
    double[] DVNUT = new double[4];

    /**
     *
     * Now we accumulate the various terms of Delta Psi and Delta epsilon as expressed on page 115
     * of the Green Book (Explanatory Supplement to the Astronomical Almanac).
     *
     */
    for (int I = 0; I < ZZWAHR_NTERM; I++) {
      double ARG = 0;
      double ARGRT = 0;
      int JJ = 9 * I;
      for (int J = 0; J < 5; J++, JJ++) {
        if (ZZWAHR_MATRIX[JJ] != 0) {
          ARG += ZZWAHR_MATRIX[JJ] * ANGLE[J];
          ARGRT += ZZWAHR_MATRIX[JJ] * ANGRT[J];
          ARG %= DTWOPI;
        }
      }
      double CL = ZZWAHR_MATRIX[JJ];
      if (ZZWAHR_MATRIX[++JJ] != 0) {
        CL += ZZWAHR_MATRIX[JJ] * T;
      }
      double CE = ZZWAHR_MATRIX[++JJ];
      if (ZZWAHR_MATRIX[++JJ] != 0) {
        CE += ZZWAHR_MATRIX[JJ] * T;
      }
      double COSANG = Math.cos(ARG);
      double SINANG = Math.sin(ARG);
      DVNUT[0] += CL * SINANG / FACTR;
      DVNUT[1] += CE * COSANG / FACTR;
      DVNUT[2] += CL * COSANG * ARGRT / FACTR;
      DVNUT[3] += CE * SINANG * ARGRT / FACTR;
    }
    /**
     *
     * Finally convert DVNUT(3) and DVNUT(4) to radians/second
     *
     */
    DVNUT[2] /= ONEDAY;
    DVNUT[3] /= ONEDAY;

    return DVNUT;

  }

  public static void main(String[] args) {

    { // BRCKTD
      double NUMBER, END1, END2;
      NUMBER = 123.0;
      END1 = 34.0;
      END2 = 74.0;
      System.out.println(BRCKTD(NUMBER, END1, END2));
      END1 = 123.0;
      END2 = 34.0;
      NUMBER = 74.0;
      System.out.println(BRCKTD(NUMBER, END1, END2));
      END2 = 123.0;
      NUMBER = 34.0;
      END1 = 74.0;
      System.out.println(BRCKTD(NUMBER, END1, END2));
      END2 = 123.0;
      END1 = 34.0;
      NUMBER = 74.0;
      System.out.println(BRCKTD(NUMBER, END1, END2));
      System.out.println();
    }

    { // DET
      double[] M1 = {1.54, 2.44, 3.69483, 4.45, 5.0, 6.3895385, 7.390349, 8.0300404, 9.23333};
      System.out.println(DET(ImmutableDoubleArray.copyOf(M1)));
      System.out.println();
    }

    { // EUL2M


    }

    { // ROTATE
      double ANGLE = -2.3456 * PI() * PI() / 1.234;
      int IAXIS = 1;
      ImmutableDoubleArray MOUT = ROTATE(ANGLE, IAXIS);
      int i = 0;
      for (double val : MOUT.toArray()) {
        System.out.println((++i) + "    " + val);
      }
      System.out.println();
    }

    { // ROTMAT
      ImmutableDoubleArray M1 = ImmutableDoubleArray.of(1, 2, 3, 4, 5, 6, 7, 8, 9);
      double ANGLE = -2.3456 * PI() / 1.234;
      int IAXIS = 3;
      ImmutableDoubleArray MOUT = ROTMAT(M1, ANGLE, IAXIS);
      int i = 0;
      for (double val : MOUT.toArray()) {
        System.out.println((++i) + "    " + val);
      }
      System.out.println();
    }

    { // ZZENUT80
      double ET = 1.304858e8;
      ImmutableDoubleArray NUTXF = ZZENUT80(ET);
      int i = 0;
      for (double val : NUTXF.toArray()) {
        System.out.println((++i) + "    " + val);
      }
      System.out.println();
    }

    { // ZZEPRC76
      double ET = 1.304858e8;
      ImmutableDoubleArray PRECXF = ZZEPRC76(ET);
      int i = 0;
      for (double val : PRECXF.toArray()) {
        System.out.println((++i) + "    " + val);
      }
      System.out.println();
    }

  }

}
