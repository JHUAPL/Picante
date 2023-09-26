package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJ;
import picante.math.vectorspace.VectorIJK;

/**
 * 
 * @author G.K.Stephens
 * 
 */
public class CoordConverters {

  private static final SphericalCoordConverter sphericalCoordConverter =
      new SphericalCoordConverter();
  private static final LatitudinalCoordConverter latitudinalCoordConverter =
      new LatitudinalCoordConverter();
  private static final RaDecCoordConverter raDecCoordConverter = new RaDecCoordConverter();
  private static final CylindricalCoordConverter cylindricalCoordConverter =
      new CylindricalCoordConverter();

  private CoordConverters() {}

  /**
   * Converts a Cartesian position to spherical position.
   * 
   * @param cartesian A {@link UnwritableVectorIJK} holding the Cartesian position.
   * @param sphericalBuffer A {@link SphericalVector} buffer holding the spherical position;
   * @return a reference to buffer for convenience.
   */
  public static SphericalVector convertToSpherical(UnwritableVectorIJK cartesian) {
    return sphericalCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts a spherical position to a Cartesian position.
   * 
   * @param spherical A {@link SphericalVector} holding the spherical position.
   * @param cartesianBuffer A {@link VectorIJK} buffer holding the Cartesian position.
   * @return a reference to buffer for convenience.
   */
  public static UnwritableVectorIJK convert(SphericalVector spherical) {
    return sphericalCoordConverter.toCartesian(spherical);
  }

  /**
   * Converts a Cartesian state to a spherical state.
   * 
   * @param cartesian A {@link CartesianState} holding the Cartesian state.
   * @param sphericalBuffer A {@link SphericalState} holding the state in this coordinate system.
   * @return a reference to buffer for convenience.
   */
  public static SphericalState convertToSpherical(CartesianState cartesian) {
    return (SphericalState) sphericalCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts a spherical state to a Cartesian state.
   * 
   * @param spherical A {@link SphericalState} holding the Spherical state.
   * @param cartesianBuffer A {@link WritableState} buffer holding the Cartesian state.
   * @return a reference to buffer for convenience.
   */
  public static CartesianState convert(SphericalState spherical) {
    return (CartesianState) sphericalCoordConverter.toCartesian(spherical);
  }

  /**
   * Converts from Cartesian coordinates to Latitudinal coordinates
   * 
   * @param cartesian
   * @param LatitudinalBuffer
   * @return
   */
  public static LatitudinalVector convertToLatitudinal(UnwritableVectorIJK cartesian) {
    return latitudinalCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts from Latitudinal coordinates to Cartesian coordinates
   * 
   * @param Latitudinal
   * @param cartesianBuffer
   * @return
   */
  public static UnwritableVectorIJK convert(LatitudinalVector Latitudinal) {
    return latitudinalCoordConverter.toCartesian(Latitudinal);
  }

  /**
   * Converts from Cartesian states to Latitudinal states
   * 
   * @param cartesian
   * @param LatitudinalBuffer
   * @return
   */
  public static LatitudinalState convertToLatitudinal(CartesianState cartesian) {
    return (LatitudinalState) latitudinalCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts from Latitudinal states to Cartesian states
   * 
   * @param Latitudinal
   * @param cartesianBuffer
   * @return
   */
  public static CartesianState convert(LatitudinalState Latitudinal) {
    return (CartesianState) latitudinalCoordConverter.toCartesian(Latitudinal);
  }

  /**
   * Converts from Cartesian coordinates to RaDec coordinates
   * 
   * @param cartesian
   * @param RaDecBuffer
   * @return
   */
  public static RaDecVector convertToRaDec(UnwritableVectorIJK cartesian) {
    return raDecCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts from RaDec coordinates to Cartesian coordinates
   * 
   * @param RaDec
   * @param cartesianBuffer
   * @return
   */
  public static UnwritableVectorIJK convert(RaDecVector RaDec) {
    return raDecCoordConverter.toCartesian(RaDec);
  }

  //
  //
  // /**
  // * Converts from Cartesian states to RaDec states
  // *
  // * @param cartesian
  // * @param RaDecBuffer
  // * @return
  // */
  // public static RaDecState convert(UnwritableCartesianState cartesian,
  // RaDecState raDecBuffer) {
  // return raDecCoordConverter.toCoordinate(cartesian, raDecBuffer);
  // }
  //
  // /**
  // * Converts from RaDec states to Cartesian states
  // *
  // * @param RaDec
  // * @param cartesianBuffer
  // * @return
  // */
  // public static CartesianState convert(UnwritableRaDecState RaDec,
  // CartesianState cartesianBuffer) {
  // return raDecCoordConverter.toCartesian(RaDec, cartesianBuffer);
  // }

  /**
   * Converts a Cartesian position to cylindrical position.
   * 
   * @param cartesian A {@link UnwritableVectorIJK} holding the Cartesian position.
   * @param cylindricalBuffer A {@link CylindricalVector} buffer holding the cylindrical position;
   * @return a reference to buffer for convenience.
   */
  public static CylindricalVector convertToCylindrical(UnwritableVectorIJK cartesian) {
    return cylindricalCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts a cylindrical position to a Cartesian position.
   * 
   * @param cylindrical A {@link CylindricalVector} holding the cylindrical position.
   * @param cartesianBuffer A {@link VectorIJK} buffer holding the Cartesian position.
   * @return a reference to buffer for convenience.
   */
  public static UnwritableVectorIJK convert(CylindricalVector cylindrical) {
    return cylindricalCoordConverter.toCartesian(cylindrical);
  }

  /**
   * Converts a Cartesian state to a cylindrical state.
   * 
   * @param cartesian A {@link CartesianState} holding the Cartesian state.
   * @param cylindricalBuffer A {@link CylindricalState} holding the state in this coordinate
   *        system.
   * @return a reference to buffer for convenience.
   */
  public static CylindricalState convertToCylindrical(CartesianState cartesian) {
    return (CylindricalState) cylindricalCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts a cylindrical state to a Cartesian state.
   * 
   * @param cylindrical A {@link CylindricalState} holding the Cylindrical state.
   * @param cartesianBuffer A {@link WritableState} buffer holding the Cartesian state.
   * @return a reference to buffer for convenience.
   */
  public static CartesianState convert(CylindricalState cylindrical) {
    return (CartesianState) cylindricalCoordConverter.toCartesian(cylindrical);
  }

  // the two dimensional guys
  private static PolarCoordConverter polarCoordConverter = new PolarCoordConverter();

  /**
   * Converts a Cartesian position to polar position.
   * 
   * @param cartesian A {@link UnwritableVectorIJ} holding the Cartesian position.
   * @param polarBuffer A {@link PolarVector} buffer holding the polar position;
   * @return a reference to buffer for convenience.
   */
  public static PolarVector convertToPolar(UnwritableVectorIJ cartesian) {
    return polarCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts a polar position to a Cartesian position.
   * 
   * @param polar A {@link PolarVector} holding the polar position.
   * @param cartesianBuffer A {@link VectorIJ} buffer holding the Cartesian position.
   * @return a reference to buffer for convenience.
   */
  public static UnwritableVectorIJ convert(PolarVector polar) {
    return polarCoordConverter.toCartesian(polar);
  }

  /**
   * Converts a Cartesian state to a polar state.
   * 
   * @param cartesian A {@link UnwritableCartesianIJState} holding the Cartesian state.
   * @param polarBuffer A {@link PolarState} holding the state in this coordinate system.
   * @return a reference to buffer for convenience.
   */
  public static PolarState convertToPolar(CartesianStateIJ cartesian) {
    return (PolarState) polarCoordConverter.toCoordinate(cartesian);
  }

  /**
   * Converts a polar state to a Cartesian state.
   * 
   * @param polar A {@link PolarState} holding the Polar state.
   * @param cartesianBuffer A {@link WritableState} buffer holding the Cartesian state.
   * @return a reference to buffer for convenience.
   */
  public static CartesianStateIJ convert(PolarState polar) {
    return (CartesianStateIJ) polarCoordConverter.toCartesian(polar);
  }

}
