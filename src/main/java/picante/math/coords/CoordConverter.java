package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * An interface which allows for the conversion between different coordinate systems to Cartesian.
 * The four methods allow you to convert positions and states from Cartesian to the coordinate
 * system and back. It is templated on the Unwritable and Writable version of the coordinate class.
 * 
 * @author G.K.Stephens
 * 
 * @param <U> The Unwritable Coordinate class.
 * @param <W> The Writable version of the Coordinate class.
 */
interface CoordConverter<U> {

  /**
   * Converts a Cartesian position to another coordinate system position.
   * 
   * @param cartesian A {@link UnwritableVectorIJK} holding the Cartesian position.
   * @param coordinateBuffer A coordinate buffer holding the position in this coordinate system.
   * @return a reference to buffer for convenience.
   */
  U toCoordinate(UnwritableVectorIJK cartesian);

  /**
   * Converts a coordinate system position to a Cartesian position.
   * 
   * @param coordinate A coordinate holding the position in this coordinate system.
   * @param cartesianBuffer A {@link UnwritableVectorIJK} buffer holding the Cartesian position.
   * @return a reference to buffer for convenience.
   */
  UnwritableVectorIJK toCartesian(U coordinate);

  /**
   * Converts a Cartesian state to another coordinate system state.
   * 
   * @param cartesian A {@link UnwritableVectorIJK} holding the Cartesian state.
   * @param coordinateBuffer A coordinate buffer holding the state in this coordinate system.
   * @return a reference to buffer for convenience.
   */
  State<U> toCoordinate(State<UnwritableVectorIJK> cartesian);

  /**
   * Converts a coordinate system state to a Cartesian state.
   * 
   * @param coordinate A coordinate state in this coordinate system.
   * @param cartesianBuffer A {@link WritableState} buffer holding the Cartesian state.
   * @return a reference to buffer for convenience.
   */
  State<UnwritableVectorIJK> toCartesian(State<U> coordinate);

}
