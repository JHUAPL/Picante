package picante.math.cones;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;
import java.util.function.Function;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;

import com.google.common.collect.ImmutableList;

/**
 * Function that converts a list of {@link CoordinateSequence}s into a list of {@link Geometry}s.
 * <p>
 * <ul>
 * <li>Sequences with a single point are converted to {@link Point}</li>
 * <li>Sequences with 4 or more entries, where first and last match exactly are converted to
 * {@link LinearRing}</li>
 * <li>All other sequences are converted to {@link LineString}</li>
 * </ul>
 * </p>
 */
class SequencesConverter implements Function<List<CoordinateSequence>, List<Geometry>> {

  private final GeometryFactory geometryFactory;

  SequencesConverter(GeometryFactory geometryFactory) {
    this.geometryFactory = checkNotNull(geometryFactory);
  }

  @Override
  public List<Geometry> apply(List<CoordinateSequence> sequences) {

    /*
     * Convert the coordinate sequences to Points, LineStrings, or Polygons depending on whether
     * they are closed on themselves and have more than one entry.
     */
    ImmutableList.Builder<Geometry> geometries =
        ImmutableList.builderWithExpectedSize(sequences.size());

    Coordinate first = new Coordinate();
    Coordinate last = new Coordinate();

    for (CoordinateSequence sequence : sequences) {
      if (sequence.size() == 1) {
        geometries.add(geometryFactory.createPoint(sequence));
      } else if (isLinearRing(sequence, first, last)) {
        geometries.add(geometryFactory.createLinearRing(sequence));
      } else {
        geometries.add(geometryFactory.createLineString(sequence));
      }
    }

    return geometries.build();
  }

  /**
   * Determines whether the sequence of coordinates should be a linear ring.
   * <p>
   * Note: this method does <b>NOT</b> validate the contents of the ring, it just simply checks that
   * the first and last entry in the supplied sequence match and that there are at least 4 entries.
   * </p>
   * 
   * @param sequence candidate sequence of coordinates
   * @param bufferA a coordinate buffer to use for retrieval from sequence
   * @param bufferB a coordinate buffer to use for retrieval from sequence
   * 
   * @return true if sequence might be a linear ring, false otherwise
   */
  boolean isLinearRing(CoordinateSequence sequence, Coordinate bufferA, Coordinate bufferB) {

    if (sequence.size() < 4) {
      return false;
    }

    sequence.getCoordinate(0, bufferA);
    sequence.getCoordinate(sequence.size() - 1, bufferB);

    return bufferA.equals(bufferB);
  }

}

