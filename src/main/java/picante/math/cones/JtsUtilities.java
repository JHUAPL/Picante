package picante.math.cones;

import static com.google.common.base.Preconditions.checkArgument;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.util.List;
import java.util.function.Function;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.MultiLineString;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.PrecisionModel;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;

/**
 * Class capturing utility methods for manipulating, creating, and converting to and from JTS
 * {@link Geometry} types.
 */
class JtsUtilities {

  /**
   * Simple {@link Geometry} filter that generates exceptions when presented with an empty geometry.
   */
  static final Function<Geometry, Geometry> EXCEPTION_ON_EMPTY_GEOMETRY = (t) -> {
    if (t.isEmpty()) {
      throw new IllegalArgumentException("Unable to process empty geometry");
    }
    return t;
  };

  /**
   * Convenience method to create coordinate arrays from pairs of xy values.
   * 
   * @param xys double array of { x0, y0, x1, y1, ..., xN, yN }
   * 
   * @return coordinate array containing (x0,y0), (x1,y1), ... (xN,yN)
   */
  static Coordinate[] createCoordinateArray(double... xys) {
    checkArgument(xys.length % 2 == 0, "Input array entries must be paired.");
    Coordinate[] result = new Coordinate[xys.length / 2];
    for (int i = 0; i < result.length; i++) {
      result[i] = new Coordinate(xys[2 * i], xys[2 * i + 1]);
    }
    return result;
  }

  /**
   * Convenience method to create {@link PackedCoordinateSequence}s from pairs of xy values.
   * 
   * @param xys double array of { x0, y0, x1, y1, ..., xN, yN }
   * 
   * @return a {@link PackedCoordinateSequence} containing the pairs of coordinates: (x0, y0), (x1,
   *         y1), ... (xN,yN)
   */
  static CoordinateSequence createPackedCoordinateSequence(double... xys) {
    checkArgument(xys.length % 2 == 0, "Input array entries must be paired.");
    return new ModifiedPackedCoordinateSequence.Double(xys, 2, 0);
  }



  /**
   * Geometry factory is thread-safe. This one is configured to use double precision.
   * <p>
   * This implementation should be thread-safe, from Martin Davis: &quot;GeometryFactory is
   * immutable so is thread safe. CoordinateSequenceFactory is an interface, so it's up to the
   * implementation. But normally they should be thread-safe.&quot;
   * (<a href= "https://gitter.im/locationtech/jts?at=5d9c3f783220922ffb4b88b8">link</a>)
   * </p>
   */
  static GeometryFactory PACKED_DOUBLE_GEOMETRY_FACTORY =
      new GeometryFactory(new PrecisionModel(PrecisionModel.FLOATING), 0,
          ModifiedPackedCoordinateSequenceFactory.DOUBLE_FACTORY);

  /**
   * Simple {@link PathIterator} filter that throws an exception when the path is done, i.e. empty.
   */
  static Function<PathIterator, PathIterator> EXCEPTION_ON_EMPTY_PATHITERTOR = (t) -> {
    if (t.isDone()) {
      throw new IllegalArgumentException("Unable to process empty path iterator");
    }
    return t;
  };

  /**
   * Simple function to convert {@link Shape} to {@link PathIterator}. This simply invokes:
   * {@link Shape#getPathIterator(AffineTransform)} with the supplied argument as null;
   */
  static Function<Shape, PathIterator> SHAPE_ITERATOR = (s) -> s.getPathIterator(null);

  /**
   * Function that filters out isolated points from a list of {@link CoordinateSequence}s.
   * <p>
   * Individual points really have no place being in shapes that are to be rendered in Java2D,
   * though the API allows for it.
   * </p>
   */
  static Function<List<CoordinateSequence>, List<CoordinateSequence>> POINT_REMOVER =
      (l) -> ImmutableList.copyOf(Iterables.filter(l, (cs) -> cs.size() > 1));


  /**
   * Supplies a function that converts a path iterator to a geometry.
   */
  static Function<PathIterator, Geometry> GEOMETRY_CONVERTER = EXCEPTION_ON_EMPTY_PATHITERTOR
      .andThen(new PathSplitter(ModifiedPackedCoordinateSequenceFactory.DOUBLE_FACTORY))
      .andThen(POINT_REMOVER).andThen(new SequencesConverter(PACKED_DOUBLE_GEOMETRY_FACTORY)
          .andThen(new GeometrySanitizer(PACKED_DOUBLE_GEOMETRY_FACTORY)));

  // static Function<PathIterator, Geometry> GEOMETRY_CONVERTER = EXCEPTION_ON_EMPTY_PATHITERTOR
  // .andThen(new PathSplitter(PackedCoordinateSequenceFactory.DOUBLE_FACTORY))
  // .andThen(POINT_REMOVER).andThen(new SequencesConverter(PACKED_DOUBLE_GEOMETRY_FACTORY)
  // .andThen(new GeometrySanitizer(PACKED_DOUBLE_GEOMETRY_FACTORY)));


  /**
   * Converts a {@link PathIterator} to a {@link Geometry}.
   * <p>
   * Isolated points are removed prior to conversion. Open paths are converted to
   * {@link LineString}. Closed paths are converted to {@link Polygon}s. If multiple of either are
   * required, then the corresponding {@link MultiLineString} or {@link MultiPolygon} are used
   * instead. If anything goes unexpectedly, then an {@link UnsupportedOperationException} will be
   * thrown or a {@link GeometryCollection} with a variety of different types will be returned.
   * </p>
   * 
   * @return a new function to convert path iterators.
   */
  public static Function<PathIterator, Geometry> pathIteratorToGeometryConverter() {
    return GEOMETRY_CONVERTER;
  }

}
