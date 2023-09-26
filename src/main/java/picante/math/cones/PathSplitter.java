package picante.math.cones;

import static com.google.common.base.Preconditions.checkNotNull;

import java.awt.geom.PathIterator;
import java.util.List;
import java.util.function.Function;

import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;

import com.google.common.collect.ImmutableList;
import com.google.common.primitives.ImmutableDoubleArray;

/**
 * Converts a {@link PathIterator} into a list of JTS {@link CoordinateSequence}s.
 * <p>
 * Any duplicate or unnecessary elements of the path are filtered out (i.e. duplicate
 * {@link PathIterator#SEG_LINETO}).
 * </p>
 */
class PathSplitter implements Function<PathIterator, List<CoordinateSequence>> {

  private final PackedCoordinateSequenceFactory coordSequenceFactory;

  PathSplitter(PackedCoordinateSequenceFactory coordSequenceFactory) {
    this.coordSequenceFactory = checkNotNull(coordSequenceFactory);
  }

  /**
   * Breaks up the shape into a series of JTS {@link CoordinateSequence}s that represent individual
   * path components of the shape.
   * 
   * @param shape the shape of interest
   * 
   * @return a list of coordinate sequences in the order provided by the shape {@link PathIterator}
   * 
   * @throws UnsupportedOperationException if any of the shape paths have with types of either:
   *         {@link PathIterator#SEG_QUADTO} or {@link PathIterator#SEG_CUBICTO}
   * 
   * @throws {@link IllegalArgumentException} if the supplied {@link PathIterator} does not start
   *         with a segment type of {@link PathIterator#SEG_MOVETO}
   */
  @Override
  public List<CoordinateSequence> apply(PathIterator iterator) {

    ImmutableList.Builder<CoordinateSequence> resultBuilder = ImmutableList.builder();

    double[] coords = new double[6];
    int type = Integer.MIN_VALUE;
    ImmutableDoubleArray.Builder accumulator = null;
    double[] start = new double[] {Double.NaN, Double.NaN};
    double[] previous = new double[] {Double.NaN, Double.NaN};

    /*
     * Unfortunately this is a convoluted, somewhat by necessity in dealing with possibly invalid
     * PathIterator behaviors in a sane manner.
     */
    for (; !iterator.isDone(); iterator.next()) {
      type = iterator.currentSegment(coords);

      if ((accumulator == null) && (type != PathIterator.SEG_MOVETO)) {
        /*
         * Invalid path start, it must begin with a MOVETO.
         */
        throw new IllegalArgumentException(
            "Supplied path iterator does not start with a PathIterator.SEG_MOVETO type.");
      }

      /*
       * Only proceed if the type is CLOSE or coords does not match previous.
       */
      if (type == PathIterator.SEG_CLOSE || previous[0] != coords[0] || previous[1] != coords[1]) {

        /*
         * MOVETO indicates that a new segment is starting.
         */
        if (type == PathIterator.SEG_MOVETO) {

          /*
           * Accumulator is only null on the first pass through the loop.
           */
          if (accumulator != null) {
            resultBuilder.add(coordSequenceFactory.create(accumulator.build().toArray(), 2));
          }

          accumulator = ImmutableDoubleArray.builder();
          accumulator.add(coords[0]);
          accumulator.add(coords[1]);

          /*
           * Cache the starting point of the current segment for use when CLOSE happens.
           */
          System.arraycopy(coords, 0, start, 0, 2);
          System.arraycopy(coords, 0, previous, 0, 2);

        } else if (type == PathIterator.SEG_CLOSE) {

          /*
           * Check to see if we need to add the closing point to the accumulator. This check is done
           * just to prevent duplicating a coordinate that may already be present in the output
           * sequence. Compare with previous, as coords is not necessarily populated when type is
           * SEG_CLOSE.
           */
          if (previous[0] != start[0] || previous[1] != start[1]) {
            accumulator.add(start[0]);
            accumulator.add(start[1]);
          }

        } else if (type == PathIterator.SEG_LINETO) {

          /*
           * Accumulate the next point along the linear boundary only if it's not equal to the
           * previous point.
           */
          accumulator.add(coords[0]);
          accumulator.add(coords[1]);
          System.arraycopy(coords, 0, previous, 0, 2);


        } else {
          throw new UnsupportedOperationException(
              "Unable to convert shape, only linear boundaries are supported."
                  + " Consider flattening the shape first.");
        }
      }

    }

    /*
     * Add the last path segment to the resultBuilder.
     */
    resultBuilder.add(coordSequenceFactory.create(accumulator.build().toArray(), 2));

    return resultBuilder.build();

  }


}
