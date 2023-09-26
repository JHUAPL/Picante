package picante.math.cones;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.MultiLineString;
import org.locationtech.jts.geom.MultiPoint;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multiset;

/**
 * Cleans up a list of {@link Geometry} instances into a single one based of an analysis of the
 * individual elements of the input list.
 * <p>
 * <ul>
 * <li>A list of {@link Point}s are turned into a single {@link MultiPoint}, unless the list has a
 * single element.</li>
 * <li>A list of {@link LineString}s are turned into a single {@link MultiLineString}, unless the
 * list has a single element.</li>
 * <li>A list of {@link Polygons}s are turned into a single {@link MultiPolygon}, unless the list
 * has a single element, or if the {@link LinearRing}s overlap in some unusual way. The
 * {@link MultiPolygon} will be built up by examining which {@link Polygon}s cover one another as
 * holes.</li>
 * </ul>
 * </p>
 * <p>
 * If the above criteria aren't met by the input, then this function just returns
 * {@link GeometryCollection} with the input list of {@link Geometry} instances.
 * </p>
 */
class GeometrySanitizer implements Function<List<? extends Geometry>, Geometry> {

  private final GeometryFactory geometryFactory;

  GeometrySanitizer(GeometryFactory geometryFactory) {
    super();
    this.geometryFactory = geometryFactory;
  }

  @Override
  public Geometry apply(List<? extends Geometry> geometries) {

    /*
     * If the list is empty, return an empty geometry.
     */
    if (geometries.size() == 0) {
      return geometryFactory.createPoint();
    }

    Class<? extends Geometry> type = isHomogeneous(geometries);

    if (type == null) {
      return geometryFactory
          .createGeometryCollection(geometries.toArray(new Geometry[geometries.size()]));
    }

    /*
     * If all the entries are points, then return a MultiPoint.
     */
    if (Point.class.equals(type)) {
      if (geometries.size() == 1) {
        return geometries.get(0);
      }
      return geometryFactory.createMultiPoint(geometries.toArray(new Point[geometries.size()]));
    }

    /*
     * If all the entries are strictly linestrings, then return a MultiLineString.
     */
    if (LineString.class.equals(type)) {
      if (geometries.size() == 1) {
        return geometries.get(0);
      }
      return geometryFactory
          .createMultiLineString(geometries.toArray(new LineString[geometries.size()]));
    }

    /*
     * If all the entries are strictly linear rings, then either return a MultiLineString, Polygon,
     * or MultiPolygon depending on the relationships between the linear rings.
     */
    if (LinearRing.class.equals(type)) {
      return processCoverings(geometries);
    }

    /*
     * If we reach here just throw everything into a GeometryCollection, as it's beyond what this
     * class attempts to support.
     */
    return geometryFactory
        .createGeometryCollection(geometries.toArray(new Geometry[geometries.size()]));

  }

  /**
   * Determines if all the entries are the same sub-class of geometry.
   * <p>
   * This method uses {@link Class#equals(Object)}, because it treats sub-classes differently. That
   * is to say, a {@link LinearRing} is not the same as a {@link LineString}.
   * </p>
   * 
   * @param geometries
   * 
   * @return true of all entries in geometries are the exactly same {@link Class}, false otherwise.
   */
  Class<? extends Geometry> isHomogeneous(List<? extends Geometry> geometries) {

    Class<? extends Geometry> result = geometries.get(0).getClass();

    for (Geometry geometry : geometries) {
      if (!result.equals(geometry.getClass())) {
        return null;
      }
    }

    return result;

  }

  Geometry processCoverings(List<? extends Geometry> rings) {

    /*
     * This is a bit of a mess. Determine if all of the rings are independent or holes in other
     * rings. If that's the case then return a MultiPolygon, otherwise return a MultiLineString.
     */
    ImmutableListMultimap<Integer, Integer> coverings = createCoverings(rings);

    /*
     * Collect all the indices of rings that are classified as holes of other rings.
     */
    Multiset<Integer> holes = HashMultiset.create(rings.size());
    holes.addAll(coverings.values());

    List<Polygon> polygons = new ArrayList<>(rings.size());

    final int ringsLength = rings.size();

    /*
     * Iterate over the entire list of rings, and only add polygons for those that do not appear as
     * holes of another ring.
     */
    for (int i = 0; i < ringsLength; i++) {

      int count = holes.count(i);

      /*
       * If there is a single hole that appears more than once as a hole of two distinct linear
       * rings, then something weird is going on. Punt and return a MultiLineString.
       */
      if (count > 1) {
        return geometryFactory.createMultiLineString(rings.toArray(new LineString[rings.size()]));
      }

      /*
       * If this index does not appear at all in the holes multiset, then it should be converted
       * from a linear ring to a proper polygon.
       */
      if (count == 0) {

        /*
         * If it has holes, then include them.
         */
        if (coverings.containsKey(i)) {

          List<Integer> holeIndices = coverings.get(i);
          LinearRing[] holeArray = new LinearRing[holeIndices.size()];
          for (int j = 0; j < holeIndices.size(); j++) {
            /*
             * Check to see if this "hole" is covered by multiple other rings in the set. If it is,
             * then return null, indicating that a basic GeometryCollection is to be returned.
             */
            holeArray[j] = (LinearRing) rings.get(holeIndices.get(j));
          }
          polygons.add(geometryFactory.createPolygon((LinearRing) rings.get(i), holeArray));

        } else {
          polygons.add(geometryFactory.createPolygon((LinearRing) rings.get(i)));
        }

      }
    }

    if (polygons.size() == 1) {
      return polygons.get(0);
    }

    return geometryFactory.createMultiPolygon(polygons.toArray(new Polygon[polygons.size()]));


  }

  ImmutableListMultimap<Integer, Integer> createCoverings(List<? extends Geometry> rings) {

    ImmutableListMultimap.Builder<Integer, Integer> resultBuilder = ImmutableListMultimap.builder();

    /*
     * Convert the input rings to Polygons. This is necessary as LinearRings have no interior.
     */
    List<Polygon> polygons = Lists.newArrayList(
        Iterables.transform(rings, (r) -> geometryFactory.createPolygon((LinearRing) r)));

    /*
     * This a bit messy, determine which rings contain each other.
     */
    final int polysLength = polygons.size();
    for (int i = 0; i < polysLength; i++) {

      Geometry polygon = polygons.get(i);

      for (int j = 0; j < polysLength; j++) {

        Geometry candidate = polygons.get(j);

        /*
         * Only operate if the two are not the exact same instance.
         */
        if (i != j) {
          if (polygon.contains(candidate)) {
            resultBuilder.put(i, j);
          }
        }

      }

    }
    return resultBuilder.build();

  }

}
