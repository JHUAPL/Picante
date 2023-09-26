package picante.math.coords;

import static picante.units.FundamentalPhysicalConstants.TWOPI;
import picante.math.vectorspace.UnwritableVectorIJK;

class RaDecCoordConverter implements CoordConverter<RaDecVector> {

  private final static LatitudinalCoordConverter LAT_CONVERTER = new LatitudinalCoordConverter();

  @Override
  public RaDecVector toCoordinate(UnwritableVectorIJK cartesian) {

    LatitudinalVector workCoord = LAT_CONVERTER.toCoordinate(cartesian);

    double r = workCoord.getRadius();
    double lat = workCoord.getLatitude();
    double lon = workCoord.getLongitude();

    if (lon < 0.0) {
      lon = lon + TWOPI;
    }

    return new RaDecVector(r, lon, lat);
  }

  @Override
  public UnwritableVectorIJK toCartesian(RaDecVector coordinate) {

    LatitudinalVector workCoord = new LatitudinalVector(coordinate.getRadius(),
        coordinate.getDeclination(), coordinate.getRightAscension());

    return LAT_CONVERTER.toCartesian(workCoord);
  }

  @Override
  public State<RaDecVector> toCoordinate(
      @SuppressWarnings("unused") State<UnwritableVectorIJK> cartesian) {
    throw new UnsupportedOperationException("not yet supported");
  }

  @Override
  public State<UnwritableVectorIJK> toCartesian(
      @SuppressWarnings("unused") State<RaDecVector> coordinate) {
    throw new UnsupportedOperationException("not yet supported");
  }

}
