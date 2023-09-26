package picante.mechanics.providers.reference;

import java.util.List;
import picante.mechanics.EphemerisProvider;
import picante.mechanics.EphemerisProviderTest;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.SourceException;

public class ReferenceEphemerisProviderTest extends EphemerisProviderTest {

  @Override
  public EphemerisProvider createProvider(List<? extends PositionVectorFunction> ephemerisSources,
      List<? extends FrameTransformFunction> frameSources) throws SourceException {
    return new ReferenceEphemerisProvider(ephemerisSources, frameSources);
  }

  // TODO: Add exception specific tests.

}
