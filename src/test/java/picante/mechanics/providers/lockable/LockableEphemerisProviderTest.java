package picante.mechanics.providers.lockable;

import java.util.List;
import picante.mechanics.EphemerisProvider;
import picante.mechanics.EphemerisProviderTest;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.SourceException;

public class LockableEphemerisProviderTest extends EphemerisProviderTest {

  @Override
  public EphemerisProvider createProvider(List<? extends PositionVectorFunction> ephemerisSources,
      List<? extends FrameTransformFunction> frameSources) throws SourceException {
    return new LockableEphemerisProvider(ephemerisSources, frameSources, LockType.FUNCTION);
  }

  // TODO: Add exception specific tests.

}
