package picante.surfaces;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Wraps a supplied {@link SurfaceNormalComputer} caching the results. This is useful when
 * repeatedly evaluating the surface normal with the same surface point, particularly if this
 * computation is slow.
 * 
 * @author G.K.Stephens
 *
 */
public class CachedSurfaceNormalComputer implements SurfaceNormalComputer {

  private final LoadingCache<UnwritableVectorIJK, VectorIJK> cache;

  /**
   * Construct for a cached {@link SurfaceNormalComputer}. Wraps a supplied
   * {@link SurfaceNormalComputer} caching the results. This is useful when repeatedly evaluating
   * the surface normal with the same surface point, particularly if this computation is slow.
   * 
   * @param delegate a supplied {@link SurfaceNormalComputer} the results of which will be cached
   */
  public CachedSurfaceNormalComputer(SurfaceNormalComputer delegate) {

    // the loader
    CacheLoader<UnwritableVectorIJK, VectorIJK> loader =
        new CacheLoader<UnwritableVectorIJK, VectorIJK>() {

          @Override
          public VectorIJK load(UnwritableVectorIJK surfacePoint) throws Exception {
            return delegate.computeOutwardNormal(surfacePoint);
          }

        };

    this.cache = CacheBuilder.newBuilder().build(loader);
  }

  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    return buffer.setTo(cache.getUnchecked(surfacePoint));
  }

}
