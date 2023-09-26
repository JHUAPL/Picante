package picante.mechanics.rotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.junit.AssertTools.assertEqualMatrix;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;

/**
 * There really isn't much to this class, as we just need to put the instance through its paces
 * which the parent test case does just fine.
 */
public class MatrixWrapperTest extends RotationTest {

  private MatrixWrapper w;
  private RotationMatrixIJK r;

  @Override
  @Before
  public void setUp() throws Exception {
    super.setUp();

    r = new RotationMatrixIJK(RotationMatrixIJK.IDENTITY);
    w = new MatrixWrapper(r);

  }

  @Override
  public Rotation createRotation(RotationMatrixIJK matrix) {
    return new MatrixWrapper(new RotationMatrixIJK(matrix));
  }

  @Test
  public void testRotationWrapper() {
    MatrixWrapper w = new MatrixWrapper();
    assertEqualMatrix(RotationMatrixIJK.IDENTITY, w.get());
    assertNotSame(RotationMatrixIJK.IDENTITY, w.get());
  }

  @Test
  public void testRotationWrapperRotationMatrixIJK() {
    RotationMatrixIJK r = new RotationMatrixIJK(RotationMatrixIJK.IDENTITY);
    MatrixWrapper w = new MatrixWrapper(r);
    assertSame(w.get(), r);
  }

  @Test
  public void testRotationWrapperRotationWrapper() {
    MatrixWrapper x = new MatrixWrapper(w);
    assertNotSame(w.get(), x.get());
    assertEqualMatrix(w.get(), x.get());
  }

  @Test
  public void testGet() {
    assertSame(r, w.get());
  }

  @Test
  public void testHashCode() {
    MatrixWrapper x = new MatrixWrapper(new RotationMatrixIJK(RotationMatrixIJK.IDENTITY));

    assertTrue(w.equals(x));
    assertEquals(w.hashCode(), x.hashCode());
  }

  @Test
  public void testEqualsObject() {

    MatrixWrapper x = new MatrixWrapper(w);

    MatrixWrapper y = new MatrixWrapper(new RotationMatrixIJK(0.9990482215818578,
        -0.03437254630962194, 0.02685477637814414, 0.04232370514336272, 0.6149337422478219,
        -0.7874421862130749, 0.010552484876318281, 0.7878293093718222, 0.6158032350983981));

    assertNotSame(x, w);
    assertTrue(x.equals(w));
    assertTrue(w.equals(x));

    assertFalse(y.equals(w));
    assertFalse(w.equals(y));
    assertFalse(w.equals(null));

    assertFalse(w.equals(Integer.valueOf(2)));

    MatrixWrapper subClass = new MatrixWrapper(w) {};

    assertTrue(subClass.equals(w));
    assertTrue(w.equals(subClass));

  }

  @Test
  public void testToString() {
    assertEquals(w.toString(), r.toString());
  }

}
