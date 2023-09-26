package picante.spice.kernelpool.content;

import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import picante.mechanics.rotations.EulerAngles;

public class EulerAnglesFactoryTest {

  private EulerAnglesFactory factory;

  @Before
  public void setUp() throws Exception {
    factory = EulerAnglesFactory.INSTANCE;
  }

  @Test
  public void testCreateAngles() throws Exception {

    assertTrue(factory.createAngles("", 1, 2, 1) instanceof EulerAngles.IJI);
    assertTrue(factory.createAngles("", 1, 3, 1) instanceof EulerAngles.IKI);
    assertTrue(factory.createAngles("", 1, 2, 3) instanceof EulerAngles.IJK);
    assertTrue(factory.createAngles("", 1, 3, 2) instanceof EulerAngles.IKJ);

    assertTrue(factory.createAngles("", 2, 1, 2) instanceof EulerAngles.JIJ);
    assertTrue(factory.createAngles("", 2, 3, 2) instanceof EulerAngles.JKJ);
    assertTrue(factory.createAngles("", 2, 1, 3) instanceof EulerAngles.JIK);
    assertTrue(factory.createAngles("", 2, 3, 1) instanceof EulerAngles.JKI);

    assertTrue(factory.createAngles("", 3, 1, 3) instanceof EulerAngles.KIK);
    assertTrue(factory.createAngles("", 3, 2, 3) instanceof EulerAngles.KJK);
    assertTrue(factory.createAngles("", 3, 1, 2) instanceof EulerAngles.KIJ);
    assertTrue(factory.createAngles("", 3, 2, 1) instanceof EulerAngles.KJI);

  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException111() throws Exception {
    factory.createAngles("KEYWORD", 1, 1, 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException222() throws Exception {
    factory.createAngles("KEYWORD", 2, 2, 2);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException333() throws Exception {
    factory.createAngles("KEYWORD", 3, 3, 3);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException112() throws Exception {
    factory.createAngles("KEYWORD", 1, 1, 2);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException113() throws Exception {
    factory.createAngles("KEYWORD", 1, 1, 3);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException211() throws Exception {
    factory.createAngles("KEYWORD", 2, 1, 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException311() throws Exception {
    factory.createAngles("KEYWORD", 3, 1, 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException221() throws Exception {
    factory.createAngles("KEYWORD", 2, 2, 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException223() throws Exception {
    factory.createAngles("KEYWORD", 2, 2, 3);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException122() throws Exception {
    factory.createAngles("KEYWORD", 1, 2, 2);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException322() throws Exception {
    factory.createAngles("KEYWORD", 3, 2, 2);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException331() throws Exception {
    factory.createAngles("KEYWORD", 3, 3, 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException332() throws Exception {
    factory.createAngles("KEYWORD", 3, 3, 2);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException133() throws Exception {
    factory.createAngles("KEYWORD", 1, 3, 3);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateAnglesException233() throws Exception {
    factory.createAngles("KEYWORD", 2, 3, 3);
  }

}
