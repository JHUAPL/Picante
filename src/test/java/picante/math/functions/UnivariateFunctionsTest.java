package picante.math.functions;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;
import org.junit.Before;
import org.junit.Test;
import picante.exceptions.RuntimeInterruptedException;
import picante.math.PicanteMath;

public class UnivariateFunctionsTest {

  private UnivariateFunction mockA;
  private UnivariateFunction mockB;
  private double value;
  private double a;
  private double b;

  @Before
  public void setUp() throws Exception {
    mockA = createMock(UnivariateFunction.class);
    mockB = createMock(UnivariateFunction.class);
    value = 25.0;
    a = 5.0;
    b = -12.0;
  }

  private void configureMocks() {
    expect(mockA.evaluate(value)).andReturn(a);
    expect(mockB.evaluate(value)).andReturn(b);
    replay(mockA, mockB);
  }

  private void verifyMocks() {
    verify(mockA, mockB);
  }

  @Test
  public void testCreate() {
    UnivariateFunction result = UnivariateFunctions.create(10.0);
    assertEquals(10.0, result.evaluate(value), 0.0);
    /*
     * It's constant, try some other values.
     */
    assertEquals(10.0, result.evaluate(0.0), 0.0);
    assertEquals(10.0, result.evaluate(1e6), 0.0);
    assertEquals(10.0, result.evaluate(-Double.MAX_VALUE), 0.0);
  }

  @Test
  public void testAdd() {
    configureMocks();
    UnivariateFunction result = UnivariateFunctions.add(mockA, mockB);
    assertEquals(a + b, result.evaluate(value), 0.0);
    verifyMocks();
  }

  @Test
  public void testSubtract() {
    configureMocks();
    UnivariateFunction result = UnivariateFunctions.subtract(mockA, mockB);
    assertEquals(a - b, result.evaluate(value), 0.0);
    verifyMocks();
  }

  @Test
  public void testMultiply() {
    configureMocks();
    UnivariateFunction result = UnivariateFunctions.multiply(mockA, mockB);
    assertEquals(a * b, result.evaluate(value), 0.0);
    verifyMocks();
  }

  @Test
  public void testDivide() {
    configureMocks();
    UnivariateFunction result = UnivariateFunctions.divide(mockA, mockB);
    assertEquals(a / b, result.evaluate(value), 0.0);
    verifyMocks();
  }

  @Test
  public void testCompose() {
    expect(mockB.evaluate(value)).andReturn(b);
    expect(mockA.evaluate(b)).andReturn(a);
    replay(mockA, mockB);
    UnivariateFunction result = UnivariateFunctions.compose(mockA, mockB);
    assertEquals(a, result.evaluate(value), 0.0);
    verify(mockA, mockB);
  }

  @Test
  public void testScale() {
    expect(mockA.evaluate(value)).andReturn(a);
    replay(mockA);
    double scale = 100.0;
    UnivariateFunction result = UnivariateFunctions.scale(scale, mockA);
    assertEquals(scale * a, result.evaluate(value), 0.0);
    verify(mockA);
  }

  @Test
  public void testNegate() {
    expect(mockA.evaluate(value)).andReturn(a);
    replay(mockA);
    UnivariateFunction result = UnivariateFunctions.negate(mockA);
    assertEquals(-a, result.evaluate(value), 0.0);
    verify(mockA);
  }

  @Test(expected = RuntimeInterruptedException.class)
  public void testInterruptibleFunction() {

    UnivariateFunction f = (t) -> t;

    UnivariateFunction interruptible = UnivariateFunctions.interruptibleFunction(f, 10);

    AtomicBoolean run = new AtomicBoolean(true);

    Timer t = new Timer(true);
    Thread current = Thread.currentThread();
    t.schedule(new TimerTask() {

      @Override
      public void run() {
        current.interrupt();
      }

    }, 10L);

    t.schedule(new TimerTask() {

      @Override
      public void run() {
        run.set(false);
      }

    }, 250L);

    int i = 0;
    while (run.get()) {
      interruptible.evaluate(i);
      i++;
    }

    t.cancel();

  }

  @Test
  public void testToDegrees() {
    UnivariateFunction result = UnivariateFunctions.toDegrees();
    assertEquals(Math.toDegrees(a), result.evaluate(a), 0.0);
    assertEquals(Math.toDegrees(b), result.evaluate(b), 0.0);
  }

  @Test(expected = NullPointerException.class)
  public void testClampNullFunctionException() {
    UnivariateFunctions.clamp(0, 1, null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testClampLowValueNotFinite() {
    UnivariateFunctions.clamp(Double.NaN, 1, mockA);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testClampHighValueNotFinite() {
    UnivariateFunctions.clamp(0, Double.POSITIVE_INFINITY, mockA);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testClampLowAboveHigh() {
    UnivariateFunctions.clamp(15, 5, mockA);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testClampLowEqualHigh() {
    UnivariateFunctions.clamp(15, 15, mockA);
  }

  @Test
  public void testClamp() {
    expect(mockA.evaluate(value)).andReturn(a);
    expect(mockA.evaluate(2.0 * value)).andReturn(-12.0);
    replay(mockA);
    UnivariateFunction result = UnivariateFunctions.clamp(-10, 2, mockA);
    assertEquals(2, result.evaluate(value), 0.0);
    assertEquals(-10, result.evaluate(2.0 * value), 0.0);
    verify(mockA);
  }

  @Test
  public void testToRadians() {
    UnivariateFunction result = UnivariateFunctions.toRadians();
    assertEquals(Math.toRadians(a), result.evaluate(a), 0.0);
    assertEquals(Math.toRadians(b), result.evaluate(b), 3.0E-17);
    assertEquals(PicanteMath.toRadians(b), result.evaluate(b), 0.0);
  }

}
