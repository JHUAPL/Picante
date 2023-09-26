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

public class DifferentiableUnivariateFunctionsTest {

  private final double TOL = 2.3E-16;

  private DifferentiableUnivariateFunction mockA;
  private DifferentiableUnivariateFunction mockB;
  private double value;
  private double a;
  private double dA;
  private double b;
  private double dB;

  @Before
  public void setUp() throws Exception {
    mockA = createMock(DifferentiableUnivariateFunction.class);
    mockB = createMock(DifferentiableUnivariateFunction.class);

    value = 25.0;
    a = 5.0;
    dA = -2.0;
    b = -17.0;
    dB = 3.0;

  }

  private void configureMocks() {
    expect(mockA.evaluate(value)).andReturn(a).atLeastOnce();
    expect(mockA.differentiate(value)).andReturn(dA).atLeastOnce();
    expect(mockB.evaluate(value)).andReturn(b).atLeastOnce();
    expect(mockB.differentiate(value)).andReturn(dB).atLeastOnce();

    replay(mockA, mockB);
  }

  private void verifyMocksAB() {
    verify(mockA, mockB);
  }

  @Test
  public void testCreate() {

    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.create(10.0);
    assertEquals(10.0, result.evaluate(value), 0.0);
    assertEquals(0.0, result.differentiate(value), 0.0);

    /*
     * It's constant, so other times...
     */
    assertEquals(10.0, result.evaluate(-100.0), 0.0);
    assertEquals(0.0, result.differentiate(-100.0), 0.0);

    assertEquals(10.0, result.evaluate(Double.MAX_VALUE), 0.0);
    assertEquals(0.0, result.differentiate(Double.MAX_VALUE), 0.0);
  }

  @Test
  public void testDerivative() {
    expect(mockA.differentiate(value)).andReturn(dA);
    replay(mockA);
    UnivariateFunction result = DifferentiableUnivariateFunctions.derivative(mockA);
    assertEquals(dA, result.evaluate(value), 0.0);
    verify(mockA);
  }

  @Test
  public void testAdd() {
    configureMocks();
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.add(mockA, mockB);
    assertEquals(a + b, result.evaluate(value), 0.0);
    assertEquals(dA + dB, result.differentiate(value), 0.0);
    verifyMocksAB();
  }

  @Test
  public void testSubtract() {
    configureMocks();
    DifferentiableUnivariateFunction result =
        DifferentiableUnivariateFunctions.subtract(mockA, mockB);
    assertEquals(a - b, result.evaluate(value), 0.0);
    assertEquals(dA - dB, result.differentiate(value), 0.0);
    verifyMocksAB();
  }

  @Test
  public void testMultiply() {
    configureMocks();
    DifferentiableUnivariateFunction result =
        DifferentiableUnivariateFunctions.multiply(mockA, mockB);
    assertEquals(a * b, result.evaluate(value), 0.0);
    assertEquals(a * dB + dA * b, result.differentiate(value), 0.0);
    verifyMocksAB();
  }

  @Test
  public void testDivide() {
    configureMocks();
    DifferentiableUnivariateFunction result =
        DifferentiableUnivariateFunctions.divide(mockA, mockB);
    assertEquals(a / b, result.evaluate(value), 0.0);
    assertEquals((b * dA - a * dB) / b / b, result.differentiate(value), 0.0);
    verifyMocksAB();
  }

  @Test
  public void testCompose() {
    expect(mockB.evaluate(value)).andReturn(b).atLeastOnce();
    expect(mockB.differentiate(value)).andReturn(dB).atLeastOnce();
    expect(mockA.evaluate(b)).andReturn(a).atLeastOnce();
    expect(mockA.differentiate(b)).andReturn(dA).atLeastOnce();
    replay(mockA, mockB);
    DifferentiableUnivariateFunction result =
        DifferentiableUnivariateFunctions.compose(mockA, mockB);
    assertEquals(a, result.evaluate(value), 0.0);
    assertEquals(dA * dB, result.differentiate(value), 0.0);
    verify(mockA, mockB);
  }

  @Test
  public void testScale() {
    expect(mockA.evaluate(value)).andReturn(a);
    expect(mockA.differentiate(value)).andReturn(dA);
    replay(mockA);
    double scale = 100;
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.scale(scale, mockA);
    assertEquals(scale * a, result.evaluate(value), 0.0);
    assertEquals(scale * dA, result.differentiate(value), 0.0);
    verify(mockA);
  }

  @Test
  public void testNegate() {
    expect(mockA.evaluate(value)).andReturn(a);
    expect(mockA.differentiate(value)).andReturn(dA);
    replay(mockA);
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.negate(mockA);
    assertEquals(-a, result.evaluate(value), 0.0);
    assertEquals(-dA, result.differentiate(value), 0.0);
    verify(mockA);
  }

  @Test(expected = RuntimeInterruptedException.class)
  public void testInterruptibleFunction() {

    DifferentiableUnivariateFunction f = new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return t;
      }

      @Override
      public double differentiate(@SuppressWarnings("unused") double t) {
        return 1;
      }
    };

    DifferentiableUnivariateFunction interruptible =
        DifferentiableUnivariateFunctions.interruptibleFunction(f, 10);

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
      interruptible.differentiate(i);
      i++;
    }
    t.cancel();

  }

  @Test
  public void testQuadraticApproximation() {
    double delta = 5.0;
    expect(mockA.evaluate(value)).andReturn(a);
    expect(mockA.evaluate(value - delta)).andReturn(a + 1.0);
    expect(mockA.evaluate(value + delta)).andReturn(a + 2.0);
    replay(mockA);
    DifferentiableUnivariateFunction result =
        DifferentiableUnivariateFunctions.quadraticApproximation(mockA, delta);
    assertEquals(a, result.evaluate(value), 0.0);
    assertEquals((a + 2.0 - a - 1.0) * 0.5 / delta, result.differentiate(value), 0.0);
  }

  @Test
  public void testToDegrees() {
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.toDegrees();
    assertEquals(Math.toDegrees(a), result.evaluate(a), 0.0);
    assertEquals(Math.toDegrees(1.0), result.differentiate(a), 0.0);
    assertEquals(Math.toDegrees(b), result.evaluate(b), 0.0);
    assertEquals(Math.toDegrees(1.0), result.differentiate(b), 0.0);
  }

  @Test
  public void testToRadians() {
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.toRadians();
    assertEquals(Math.toRadians(a), result.evaluate(a), 0.0);
    assertEquals(Math.toRadians(1.0), result.differentiate(a), 0.0);
    assertEquals(Math.toRadians(b), result.evaluate(b), TOL);
    assertEquals(PicanteMath.toRadians(b), result.evaluate(b), 0.0);
    assertEquals(Math.toRadians(1.0), result.differentiate(b), 0.0);
  }

  @Test
  public void testCosine() {
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.cosine();
    assertEquals(Math.cos(a), result.evaluate(a), TOL);
    assertEquals(PicanteMath.cos(a), result.evaluate(a), 0.0);
    assertEquals(-Math.sin(a), result.differentiate(a), TOL);
    assertEquals(-PicanteMath.sin(a), result.differentiate(a), 0.0);
    assertEquals(Math.cos(b), result.evaluate(b), TOL);
    assertEquals(PicanteMath.cos(b), result.evaluate(b), 0.0);
    assertEquals(-Math.sin(b), result.differentiate(b), TOL);
    assertEquals(-PicanteMath.sin(b), result.differentiate(b), 0.0);
  }

  @Test
  public void testSine() {
    DifferentiableUnivariateFunction result = DifferentiableUnivariateFunctions.sine();
    assertEquals(Math.sin(a), result.evaluate(a), TOL);
    assertEquals(PicanteMath.sin(a), result.evaluate(a), 0.0);
    assertEquals(Math.cos(a), result.differentiate(a), TOL);
    assertEquals(PicanteMath.cos(a), result.differentiate(a), 0.0);
    assertEquals(Math.sin(b), result.evaluate(b), TOL);
    assertEquals(PicanteMath.sin(b), result.evaluate(b), 0.0);
    assertEquals(Math.cos(b), result.differentiate(b), TOL);
    assertEquals(PicanteMath.cos(b), result.differentiate(b), 0.0);
  }

}
