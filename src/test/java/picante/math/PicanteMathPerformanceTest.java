package picante.math;

import org.junit.Ignore;
import org.junit.Test;
import com.google.common.base.Stopwatch;
import net.jafama.FastMath;

/**
 * Let's see just how much faster Crucible Math and JAFAMA are compared to the JDK. We have to
 * interpret the results carefully because JAFAMA utilizes lazily initialized lookup tables and
 * Crucible math is using JAFAMA under its hood. Initializing the lookup tables slows down the code,
 * and the order (i.e. whether we evaluate FastMath or CrucibleMath first) matters.
 * <p>
 * For these reasons, we want the tests to be more realistic, so instead of simply testing only over
 * a given domain, we loop over that domain a certain number of times. This is more likely to
 * simulate real life behavior.
 * <p>
 * We decided to run Crucible Math before JAFAMA, meaning the times for Crucible math will include
 * the look up table initialization and are expected to be slower than JAFAMA.
 * <p>
 * See the output file for what the performances were on my current Mac. The class currently has
 * the @Ignore annotation as we don't want performance tests as part of our standard unit testing
 * suite.
 * 
 * @author G.K.Stephens
 *
 */
@Ignore
public class PicanteMathPerformanceTest {

  private final int repeat = 1000;
  private final int num = 2000;
  private final double dx = 0.01;

  @Test
  public void testSin() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.sin(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.sin(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.sin(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      sin: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible sin: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   sin: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testCos() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.cos(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.cos(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.cos(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      cos: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible cos: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   cos: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testTan() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.tan(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.tan(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.tan(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      tan: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible tan: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   tan: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testAsin() {

    // make dx smaller since this is only define for |x| <= 1
    double dx = 0.001;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.asin(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.asin(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.asin(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      asin: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible asin: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   asin: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testAcos() {

    // make dx smaller since this is only define for |x| <= 1
    double dx = 0.001;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.acos(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.acos(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.acos(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      acos: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible acos: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   acos: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testAtan() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.atan(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.atan(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.atan(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      atan: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible atan: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   atan: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testToRadians() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.toRadians(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.toRadians(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.toRadians(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      toRadians: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible toRadians: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   toRadians: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testToDegrees() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.toDegrees(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.toDegrees(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.toDegrees(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      toDegrees: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible toDegrees: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   toDegrees: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testExp() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.exp(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.exp(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.exp(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      exp: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible exp: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   exp: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testLog() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.log(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.log(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.log(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      log: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible log: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   log: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testLog10() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.log10(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.log10(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.log10(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      log10: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible log10: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   log10: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testSqrt() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.sqrt(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.sqrt(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.sqrt(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      sqrt: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible sqrt: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   sqrt: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testCbrt() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.cbrt(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.cbrt(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.cbrt(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      cbrt: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible cbrt: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   cbrt: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testIEEEremainder() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          jSum += Math.IEEEremainder(x, y);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          cSum += PicanteMath.IEEEremainder(x, y);
        }
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          fSum += FastMath.IEEEremainder(x, y);
        }
      }
    }
    fSw.stop();

    System.out.println("JDK      IEEEremainder: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible IEEEremainder: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   IEEEremainder: " + fSw + ", sum: " + fSum);
    System.out.println("");

  }

  @Test
  public void testCeil() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.ceil(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.ceil(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.ceil(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      ceil: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible ceil: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   ceil: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testFloor() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.floor(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.floor(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.floor(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      floor: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible floor: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   floor: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testAtan2() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          jSum += Math.atan2(y, x);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          cSum += PicanteMath.atan2(y, x);
        }
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          fSum += FastMath.atan2(y, x);
        }
      }
    }
    fSw.stop();

    System.out.println("JDK      atan2: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible atan2: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   atan2: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testPow() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          jSum += Math.pow(x, y);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          cSum += PicanteMath.pow(x, y);
        }
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          fSum += FastMath.pow(x, y);
        }
      }
    }
    fSw.stop();

    System.out.println("JDK      pow: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible pow: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   pow: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testRound() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    long jSum = 0;
    long cSum = 0;
    long fSum = 0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.round(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.round(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.round(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      round: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible round: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   round: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testAbsInt() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();

    long jSum = 0;
    long cSum = 0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        jSum += Math.abs(i);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        cSum += PicanteMath.abs(i);
      }
    }
    cSw.stop();

    System.out.println("JDK      abs: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible abs: " + cSw + ", sum: " + cSum);
    System.out.println("");
  }

  @Test
  public void testAbsDouble() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0;
    double cSum = 0;
    double fSum = 0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.abs(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.abs(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.abs(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      abs: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible abs: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   abs: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testMaxIntInt() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();

    int jSum = 0;
    int cSum = 0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          jSum += Math.max(i, jj);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          cSum += PicanteMath.max(i, jj);
        }
      }
    }
    cSw.stop();

    System.out.println("JDK      max: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible max: " + cSw + ", sum: " + cSum);
    System.out.println("");
  }

  @Test
  public void testMaxDoubleDouble() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          jSum += Math.max(x, y);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          cSum += PicanteMath.max(x, y);
        }
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          fSum += FastMath.max(x, y);
        }
      }
    }
    fSw.stop();

    System.out.println("JDK      max: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible max: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   max: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testMinIntInt() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();

    int jSum = 0;
    int cSum = 0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          jSum += Math.min(i, jj);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          cSum += PicanteMath.min(i, jj);
        }
      }
    }
    cSw.stop();

    System.out.println("JDK      min: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible min: " + cSw + ", sum: " + cSum);
    System.out.println("");
  }

  @Test
  public void testMinDoubleDouble() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          jSum += Math.min(x, y);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          cSum += PicanteMath.min(x, y);
        }
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          fSum += FastMath.min(x, y);
        }
      }
    }
    fSw.stop();

    System.out.println("JDK      min: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible min: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   min: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testUlp() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.ulp(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.ulp(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.ulp(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      ulp: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible ulp: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   ulp: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testSignum() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.signum(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.signum(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.signum(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      signum: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible signum: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   signum: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testSinh() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.sinh(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.sinh(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.sinh(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      sinh: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible sinh: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   sinh: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testCosh() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.cosh(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.cosh(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.cosh(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      cosh: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible cosh: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   cosh: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testTanh() {

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        jSum += Math.tanh(x);
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        cSum += PicanteMath.tanh(x);
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        double x = i * dx;
        fSum += FastMath.tanh(x);
      }
    }
    fSw.stop();

    System.out.println("JDK      tanh: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible tanh: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   tanh: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

  @Test
  public void testHypot() {

    int num = 44;

    Stopwatch jSw = Stopwatch.createUnstarted();
    Stopwatch cSw = Stopwatch.createUnstarted();
    Stopwatch fSw = Stopwatch.createUnstarted();

    double jSum = 0.0;
    double cSum = 0.0;
    double fSum = 0.0;

    jSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          jSum += Math.hypot(x, y);
        }
      }
    }
    jSw.stop();

    cSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          cSum += PicanteMath.hypot(x, y);
        }
      }
    }
    cSw.stop();

    fSw.start();
    for (int count = 0; count < repeat; count++) {
      for (int i = -num; i <= num; i++) {
        for (int jj = -num; jj <= num; jj++) {
          double x = i * dx;
          double y = jj * dx;
          fSum += FastMath.hypot(x, y);
        }
      }
    }
    fSw.stop();

    System.out.println("JDK      hypot: " + jSw + ", sum: " + jSum);
    System.out.println("Crucible hypot: " + cSw + ", sum: " + cSum);
    System.out.println("JAFAMA   hypot: " + fSw + ", sum: " + fSum);
    System.out.println("");
  }

}
