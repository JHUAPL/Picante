package picante.spice.kernel.spk;

import static org.junit.Assert.assertSame;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Test;
import picante.mechanics.StateVector;

public class TwoBodyPropagatorTest {

  private final double TOL = 1e-16;

  @Test
  public void testPositiveF() {

    StateVector initialState = new StateVector(-1.8276966996656E+09, -5.9817694199201E+09,
        -1.7044851611248E+09, 4.3758733772182E+00, -1.1068356354252E+00, -2.1045070068570E-01);
    TwoBodyPropagator propagator = new TwoBodyPropagator();
    propagator.configure(1.3289051279402E+11, 0, initialState);
    StateVector buffer = new StateVector();

    StateVector result = propagator.getState(0, buffer);
    assertSame(result, buffer);
    assertComponentEquals(initialState, result, TOL);

    result = propagator.getState(1000, buffer);
    assertSame(result, buffer);
    StateVector expected = new StateVector(-1827692323.7917769, -5981770526.7542763,
        -1704485371.5750849, 4.3758742686748029, -1.1068327178214608, -0.21044986932431223);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(-1000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(-1827701075.5385315, -5981768313.0830059, -1704484950.6736834,
        4.3758724857595270, -1.1068385530281872, -0.21045153204692468);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(-100000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(-1828134282.5456886, -5981658721.7686663, -1704464111.8979521,
        4.3757842211068541, -1.1071273920017044, -0.21053383600089493);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(10000000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(-1783893738.2383833, -5992691771.3295841, -1706548073.0453534,
        4.3846843551692452, -1.0776222231842678, -0.20212899712566065);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(1000000000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(2626309201.2815962, -5564365904.3180103, -1489237062.6373997,
        4.1509028749581667, 1.9423771424869039, 0.63266460929596380);
    assertComponentEquals(expected, result, TOL);

  }

  @Test
  public void testNegativeF() {

    StateVector initialState = new StateVector(0, -5.9817694199201E+09, -1.7044851611248E+09, 0.0,
        -1.1068356354252E+00, -2.1045070068570E-01);
    TwoBodyPropagator propagator = new TwoBodyPropagator();
    propagator.configure(1.3289051279402E+8, 0, initialState);
    StateVector buffer = new StateVector();

    StateVector result = propagator.getState(0, buffer);
    assertSame(result, buffer);
    assertComponentEquals(initialState, result, TOL);

    result = propagator.getState(1000, buffer);
    assertSame(result, buffer);
    StateVector expected = new StateVector(0.0, -5981770526.7557344, -1704485371.5755002, 0.0,
        -1.1068356321216661, -0.21045069974436911);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(-1000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(0.0, -5981768313.0844641, -1704484950.6740987, 0.0,
        -1.1068356387287352, -0.21045070162703128);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(-100000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(0.0, -5981658736.3400402, -1704464116.0500247, 0.0,
        -1.1068359657845308, -0.21045079482077456);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(10000000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(0.0, -5992837611.2934866, -1706589621.1305449, 0.0,
        -1.1068026588115871, -0.21044130699737565);
    assertComponentEquals(expected, result, TOL);

    result = propagator.getState(1000000000, buffer);
    assertSame(result, buffer);
    expected = new StateVector(0.0, -7087126433.1278620, -1914522049.9677811, 0.0,
        -1.1040314036539796, -0.20967244334495144);
    assertComponentEquals(expected, result, TOL);

  }

}
