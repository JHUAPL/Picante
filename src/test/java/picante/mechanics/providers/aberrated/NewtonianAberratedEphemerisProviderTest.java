package picante.mechanics.providers.aberrated;

// package crucible.core.mechanics.derived.aberrations;
//
// import static crucible.core.junit.AssertTools.assertComponentRelativeEquality;
// import static crucible.core.junit.AssertTools.assertRelativeEquality;
// import static org.easymock.EasyMock.createMock;
// import static org.easymock.EasyMock.expect;
// import static org.easymock.EasyMock.replay;
// import static org.easymock.EasyMock.verify;
// import static org.junit.Assert.assertFalse;
// import static org.junit.Assert.assertSame;
//
// import java.util.List;
// import java.util.Map;
// import java.util.Set;
//
// import org.junit.Before;
// import org.junit.Test;
//
// import com.google.common.collect.ImmutableMap;
// import com.google.common.collect.Lists;
// import com.google.common.collect.Sets;
// import com.google.common.io.Resources;
//
// import crucible.core.math.vectorspace.RotationMatrixIJK;
// import crucible.core.math.vectorspace.VectorIJK;
// import crucible.core.mechanics.CelestialBodies;
// import crucible.core.mechanics.CelestialFrames;
// import crucible.core.mechanics.Coverage;
// import crucible.core.mechanics.EphemerisAndFrameProvider;
// import crucible.core.mechanics.EphemerisID;
// import crucible.core.mechanics.EphemerisSource;
// import crucible.core.mechanics.FrameID;
// import crucible.core.mechanics.FrameSource;
// import crucible.core.mechanics.FrameTransformFunction;
// import crucible.core.mechanics.FrameTransformFunctions;
// import crucible.core.mechanics.PositionVectorFunction;
// import crucible.core.mechanics.PositionVectorFunctions;
// import crucible.core.mechanics.StateTransformFunction;
// import crucible.core.mechanics.StateTransformFunctions;
// import crucible.core.mechanics.StateVector;
// import crucible.core.mechanics.StateVectorFunction;
// import crucible.core.mechanics.StateVectorFunctions;
// import crucible.core.mechanics.providers.reference.ReferenceEphemerisProvider;
// import crucible.mantle.spice.SpiceEnvironment;
// import crucible.mantle.spice.SpiceEnvironmentBuilder;
//
// public class NewtonianAberratedEphemerisProviderTest {
//
// private final static double TOL = 1.0E-13;
//
// /**
// * This tolerance is necessary to introduce, because of the fact that our position vector
// * implementations of stellar aberration are not able to evaluate directly to the SPICE backed
// * state vector functions. The code numerically differentiates the position to obtain the velocity
// * that is fed into the code.
// */
// private final static double POS_STEL_TOL = 1.0E-12;
//
// private EphemerisAndFrameProvider mock;
// private NewtonianAberratedEphemerisProvider providerWithMock;
// private AberratedEphemerisProvider ltProvider;
// private AberratedEphemerisProvider cnProvider;
//
// @Before
// public void setUp() throws Exception {
//
// Map<FrameID, EphemerisID> frameCenterMap =
// ImmutableMap.<FrameID, EphemerisID>of(CelestialFrames.IAU_URANUS, CelestialBodies.URANUS,
// CelestialFrames.IAU_EARTH, CelestialBodies.EARTH);
//
// mock = createMock(EphemerisAndFrameProvider.class);
// providerWithMock =
// new NewtonianAberratedEphemerisProvider(mock, mock, CelestialBodies.SOLAR_SYSTEM_BARYCENTER,
// CelestialFrames.J2000, frameCenterMap, 2, 1.0);
// SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
// builder.load("lt.bsp",
// Resources.asByteSource(Resources.getResource(NewtonianAberratedEphemerisProvider.class,
// "lt.bsp")));
// builder.load("sat.bsp",
// Resources.asByteSource(Resources.getResource(NewtonianAberratedEphemerisProvider.class,
// "sat.bsp")));
// builder.load("lsk.tls",
// Resources.asByteSource(Resources.getResource(NewtonianAberratedEphemerisProvider.class,
// "lsk.tls")));
// builder.load("pck.tpc",
// Resources.asByteSource(Resources.getResource(NewtonianAberratedEphemerisProvider.class,
// "pck.tpc")));
// SpiceEnvironment env = builder.build();
// ReferenceEphemerisProvider provider =
// new ReferenceEphemerisProvider(env.getEphemerisSources(), env.getFrameSources());
//
// this.ltProvider =
// new NewtonianAberratedEphemerisProvider(provider, provider,
// CelestialBodies.SOLAR_SYSTEM_BARYCENTER,
// CelestialFrames.J2000, frameCenterMap, 1, 1.0);
//
// this.cnProvider =
// new NewtonianAberratedEphemerisProvider(provider, provider,
// CelestialBodies.SOLAR_SYSTEM_BARYCENTER,
// CelestialFrames.J2000, frameCenterMap, 3, 1.0);
//
// }
//
// @Test
// public void testGetEphemerisSourcesInLoadOrder() {
// List<EphemerisSource> sources = Lists.newArrayList();
// expect(mock.getEphemerisSourcesInLoadOrder(sources)).andReturn(sources);
// replay(mock);
// List<EphemerisSource> result = providerWithMock.getEphemerisSourcesInLoadOrder(sources);
// assertSame(result, sources);
// verify(mock);
// }
//
// @Test
// public void testGetKnownObjects() {
// Set<EphemerisID> ids = Sets.newHashSet();
// expect(mock.getKnownObjects(ids)).andReturn(ids);
// replay(mock);
// Set<EphemerisID> result = providerWithMock.getKnownObjects(ids);
// assertSame(result, ids);
// verify(mock);
// }
//
// @Test
// public void testIsAwareOfEphemerisID() {
// expect(mock.isAwareOf(CelestialBodies.ADRASTEA)).andReturn(false);
// replay(mock);
// boolean result = providerWithMock.isAwareOf(CelestialBodies.ADRASTEA);
// assertFalse(result);
// verify(mock);
// }
//
// @Test
// public void testGetFrameProvider() {
// assertSame(mock, providerWithMock.getFrameProvider());
// }
//
// @Test
// public void testCreateStateVectorFunction() throws Exception {
// StateVectorFunction function =
// StateVectorFunctions.createFixed(CelestialBodies.ADRASTEA, CelestialBodies.AEGAEON,
// CelestialFrames.B1950, VectorIJK.K);
// expect(
// mock.createStateVectorFunction(CelestialBodies.ADRASTEA, CelestialBodies.AEGAEON,
// CelestialFrames.B1950, Coverage.ALL_TIME)).andReturn(function);
// replay(mock);
// StateVectorFunction result =
// providerWithMock.createStateVectorFunction(CelestialBodies.ADRASTEA,
// CelestialBodies.AEGAEON, CelestialFrames.B1950, Coverage.ALL_TIME);
// assertSame(result, function);
// verify(mock);
// }
//
// @Test
// public void testCreatePositionVectorFunction() throws Exception {
// PositionVectorFunction function =
// PositionVectorFunctions.createFixed(CelestialBodies.ADRASTEA, CelestialBodies.AEGAEON,
// CelestialFrames.B1950, VectorIJK.K);
// expect(
// mock.createPositionVectorFunction(CelestialBodies.ADRASTEA, CelestialBodies.AEGAEON,
// CelestialFrames.B1950, Coverage.ALL_TIME)).andReturn(function);
// replay(mock);
// PositionVectorFunction result =
// providerWithMock.createPositionVectorFunction(CelestialBodies.ADRASTEA,
// CelestialBodies.AEGAEON, CelestialFrames.B1950, Coverage.ALL_TIME);
// assertSame(result, function);
// verify(mock);
// }
//
// @Test
// public void testGetFrameSourcesInLoadOrder() {
// List<FrameSource> sources = Lists.newArrayList();
// expect(mock.getFrameSourcesInLoadOrder(sources)).andReturn(sources);
// replay(mock);
// List<FrameSource> result = providerWithMock.getFrameSourcesInLoadOrder(sources);
// assertSame(result, sources);
// verify(mock);
// }
//
// @Test
// public void testGetKnownFrames() {
// Set<FrameID> ids = Sets.newHashSet();
// expect(mock.getKnownFrames(ids)).andReturn(ids);
// replay(mock);
// Set<FrameID> result = providerWithMock.getKnownFrames(ids);
// assertSame(result, ids);
// verify(mock);
// }
//
// @Test
// public void testIsAwareOfFrameID() {
// expect(providerWithMock.isAwareOf(CelestialFrames.B1950)).andReturn(false);
// replay(mock);
// boolean result = providerWithMock.isAwareOf(CelestialFrames.B1950);
// assertFalse(result);
// verify(mock);
// }
//
// @Test
// public void testCreateFrameTransformFunction() throws Exception {
// FrameTransformFunction function =
// FrameTransformFunctions.createFixed(CelestialFrames.B1950, CelestialFrames.DE102,
// RotationMatrixIJK.IDENTITY);
// expect(
// mock.createFrameTransformFunction(CelestialFrames.B1950, CelestialFrames.DE102,
// Coverage.ALL_TIME)).andReturn(function);
// replay(mock);
// FrameTransformFunction result =
// providerWithMock.createFrameTransformFunction(CelestialFrames.B1950, CelestialFrames.DE102,
// Coverage.ALL_TIME);
// assertSame(result, function);
// verify(mock);
// }
//
// @Test
// public void testCreateStateTransformFunction() throws Exception {
// StateTransformFunction function =
// StateTransformFunctions.createFixed(CelestialFrames.B1950, CelestialFrames.DE102,
// RotationMatrixIJK.IDENTITY);
// expect(
// mock.createStateTransformFunction(CelestialFrames.B1950, CelestialFrames.DE102,
// Coverage.ALL_TIME)).andReturn(function);
// replay(mock);
// StateTransformFunction result =
// providerWithMock.createStateTransformFunction(CelestialFrames.B1950, CelestialFrames.DE102,
// Coverage.ALL_TIME);
// assertSame(result, function);
// verify(mock);
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testInertialAPVF_GEO_LT_UOE() throws Exception {
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME, AberrationCorrection.NONE)
// .getLightTime(1000.0);
// }
//
// @Test
// public void testInertialAPVF_GEO() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.NONE);
//
// assertSame(AberrationCorrection.NONE, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertComponentRelativeEquality(new VectorIJK(-1.4507978201886E+09, -4.3183340565292E+09,
// -9.1829680868899E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_LT() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4508792552512E+09, -4.3183036101542E+09,
// -9.1826277137983E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_XLT() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4507163846155E+09, -4.3183645014587E+09,
// -9.1833084570091E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_CN() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4508792550831E+09, -4.3183036102171E+09,
// -9.1826277145009E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_XCN() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4507163844473E+09, -4.3183645015215E+09,
// -9.1833084577117E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_LTS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4512712012555E+09, -4.3181740929521E+09,
// -9.1825247261046E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_XLTS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4503244125572E+09, -4.3184939726888E+09,
// -9.1834113727255E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_CNS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4512712010874E+09, -4.3181740930150E+09,
// -9.1825247268073E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testInertialAPVF_XCNS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4503244123891E+09, -4.3184939727516E+09,
// -9.1834113734282E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testInertialASVF_GEO_LT_UOE() throws Exception {
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME, AberrationCorrection.NONE)
// .getLightTime(1000.0);
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testInertialASVF_GEO_DLT_UOE() throws Exception {
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME, AberrationCorrection.NONE)
// .getLightTimeDerivative(1000.0);
// }
//
// @Test
// public void testInertialASVF_GEO() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.NONE);
//
// assertSame(AberrationCorrection.NONE, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertComponentRelativeEquality(new VectorIJK(-1.4507978201886E+09, -4.3183340565292E+09,
// -9.1829680868899E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4507978201886E+09, -4.3183340565292E+09,
// -9.1829680868899E+08, 3.5037329581422E+01, 3.0712319773748E+00, -1.2717641650886E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_LT() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4508792552512E+09, -4.3183036101542E+09,
// -9.1826277137983E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184746320087E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4508792552512E+09, -4.3183036101542E+09,
// -9.1826277137983E+08, 3.5037538304582E+01, 3.0710483801115E+00, -1.2837821388766E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_XLT() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4507163846155E+09, -4.3183645014587E+09,
// -9.1833084570091E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894227644883E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4507163846155E+09, -4.3183645014587E+09,
// -9.1833084570091E+08, 3.5037120870533E+01, 3.0714155597799E+00, -1.2597470246282E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_CN() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4508792550831E+09, -4.3183036102171E+09,
// -9.1826277145009E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184742727556E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4508792550831E+09, -4.3183036102171E+09,
// -9.1826277145009E+08, 3.5037538304649E+01, 3.0710483803040E+00, -1.2837821349171E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_XCN() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4507163844473E+09, -4.3183645015215E+09,
// -9.1833084577117E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894224052163E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4507163844473E+09, -4.3183645015215E+09,
// -9.1833084577117E+08, 3.5037120870601E+01, 3.0714155599724E+00, -1.2597470206702E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_LTS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4512712012555E+09, -4.3181740929521E+09,
// -9.1825247261046E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184746320087E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4512712012555E+09, -4.3181740929521E+09,
// -9.1825247261046E+08, 3.5078777595238E+01, 3.0586769407814E+00, -3.4359754005848E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_XLTS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4503244125572E+09, -4.3184939726888E+09,
// -9.1834113727255E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894227644883E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4503244125572E+09, -4.3184939726888E+09,
// -9.1834113727255E+08, 3.4995881037707E+01, 3.0837775813318E+00, 8.9216167605068E-03),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_CNS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4512712010874E+09, -4.3181740930150E+09,
// -9.1825247268073E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184742727556E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4512712010874E+09, -4.3181740930150E+09,
// -9.1825247268073E+08, 3.5078777595304E+01, 3.0586769409763E+00, -3.4359753964850E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testInertialASVF_XCNS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.J2000, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.J2000, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.4503244123891E+09, -4.3184939727516E+09,
// -9.1834113734282E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894224052163E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.4503244123891E+09, -4.3184939727516E+09,
// -9.1834113734282E+08, 3.4995881037777E+01, 3.0837775815219E+00, 8.9216167986836E-03),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testNonInertialAPVF_GEO_LT_UOE() throws Exception {
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.NONE).getLightTime(1000.0);
// }
//
// @Test
// public void testNonInertialAPVF_GEO() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.NONE);
//
// assertSame(AberrationCorrection.NONE, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertComponentRelativeEquality(new VectorIJK(3.5119965205180E+08, -4.2956665839642E+08,
// 4.6139163037581E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_LT() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9637504662283E+08, -5.1904125584626E+08,
// 4.6138959910129E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_XLT() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4754047886137E+08, 8.9274158577805E+07,
// 4.6139366149563E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_CN() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9637504659970E+08, -5.1904125565955E+08,
// 4.6138959910548E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_XCN() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4754047868809E+08, 8.9274158504518E+07,
// 4.6139366149982E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_LTS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9628907243190E+08, -5.1944298254806E+08,
// 4.6138544394049E+09), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_XLTS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4723548969670E+08, 8.8998894897286E+07,
// 4.6139781158713E+09), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_CNS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9628907240877E+08, -5.1944298236137E+08,
// 4.6138544394469E+09), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test
// public void testNonInertialAPVF_XCNS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4723548952341E+08, 8.8998894823994E+07,
// 4.6139781159132E+09), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testNonInertialASVF_GEO_LT_UOE() throws Exception {
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.NONE).getLightTime(1000.0);
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testNonInertialASVF_GEO_DLT_UOE() throws Exception {
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.NONE).getLightTimeDerivative(1000.0);
// }
//
// @Test
// public void testNonInertialASVF_GEO() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.NONE);
//
// assertSame(AberrationCorrection.NONE, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertComponentRelativeEquality(new VectorIJK(3.5119965205180E+08, -4.2956665839642E+08,
// 4.6139163037581E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.5119965205180E+08, -4.2956665839642E+08,
// 4.6139163037581E+09, 4.3457127668304E+04, 3.5567500885237E+04, -1.0316316729210E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_LT() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9637504662283E+08, -5.1904125584626E+08,
// 4.6138959910129E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184746320087E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.9637504662283E+08, -5.1904125584626E+08,
// 4.6138959910129E+09, 5.2539069164289E+04, -1.9845965125422E+04, -1.0316156651528E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_XLT() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4754047886137E+08, 8.9274158577805E+07,
// 4.6139366149563E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894227644883E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(5.4754047886137E+08, 8.9274158577805E+07,
// 4.6139366149563E+09, -9.0651678520735E+03, 5.5414300699254E+04, -1.0316476793322E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_CN() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9637504659970E+08, -5.1904125565955E+08,
// 4.6138959910548E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184742727556E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.9637504659970E+08, -5.1904125565955E+08,
// 4.6138959910548E+09, 5.2539069145389E+04, -1.9845965123080E+04, -1.0316156651734E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_XCN() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4754047868809E+08, 8.9274158504518E+07,
// 4.6139366149982E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894224052163E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(5.4754047868809E+08, 8.9274158504518E+07,
// 4.6139366149982E+09, -9.0651678446538E+03, 5.5414300681711E+04, -1.0316476793528E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_LTS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9628907243190E+08, -5.1944298254806E+08,
// 4.6138544394049E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184746320087E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.9628907243190E+08, -5.1944298254806E+08,
// 4.6138544394049E+09, 5.2579747148988E+04, -1.9837215670838E+04, -1.0307617166796E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_XLTS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4723548969670E+08, 8.8998894897286E+07,
// 4.6139781158713E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894227644883E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(5.4723548969670E+08, 8.8998894897286E+07,
// 4.6139781158713E+09, -9.0372540476336E+03, 5.5383436565555E+04, -1.0325006550829E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_CNS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(-1.9628907240877E+08, -5.1944298236137E+08,
// 4.6138544394469E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184742727556E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(-1.9628907240877E+08, -5.1944298236137E+08,
// 4.6138544394469E+09, 5.2579747130089E+04, -1.9837215668496E+04, -1.0307617167004E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testNonInertialASVF_XCNS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_URANUS, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_URANUS, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(5.4723548952341E+08, 8.8998894823994E+07,
// 4.6139781159132E+09), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894224052163E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(5.4723548952341E+08, 8.8998894823994E+07,
// 4.6139781159132E+09, -9.0372540402134E+03, 5.5383436548010E+04, -1.0325006551033E+01),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testZeroFrameNonInertialAPVF_GEO_LT_UOE() throws Exception {
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.NONE).getLightTime(1000.0);
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_GEO() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.NONE);
//
// assertSame(AberrationCorrection.NONE, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertComponentRelativeEquality(new VectorIJK(3.8251056212497E+09, -2.4741443605658E+09,
// -9.1829681315825E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_LT() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8250559726152E+09, -2.4742157304774E+09,
// -9.1826277584934E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_XLT() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8251552686100E+09, -2.4740729898020E+09,
// -9.1833085016991E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_CN() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8250559727177E+09, -2.4742157303301E+09,
// -9.1826277591960E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_XCN() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8251552687125E+09, -2.4740729896546E+09,
// -9.1833085024018E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_LTS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8248335059426E+09, -2.4745634443764E+09,
// -9.1825247708118E+08), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_XLTS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// ltProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8253776971862E+09, -2.4737252392845E+09,
// -9.1834114174035E+08), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_CNS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8248335060451E+09, -2.4745634442291E+09,
// -9.1825247715145E+08), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialAPVF_XCNS() throws Exception {
//
// AberratedPositionVectorFunction apvf =
// cnProvider.createAberratedPositionVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8253776972887E+09, -2.4737252391372E+09,
// -9.1834114181062E+08), apvf.getPosition(1000.0, new VectorIJK()), POS_STEL_TOL);
//
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testZeroFrameNonInertialASVF_GEO_LT_UOE() throws Exception {
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.NONE).getLightTime(1000.0);
// }
//
// @Test(expected = UnsupportedOperationException.class)
// public void testZeroFrameNonInertialASVF_GEO_DLT_UOE() throws Exception {
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.NONE).getLightTimeDerivative(1000.0);
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_GEO() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.NONE);
//
// assertSame(AberrationCorrection.NONE, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertComponentRelativeEquality(new VectorIJK(3.8251056212497E+09, -2.4741443605658E+09,
// -9.1829681315825E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8251056212497E+09, -2.4741443605658E+09,
// -9.1829681315825E+08, -1.8041175849744E+05, -2.7889639092007E+05, -1.7186792270665E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_LT() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8250559726152E+09, -2.4742157304774E+09,
// -9.1826277584934E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184746320087E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8250559726152E+09, -2.4742157304774E+09,
// -9.1826277584934E+08, -1.8041696264398E+05, -2.7889277032784E+05, -1.7307222872878E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_XLT() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8251552686100E+09, -2.4740729898020E+09,
// -9.1833085016991E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894227644883E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8251552686100E+09, -2.4740729898020E+09,
// -9.1833085016991E+08, -1.8040655428874E+05, -2.7890001141939E+05, -1.7066370000156E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_CN() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT);
//
// assertSame(AberrationCorrection.LT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8250559727177E+09, -2.4742157303301E+09,
// -9.1826277591960E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184742727556E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8250559727177E+09, -2.4742157303301E+09,
// -9.1826277591960E+08, -1.8041696263323E+05, -2.7889277033531E+05, -1.7307222832765E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_XCN() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT);
//
// assertSame(AberrationCorrection.XLT, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8251552687125E+09, -2.4740729896546E+09,
// -9.1833085024018E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894224052163E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8251552687125E+09, -2.4740729896546E+09,
// -9.1833085024018E+08, -1.8040655427799E+05, -2.7890001142686E+05, -1.7066369960058E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_LTS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877848E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8248335059426E+09, -2.4745634443764E+09,
// -9.1825247708118E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184746320087E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8248335059426E+09, -2.4745634443764E+09,
// -9.1825247708118E+08, -1.8044229615110E+05, -2.7887651090615E+05, -3.8830362773114E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_XLTS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// ltProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881670E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8253776971862E+09, -2.4737252392845E+09,
// -9.1834114174035E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894227644883E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8253776971862E+09, -2.4737252392845E+09,
// -9.1834114174035E+08, -1.8038121810236E+05, -2.7891626806591E+05, 4.4539243700458E-03),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_CNS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.LT_S);
//
// assertSame(AberrationCorrection.LT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501223877914E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8248335060451E+09, -2.4745634442291E+09,
// -9.1825247715145E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5999184742727556E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8248335060451E+09, -2.4745634442291E+09,
// -9.1825247715145E+08, -1.8044229614036E+05, -2.7887651091362E+05, -3.8830362731598E-02),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// @Test
// public void testZeroFrameNonInertialASVF_XCNS() throws Exception {
//
// AberratedStateVectorFunction apvf =
// cnProvider.createAberratedStateVectorFunction(CelestialBodies.PLUTO_BARYCENTER,
// CelestialBodies.EARTH, CelestialFrames.IAU_EARTH, Coverage.ALL_TIME,
// AberrationCorrection.XLT_S);
//
// assertSame(AberrationCorrection.XLT_S, apvf.getCorrection());
// assertSame(CelestialBodies.PLUTO_BARYCENTER, apvf.getTargetID());
// assertSame(CelestialBodies.EARTH, apvf.getObserverID());
// assertSame(CelestialFrames.IAU_EARTH, apvf.getFrameID());
//
// assertRelativeEquality(1.5501287881736E+04, apvf.getLightTime(1000.0), TOL);
//
// assertComponentRelativeEquality(new VectorIJK(3.8253776972887E+09, -2.4737252391372E+09,
// -9.1834114181062E+08), apvf.getPosition(1000.0, new VectorIJK()), TOL);
//
// assertRelativeEquality(-4.5995894224052163E-05, apvf.getLightTimeDerivative(1000.0), TOL);
//
// assertComponentRelativeEquality(new StateVector(3.8253776972887E+09, -2.4737252391372E+09,
// -9.1834114181062E+08, -1.8038121809162E+05, -2.7891626807338E+05, 4.4539244087406E-03),
// apvf.getState(1000.0, new StateVector()), TOL);
//
// }
//
// }
