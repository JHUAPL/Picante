# Picante

Picante is a Java implementation of the core functionality of [NAIF SPICE](https://naif.jpl.nasa.gov).

Key features:

- Pure Java; no native libraries needed
- replicates NAIF SPICE ephemeris calculations and frame transforms to 1e-12 precision
- Thread safe

Although Picante reproduces many SPICE capabilities, it is implemented very differently. Kernels are loaded into a "
SpiceEnvironment". The user can instantiate multiple SpiceEnvironments to compare calculations with different sets of
kernels, or to run a multithreaded application.

Picante was developed by the Analysis and Applications group of the Space Exploration Sector of the Johns Hopkins
University Applied Physics Laboratory.
It is released under the [MIT](LICENSE.md) License.

## Maven Coordinates

```
<dependency>
    <groupId>edu.jhuapl.ses</groupId>
    <artifactId>picante</artifactId>
    <version>0.0.0</version>
</dependency>
```

### Demo Code

The [unit tests](src/test/java/picante/demo) show how to replicate many common NAIF functions using Picante.
The [demo](src/main/java/picante/demo) package contains a longer example, modeled on an example in the
SPICE [tutorials](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/Tutorials/pdf/individual_docs/38_program_idl.pdf).

Many of the binary CK and SPK files needed to run the examples are not included in this repository. Run
the [getCK](src/test/resources/kernels/ck/getCK.bash) and [getSPK](src/test/resources/kernels/spk/getSPK.bash) scripts
to download them from NAIF.

## Incompatibilities with NAIF/SPICE:

- CK types other than 2 or 3 are not supported.
- Unsupported SPK types:
    - 10
    - 14
    - 15
    - 18 and higher
- DSK files are not supported.
- Product and Switch frames (new in N0067) are not supported.
- Time format incompatibilities. Picante's [TimeConversion](src/main/java/picante/time/TimeConversion.java) class can parse all the example time format strings listed in
  the notes for [STR2ET](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/str2et.html).
    - All times are assumed to be UTC. Neither time system modifiers (e.g. TT, TDT, TDB, etc) nor U.S. Time zones are
      supported.
        - TimeConversion fails on "2000 Jan 01 00:00:00 PST" but STR2ET will accept it.
        - TimeConversion fails on "2000 Jan 01 00:00:00 TDB" but STR2ET will accept it.
    - STR2ET will fail on the following examples, but TimeConversion will parse them:
        - 1997 Jan 32 12:29:29 translates to 1997-02-01T12:29:29.000
        - 1997 Feb 29, 12:29:20.0 translates to 1997-03-01T12:29:20.000
        - 1992 Mar 12 12:62:20 translates to 1992-03-12T13:02:20.000
        - 1993 Mar 18 15:29:60.5 translates to 1993-03-18T15:30:00.500
- Built-in enumerations containing commonly used solar system bodies (`CelestialBodies`) and frames (`CelestialFrames`)
  have entries that deviate from the standard NAIF string names:
    - `DE-###` frames have the minus sign removed: `DE###`
    - The asteroid `52_EUROPA` is assigned the entry: `A52_EUROPA`
    - The corresponding IAU body-fixed frame is: `IAU_A52_EUROPA` to preserve the `IAU_` prefix property commonly
      expected.
