KPL/FK

LRO Frame Definitions Kernel -- Initial Post-Launch Version
==============================================================================

   This frame kernel contains the LRO spacecraft and science instrument
   frame definitions. This frame kernel also contains name - to - NAIF
   ID mappings for LRO science instruments and s/c structures (see the
   last section of the file.)


Version and Date
--------------------------------------------------------
  Version 2.0 Post-Launch Release -- February 5, 2010
             Ralph Casasanta with inputs from LAMP and LOLA SOCs
  
      Updated frames information for LAMP and LOLA instrument alignments 
      as calculated during calibration activities conducted during the
      LRO commissioning phase (approximately 24 June through 14 September 2009). 
      The updated frames are derived from on-orbit calibration from the
      LRO commissioning orbit(30 x 216 Km).
      Also modified "origin to reference frame" information to reflect metric units 
      (in centimeters) rather than inches.

  Version 1.0 Pre-Launch release -- June 15, 2009
             Leslie Hartz, Ralph Casasanta
  
      Updated frames information for instrument alignment as calculated
      during calibration activities conducted by LRO Project personnel during
      from April 2009. The updated frames are based on 
      LRO optical measurements or LRO mechanical drawings.
      
      The spacecraft body orbital SPK file is modeled as a point-mass.

      Currently, the offsets noted within this kernel are referenced in inches; 
      we will modifiy this information to SI units when we release the 
      post-launch version sometime after the commissioning and calibration 
      portion of the mission.
      This will occur sometime after the L + 60D timeframe or whenever 
      LRO completes the calibration and commissioning phase. 

  Version 0.2 draft -- May 06, 2008 -- Ralph Casasanta, Boris Semenov

      Modified HGA and SA IDs for the CK identifier to indicate we use
      the main object structure and not to the articulating booms.
      NOTE: Still does not contain a description for any of the frames.

   Version 0.1 draft -- November 14, 2007 -- Boris Semenov

      Added HGA and SA definitions and changed their IDs and
      relationship. Fixed frame ID that is a part of the keyword name
      in numerous fixed offset frames. Added name-ID mapping keywords.
      Minor revisions to the comments. Still does not contain a
      description for any of the frames.

   Version 0.0 draft -- November 14, 2007 -- Eric B. Holmes

      Initial Release. Contains Euler angles from LRO I-Kernel
      files. Does not contain a description for any of the frames.


References
--------------------------------------------------------

   1. C-kernel Required Reading

   2. Kernel Pool Required Reading

   3. Frames Required Reading

   4. Cosmic Ray Telescope for the Effects of Radiation (CRaTER)
      I-Kernel File "crater???.ti" TBD

   5a. Diviner Lunar Radiometer Experiment detector layout  
       relative to the instrument fixed reference frame LRO_DLRE; 
       "lro_dlre_frames_2009160_v01.tf"; Version 1.0, Jia Zong, June 09, 2009
   5b Diviner Lunar Radiometer Experiment (DLRE) I-Kernel File
      "lro_dlre_2009160_v01.ti"; Version 1.0, Jia Zong, May 21 2009

   6. Lyman-Alpha Mapping Project (LAMP) I-Kernel File "lamp???.ti" TBD

   7. Lunar Explorer Neutron Detector (Lend) I-Kernel File "lend???.ti"
      TBD

   8. Lunar Orbiter Laser Altimeter (LOLA) I-Kernel File "lola???.ti"
      TBD

   9. Lunar Reconnaissance Orbiter Camera Instrument Kernel for NAC-L
      NAC-R and the WAC; "lro_lroc_v01.ti"; TBD

  10. (Mini RF) I-Kernel File "minirf???.t1" TBD

  11. Primary Star Tracker (STARP) I-Kernel File "starp???.ti" TBD

  12. Secondary Star Tracker (STARS) I-Kernel File "stars???.ti" TBD

  13. Miniature Inertial Measurement Unit (MIMU) I-Kernel File
     "mimu???.ti" TBD


Contact Information
-------------------------------------------------------------------------------

   Eric B. Holmes, Code 591, (301)-286-4046, eric.b.holmes@nasa.gov


Implementation Notes
--------------------------------------------------------

   This file is used by the SPICE system as follows: programs that make
   use of this frame kernel must ``load'' the kernel, normally during
   program initialization. The SPICELIB routine FURNSH loads a kernel
   file into the pool as shown below.

      CALL FURNSH ( 'frame_kernel_name; )       -- FORTRAN
      furnsh_c ( "frame_kernel_name" );         -- C
      cspice_furnsh, "frame_kernel_name"        -- IDL
      cspice_furnsh ( 'frame_kernel_name' );    -- MATLAB

   This file was created and may be updated with a text editor or word
   processor.


LRO Frames
-------------------------------------------------------------------------------

   The following LRO frames are defined in this kernel file:

        Frame Name                    Relative to              Type    NAIF ID
   =========================     =========================    =======   =======

   Spacecraft Bus and Spacecraft Structure Frames:
   -----------------------------------------------
      LRO_SC_BUS                rel.to J2000                  CK       -85000
      
      LRO_STARP                 rel.to SC_BUS                 FIXED    -85010
      LRO_STARS                 rel.to SC_BUS                 FIXED    -85011

      LRO_MIMU                  rel.to SC_BUS                 FIXED    -85012

      LRO_HGA                   rel.to SC_BUS                 CK       -85020

      LRO_SA                    rel.to SC_BUS                 CK       -85030

   Instrument Frames:
   ------------------
      LRO_CRATER                rel.to SC_BUS                 FIXED    -85100

      LRO_DLRE                  rel.to SC_BUS                 FIXED    -85200

      LRO_LAMP                  rel.to SC_BUS                 FIXED    -85300

      LRO_LEND                  rel.to SC_BUS                 FIXED    -85400

      LRO_LOLA                  rel.to SC_BUS                 FIXED    -85500

      LRO_LROCNACA              rel.to SC_BUS                 FIXED    -85600
      LRO_LROCNACB              rel.to SC_BUS                 FIXED    -85610
      LRO_LROCWAC               rel.to SC_BUS                 FIXED    -85620

      LRO_MINIRF                rel.to SC_BUS                 FIXED    -85700


LRO Frames Hierarchy
-------------------------------------------------------------------------------

   The diagram below shows LRO frames hierarchy:


                               "J2000" INERTIAL
               +--------------------------------------------+
               |                                            |
               | <--ck                                      | <--pck
               |                                            |
               |                                            V
               |                                        "IAU_EARTH"
               |                                        EARTH BFR(*)
               |                                        ------------
               |
               |
               |
               |
               |
               |
               V
          "LRO_SC_BUS"
        +----------------------------------------------------------------------+
        |              |                |              |            |          |
        |<--fixed      |<--fixed        |<-fixed       |<--fixed    |<--fixed  |
        |              |                |              |            |          |
        V              V                V             V             V          |
     "LRO_CRATER"   "LRO_DLRE"       "LRO_LAMP"     "LRO_LEND"   "LRO_LOLA"    |
     ------------   ----------       ----------     ----------   ----------    |
                                                                               |
   +---------------------------------------------------------------------------+
   |    |               |               |              |
   |    |<--fixed       |<--fixed       |<--fixed      |<-fixed
   |    |               |               |              |
   |    V               V               V              V
   | "LRO_LROCNACR"  "LRO_LROCNACL"  "LRO_LROCWAC"  "LRO_MINIRF"
   | --------------  --------------  -------------  ------------
   |
   +--------------------------------------------------------------
     |               |            |             |              |
     |<--fixed       |<--fixed    |<--fixed     |<--ck         |<--ck         
     V               V            V             V              V
  "LRO_STARP"   "LRO_STARS"   "LRO_MIMU"     "LRO SA"       "LRO HGA"
  -----------   -----------   ----------     -------        ---------   


Spacecraft Bus Frame
--------------------------------------------------------

   The spacecraft bus frame is defined by the spacecraft design as
   follows:

      *  +X axis is in the direction of the velocity vector half the year.  
      *    The other half of the year, the +X axis is opposite the velocity vector ;

      *  +Y axis is the anti-sun side of the spacecraft;

      *  +Z axis is in the in the nadir direction, instrument boresight direction;

      *  the origin of this frame is at the center of the spacecraft to launch 
         vehicle interface;

   Spacecraft bus attitude with respect to an inertial frame is
   provided by a C kernel (see [1] for more information).

   \begindata

      FRAME_LRO_SC_BUS         = -85000
      FRAME_-85000_NAME        = 'LRO_SC_BUS'
      FRAME_-85000_CLASS       = 3
      FRAME_-85000_CLASS_ID    = -85000
      FRAME_-85000_CENTER      = -85
      CK_-85000_SCLK           = -85
      CK_-85000_SPK            = -85

   \begintext


Cosmic Ray Telescope for the Effects of Radiation Frame
-------------------------------------------------------

   The CRaTER instrument frame is defined by the instrument design as
   follows:

      *  +X axis is parallel to the spacecraft +X axis;

      *  +Y axis is parallel to the spacecraft +Y axis;

      *  +Z axis is the boresight of the nadir telescope and parallel to the +Z axis
         of the spacecraft;

      *  the origin of this frame is at the spacecraft to instrument interface, 276.86, 25.40, 55.88;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern.  There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. This frame is based on LRO mechanical drawings.  
   It was not verified by measurement.

   \begindata

      FRAME_LRO_CRATER         = -85100
      FRAME_-85100_NAME        = 'LRO_CRATER'
      FRAME_-85100_CLASS       = 4
      FRAME_-85100_CLASS_ID    = -85100
      FRAME_-85100_CENTER      = -85
      TKFRAME_-85100_SPEC      = 'ANGLES'
      TKFRAME_-85100_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85100_ANGLES    = ( 0.0, 0.0, 0.0 )
      TKFRAME_-85100_AXES      = ( 1,   2,   3   )
      TKFRAME_-85100_UNITS     = 'DEGREES'

   \begintext


Diviner Lunar Radiometer Experiment Frame
-----------------------------------------

   The DLRE instrument frame is defined by the instrument alignment 
   cube face normals measured in the LRO_SC_BUS:

      *  +X axis is the normal of instrument alignment cube face 1;

      *  +Y axis is the normal of instrument alignment cube face 2;

      *  +Z axis is the cross product of X and Y;

      *  the origin of this frame is at the spacecraft to instrument interface, 236.22, 45.72, 60.96;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern.  There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles provided in the frame
   definition below are extracted from [5a]. The following transforms 
   convert directions from LRO_DLRE into LRO_SC_BUS.

   \begindata

      FRAME_LRO_DLRE           = -85200
      FRAME_-85200_NAME        = 'LRO_DLRE'
      FRAME_-85200_CLASS       = 4
      FRAME_-85200_CLASS_ID    = -85200
      FRAME_-85200_CENTER      = -85
      TKFRAME_-85200_SPEC      = 'MATRIX'
      TKFRAME_-85200_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85200_MATRIX    = ( 
				-0.867153123 
				-0.498040818 
				-0.000898401621
        			-0.498028823
				 0.867025903
				 0.0152766628
        			-0.00682946414
				 0.0136946357 
				-0.999882902
				)

   \begintext


Lyman-Alpha Mapping Project Frame
---------------------------------

   The LAMP instrument frame is defined by the instrument design as
   follows:

      *  +X axis is parallel to the +X axis of the spacecraft;

      *  +Y axis is rotated slightly from the +Y axis of the spacecraft;

      *  +Z axis is the boresight of the instrument and is nearly parallel to the +Z axis 
            of the spacecraft;

      *  the origin of this frame is at the instrument to spacecraft interface, 142.24, 109.22, 20.32;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern. There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below. 
   To determine the pointing of LAMP, we conducted the LAMP-403 scans, which are raster 
   observations of the stars gam Gru and zet Cas across the open LAMP aperture. The LAMP instrument
   team decided that the following information would define the boresight of the instrument 
   such that when the boresight was pointed towards a point source, the target spectrum would 
   appear primarily in detector row 14 (zero-indexed). The end result of the LAMP-403 analysis 
   is that to go from the spacecraft frame to the LAMP instrument frame it is necessary to rotate:
 -0.01516987 degrees about Y (offset in cross-slit direction)
  0.57339189 degrees about X (offset in along-slit direction)
  0.13802984 degrees about Z (rotation of slit about boresight)

   \begindata

      FRAME_LRO_LAMP           = -85300
      FRAME_-85300_NAME        = 'LRO_LAMP'
      FRAME_-85300_CLASS       = 4
      FRAME_-85300_CLASS_ID    = -85300
      FRAME_-85300_CENTER      = -85
      TKFRAME_-85300_SPEC      = 'ANGLES'
      TKFRAME_-85300_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85300_ANGLES    = ( -0.015169872, 0.57339189, 0.13802984   
                                   )
      TKFRAME_-85300_AXES      = (  2,   1,   3 )
      TKFRAME_-85300_UNITS     = 'DEGREES'

   \begintext


Lunar Explorer Neutron Detector (LEND) Frame
--------------------------------------------

   The LEND instrument frame is defined by the instrument design as
   follows:

      *  +X axis is parallel to the +X axis of the spacecraft;

      *  +Y axis is parallel to the +Y axis of the spacecraft;

      *  +Z axis is parallel to the +Z axis of the spacecraft and it in the same direction 
          as the LEND collimators;

      *  the origin of this frame is at the spacecraft to instrument interface, 45.72, 81.28, 60.96;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern. There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below, are based on LRO mechanical drawings.

   \begindata

      FRAME_LRO_LEND           = -85400
      FRAME_-85400_NAME        = 'LRO_LEND'
      FRAME_-85400_CLASS       = 4
      FRAME_-85400_CLASS_ID    = -85400
      FRAME_-85400_CENTER      = -85
      TKFRAME_-85400_SPEC      = 'ANGLES'
      TKFRAME_-85400_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85400_ANGLES    = ( 0.0, 0.0, 0.0 )
      TKFRAME_-85400_AXES      = ( 1,   2,   3   )
      TKFRAME_-85400_UNITS     = 'DEGREES'

   \begintext


Lunar Orbiter Laser Altimeter (LOLA) Frame
------------------------------------------

The location of the LOLA instrument [ref 8] and the s/c center of mass
   for the mapping configuration is specified relative to the s/c fixed
   reference, which is defined as follows:
   
The LOLA instrument frame is defined by the instrument design as
   follows:

      *  +X axis is parallel to the +X axis of the spacecraft;

      *  +Y axis is nearly parallel to the +Y axis of the spacecraft;

      *  +Z axis is nearly parallel to the spacecraft +Z axis and is laser channel 1;

      *  The LOLA offset is identified as the distance from the spacecraft bus frame to the LOLA 
          reference cube, which is at the base of the laser beam expander telescope, plus the 
          additional 0.15174 m to the top of the LOLA telescope along the z-axis. 
          The following values are the x-, y-, and z- components for this 
          offset and are listed in meters. (2.04608938, 0.96087438, 0.52301394) 


LOLA to s/c center of mass location
------------------------------------------

   The location of the LOLA instrument [ref 8] and the s/c center of mass
   for the mapping configuration is specified relative to the s/c fixed
   reference, which is defined as noted above.  The center of gravity mapping
   coordinates are given in meters.  Currently this information is:
   (TBD, TBD, TBD)

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below, are based on LRO optical measurements.

   The components of the LOLA Cube in the s/c Frame as a matrix are:

         ( 0.99998757, 0.00477017, 0.00145188,
          -0.0047642,  0.99998032, -0.0040808,
          -0.0014713, 0.00407379, 0.99999062 )

   However SPICE uses column major (FORTRAN) notation, as noted in the 
   data section below.

   \begindata

      FRAME_LRO_LOLA           = -85500
      FRAME_-85500_NAME        = 'LRO_LOLA'
      FRAME_-85500_CLASS       = 4
      FRAME_-85500_CLASS_ID    = -85500
      FRAME_-85500_CENTER      = -85
      TKFRAME_-85500_SPEC      = 'ANGLES' 'MATRIX'
      TKFRAME_-85500_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85500_MATRIX= (
                              0.99998757,
                             -0.0047642,
                             -0.0014713,
                              0.00477017,
                              0.99998032,
                              0.00407379,
                              0.00145188,
                             -0.0040808,
                              0.99999062 )
      TKFRAME_-85500_SC_MAPPING_CG_LOC = (0, 0, 0)


   \begintext


Lunar Reconnaissance Orbiter Camera-Narrow Angle Camera 1 (LROC NACL) Frame
--------------------------------------------------------------------------
   The location of the LROC NACL instrument is provided by [ref 9]; this provides
   all information for the NAC-L NAC-R and the WAC. 
   
   The LROC NACL instrument frame is defined by the instrument design as
   follows:

      *  +X axis is parallel to the +X axis of the spacecraft;

      *  +Y axis is approximately parallel to the +Y axis of the spacecraft;

      *  +Z axis is the boresight of the NACL;

      *  the origin of this frame is at the instrument to spacecraft interface, 134.62, 88.90, -17.78;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern. There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below, are from LRO optical measurements.

   \begindata

      FRAME_LRO_LROCNACA       = -85600
      FRAME_-85600_NAME        = 'LRO_LROCNACL'
      FRAME_-85600_CLASS       = 4
      FRAME_-85600_CLASS_ID    = -85600
      FRAME_-85600_CENTER      = -85
      TKFRAME_-85600_SPEC      = 'ANGLES'
      TKFRAME_-85600_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85600_ANGLES    = ( 1.636, -0.027, 0.0 )
      TKFRAME_-85600_AXES      = ( 1,   2,   3   )
      TKFRAME_-85600_UNITS     = 'DEGREES'

   \begintext


Lunar Reconnaissance Orbiter Camera-Narrow Angle Camera 2 (LROCNACR) Frame
--------------------------------------------------------------------------
   The location of the LROC NACL instrument is provided by [ref 9]; this provides
   all information for the NAC-L NAC-R and the WAC.

   The LROC NACR reference frame is rotated 180 degrees about the Z axis. 
   This rotation is performed first. The LROC NACR instrument frame is defined by the 
   instrument design as follows:

      *  +X axis is parallel to the spacecraft - X axis;

      *  +Y axis is parallel to the spacecraft - Y axis;

      *  +Z axis is the boresight of the NACR camera and is approximately in the +Z axis 
            of the spacecraft;

      *  the origin of this frame is at the spacecraft to instrument interface, 101.60, 88.90, -17.78;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern. There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below, are extracted from LRO optical measurements.

   \begindata

      FRAME_LRO_LROCNACB       = -85610
      FRAME_-85610_NAME        = 'LRO_LROCNACR'
      FRAME_-85610_CLASS       = 4
      FRAME_-85610_CLASS_ID    = -85610
      FRAME_-85610_CENTER      = -85
      TKFRAME_-85610_SPEC      = 'ANGLES'
      TKFRAME_-85610_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85610_ANGLES    = ( -1.129, 0.079, 180.0 )
      TKFRAME_-85610_AXES      = ( 3,   2,   1   )
      TKFRAME_-85610_UNITS     = 'DEGREES'

   \begintext


Lunar Reconnaissance Orbiter Camera-Wide Angle Camera (LROCWAC) Frame
---------------------------------------------------------------------
   The location of the LROC NACL instrument is provided by [ref 9]; this provides
   all information for the NAC-L NAC-R and the WAC.

   The LROC WAC instrument frame is defined by the instrument design as
   follows:

      *  +X axis is parallel to the spacecraft +X axis;

      *  +Y axis is parallel to the spacecraft +Y axis;

      *  +Z axis is parallel to the spacecraft +Z axis and is the boresight of the camera;

      *  the origin of this frame is at the spacecraft to instrument interface, 200.66, 106.68, 50.80;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern. There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below, are from LRO mechanical drawings.

   \begindata

      FRAME_LRO_LROCWAC        = -85620
      FRAME_-85620_NAME        = 'LRO_LROCWAC'
      FRAME_-85620_CLASS       = 4
      FRAME_-85620_CLASS_ID    = -85620
      FRAME_-85620_CENTER      = -85
      TKFRAME_-85620_SPEC      = 'ANGLES'
      TKFRAME_-85620_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85620_ANGLES    = ( 0.0, 0.0, 0.0 )
      TKFRAME_-85620_AXES      = ( 1,   2,   3   )
      TKFRAME_-85620_UNITS     = 'DEGREES'

   \begintext


MINI RF (MINIRF) Frame
----------------------
   The location of the Mini-RF instrument is provided by [ref 10].

   The MINI RF frame is defined by the instrument design as follows:

      *  +X axis is parallel to the spacecraft +X axis;

      *  +Y axis completes the frame;

      *  +Z axis is perpendicular the plane of the Mini RF antenna;

      *  the origin of this frame is at the instrument to spacecraft interface, 114.30, -38.10, 93.98;
         this offset (in centimeters) is from the LRO separation plane to the center 
         of the instrument to spacecraft bolt pattern. There is no implied accuracy/precision 
         in the conversion from inches to centimeters, other than the standard 2.54 centimeters
         per inch conversion.

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles, as documented in the frame
   definition below, are from LRO mechanical drawings.

   \begindata

      FRAME_LRO_MINIRF         = -85700
      FRAME_-85700_NAME        = 'LRO_MINIRF'
      FRAME_-85700_CLASS       = 4
      FRAME_-85700_CLASS_ID    = -85700
      FRAME_-85700_CENTER      = -85
      TKFRAME_-85700_SPEC      = 'ANGLES'
      TKFRAME_-85700_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85700_ANGLES    = ( -47.6, 0.0, 0.0 )
      TKFRAME_-85700_AXES      = (   1,   2,   3   )
      TKFRAME_-85700_UNITS     = 'DEGREES'

   \begintext


Primary Star Tracker (STARP) Frame
----------------------------------

   The Primary Star Tracker frame is defined by the instrument design
   as follows:

      *  +X axis is defined by the alignment cube face 1;

      *  +Y axis is defined by alignment cube face 2;

      *  +Z axis is the boresight of the star tracker;


   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles provided in the frame
   definition below are extracted from [11].   

   \begindata

      FRAME_LRO_STARP          = -85010
      FRAME_-85010_NAME        = 'LRO_STARP'
      FRAME_-85010_CLASS       = 4
      FRAME_-85010_CLASS_ID    = -85010
      FRAME_-85010_CENTER      = -85
      TKFRAME_-85010_SPEC      = 'ANGLES'
      TKFRAME_-85010_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85010_ANGLES    = ( 120.0, -30.0, 0.0 )
      TKFRAME_-85010_AXES      = (   2,     1,   3   )
      TKFRAME_-85010_UNITS     = 'DEGREES'

   \begintext


Secondary Star Tracker (STARS) Frame
------------------------------------

   The Secondary Star Tracker frame is defined by the instrument design
   as follows:

      *  +X axis is defined by alignment cube face 1;

      *  +Y axis is defined by alignment cube face 2;

      *  +Z axis is the boresight of the star tracker;

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles provided in the frame
   definition below are extracted from [12].

   \begindata

      FRAME_LRO_STARS          = -85011
      FRAME_-85011_NAME        = 'LRO_STARS'
      FRAME_-85011_CLASS       = 4
      FRAME_-85011_CLASS_ID    = -85011
      FRAME_-85011_CENTER      = -85
      TKFRAME_-85011_SPEC      = 'ANGLES'
      TKFRAME_-85011_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85011_ANGLES    = ( 180.0, -30.0, 0.0 )
      TKFRAME_-85011_AXES      = (   2,     1,   3   )
      TKFRAME_-85011_UNITS     = 'DEGREES'

   \begintext


Miniature Inertial Measurement Unit (MIMU) Frame
------------------------------------------------

   The MIMU frame is defined by the instrument design as follows:

      *  +X axis is parallel to the +X axis of the spacecraft;

      *  +Y axis is parallel to the +Y axis of the spacecraft;

      *  +Z axis is parallel to the +Z axis of the spacecraft;

   The orientation of this frame is fixed with respect to the
   spacecraft frame. The rotation angles provided in the frame
   definition below are extracted from [13].

   \begindata

      FRAME_LRO_MIMU           = -85012
      FRAME_-85012_NAME        = 'LRO_MIMU'
      FRAME_-85012_CLASS       = 4
      FRAME_-85012_CLASS_ID    = -85012
      FRAME_-85012_CENTER      = -85
      TKFRAME_-85012_SPEC      = 'ANGLES'
      TKFRAME_-85012_RELATIVE  = 'LRO_SC_BUS'
      TKFRAME_-85012_ANGLES    = ( 0.0, 0.0, 0.0 )
      TKFRAME_-85012_AXES      = ( 1,   2,   3   )
      TKFRAME_-85012_UNITS     = 'DEGREES'

   \begintext

High Gain Antenna (HGA) Frame
-----------------------------

   The HGA frame is defined by the antenna design as follows:
When both antenna gimbals are in the zero, zero position, the antenna dish is 
pointed along the -Z axis

      *  +X axis is aligned to the spacecraft +X axis;

      *  +Y axis is aligned to the spacecraft +Y axis;

      *  +Z axis is aligned to the spacecraft +Z axis;

   The orientation of this frame is provided in CK files.


   \begindata

      FRAME_LRO_HGA            = -85020
      FRAME_-85020_NAME        = 'LRO_HGA'
      FRAME_-85020_CLASS       = 3
      FRAME_-85020_CLASS_ID    = -85020
      FRAME_-85020_CENTER      = -85
      CK_-85020_SCLK           = -85
      CK_-85020_SPK            = -85

   \begintext


Solar Array (SA) Frame
----------------------

   The SA frame is defined by the solar array design as follows:

      *  +X axis is aligned to the spacecraft +X axis;

      *  +Y axis is aligned to the spacecraft +Y axis;

      *  +Z axis is aligned to the spacecraft +Z axis;

   The orientation of this frame is provided in CK files.

   \begindata

      FRAME_LRO_SA             = -85030
      FRAME_-85030_NAME        = 'LRO_SA'
      FRAME_-85030_CLASS       = 3
      FRAME_-85030_CLASS_ID    = -85030
      FRAME_-85030_CENTER      = -85
      CK_-85030_SCLK           = -85
      CK_-85030_SPK            = -85

   \begintext

Lunar Reconnaissance Orbiter NAIF ID Codes -- Definitions
========================================================================

   This section contains name to NAIF ID mappings for the LRO mission.
   Once the contents of this file is loaded into the KERNEL POOL, these
   mappings become available within SPICE, making it possible to use
   names instead of ID code in the high level SPICE routine calls.

   Spacecraft:
   -----------

      LRO                           -85
      LUNAR RECONNAISSANCE ORBITER  -85
      LRO_SPACECRAFT                -85000
      LRO_SC_BUS                    -85000

   Spacecraft structures:
   ----------------------

      LRO_STARP                     -85010
      LRO_STARS                     -85011
      LRO_MIMU                      -85012
      LRO_HGA                       -85020
      LRO_SA                        -85030

   Science Instruments:
   --------------------

      LRO_CRATER                    -85100
      LRO_DLRE                      -85200
      LRO_LAMP                      -85300
      LRO_LEND                      -85400
      LRO_LOLA                      -85500
      LRO_LROCNACL                  -85600
      LRO_LROCNACR                  -85610
      LRO_LROCWAC                   -85620
      LRO_MINIRF                    -85700

   The mappings summarized in this table are implemented by the keywords
   below.

   \begindata

      NAIF_BODY_NAME += ( 'LRO' )
      NAIF_BODY_CODE += ( -85 )

      NAIF_BODY_NAME += ( 'LUNAR RECONNAISSANCE ORBITER' )
      NAIF_BODY_CODE += ( -85 )

      NAIF_BODY_NAME += ( 'LRO_SPACECRAFT' )
      NAIF_BODY_CODE += ( -85000 )

      NAIF_BODY_NAME += ( 'LRO_SC_BUS' )
      NAIF_BODY_CODE += ( -85000 )

      NAIF_BODY_NAME += ( 'LRO_STARP' )
      NAIF_BODY_CODE += ( -85010 )

      NAIF_BODY_NAME += ( 'LRO_STARS' )
      NAIF_BODY_CODE += ( -85011 )

      NAIF_BODY_NAME += ( 'LRO_MIMU' )
      NAIF_BODY_CODE += ( -85012 )

      NAIF_BODY_NAME += ( 'LRO_HGA' )
      NAIF_BODY_CODE += ( -85020 )

      NAIF_BODY_NAME += ( 'LRO_SA' )
      NAIF_BODY_CODE += ( -85030 )

      NAIF_BODY_NAME += ( 'LRO_CRATER' )
      NAIF_BODY_CODE += ( -85100 )

      NAIF_BODY_NAME += ( 'LRO_DLRE' )
      NAIF_BODY_CODE += ( -85200 )

      NAIF_BODY_NAME += ( 'LRO_LAMP' )
      NAIF_BODY_CODE += ( -85300 )

      NAIF_BODY_NAME += ( 'LRO_LEND' )
      NAIF_BODY_CODE += ( -85400 )

      NAIF_BODY_NAME += ( 'LRO_LOLA' )
      NAIF_BODY_CODE += ( -85500 )

      NAIF_BODY_NAME += ( 'LRO_LROCNACL' )
      NAIF_BODY_CODE += ( -85600 )

      NAIF_BODY_NAME += ( 'LRO_LROCNACR' )
      NAIF_BODY_CODE += ( -85610 )

      NAIF_BODY_NAME += ( 'LRO_LROCWAC' )
      NAIF_BODY_CODE += ( -85620 )

      NAIF_BODY_NAME += ( 'LRO_MINIRF' )
      NAIF_BODY_CODE += ( -85700 )

   \begintext

