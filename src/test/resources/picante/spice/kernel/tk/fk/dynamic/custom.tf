KPL/FK

   \begintext


Mean Equator and Equinox of Date Frame
========================================================================

   From NAIF's documentations
   
   Examples of Mean Equator and Equinox of Date Frames
      
   \begindata

   FRAME_EQUATDATE           =  96199
   FRAME_96199_NAME          =  'EQUATDATE'
   FRAME_96199_CLASS         =  5
   FRAME_96199_CLASS_ID      =  96199
   FRAME_96199_CENTER        =  399
   FRAME_96199_RELATIVE      = 'J2000'
   FRAME_96199_DEF_STYLE     = 'PARAMETERIZED'
   FRAME_96199_FAMILY        = 'MEAN_EQUATOR_AND_EQUINOX_OF_DATE'
   FRAME_96199_PREC_MODEL    = 'EARTH_IAU_1976'
   FRAME_96199_ROTATION_STATE= 'ROTATING'
   
   \begintext


True Equator and Equinox of Date Frame
========================================================================

   From NAIF's documentations
   
   Examples of True Equator and Equinox of Date Frames
     
   \begindata

   FRAME_TRUEEQDATE          =  96198
   FRAME_96198_NAME          =  'TRUEEQDATE'
   FRAME_96198_CLASS         =  5
   FRAME_96198_CLASS_ID      =  96198
   FRAME_96198_CENTER        =  399
   FRAME_96198_RELATIVE      = 'J2000'
   FRAME_96198_DEF_STYLE     = 'PARAMETERIZED'
   FRAME_96198_FAMILY        = 'TRUE_EQUATOR_AND_EQUINOX_OF_DATE'
   FRAME_96198_PREC_MODEL    = 'EARTH_IAU_1976'
   FRAME_96198_NUT_MODEL     = 'EARTH_IAU_1980'
   FRAME_96198_ROTATION_STATE= 'ROTATING'

   \begintext
     

Mean Ecliptic and Equinox of Date Frame
========================================================================

   From NAIF's documentations
   
   Example of a Mean Ecliptic and Equinox of Date Frame
     
   \begindata

   FRAME_MEANECLIPTIC       =  96197
   FRAME_96197_NAME          =  'MEANECLIPTIC'
   FRAME_96197_CLASS         =  5
   FRAME_96197_CLASS_ID      =  96198
   FRAME_96197_CENTER        =  399
   FRAME_96197_RELATIVE      = 'J2000'
   FRAME_96197_DEF_STYLE     = 'PARAMETERIZED'
   FRAME_96197_FAMILY        = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'
   FRAME_96197_PREC_MODEL    = 'EARTH_IAU_1976'
   FRAME_96197_OBLIQ_MODEL   = 'EARTH_IAU_1980'
   FRAME_96197_ROTATION_STATE= 'ROTATING'

   \begintext

   Mean Ecliptic of Date (ECLIPDATE):

         All vectors are geometric: no aberration corrections are used.

         The X axis is the first point in Aries for the mean ecliptic of
         date, and the Z axis points along the ecliptic north pole.

         The Y axis is Z cross X, completing the right-handed reference frame.

      \begindata

         FRAME_SPP_ECLIPDATE          = -96900
         FRAME_-96900_NAME            = 'SPP_ECLIPDATE'
         FRAME_-96900_CLASS           = 5
         FRAME_-96900_CLASS_ID        = -96900
         FRAME_-96900_CENTER          = 399
         FRAME_-96900_RELATIVE        = 'J2000'
         FRAME_-96900_DEF_STYLE       = 'PARAMETERIZED'
         FRAME_-96900_FAMILY          = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'
         FRAME_-96900_PREC_MODEL      = 'EARTH_IAU_1976'
         FRAME_-96900_OBLIQ_MODEL     = 'EARTH_IAU_1980'
         FRAME_-96900_ROTATION_STATE  = 'ROTATING'

      \begintext

Mercury Based Frames
---------------------------------------------------------------

   These dynamic frames are used for analyzing data in a reference
   frame tied to the dynamics of Mercury.

   From [4]:

   Mercury-centric Solar Orbital (MSO):

         This system has its X axis pointing from the planet to
         the Sun, -Y points in direction of the planetary orbital
         velocity vector, and Z completes the right-handed system.

      \begindata

         FRAME_SPP_MSO                = -96903
         FRAME_-96903_NAME            = 'SPP_MSO'
         FRAME_-96903_CLASS           = 5
         FRAME_-96903_CLASS_ID        = -96903
         FRAME_-96903_CENTER          = 199
         FRAME_-96903_RELATIVE        = 'J2000'
         FRAME_-96903_DEF_STYLE       = 'PARAMETERIZED'
         FRAME_-96903_FAMILY          = 'TWO-VECTOR'
         FRAME_-96903_PRI_AXIS        = 'X'
         FRAME_-96903_PRI_VECTOR_DEF  = 'OBSERVER_TARGET_POSITION'
         FRAME_-96903_PRI_OBSERVER    = 'MERCURY'
         FRAME_-96903_PRI_TARGET      = 'SUN'
         FRAME_-96903_PRI_ABCORR      = 'NONE'
         FRAME_-96903_SEC_AXIS        = '-Y'
         FRAME_-96903_SEC_VECTOR_DEF  = 'OBSERVER_TARGET_VELOCITY'
         FRAME_-96903_SEC_OBSERVER    = 'SUN'
         FRAME_-96903_SEC_TARGET      = 'MERCURY'
         FRAME_-96903_SEC_FRAME       = 'J2000'
         FRAME_-96903_SEC_ABCORR      = 'NONE'

      \begintext

   Heliocentric Inertial (HCI) Frame:

         All vectors are geometric: no aberration corrections are used.

         The solar rotation axis is the primary vector: the Z axis points
         in the solar north direction.

         The ascending node on the Earth ecliptic of J2000 of the solar 
         equator forms the X axis. This is accomplished by using the +Z 
         axis of the ecliptic of J2000 as the secondary vector and HCI +Y 
         as the secondary axis.

         The Y axis is Z cross X, completing the right-handed reference frame.

      \begindata

         FRAME_SPP_HCI                = -96911
         FRAME_-96911_NAME            = 'SPP_HCI'
         FRAME_-96911_CLASS           = 5
         FRAME_-96911_CLASS_ID        = -96911
         FRAME_-96911_CENTER          = 10
         FRAME_-96911_RELATIVE        = 'J2000'
         FRAME_-96911_DEF_STYLE       = 'PARAMETERIZED'
         FRAME_-96911_FAMILY          = 'TWO-VECTOR'
         FRAME_-96911_PRI_AXIS        = 'Z'
         FRAME_-96911_PRI_VECTOR_DEF  = 'CONSTANT'
         FRAME_-96911_PRI_FRAME       = 'IAU_SUN'
         FRAME_-96911_PRI_SPEC        = 'RECTANGULAR'
         FRAME_-96911_PRI_VECTOR      = ( 0, 0, 1 )
         FRAME_-96911_SEC_AXIS        = 'Y'
         FRAME_-96911_SEC_VECTOR_DEF  = 'CONSTANT'
         FRAME_-96911_SEC_FRAME       = 'ECLIPJ2000'
         FRAME_-96911_SEC_SPEC        = 'RECTANGULAR'
         FRAME_-96911_SEC_VECTOR      = ( 0, 0, 1 )

      \begintext

   Heliocentric Earth Equatorial (HEEQ) Frame:

         All vectors are geometric: no aberration corrections are used.

         The solar rotation axis is the primary vector: the Z axis points
         in the solar north direction.

         The position of the Earth relative to the Sun is the secondary
         vector: the X axis is the component of this position vector
         orthogonal to the Z axis.

         The Y axis is Z cross X, completing the right-handed reference frame.

      \begindata

         FRAME_SPP_HEEQ               = -96913
         FRAME_-96913_NAME            = 'SPP_HEEQ'
         FRAME_-96913_CLASS           = 5
         FRAME_-96913_CLASS_ID        = -96913
         FRAME_-96913_CENTER          = 10
         FRAME_-96913_RELATIVE        = 'J2000'
         FRAME_-96913_DEF_STYLE       = 'PARAMETERIZED'
         FRAME_-96913_FAMILY          = 'TWO-VECTOR'
         FRAME_-96913_PRI_AXIS        = 'Z'
         FRAME_-96913_PRI_VECTOR_DEF  = 'CONSTANT'
         FRAME_-96913_PRI_FRAME       = 'IAU_SUN'
         FRAME_-96913_PRI_SPEC        = 'RECTANGULAR'
         FRAME_-96913_PRI_VECTOR      = ( 0, 0, 1 )
         FRAME_-96913_SEC_AXIS        = 'X'
         FRAME_-96913_SEC_VECTOR_DEF  = 'OBSERVER_TARGET_POSITION'
         FRAME_-96913_SEC_OBSERVER    = 'SUN'
         FRAME_-96913_SEC_TARGET      = 'EARTH'
         FRAME_-96913_SEC_ABCORR      = 'NONE'
         FRAME_-96913_SEC_FRAME       = 'IAU_SUN'

      \begintext

   Heliographic Inertial (HGI) Frame:

         This frame is the same as the HCI frame except HGI is frozen at 
         epoch J1900 whereas HCI is frozen at epoch J2000.

         All vectors are geometric: no aberration corrections are used.

         The solar rotation axis is the primary vector: the Z axis points
         in the solar north direction.

         The ascending node on the Earth ecliptic of J1900 of the solar 
         equator forms the X axis. This is accomplished by using the +Z 
         axis of the ecliptic of J1900 as the secondary vector and HGI +Y 
         as the secondary axis.

         The Y axis is Z cross X, completing the right-handed reference frame.

      \begindata

         FRAME_SPP_HGI                = -96916
         FRAME_-96916_NAME            = 'SPP_HGI'
         FRAME_-96916_CLASS           = 5
         FRAME_-96916_CLASS_ID        = -96916
         FRAME_-96916_CENTER          = 10
         FRAME_-96916_RELATIVE        = 'J2000'
         FRAME_-96916_DEF_STYLE       = 'PARAMETERIZED'
         FRAME_-96916_FAMILY          = 'TWO-VECTOR'
         FRAME_-96916_PRI_AXIS        = 'Z'
         FRAME_-96916_PRI_VECTOR_DEF  = 'CONSTANT'
         FRAME_-96916_PRI_FRAME       = 'IAU_SUN'
         FRAME_-96916_PRI_SPEC        = 'RECTANGULAR'
         FRAME_-96916_PRI_VECTOR      = ( 0, 0, 1 )
         FRAME_-96916_SEC_AXIS        = 'Y'
         FRAME_-96916_SEC_VECTOR_DEF  = 'CONSTANT'
         FRAME_-96916_SEC_FRAME       = 'SPP_ECLIPDATE'
         FRAME_-96916_SEC_SPEC        = 'RECTANGULAR'
         FRAME_-96916_SEC_VECTOR      = ( 0, 0, 1 )
         FRAME_-96916_FREEZE_EPOCH    = @1900-JAN-01/12:00:00

      \begintext

   Heliographic Co-Rotating Spectroscopic (HGSPEC) Frame:

         This frame is similar to the HG Carrington frame except
         rotates with a frequency of 2.851 urad / s ~ 14.114... deg / day.
         
         From Snodgrass & Ulrich (1990), Table 1, Spectroscopic Coef A. 

         All vectors are geometric: no aberration corrections are used.

         The solar rotation axis is the primary vector: the Z axis points
         in the solar north direction.

         When viewed in the HCI frame, the HGSPEC X axis rotates about the
         Z axis in a right-handed sense at a rate of 2.851 urad / s. The
         HGSPEC frame of J2000 coincides with the HCI frame. 

         The Y axis is Z cross X, completing the right-handed reference frame.

      \begindata

         FRAME_SPP_HGSPEC               = -96919
         FRAME_-96919_NAME              = 'SPP_HGSPEC'
         FRAME_-96919_CLASS             = 5
         FRAME_-96919_CLASS_ID          = -96919
         FRAME_-96919_CENTER            = 10
         FRAME_-96919_RELATIVE          = 'SPP_HCI'
         FRAME_-96919_DEF_STYLE         = 'PARAMETERIZED'
         FRAME_-96919_FAMILY            = 'EULER'
         FRAME_-96919_EPOCH             = @2000-JAN-1/12:00:00
         FRAME_-96919_AXES              = ( 3, 1, 3 )
         FRAME_-96919_UNITS             = 'DEGREES'
         FRAME_-96919_ANGLE_1_COEFFS    = ( 0, -1.63350267391797697E-04  )
         FRAME_-96919_ANGLE_2_COEFFS    = ( 0 )
         FRAME_-96919_ANGLE_3_COEFFS    = ( 0 )
         
      \begintext

