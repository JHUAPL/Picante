December 27, 2010
                      Space Weather Prediction Center (SWPC)
                 ACE Real-Time Solar Wind (RTSW) DATA LISTS


1- and 5-min ACE RTSW data lists: 
    Anonymous          FTP ftp.swpc.noaa.gov/pub/lists/ace
    Web                http://www.swpc.noaa.gov/ftpmenu/lists/ace.html
    
Hourly data & Satellite Location:  
    Anonymous FTP      ftp.swpc.noaa.gov/pub/lists/ace2
    Web                http://www.swpc.noaa.gov/ftpmenu/lists/ace2.html

ACE RTSW Plots and Ground Station Tracking: 
    http://www.swpc.noaa.gov/ace/

Older ACE RTSW Announcements are shown at the bottom of this file.


   ******************************************************************
   ** Please read the SWPC Disclaimer at http://www.swpc.noaa.gov/ **
   ******************************************************************

   SWPC provides near-real-time and recent data, solar and geomagnetic 
   indices and solar event reports created from preliminary reports. 
   Preliminary data may contain errors or be revised after further 
   review. The historical products in this SWPC Warehouse are the 
   preliminary reports as originally published. SWPC does not encourage 
   the use of preliminary data for research purposes. 
   
   Links to on-line data at SWPC and archive sites with final data:
                    http://www.swpc.noaa.gov/Data/
   ****************************************************************** 


Please send comments and questions to SWPC.Webmaster@noaa.gov
Report problems to                    SWPC.CustomerSupport@noaa.gov

********************************************************************************
********************************************************************************

Real-Time Solar Wind (RTSW) data from the Advanced Composition Explorer (ACE) 
spacecraft are made available by the Space Weather Prediction Center (SWPC) as 
updating data lists and plots. The plots and more information is available 
at the SWPC Web site:  http://www.swpc.noaa.gov/ace/index.html 

--- ACE RTSW Instruments ---                            Average Intervals

MAG    - Magnetometer                                    1-min and 1-hour
SWEPAM - Solar Wind Electron Proton Alpha Monitor        1-min and 1-hour
EPAM   - Electron Proton and Alpha Monitor               5-min and 1-hour
SIS    - Solar Isotope Spectrometer                      5-min and 1-hour

All data lists are ASCII text files with a standard SWPC data list format. 
There are two lists for each of the four ACE instruments; a 2-hour list and 
a daily list. The 2-hour lists contain the most recent data from the 
instrument and are updated in near-real-time. The Magnetometer and SWEPAM data 
is available as 1-min averages and updated once a minute. The EPAM and SIS 
data is available as 5-minute averages and updated once every 5 minutes. The 
Daily lists for all instruments are updated once a hour beginning 0101UT daily. 

 
 NOTE: Previous or missed ACE data is never available to SWPC. Therefore
       SWPC data lists are never altered once the period is complete. 


--- File Naming Conventions ---

The filenames for the 2 hour lists do not change. Users retrieving data 
frequently are requested to use these short files.

               ace_mag_1m.txt
               ace_swepam_1m.txt
               ace_epam_5m.txt
               ace_sis_5m.txt

The daily lists have the UT date prefixed to the basic filename.
For example 20071101_ace_mag_1m.txt is the filename for ACE Magnetometer data 
on November 1, 20077.

               20071101_ace_mag_1m.txt
               20071101_ace_swepam_1m.txt
               20071101_ace_epam_5m.txt
               20071101_ace_sis_5m.txt

HOUR DATA FILES in /ace2 directory
    Filenames: yearmonth_ace_instrument_1h.txt, e.g.
       200710_ace_mag_1h.txt,      200710_ace_sis_1h.txt, 
       200710_ace_swepam_1h.txt   200704_ace_epam_1h.txt
    Update: Hourly data files are updated at 10 minutes past the hour.


--- Retention ---

Daily files of 1-min and 5-min data are retained for 30 days.
Monthly files of hourly values are retained for 1 year.

--- File format ---

   o The file format follows a standard data list conventions where header  
       lines start with either # or :. 
   o The lines following the header (# and :) lines are data records, there 
        are no header lines after the data begins.
   o The file ends with the last data record.
   o There is a data record for each time interval. For example, a completed 
     daily 5-min file contains 288 data records -- one for each 5 minute interval
     in the day. When no data are available there is a record containing missing 
     data values for that interval.
   o Fields within each record are delimited by white space. Records are 
     written with a formatted write statement so the field widths do not change.
     However, SWPC reserves the right the change the format as needed.

--- Valid Data ---

     The header lines on each list identify the data values, units, and missing
     data values. The range of valid values are shown in the instrument 
     descriptions below.


--- Date/time fields for all data lists ---

   Dates are shown as year month day (2003 09 06) and
   "Modified Julian Day (MJD) as defined in an early SWPC database.
   The SWPC MJD for Jan 1, 2003 is 52640
   The SWPC MJD for Jan 1, 2004 is 53005 
   
   Time is shown as HourMin  (0035) and seconds of the day (2100)


--- Status Flags ---

Status fields for all instruments are a one digit integer from 0 through 9 
where 0 = nominal data, 9 = no data, and 1-8 indicate bad data. 
More specifically:
     0 = Data values a nominal
     1 = SWEPAM data flag non-zero
     2 = Spacecraft attitude may be in error (following a maneuver)
     3 = SWEPAM mode is bad
     4 = The magnetometer flip bit is on, data are not processed
     5 = The magnetometer is in calibration mode, data are not processed during  
         calibration
     6 = Some values in the data record were out of their valid range, unable to
         to process the data
     7 = Some values in the data record were missing, unable to process the data
     8 = The data record was incomplete, unable to process the data
     9 = No data were received at SWPC, usually because no site was tracking ACE


--- SPACECRAFT LOCATION FILES in /ace2 directory ---
 
    Filenames: yearmonth_ace_loc_1h.txt, e.g.200306_ace_loc_1h.txt
    Updates: Current month is recreated daily at 0012UT. 
    
    SWPC uses predicted ACE spacecraft location information to
    create a monthly list of hourly predicted locations.

    Files contain values from the 1st of the month through hour
    23 of the current day. 

    The hourly predicted location values are X, Y, and Z position in
    GSE coordinates, with an accuracy 0.1 earth radii (about 600 km). 


--------------------------------------------------------------------------------

MAG - Magnetometer
Description                             Format       Units          min/max

IMF X-component in GSM coordinates       Float         nT        -200.0 / 200.0
IMF Y-component in GSM coordinates       Float         nT        -200.0 / 200.0
IMF Z-component in GSM coordinates       Float         nT        -200.0 / 200.0
IMF component magnitude                  Float         nT           0.0 / 200.0
IMF latitude in GSM coordinates          Float       degrees      -90.0 / 90.0
IMF longitude in GSM coordinates         Float       degrees        0.0 / 360.0

--------------------------------------------------------------------------------

SWEPAM - Solar Wind Electron Proton Alpha Monitor
Description                                   Format    Units         min/max  

Solar wind proton density                      Float     p/cc       0.0 /  200.0
Solar wind bulk speed                          Float     km/s     200.0 / 2000.0
Solar wind ion temperature                     Float  degrees K  1.00E4 / 1.00E7
Solar wind vector velocity in GSE x-direction  Float     km/s   -2000.0 / -200.0
Solar wind vector velocity in GSE y-direction  Float     km/s    -200.0 /  200.0
Solar wind vector velocity in GSE Z-direction  Float     km/s    -200.0 /  200.0

--------------------------------------------------------------------------------

EPAM - Electron Proton and Alpha Monitor
Description                             Format       Units              min/max
                         
Differential electron flux 38-53 keV    Float  p/(cm2-sec-ster-MeV)   0.00/1.0E8
Differential electron flux 175-315 keV  Float  p/(cm2-sec-ster-MeV)   0.00/1.0E7
Differential proton flux 47-68 keV      Float  p/(cm2-sec-ster-MeV)   0.00/1.0E8
Differential proton flux 115-195 keV    Float  p/(cm2-sec-ster-MeV)   0.00/1.0E7
Differential proton flux 310-580 keV    Float  p/(cm2-sec-ster-MeV)   0.00/1.0E7
Differential proton flux 1060-1900 keV  Float  p/(cm2-sec-ster-MeV)   0.00/1.0E6
FP6p (761-1220 keV proton flux)         Float  p/(cm2-sec-ster-MeV)   0.00/1.0E7
Anisotropy Index                        Float     Dimensionless        0.0/2.0


                 *** Special Note on EPAM Proton Data: ***

The EPAM LEMS120 instrument which provides proton data in the energy ranges 
47-68, 115-195, 310-580, and 1060-1900 keV is oriented on the ACE spacecraft at 
120 degrees off the sun-Earth line, back towards the Earth. A consequence of this
is that "upstream" magnetospheric particle bursts are often seen in these data.
These bursts are most noticeable in the lower energy ranges, can occur several 
times a day, and can last for tens of minutes. The proton data in the 761-1220keV
range is derived from a different instrument (CA60) oriented more towards the sun
and is typically not affected by these upstream bursts.
                
--------------------------------------------------------------------------------

SIS - Solar Isotope Spectrometer Integral Flux of High-energy Solar Protons
Description                             Format       Units             min/max

Integral proton flux at >10 MeV         Float  p/(cm2-sec-ster)     0.00 / 1.00E5
Integral proton flux at >30 MeV         Float  p/(cm2-sec-ster)    >0.00 / 1.00E5

--------------------------------------------------------------------------------

ACE Spacecraft Location
Description                                   Format      Units       min/max

X position in GSE coordinate (Earth radii)     Float        Re       0.0 / 300.0
Y position in GSE coordinate (Earth radii)     Float        Re    -200.0 / 200.0
Z position in GSE coordinate (Earth radii)     Float        Re    -200.0 / 200.0




=================================================================================
=================================================================================
                             ACE RTSW Announcements                                
                                    
==========================================================================
October 19, 2010 -- Based on the advice of the ACE EPAM science 
team, the EPAM FP6p proton channel energy range shown in headers
was corrected from 761-1220 keV to 795-1193 keV. 
                                    
==========================================================================
             ACE Real-time Solar Wind Data Resumes Nov 25, 1910UT

Nov 25, 2009 -- The ACE Real-time Solar Wind data was unexpectedly 
interrupted from 1837UT Nov 24 until 1900UT Nov 25.

==================================================================
June 1, 2006         ACE RTSW SWEPAM Data Improvement

The ACE RTSW SWEPAM data processing was updated at 0000 UTC 
June 1, 2006. This change will result in improved solar wind 
parameters (speed, density, and temperature) that more closely 
match ACE Science Center data. 

ACE RTSW MAG, EPAM, and SIS data are unaffected.

==================================================================
May 5, 2006         ACE RTSW SWEPAM Data Improvement

At 0000 UTC on June 1, 2006, the ACE RTSW SWEPAM data processing
will be updated. This change will result in improved solar wind 
parameters (speed, density, and temperature) that more closely 
match ACE Science Center data. The most significant impact will 
be higher RTSW density values during low-speed, low-density 
solar wind conditions. ACE RTSW MAG, EPAM, and SIS data are 
unaffected.
==================================================================
May 15, 2006       ACE RTSW Data Timing Problem Resolved

From 1300UT May 12 to 1300 UT May 13 there was a problem in the SEC 
ACE RTSW data. Values were correct but the associated time tags were 
about 15 minutes earlier than the actual reading. 

==================================================================
April 3, 2006    Termination of ACE Solar Wind Data

a comment period for the proposed NWS Termination of ACE Solar 
Wind Data was April 3 - May 18, 2006
==================================================================
September 12, 2005
          ACE SOLAR WIND DATA CONTAMINATION ENDS

Recent solar energetic proton fluxes which had contaminated the ACE
SWEPAM instrument data have subsided. The period of ACE contamination 
was about 1300 UTC, 09 September to 1000 UTC, 11 September. 
==================================================================
Sept 09, 2005
ACE SOLAR WIND DATA CONTAMINATED

Recent solar energetic proton events have contaminated the ACE SWEPAM
instrument data resulting in incorrect solar wind density, speed,
and temperature values being reported by SEC.  This contamination  
started at about 1300 UTC on 09 September and is expected to continue 
until energetic proton fluxes subside.  
==================================================================
May 19, 2005 
		 SIS DATA noise

 There is currently noise in the SIS RTSW proton rates.  The SIS 
 team is investigating the cause and expects it to be corrected 
 in the next few days.

==================================================================
January 24, 2005
                 ACE SOLAR WIND DATA CONTAMINATED
 
 Recent solar energetic proton events contaminated the ACE SWEPAM 
 instrument data resulting in incorrect solar wind density, speed, 
 and temperature values being reported by SEC. There were essentially 
 two outages due to the proton events:
                                                                                                                          
        Jan 17 13:22 -- Jan 18 22:40 UTC
        Jan 20 07:00 -- Jan 21 17:00 UTC

===========================================================================
August 24, 2004      ACE MAG Data Processing Change
                                                                                                               
ACE MAG data processing was updated at 0000 UTC on 24 August, 2004.
This update should result in improved RTSW magnetic field values that
more closely match ACE Science Center data.

===========================================================================

February 4, 2004: Real-time proton data from the EPAM LEMS30 instrument has 
been replaced with similar proton data from the LEMS120 sensor. The new 
proton channels cover the following energy ranges: 47-68 keV, 115-195 keV, 
310-580 keV, and 1.06-1.9 MeV. Previous real-time electron (38-53, 175-315 keV)
and proton (0.761-1.22 MeV) data from the EPAM CA60 telescope remain
unchanged.  Plots and Data Lists of 5-minute and 1-hour averaged data were
backfilled to January 29, 2004. 
===========================================================================
                                    
January 15, 2004
SEC is proceeding to switch the ACE RTSW EPAM proton data processing to 
another sensor. The operational switch over to the new sensor data has
been reschedule to February 4, 2004. It is anticipated that the following 
proton energy ranges will be available: 47-68 keV (P1'), 115-195 keV (P3'), 
321-587 keV (P5'), and 1060-1900 keV (P7').

November 12, 2003
As a result of the last two weeks of high solar activity several of the RTSW
EPAM proton channels have become abnormally high and have shown excessive
noise. As a result the data from the 65-112 keV (P2), 112-187 keV (P3), and
310-50 keV (P5) energy channels have been removed from the real-time plots
and lists. The 1060-1910 keV (P7) channel remains but has also begun to
show noise. The 761-1220 (W1) proton channel and electron channels, being 
from another EPAM sensor, have been unaffected. SEC is investigating the 
switch to another EPAM proton sensor for its RTSW data stream.

===========================================================================
November 4, 2003
             ACE SOLAR WIND DATA CONTAMINATION ENDS

 The solar energetic proton event that contaminated the ACE SWEPAM instrument 
 data resulting in incorrect solar wind density, speed, and temperature values 
 being reported by SEC Nov 2-3 ended November 3 about 2040UT. 
==========================================================================
November 3, 2003
                       ACE EPAM Data Problems
                   
 On October 29 the ACE EPAM 65-112 keV (P2) and 112-187 keV (P3) energy 
 channels became spuriously high following the onset of a large solar 
 energetic particle event and have not returned to nominal values. 
 In addition, the 310-580 keV (P5) channel became elevated and erratic on
 November 1 and 2 during a second solar energetic particle event. The P5
 channel appears to have recovered; however, this does not preclude
 similar problems in the future. The EPAM instrument team and SEC are
 investigating the cause of these problems and a possible cure. Look for 
 more information here as information becomes available.


                  ACE SOLAR WIND DATA CONTAMINATED
 
 A solar energetic proton event is contaminating the ACE SWEPAM instrument 
 data resulting in incorrect solar wind density, speed, and temperature 
 values being reported by SEC. The contamination begin Nov 2 at 1820 UTC.
==========================================================================
October 31, 2003
                ACE SOLAR WIND DATA CONTAMINATION ENDS

 The solar energetic proton event that contaminated the ACE SWEPAM 
 instrument data resulting in incorrect solar wind density, speed, and 
 temperature values being reported by SEC Oct 28 - 30 ended October 31 
 about 0052 UTC. 
==========================================================================
October 28, 2003
                ACE SOLAR WIND DATA CONTAMINATED

 A solar energetic proton event is contaminating the ACE SWEPAM instrument
 data resulting in incorrect solar wind density, speed, and temperature 
 values being reported by SEC. The contamination begin Oct 28 at 1242 UTC.
======================================================================== 
March 31, 2003

Technical difficulties at CRL resulted in an ACE data outage from March 
30, 2300UT to March 31, 0700UT. Alternative coverage has been arranged 
until the CRL problem is corrected. 
======================================================================== 
March 18, 2001
                    ACE EPAM Data Processing Changed

ACE EPAM data processing was updated. The visible change was replacing
the noisy Proton 47 to 65 keV data with the 65 to 112 keV data in EPAM 
lists and plots. 
======================================================================== 
March 5, 2002
                      EPAM Data Processing Change

On March 18, 2002 EPAM data processing will be updated. On data lists,
the header line changes to show the new energy range for the first Proton
channel. On data plots, the legend changes.

EPAM data processing update:

In late November and early December 2001 the P1 channel (47 to 65 keV) 
data increased during an energetic particle event (small radiation storm) 
and never returned to nominal levels. The channel became "noisy" and 
after monitoring for a few weeks the decision was made to replace the
P1 channel with the P2 channel (65 to 112 keV) data, the next higher energy
channel within the same detector on EPAM. 

During the later part of December 2001 the P5 channel (310 to 580 keV) 
showed anomalous periods of increased particle flux. The instrument PI from 
JHU/APL determined that when the detector was pointed in the general 
direction of the sun, the sector pointed toward the sun showed large 
increases in particle flux.  This is believed to be due to solar 
contamination and only happens during limited times between each set of
spacecraft maneuvers. The decision was to change the processing of all 
channels from this detector to eliminate the solar noise problem. A third 
problem was induced by the solar noise problem: Whenever the P5 channel 
became noisy the P1 and P3 data would drop out. The cause of the drop out 
was a programming filter designed to eliminate "bad" data. Since not all 
of the data were "bad", only the sunward directed sector, a decision was 
made to eliminate all data from the sunward sector. Problems two and three 
were solved by the removal of the data from the sunward sectors for all 
channels.  The only impact from removing the sunward sector data is larger 
fluctuations in the data when the flux is near background levels.

======================================================================== 
March 4, 2002
                       ACE Tracking Reduced
                   
RAL will not be tracking for 3 weeks starting March 5 while their 
antenna is upgraded.

========================================================================  
January 25, 2002 
                  CRL to lose tracking for 2 days
Tracking from CRL will be unavailable starting at 
21:42 UTC on 25 January until 21:37 UTC on 27 January.

EPAM lowest energy channel (P1) 47-65 keV 
data are suspect since late November. This channel will be
replaced by late February. Possible cures are being 
investigated by the instrument team and SEC.  

======================================================================== 
December 20, 2001
                   ACE EPAM P1 Channel Data Suspect
                   
Data from the ACE EPAM lowest energy channel (P1) 47-65 keV are suspect 
since late November, following the large proton event that ended on the 
24th. The cause and possible cure are being investigated by the instrument 
team and SEC. Look for further messages here as more information becomes 
available.

======================================================================== 
November 26, 2001
                 ACE SOLAR WIND DATA CONTAMINATION ENDS

    The solar energetic proton event that contaminated the ACE SWEPAM 
    instrument data resulting in incorrect solar wind density, speed, and 
    temperature values being reported by SEC on November 23 and 24
    ended November 24 about 1800 UTC. 
======================================================================== 
November 7, 2001
                ACE SOLAR WIND DATA CONTAMINATION ENDS

    A solar energetic proton event contaminated the ACE SWEPAM instrument
    data resulting in incorrect solar wind density, speed, and 
    temperature values being reported by SEC on November 5 and 6. 
    The contamination ended November 7 about 0300 UTC. 
======================================================================== 
1545 UTC Sept 27, 2001
                     ACE DATA RETURNS TO NOMINAL

    A solar energetic proton event which was contaminating the ACE 
    SWEPAM instrument has subsided. ACE solar wind density, speed, 
    and temperature values reported by SEC are now correct. The values
    were incorrect from 1602 UTC on 24 Sept until 0945 UTC on Sept 27th. 
======================================================================== 
July 27, 2001
             ACE SPACECRAFT MANEUVER COMPLETED
             
   NASA performed a Z-axes maneuver, to keep the Spacecraft 
   in L1 orbit. ACE Real-Time Solar Wind data was unavailable 
   from 1534 UTC July 23th until 1825 UTC July 26, 2001.

========================================================================
July 26, 2000
            ACE EPAM Data Lists Header Line Changes
                    
The ACE EPAM 5-min data lists, had incorrect Differential Proton Flux 
ranges. The header lines were changed beginning 26 July 2000.

CORRECTED:                        
#                 -- Differential Flux --------------------------
# UT Date   Time  ------------------ Protons keV ----------------
# YR MO DA  HHMM  47-65    112-187   310-580   761-1220 1060-1910
                        
INCORRECT:                        
#                 -- Differential Flux --------------------------
# UT Date   Time  ------------------ Protons keV ----------------
# YR MO DA  HHMM  56-78    130-214   337-594   761-1220 1073-1802

==================================================================
July 30, 1998

                     ACE Hourly Averaged data

    SEC creates monthly files of hourly averaged data from each
    of the ACE instruments. The files are updated every 3 hours
    beginning at 0010UT. The current month's files are recreated
    at each run. The previous month's files is created for the 
    last time on the first day of the new month.

========================================================================
July 28, 1998

   The EPAM 761-1220 MeV Proton algorithm was adjusted at 7/28 1600UT.
   There was factor of 3 decrease which matches expected values.

========================================================================


4/3/98
               Update on quality of ACE RTSW data sets

MAG:  All data nominal

SWEPAM:  All data nominal.  When the velocity of the solar wind falls to
low values the current algorithm will often set a flag indicating a
problem exists.  SEC flags the data in the data base and does not plot
the data on the web site.  The end result is a number of missing data at
the one minute cadence.
 
EPAM:  All differential flux channels nominal, except for W1.  This
channel is impacted by above normal temperatures on ACE.  APL is looking
at this issue.  The only impact is the anisotropy index is not valid.
The energy range of this channel is covered by the other existing
channels.

SIS:  Both integral channels nominal.  Integral flux is at background
until a large event is detected


Update on tracking:

RAL, NASA, and AFSCN are fully operational.  CRL is down for antenna
repair and is expected up in two months.  AFSCN has added extra tracking
coverage during the time period normally covered by CRL.  Occasionally
short dropouts occur during a tracking pass, usually due to data
transmission problems.  Bad data records are received during some
tracking passes.  Software eliminates most of the problem data, but not
all problem data.  

=========================================================================

2/9/98 - A software solution to the SWEPAM instrument problem was installed on
         2/5. Density, Bulk Speed and Ion Temperature data now appear nominal.
         
2/2/98 - The Spacecraft Location files will not be available until March.

2/2/98 - The Magnetometer data is still in spacecraft coordinates (RTN). 
         Will not be switched to GSM coordinates in March. 

2/2/98 - Lists of hourly average will not be available until March.

2/2/98 - The limits shown below for Mag component magnitude, SWEPAM bulk speed, 
         SWEPAM X vector velocity, and Xgse location were corrected 2/2/98.

1/21/98 - Operational ACE data lists began running. 
