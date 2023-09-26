#!/bin/bash

cd $(dirname $0)

curl -ROz 020514_SE_SAT105.bsp https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/spk/020514_SE_SAT105.bsp
curl -ROz 030201AP_SK_SM546_T45.bsp https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/spk/030201AP_SK_SM546_T45.bsp
curl -ROz 041014R_SCPSE_01066_04199.bsp https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/spk/041014R_SCPSE_01066_04199.bsp
curl -ROz 080428R_SCPSE_08045_08067.bsp https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/spk/080428R_SCPSE_08045_08067.bsp
curl -ROz 981005_PLTEPH-DE405S.bsp https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/spk/981005_PLTEPH-DE405S.bsp
curl -ROz de414.bsp https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/a_old_versions/de414.bsp
curl -ROz de421.bsp https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/a_old_versions/de421.bsp
curl -ROz de430.bsp https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de430.bsp
curl -ROz earthstns_itrf93_050714.bsp https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/stations/a_old_versions/earthstns_itrf93_050714.bsp
curl -ROz mar097.bsp https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/satellites/mar097.bsp
curl -ROz mgs_ext12_ipng_mgs95j.bsp https://naif.jpl.nasa.gov/pub/naif/pds/data/mgs-m-spice-6-v1.0/mgsp_1000/data/spk/mgs_ext12_ipng_mgs95j.bsp
curl -ROz mgs_ext13_ipng_mgs95j.bsp https://naif.jpl.nasa.gov/pub/naif/pds/data/mgs-m-spice-6-v1.0/mgsp_1000/data/spk/mgs_ext13_ipng_mgs95j.bsp
curl -ROz mgs_ext26_ipng_mgs95j.bsp https://naif.jpl.nasa.gov/pub/naif/pds/data/mgs-m-spice-6-v1.0/mgsp_1000/data/spk/mgs_ext26_ipng_mgs95j.bsp
curl -ROz mro_psp22.bsp https://naif.jpl.nasa.gov/pub/naif/MRO/kernels/spk/mro_psp22.bsp
