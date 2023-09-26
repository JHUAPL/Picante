#!/bin/bash

cd $(dirname $0)

curl -ROz 030810_031019_c39_port1_pa.bc https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/ck/030810_031019_c39_port1_pa.bc
curl -ROz 04135_04171pc_psiv2.bc https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/ck/04135_04171pc_psiv2.bc
curl -ROz 04153_04182ca_ISS.bc https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/ck/04153_04182ca_ISS.bc
curl -ROz 04161_04164ra.bc https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/ck/04161_04164ra.bc
curl -ROz 08052_08057ra.bc https://naif.jpl.nasa.gov/pub/naif/CASSINI/kernels/ck/08052_08057ra.bc
curl -ROz mgs_sc_ext12.bc https://naif.jpl.nasa.gov/pub/naif/pds/data/mgs-m-spice-6-v1.0/mgsp_1000/data/ck/mgs_sc_ext12.bc
