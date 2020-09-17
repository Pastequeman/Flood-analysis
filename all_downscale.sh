#!/bin/env bash

# launch easily all exposure assessment
# two experiments
# 5 ssps
# 8 GCMs
## input
GCMS="H2C_ H3C_"
#GCMS="I2C_ I3C_"
#GCMS="M2C_ M3C_"
#GCMS="G2C_ G3C_"


EXPS="nodam_trim dam_trim"
COUNTRY="yes"
SSPS="ssp1 ssp2 ssp3 ssp4 ssp5"
## job
for GCM in ${GCMS}; do
    for EXP in ${EXPS}; do
	for SSP in ${SSPS}; do
	    #echo $GCM $EXP $COUNTRY $SSP
	    nohup ./downscale_population_exposure.r ${GCM} ${EXP} ${COUNTRY} ${SSP} >/dev/null 2>&1 &
	done
    done
done
	 
