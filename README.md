# YosemiteSnowDepths

# This ReadMe provides an overview of the files in this repository.

# The first group of files are the raw daily records of snow water equivalent (SWE) from the California Department of Water Resources.
# The files are arranged from high to low elevation within each drainage basin from north to south.
# DAN_daily.txt = the raw daily SWE data for Dana Meadows from http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DAN
# SLI_daily.txt = the raw daily SWE data for Slide Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=SLI
# TUM_daily.txt = the raw daily SWE data for Tuolumne Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=TUM
# HRS_daily.txt = the raw daily SWE data for Horse Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HRS
# WHW_daily.txt = the raw daily SWE data for White Wolf from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=WHW
# PDS_daily.txt = the raw daily SWE data for Paradise Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PDS
# SNF_daily.txt = the raw daily SWE data for Snow Flat from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=SNF
# STR_daily.txt = the raw daily SWE data for Ostrander Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=STR
# TNY_daily.txt = the raw daily SWE data for Tenaya Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=TNY
# VLC_daily.txt = the raw daily SWE data for Volcanic Knob from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=VLC
# AGP_daily.txt = the raw daily SWE data for Agnew Pass from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=AGP
# KSP_daily.txt = the raw daily SWE data for Kaiser Point from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=KSP
# GRM_daily.txt = the raw daily SWE data for Green Mountain from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GRM
# DPO_daily.txt = the raw daily SWE data for Devil's Postpile from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DPO
# TMR_daily.txt = the raw daily SWE data for Tamarack Summit from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=TMR
# CHM_daily.txt = the raw daily SWE data for Chilkoot Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CHM
# HNT_daily.txt = the raw daily SWE data for Huntington Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HNT
# GRV_daily.txt = the raw daily SWE data for Graveyard Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GRV
# PSR_daily.txt = the raw daily SWE data for Poison Ridge from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PSR

# Processing these files through the clean_daily_records.R script will result in the "clean" version of these files.

# The snow shielding correction also draws upon the following monthly records of SWE:
# DAN_monthly.txt = Dana Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DAN
# RFM_monthly.txt = Rafferty Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=RFM
# BNP_monthly.txt = Bond Pass from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BNP
# NGM_monthly.txt = New Grace Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=NGM
# TUM_monthly.txt = Tuolumne Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=TUM
# HRS_monthly.txt = Horse Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HRS
# WLW_monthly.txt = Wilma Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=WLW
# SAS_monthly.txt = Sachse Springs from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=SAS
# SPF_monthly.txt = Spotted Fawn from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=SPF
# HCL_monthly.txt = Huckleberry Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HCL
# PDS_monthly.txt = Paradise Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PDS
# KRC_monthly.txt = Kerrick Corral from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=KRC
# VNN_monthly.txt = Vernon Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=VNN
# LKB_monthly.txt = Lower Kibbie Ridge from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=LKB
# UKR_monthly.txt = Upper Kibbie Ridge from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=UKR
# BEM_monthly.txt = Bell Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BEM
# BHV_monthly.txt = Beehive Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BHV
# SNF_monthly.txt = Snow Flat from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=SNF
# STR_monthly.txt = Ostrander Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=STR
# TNY_monthly.txt = Tenaya Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=TNY
# PGM_monthly.txt = Peregoy Meadows from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PGM
# GFL_monthly.txt = Gin Flat from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GFL
# MNP_monthly.txt = Mono Pass from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=MNP
# PPS_monthly.txt = Piute Pass from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PPS
# EML_monthly.txt = Emerald Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=EML
# PNB_monthly.txt = Pioneer Basin from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PNB
# HRT_monthly.txt = Heart Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HRT
# VLC_monthly.txt = Volcanic Knob from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=VLC
# RMR_monthly.txt = Rose Marie from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=RMR
# CMB_monthly.txt = Colby Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CBM
# AGP_monthly.txt = Agnew Pass from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=AGP
# DTL_monthly.txt = Dutch Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DTL
# KSR_monthly.txt = Kaiser Pass from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=KSR
# CYT_monthly.txt = Coyote Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CYT
# CRA_monthly.txt = Cora Lakes from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CRA
# BDF_monthly.txt = Badger Flat from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BDF
