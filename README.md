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
# NLL_monthly.txt = Nellie Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=NLL
# THE_monthly.txt = Lake Thomas Edison from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=THE
# CKT_monthly.txt = Chilkoot Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CKT
# HTT_monthly.txt = Huntington Lake from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HTT
# CLM_monthly.txt = Clover Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CLM
# JCM_monthly.txt = Jackass Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=JCM
# CHQ_monthly.txt = Chiquito Creek from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CHQ
# PMD_monthly.txt = Poison Meadow from http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PMD

# Sierra_Nevada_SWE.txt = provides info about each snow station, which the Khione R script will expect.
# The published snow-shielding correction is based upon elevation. Using an elevation-based snow correction is about 10% better than assuming that SWE is independent of elevation. I explored also using total (modern) monthly insolation--in combination with but this added no additional predictive skill.

# all_the_monthly_differences.txt = all the individual monthly differences between (1) the monthly average of the daily SWE observations (from the snow pillows) and (2) the monthly, individual SWE measurements (from the snow courses). The daily data provide more robust monthly averages--but these records typically only begin in the 1980s whereas the snow course measurements typical begin in the 1930s.

# The R script Khione uses the documented differences where there are overlaps in the record to estimate the monthly _average_ SWE value from the individual snow course measurement when there isn't overlap between the two SWE measurements.

# repository.txt = the results of the SWE by month vs. elevation analysis. The R code Khione will generate it, but I've also included it here for those that wish to jump into the R code at this point (line 1691 of Khione) without redoing the time-consuming calculations of SWE by month vs. elevation. (Cleaning the daily SWE observations, to the generate the "clean" daily files is especially slow.)

# snow_depth.txt = the raw data for the relationship between daily snow depth and daily SWE. This data comes from the "clean daily" files above, and Khione will compile into a single snow_depth.txt file for you--but if you're jumping in at this point (line 1691 of Khione) you're going to need this file.

# Marrero_simple_YESerosion_NOsnow_TMandLC.txt = the file that hosts the info about the new Be-10 sampling sites in Tuolumne Meadows and Lyell Canyon that our manuscript (in prep for Quaternary Science Reviews) reports.

# Boulder_Height.txt = info on the heights of the samples and the estimated precision of our knowledge of boulder height.

# When the "CALCULATE SNOW SHIELDING CORRECTIONS" portion of the Khione R script is run, it will generate 2n+1 .txt files, where n is the number of samples in the sample input file (here "Marrero_simple_YESerosion_NOsnow_TMandLC.txt"). For each sample in the input txt file, the Khione code calculates a distribution of boulder heights (based on the info in Boulder_Height.txt) and exports those. For each sample in the input file, it also generates a .txt file with "sims" number (typically 1000) of Monte Carlo variations on the input parameters required by version 3 of the Balco et al. (2008) calculator. By opening the text file and copying and pasting the 1000 variations on a single sample into the Balco et al. (2008) calculator's webform (MAKE SURE YOU ASK GREG FIRST!), you get 1000 potential ages and uncertainties for that single sample. The camelGrapher R script will then assemble those 1000 variations into a single estimated age and uncertainty. Lastly, the "+1" file generated contains all the samples in the input .txt file, ready for pasting into the Balco et al. (2008) calculator's webform. This file enables users of this script to use the Balco et al. (2008) calculator in the standard, non-Monte-Carlo way.

# "exportBoulderHeight... .txt" = the distributions of boulder height used for the samples from Tuolumne Meadows and Lyell Canyon, which are in preparation for publication with Quaternary Science Reviews.

# "monte_carlo_... .txt" = the samples that we fed into v. 3.0.3. of the Balco et al. (2008) calculator in our ad hoc Monte Carlo approach to using it.
