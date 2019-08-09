# The goal of this script is to ingest text files from the Balculator and
#  calculate normal kernel density estimates.
# WARNING: Running this entire script at once will likely take ~3-5 days.
# If you skip the WARNING sections it should only take ~5 minutes.
# Running the first part, up through "Load datasets" will pull in some
#  files and create some vectors to later iterate over.
# Running the second part, starting with "Reload percentiles..." will
#  pull in some saved files (reflecting the work of the intermediary
#  steps, that are being skipped) and continue the analysis.

# Install and load packages--------------------------------------------------
# data.table for the fast loading and analysis of large datasets
# install.packages("data.table")
library(data.table)

# ggplot2 for its advanced plotting capacity
# install.packages("ggplot2")
library(ggplot2)

# # ggtheme for its selection of ggplot2 themes
# install.packages("ggthemes")
# library(ggthemes)

# broom for tidying model objects
# install.packages("broom")
library(broom)

# tidyr for the cleaning data
# install.packages("tidyr")
library(tidyr)

# moments for the skewness function
install.packages("moments")
library(moments)

# Bchron, a Bayesian approach to calibrating radiocarbon ages
# install.packages('Bchron')
library(Bchron)

# egg package, to control panel size in ggplot
install.packages("egg")
library(egg)


# Define functions-----------------------------------------------------------
clean_and_consolidate_iterations <- function(DT, HistSnowPercent,
            CalibDataset, nkdeRepository = camelDT,
            dateRepository = allDates, startYearBP = 10000L,
            endYearBP = 30000L, timestep = 25L,
            scaling_model = "LSDn", err = "internal",
            proportionalVariability = TRUE,
            compileCamels = FALSE) {
  # Purpose:
  #   This function has two purposes: (1) It cleans the monte carlo
  #   files and combines all the individual monte carlo simulations into
  #   a single normal kernel density estimate and (2) It copies all the
  #   individual simulations, for all the samples, into a single DT.
  #
  # Args:
  #   DT: A data.table containing multiple monte carlo simulations of a 
  #       cosmogenic sample's age. The following specific column names
  #       are required: "Sample_Name", "St_Age_yr", "St_Interr_yr",
  #       "St_Exterr_yr", "Lm_Age_yr", "Lm_Interr_yr", "Lm_Exterr_yr",
  #       "LSDn_Age_yr", "LSDn_Interr_yr", and "LSDn_Exterr_yr". These
  #       columns are produced by version 3 of the online calculator
  #       formerly known as the CRONUS-Earth online calculator.
  #   HistSnowPercent: The ages in DT were calculated assuming a certain
  #       percentage of historical snow cover.  100% is most typical, but
  #       other values are also commonly explored to test the
  #       sensitivity of the results to the snow correction.
  #   CalibDataset: A character vector of length 1 naming the
  #       calibration dataset used in calculating the 10Be ages.
  #   nkdeRepository: A data.table that accumulates the results of the 
  #       normal kernel density estimates. The repository DT enables
  #       all the nkd estimates for all the samples in the stored in a 
  #       single location.
  #   dateRepository: A DT that accumulates all the individual
  #       simulations for all the samples being used in the study.
  #   startYearBP: The younger end of the age range to investigate. The 
  #       function will generate a series of ages and calculate the 
  #       normal kernel density estimate at each time step.
  #   endYearBP: The older end of the age range to investigate.
  #   timestep: The timestep to use in generate the series of ages to
  #       use in the normal kernel density estimates.
  #   scaling_model: The online calculator formerly known as the
  #       CRONUS-Earth online calculator produces ages using three
  #       cosmogenic nuclide scaling models: St, Lm, and LSDn. (The
  #       scaling model Ag may be added in the future.) The 
  #       "scaling_model" switch enables which scaling model for which
  #       they would like to calculate normal kernel density estimates.
  #   err: A toggle that lets users select either internal or external
  #       uncertainties.
  #   proportionalVariability: A toggle that tells the function
  #       whether the ages were calculated with proportional
  #       variability (i.e., an 80% snow correction has 80% the SWE
  #       and 80% the variability as the historical record) or
  #       historial variability (where all the snow corrections have
  #       the same variability in SWE as the historical record).
  #   compileCamels: A toggle that tells the function whether to
  #       combine all the individual monte carlo iterations for one
  #       sample into a single curve.  Good to do, but SLOW.
  #
  # Returns:
  #   Two 'repository' DTs, one with info about the normal kernel
  #   density estimate for each sample (integrating all the
  #   individual simulations) and another with all the simulations
  #   for all the samples.
  #
  # Error Checking:
  if (!is.data.table(DT)) {
    stop("DT needs to be a data.table.", call. = FALSE)
  }
  if ("Sample_Name" %in% names(DT) == FALSE) {
    stop("The DT needs a 'Sample_Name' column.", call. = FALSE)
  }
  if ("St_Age_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'St_Age_yr' column.", call. = FALSE)
  }
  if ("St_Interr_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'St_Interr_yr' column.", call. = FALSE)
  }
  if ("St_Exterr_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'St_Exterr_yr' column.", call. = FALSE)
  }
  if ("Lm_Age_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'Lm_Age_yr' column.", call. = FALSE)
  }
  if ("Lm_Interr_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'Lm_Interr_yr' column.", call. = FALSE)
  }
  if ("Lm_Exterr_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'Lm_Exterr_yr' column.", call. = FALSE)
  }
  if ("LSDn_Age_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'LSDn_Age_yr' column.", call. = FALSE)
  }
  if ("LSDn_Interr_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'LSDn_Interr_yr' column.", call. = FALSE)
  }
  if ("LSDn_Exterr_yr" %in% names(DT) == FALSE) {
    stop("The DT needs a 'LSDn_Exterr_yr' column.", call. = FALSE)
  }
  if (!is.integer(HistSnowPercent)) {
    stop("HistSnowPercent must be integer.", call. = FALSE)
  }
  if (!is.character(CalibDataset)) {
    stop("CalibDataset must be a character vector", call. = FALSE)
  }
  if (length(CalibDataset) > 1) {
    stop("CalibDataset must be length 1.", call. = FALSE)
  }
  if (!is.data.table(nkdeRepository)) {
    stop("'nkdeRepository' needs to be a data.table.", call. = FALSE)
  }
  if ("Year" %in% names(nkdeRepository) == FALSE) {
    stop("'nkdeRepository' DT needs to have a column 'Year'.",
         call. = FALSE)
  }
  if ("PercentSnow" %in% names(nkdeRepository) == FALSE) {
    stop("'nkdeRepository' DT needs a 'PercentSnow' column.",
         call. = FALSE)
  }
  if (!is.data.table(dateRepository)) {
    stop("'dateRepository' must be a data.table.", call. = FALSE)
  }
  if ("Sample_Name" %in% names(dateRepository) == FALSE) {
    stop("'dateRepository' must have a 'Sample_Name' column.",
         call. = FALSE)
  }
  if ("PercentSnow" %in% names(dateRepository) == FALSE) {
    stop("'dateRepository' must have a 'PercentSnow' column.",
         call. = FALSE)
  }
  if (!is.integer(startYearBP)) {
    stop("'startYearBP' needs to be an integer.", call. = FALSE)
  }
  if (!is.integer(endYearBP)) {
    stop("'endYearBP' needs to be an integer.", call. = FALSE)
  }
  if (!is.integer(timestep)) {
    stop("'timestep' needs to be an integer.", call. = FALSE)
  }
  if (nkdeRepository[1, Year] != startYearBP) {
    stop("First year in 'nkdeRepository' must equal startYearBP.",
         call. = FALSE)
  }
  if (nkdeRepository[.N, Year] != endYearBP) {
    stop("Last year in 'nkdeRepository' must equal endYearBP.",
         call. = FALSE)
  }
  repoTimeStep <- nkdeRepository[2, Year] - nkdeRepository[1, Year]
  if (repoTimeStep != timestep) {
    stop("The timestep in 'nkdeRepository' must match 'timestep'.",
         call. = FALSE)
  }
  okay_scaling_models <- c("St", "Lm", "LSDn", "Ag")
  if (scaling_model %in% okay_scaling_models == FALSE) {
    stop("scaling_model must be 'St', 'Lm', 'LSDn', or 'Ag'",
         call. = FALSE)
  }
  okay_err <- c("internal", "external")
  if (err %in% okay_err == FALSE) {
    stop("err must be either 'internal' or 'external'.", call. = FALSE)
  }
  if (!is.logical(proportionalVariability)) {
    stop("'proportionalVariability' must be logical.", call. = FALSE)
  }
  if (!is.logical(compileCamels)) {
    stop("'compileCamels' must be logical.", call. = FALSE)
  }
  #
  # MAIN BODY:
  #
  # Step 1: Clean file...
  # Eliminate blank rows
  DT <- DT[Sample_Name != ""]
  # more error checking
    # every Sample_Name in DT must contain a "--" after the actual
    # sample name.
    grepResults <- grep("--", DT[, Sample_Name])
    if (length(grepResults) != DT[, .N]) {
      stop("Every sample name in Sample_Name must be followed by '--'",
           call. = FALSE)
    }
  #
  # Step 2: Determine which columns will be extracted from DT  
    Age_Col <- paste0(scaling_model, "_Age_yr")
    IntErrName <- paste0(scaling_model, "_Interr_yr")
    ExtErrName <- paste0(scaling_model, "_Exterr_yr")
    sampleName <- strsplit(DT[1, Sample_Name], "--")[[1]][1]
  #
  # Step 3: Load boulder heights
    if (proportionalVariability == TRUE) {
      varType <- "proportionalVariability_"
    } else {
      varType <- "historicalVariability_"
    }
    fileToLoad <- paste0("exportBoulderHeight_",
                         HistSnowPercent,
                         "PercentHistoricalSnow_",
                         varType,
                         sampleName,
                         ".txt")
    bouHeights <- fread(fileToLoad)
    keepLength <- bouHeights[, .N] / 12L
    bouHeights <- bouHeights[1:keepLength, Boulder_Height_m]
    if (is.integer(bouHeights)) {
      bouHeights <- as.numeric(bouHeights)
    }
  # 
  # Step 3: Save all the individual iterations in dateRepository
    ages <- DT[, get(Age_Col)]
    IntErr  <- DT[, get(IntErrName)]
    ExtErr  <- DT[, get(ExtErrName)]
    dateRepository[Sample_Name == sampleName &
                     PercentSnow == HistSnowPercent &
                     CalibDataset == calibdataset,
                   ':=' (Boulder_Height_m = bouHeights,
                         Best_Est_Age_yr = ages,
                         Internal_SD_yr = IntErr,
                         External_SD_yr = ExtErr)]
  #
  # Step 4: Adjust ages and uncertainties
    # I just discovered (3/16/2018, 7 am) that the Be-10 concentrations
    #  that I used in all 1.386 million age calculations are exactly
    #  3.1% lower than the values in my lab chemistry spreadsheet. 
    # This is because we had the [Be] in our spike remeasured and I forgot
    #  to update my age calculation spreadsheets.
    # To avoid redoing all 1.386 million age calculations, I'm going to
    #  adjust all the calculated ages upward by 3.1%. This isn't strictly
    #  valid, as Be-10 production rates are time varying due to the
    #  varying strength and orientation of the magnetic field, but the
    #  difference is minor and will not affect our conclusions.
    dateRepository[Sample_Name == sampleName &
                     PercentSnow == HistSnowPercent &
                     CalibDataset == calibdataset,
                   Best_Est_Age_yr :=
                     as.integer(round(Best_Est_Age_yr * 1.031))]
    # The external uncertainties are also affected.
    dateRepository[Sample_Name == sampleName &
                     PercentSnow == HistSnowPercent &
                     CalibDataset == calibdataset,
                   External_SD_yr :=
                     as.integer(round(External_SD_yr * 1.031))]
    # The uncertainties on the Be-10 concentrations are also different
    #  between the file I previously used to calculate the ages and
    #  the lab chemistry spreadsheet, but variably different at 1-2%.
    AdjustmentFile <- fread("Adjustments.txt")
    dateRepository[Sample_Name == sampleName &
                     PercentSnow == HistSnowPercent &
                     CalibDataset == calibdataset,
                   Internal_SD_yr := as.integer(round(Internal_SD_yr *
                     AdjustmentFile[Sample_Name == sampleName,
                                           UncertaintyAdjustmentFactor]))]
    # Convert from exposure ages prior to sampling, to exposure dates
    #  relative to 1950, to facilitate comparison with C-14 dates.
    dateRepository[Sample_Name == sampleName &
                     PercentSnow == HistSnowPercent &
                     CalibDataset == calibdataset,
                   Best_Est_Age_yr := Best_Est_Age_yr -
                     (AdjustmentFile[Sample_Name == sampleName,
                                    SamplingYear] - 1950L)]
  #
  # Step 5: If compileCamels == TRUE,
  if (compileCamels == TRUE) {
    # create a temporary DT to accumulate the kernel density estimates
    temp <- data.table(Year = seq.int(startYearBP, endYearBP, timestep))
    # and for every individual monte carlo simulation,
    #  calculate normal kernel density estimates
    for (i in 1:dateRepository[Sample_Name == sampleName &
                               PercentSnow == HistSnowPercent &
                               CalibDataset == calibdataset, .N]) {
      # extract mean and sd
      meanAge <- dateRepository[Sample_Name == sampleName &
                                  PercentSnow == HistSnowPercent &
                                  CalibDataset == calibdataset &
                                  Trial == i, Best_Est_Age_yr]
      if (err == "internal") {
        sdAge <- dateRepository[Sample_Name == sampleName &
                                   PercentSnow == HistSnowPercent &
                                   CalibDataset == calibdataset &
                                   Trial == i, Internal_SD_yr]
      } else if (err == "external") {
        sdAge <-  dateRepository[Sample_Name == sampleName &
                                    PercentSnow == HistSnowPercent &
                                    CalibDataset == calibdataset &
                                    Trial == i, External_SD_yr]
      } else {
        stop("err must equal 'internal' or 'external'", call. = FALSE)
      }
      # calculate normal kernel density estimates
      temp[, DT[i, Sample_Name] := (1 / sqrt(2 * pi * sdAge ^ 2) *
                      exp(-(Year - meanAge) ^ 2 / (2 * sdAge ^ 2)))]
    }
    # sum over all the columns by Year
    temp[, summaryCol := sum(.SD), by = Year]
    # export summaryCol to the nkdeRepository DT
    nkdeRepository[PercentSnow == HistSnowPercent,
                   (sampleName) := temp[, summaryCol]]
  }  
 #
 # End of Function
}

compile_camels <- function(dateRepository = allDates,
                           nkdeRepository = camelDTexternal,
                           meanAge = "Years_B1950",
                           AgeSD = "External_SD_yr",
                           startYearBP = 10000L,
                           endYearBP   = 30000L,
                           timestep = 25L) {
  # Purpose:
  #   This function reads through a DT and uses the values in the
  #   "meanAge" and "AgeSD" columns to calculate normal kernel density
  #   estimates. These nkde's are summed over all the iterations for
  #   a particular sample and the summed value is record in the
  #   "nkdeRepository".
  #
  # Args:
  #   dateRepository: The DT that hosts all the monte carlo simulations
  #       for this study.
  #   nkdeRepository: The DT that accumulates the results of the 
  #       normal kernel density estimates. The repository DT enables
  #       all the nkd estimates for all the samples in the stored in a 
  #       single location.
  #   meanAge: A length 1 character vector that names the column in
  #       dateRepository with the mean values to be used in the
  #       nkde calculations.
  #   AgeSD: A length 1 character vector that names the column in
  #       dateRepository with the standard deviations to be used in the
  #       nkde calculations.
  #   startYearBP: The younger end of the age range to investigate. The 
  #       function will generate a series of ages and calculate the 
  #       normal kernel density estimate at each time step.
  #   endYearBP: The older end of the age range to investigate.
  #   timestep: The timestep to use in generate the series of ages to
  #       use in the normal kernel density estimates.
  #
  # Returns:
  #   Returns an updated nkdeRepository DT with the nkdes.
  #
  # Error Checking:
  if (!is.data.table(dateRepository)) {
    stop("'dateRepository' must be a DT.", call. = FALSE)
  }
  if (!is.data.table(nkdeRepository)) {
    stop("'nkdeRepository' must be a DT.", call. = FALSE)
  }
  if (!is.character(meanAge)) {
    stop("meanAge must be a character vector", call. = FALSE)
  }
  if (length(meanAge) > 1) {
    stop("meanAge must be length 1.", call. = FALSE)
  }
  if (meanAge %in% names(dateRepository) == FALSE) {
    stop("meanAge must be a column name in dateRepository",
         call. = FALSE)
  }
  if (!is.character(AgeSD)) {
    stop("AgeSD must be a character vector", call. = FALSE)
  }
  if (length(AgeSD) > 1) {
    stop("AgeSD must be length 1.", call. = FALSE)
  }
  if (AgeSD %in% names(dateRepository) == FALSE) {
    stop("AgeSD must be a column name in dateRepository",
         call. = FALSE)
  }
  if (!is.integer(startYearBP)) {
    stop("'startYearBP' needs to be an integer.", call. = FALSE)
  }
  if (!is.integer(endYearBP)) {
    stop("'endYearBP' needs to be an integer.", call. = FALSE)
  }
  if (!is.integer(timestep)) {
    stop("'timestep' needs to be an integer.", call. = FALSE)
  }
  if (nkdeRepository[1, Year] != startYearBP) {
    stop("First year in 'nkdeRepository' must equal startYearBP.",
         call. = FALSE)
  }
  if (nkdeRepository[.N, Year] != endYearBP) {
    stop("Last year in 'nkdeRepository' must equal endYearBP.",
         call. = FALSE)
  }
  repoTimeStep <- nkdeRepository[2, Year] - nkdeRepository[1, Year]
  if (repoTimeStep != timestep) {
    stop("The timestep in 'nkdeRepository' must match 'timestep'.",
         call. = FALSE)
  }
  #
  # MAIN BODY:
  #
  # Step 1: Create a temporary DT to accumulate the kernel density estimates
  
  #
  # Step 2: Create vectors to loop over
  sampleNames <- dateRepository[, unique(Sample_Name)]
  PercentSnows <- dateRepository[, unique(PercentSnow)]
  calibs <- dateRepository[, unique(CalibDataset)]
  #
  # Step 3: Run through dateRepository, calculating nkdes.
  for (i in 1:length(calibs)) {
    for (j in 1:length(PercentSnows)) {
      for (k in 1:length(sampleNames)) {
        # Create a temporary DT to accumulate the kernel density estimates
        temp <- data.table(Year = seq.int(startYearBP, endYearBP, timestep))
        # loop over the trials
        for (l in 1:dateRepository[Sample_Name == sampleNames[k] &
                                   PercentSnow == PercentSnows[j] &
                                   CalibDataset == calibs[i],
                                   .N]) {
          # create tempColName
          tempColName <- paste0("Trial", l)
          # extract meanAge value
          meanAgeValue <- dateRepository[Sample_Name == sampleNames[k] &
                                           PercentSnow == PercentSnows[j] &
                                           CalibDataset == calibs[i] &
                                           Trial == l,
                                         get(meanAge)]
          # extract AgeSD value
          AgeSDValue <- dateRepository[Sample_Name == sampleNames[k] &
                                         PercentSnow == PercentSnows[j] &
                                         CalibDataset == calibs[i] &
                                         Trial == l,
                                       get(AgeSD)]
          # calculate nkde
          temp[, (tempColName) := (1 / sqrt(2 * pi * AgeSDValue ^ 2) *
            exp(-(Year - meanAgeValue) ^ 2 / (2 * AgeSDValue ^ 2)))]
        }
        # sum over all the columns by Year
        temp[, summaryCol := sum(.SD), by = Year]
        # export summaryCol to the nkdeRepository DT
        nkdeRepository[PercentSnow == PercentSnows[j] &
                         CalibDataset == calibs[i],
                       sampleNames[k] := temp[, summaryCol]]
      }
    }
  }
}


find_camel_percentiles <- function(DT, returnDTname, 
           percentiles = c(0.025, 0.159, 0.5, 0.841, 0.975)) {
  # Purpose:
  #   This function finds the ages associated with various percentiles
  #   in the curves of normal kernel density estimates (i.e. camel
  #   graphs).
  #
  # Args:
  #   DT: A data.table containing the following columns: Year, PercentSnow,
  #       CalibDataset, Sample_Name, and nkde.
  #   returnDTname: The name of the data.table that will host the results 
  #       of this function's calculations.
  #   percentiles: A numeric vector that contains values between >0 and
  #       <1. These are the percentiles at which the code will find the
  #       sample ages.
  #
  # Returns:
  #   A percentileDT that is populated with ages of the selected
  #   percentiles for all the samples.   
  #
  # Error Checking:
    if (!is.data.table(DT)) {
      stop("DT needs to be a data.table.", call. = FALSE)
    }
    if ("Year" %in% names(DT) == FALSE) {
      stop("The DT needs a 'Year' column.", call. = FALSE)
    }
    if ("PercentSnow" %in% names(DT) == FALSE) {
      stop("The DT needs a 'PercentSnow' column.", call. = FALSE)
    }
    if ("CalibDataset" %in% names(DT) == FALSE) {
      stop("The DT needs a 'CalibDataset' column.", call. = FALSE)
    }
    if ("Sample_Name" %in% names(DT) == FALSE) {
      stop("The DT needs a 'Sample_Name' column.", call. = FALSE)
    }
    if ("nkde" %in% names(DT) == FALSE) {
      stop("The DT needs a 'nkde' column.", call. = FALSE)
    }
    if (!is.character(returnDTname)) {
      stop("'returnDTname' must be a char vector", call. = FALSE)
    }
    if (length(strsplit(returnDTname, " ")[[1]]) > 1) {
      stop("returnDTname cannot contain spaces.", call. = FALSE)
    }
    if (!is.numeric(percentiles)) {
      stop("'percentiles' must be numeric.", call. = FALSE)
    }
    if (min(percentiles) <= 0.0) {
      stop("The minimum value in percentiles must be greater than 0.",
           call. = FALSE)
    }
    if (max(percentiles) >= 1.0) {
      stop("The maximum value in percentiles must be less than 1.",
           call. = FALSE)
    }
  #
  # Main Body:
  #
  # Step 1: create a DT to host the results
    returnDT <- data.table(expand.grid(
      Percentile = percentiles,
      Sample_Name = DT[, unique(Sample_Name)],
      PercentSnow = DT[, unique(PercentSnow)],
      Age_yr = as.integer(NA)))
  #
  # Step 2: sum DT's nkde column by Sample_Name,
    #  to calculate the area under the prob. dist. for each sample
    DT[, nkde_sum := sum(nkde),
       by = .(Sample_Name, PercentSnow, CalibDataset)]
  #
  # Step 3: add a cumulative nkde column for each Sample
    unique_years <- DT[, unique(Year)]
    for (i in length(unique_years):1) {
      DT[Year <= unique_years[i],
                   cum_nkde := sum(nkde), 
                   by = .(Sample_Name, PercentSnow, CalibDataset)]
    }
  #
  # Step 4: find percentile ages
    # set up vectors for iteration
    snowValues <- DT[, unique(PercentSnow)]
    nameValues <- DT[, unique(Sample_Name)]
    yearValues <- DT[, unique(Year)]
    timestep <- yearValues[2] - yearValues[1]
    # find percentile ages
    for (i in 1:length(snowValues)) {
      for (j in 1:length(nameValues)) {
        for (k in 1:length(percentiles)) {
          nkdeTarget <- percentiles[k] * DT[PercentSnow == snowValues[i] &
                                              Sample_Name == nameValues[j],
                                            max(nkde_sum)]
          big_year <- DT[PercentSnow == snowValues[i] &
                           Sample_Name == nameValues[j] &
                           cum_nkde >= nkdeTarget,
                         min(Year)]
          big_nkde <- DT[PercentSnow == snowValues[i] &
                           Sample_Name == nameValues[j] &
                           cum_nkde >= nkdeTarget,
                         min(cum_nkde)]
          small_nkde <- DT[PercentSnow == snowValues[i] &
                             Sample_Name == nameValues[j] &
                             Year == big_year - timestep,
                           cum_nkde]
          difference <- big_nkde - small_nkde
          below <- big_nkde - nkdeTarget
          percentBelow <- below / difference
          percentileYear <- round(big_year - (timestep * percentBelow))
          
          # record value in returnDT
          returnDT[Percentile == percentiles[k] &
                     Sample_Name == nameValues[j] &
                     PercentSnow == snowValues[i],
                   Age_yr := percentileYear]
        }
      }
    }
  #
  # Step 5: Export returnDT as a text file
    returnDTexport <- as.data.table(spread(returnDT, Percentile,
                                           Age_yr))
    # change column names
    old_names <- names(returnDTexport)
    length_old_names <- length(old_names)
    old_names <- old_names[3:length_old_names]
    new_names <- rep_len(as.character(NA), length(old_names))
    for (i in 1:length(old_names)) {
      new_names[i] <- paste0("Percentile_", old_names[i])
    }
    setnames(returnDTexport, old_names, new_names)
    # add boulder height
    boulder_height <- fread("Boulder_Height.txt")
    for (i in 1:(length(nameValues))) {
      returnDTexport[Sample_Name == nameValues[i],
       Boulder_Height_m := boulder_height[Sample_Name == nameValues[i],
                                          Boulder_Height_m]]
    }
    fwrite(returnDTexport, paste0(returnDTname, ".txt"), sep = "\t")
    return(returnDT)
  #
  # End of Function
}

prob_consistent_with_deglac <- function(DT, calc_col, calc_col_SD,
                            best_est = NA, min14C = NA, max14C = NA) {
  # Purpose:
  #   This function finds the probabiliy that the inferred timing of
  #   deglaciation from an 10Be dataset is consistent with the 
  #   independent evidence for deglaciation.
  #
  # Args:
  #   DT: A data.table containing the column identified in "calc_col"
  #       and a column named "PercentSnow".
  #   calc_col: The column of inferred deglaciation dates to compare
  #       with the independent evidence.
  #   calc_col_SD: The column with the SD information for calc_col.
  #   best_est: The best estimated timing of deglaciation from
  #       independent evidence. A length 2 numeric vector with the first
  #       value being the most likely time and the second value being
  #       the 1 sigma uncertainty. Code assumes a normal distribution.
  #   min14C: The minimum limiting 14C age. If this is other than NA,
  #       best_est MUST be NA and max14C CANNOT be NA.
  #   max14C: The maximum limiting 14C age. If this is other than NA,
  #       best_est MUST be NA and min14C CANNOT be NA.
  #
  # Returns:
  #   An updated DT with a new column of probabilities.   
  #
  # Error Checking:
  if (!is.data.table(DT)) {
    stop("DT needs to be a data.table.", call. = FALSE)
  }
  if (!is.character(calc_col)) {
    stop("calc_col must be character", call. = FALSE)
  }
  if (length(calc_col) != 1) {
    stop("calc_col must be length 1", call. = FALSE)
  }
  if (calc_col %in% names(DT) == FALSE) {
    stop("The DT must include the column listed in calc_col.",
         call. = FALSE)
  }
  if ("PercentSnow" %in% names(DT) == FALSE) {
    stop("DT must contain a column named 'PercentSnow'.",
         call. = FALSE)
  }
  if (calc_col_SD %in% names(DT) == FALSE) {
    stop("The DT must include the column listed in calc_col_SD.",
         call. = FALSE)
  }
  best_est1 <- best_est[1]
  min14C1 <- min14C[1]
  max14C1 <- max14C[1]
  if (is.na(best_est1)) {
    if (is.na(min14C1)) {
      stop("min14C cannot be NA if best_est is also NA.", call. = FALSE)
    }
    if (!is.integer(min14C) & !is.numeric(min14C)) {
      stop("min14C must be integer or numeric.", call. = FALSE)
    }
    if (length(min14C) != 2L) {
      stop("min14C must be length 2", call. = FALSE)
    }
    if (is.na(max14C1)) {
      stop("max14C cannot be NA if best_est is also NA.", call. = FALSE)
    }
    if (!is.integer(max14C) & !is.numeric(max14C)) {
      stop("max14C must be integer or numeric.", call. = FALSE)
    }
    if (length(max14C) != 2L) {
      stop("max14C must be length 2", call. = FALSE)
    }
  }
  if (!is.na(best_est1)) {
    if (!is.na(min14C1)) {
      stop("min14C MUST be NA if best_est is NOT NA.", call. = FALSE)
    }
    if (!is.na(max14C1)) {
      stop("max14C MUST be NA if best_est is NOT NA.", call. = FALSE)
    }
    if (!is.integer(best_est) & !is.numeric(best_est)) {
      stop("best_est must be integer or numeric.", call. = FALSE)
    }
    if (length(best_est) != 2L) {
      stop("best_est must be length 2.", call. = FALSE)
    }
  }
  #
  # Main Body:
  # Determine new column name
  new_colName <- paste0("P_", calc_col, "_withinDegl")
  # if best_est ISN'T NA...
  if (!is.na(best_est1)) {
    # find probability that calc_col is within boundaries of best_est
    DT[!is.na(get(calc_col)), (new_colName) := 
         pnorm(get(calc_col), best_est[1], best_est[2])]
    DT[get(new_colName) > 0.5, (new_colName) := 1 - get(new_colName)]
    DT[!is.na(get(new_colName)), (new_colName) := 2 * get(new_colName)]
  }
  # if best_est ISN'T NA...
  if (is.na(best_est1)) {
    # find prob that a random number from calc_col +/- SD is greater than
    #  min14C distribution
    # see https://math.stackexchange.com/questions/40224/
    DT[!is.na(get(calc_col)), ProbOlder := 
         pnorm(0, get(calc_col) - min14C[1],
               sqrt(get(calc_col_SD) ^ 2 + min14C[2] ^ 2),
               lower.tail = FALSE)]
    # find prob that random number from calc_col is less than max14C
    DT[!is.na(get(calc_col)), ProbYounger := 
         pnorm(0, get(calc_col) - max14C[1],
               sqrt(get(calc_col_SD) ^ 2 + max14C[2] ^ 2))]
    # find prob both older and younger
    DT[!is.na(get(calc_col)), (new_colName) := ProbOlder * ProbYounger]
    DT[, ':=' (ProbOlder = NULL,
               ProbYounger = NULL)]
  }
  # End of Function
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # FROM: http://www.cookbook-r.com/Graphs/ ...
  #                        Multiple_graphs_on_one_page_(ggplot2)/
  #
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #  
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
  # End of Function.  
}


# Load datasets------------------------------------------------------------
# The datasets are the output from version 3 of the online calculator
#  formerly known as the CRONUS-Earth online calculator.

# Each dataset represents the results for a single sample.  Khione was used
#  to generate a series of monte carlo trials (1000) for each
#  cosmo sample, in order to estimate the age uncertainty associated with
#  the uncertainty in the various parameters. Twelve parameters are
#  considered in the monte carlo uncertainty calculation: latitude,
#  longitude, elevation, sample thickness, sample density, topographic
#  shielding, erosion rate, Be-10 concentration, boulder height,
#  cosmic ray attentuation length and snow shielding.

# Experimenting in Khione with n = 10, 100, and 1000 reveals that, when
#  the monte carlo simulation has an n of 1000, the standard deviation
#  of the mean age (for 10 experiments of n = 1000 each) is less than 0.1%. 
#  Thus, n = 1000 represents a reasonable
#  balance between calculation time (~2 min / experiment for the online
#  calculator) and precision.

# Each dataset loaded in here, therefore, will represent 1000 trials
#  of the same sample.  This script will read the text file and combine
#  all these calculated ages into a single normal kernel density estimate
#  for that sample.

# Find the files to load...
results_files <- list.files(pattern = "results")
txt_index <- grepl(".txt", results_files)
results_files <- results_files[txt_index]

# create empty vectors for later use
DTnameVector <-
  as.character(rep(NA, length.out = length(results_files)))
SnowPercentVector <-
  as.character(rep(NA, length.out = length(results_files)))
SampleNameVector <-
  as.character(rep(NA, length.out = length(results_files)))
CalibDatasetVector <-
  as.character(rep(NA, length.out = length(results_files)))

# load the files...
for (i in 1:length(results_files)) {
  # construct DT name
   # extract sample name
    DTname1 <- strsplit(results_files[i], "Variability_")
    nchar_DTname1 <- nchar(DTname1[[1]][2])
    nchar_keep <- nchar_DTname1 - 4
    DTname2 <- substr(DTname1[[1]][2], 1, nchar_keep)
   # assign DTname2 to SampleNameVector
    SampleNameVector[i] <- DTname2
   # extract percent historical snow
    SnowNum <- strsplit(DTname1[[1]][1], "_results_monte_carlo_")
    SnowNum1 <- strsplit(SnowNum[[1]][2], "PercentHistoricalSnow")
    SnowNum1 <- SnowNum1[[1]][1]
   # record snow percent in SnowPercentVector
    SnowPercentVector[i] <- SnowNum1
   # record the calibration dataset used in age calcs
    CalibDatasetVector[i] <- SnowNum[[1]][1]
    DTname <- paste0(DTname2, "_",
                     SnowPercentVector[i], "Snow", "_",
                     CalibDatasetVector[i])
   # drop dashes into underscores
    DTname <- gsub("-", "_", DTname)
  # record DTname in DTnameVector
   DTnameVector[i] <- DTname
  
  # load file, assign it to DTname
  assign(DTname, fread(results_files[i]))
}

# coerce SnowPercentVector to integer, remove duplicates, and sort
SnowPercentVector <- as.integer(SnowPercentVector)
SnowPercentVector <- unique(SnowPercentVector)
SnowPercentVector <- sort.int(SnowPercentVector)

# remove duplicates from SampleNameVector
SampleNameVector <- unique(SampleNameVector)

# remove duplicates from CalibDatasetVector
CalibDatasetVector <- unique(CalibDatasetVector)

# Error checking: Are any records missing?
DTLength <- length(DTnameVector)
snowLength <- length(SnowPercentVector)
if (DTLength / snowLength != 22) {
  stop("At least 1 'results_monte_carlo...' file is missing.",
       call. = FALSE)
}
# If you're using this script off GitHub, there might not be an error.
#  Just be certain that all the files you want are present.

# create nkdeRepository DTs
startYearBP <- 10000L
endYearBP   <- 30000L
timestep    <-    25L
Year <- seq.int(startYearBP, endYearBP, timestep)
camelDTinternal <- data.table(expand.grid(Year = Year,
                                  PercentSnow = SnowPercentVector,
                                  CalibDataset = CalibDatasetVector))
camelDTexternal <- data.table(expand.grid(Year = Year,
                                          PercentSnow = SnowPercentVector,
                                          CalibDataset = CalibDatasetVector))

# create DT to accumulate all the individual monte carlo sims
sims <- 1000L
sim_seq <- seq_len(sims)
allDates <- data.table(expand.grid(Sample_Name = SampleNameVector,
                                   Trial = sim_seq,
                                   PercentSnow = SnowPercentVector,
                                   CalibDataset = CalibDatasetVector))
allDates[, ':=' (Boulder_Height_m = as.numeric(NA),
                 Best_Est_Age_yr = as.integer(NA),
                 Internal_SD_yr = as.integer(NA),
                 External_SD_yr = as.integer(NA),
                 Z_Factor = rnorm(allDates[, .N]))]


# WARNING: process datasets--------------------------------------------------

for (i in 1:length(DTnameVector)) {
  # determine HistSnowPercent
  HistSnowPercent <- strsplit(DTnameVector[i], "_")
  snowLength <- length(HistSnowPercent[[1]])
  calibdataset <- HistSnowPercent[[1]][snowLength]
  HistSnowPercent <-
    as.integer(strsplit(HistSnowPercent[[1]][snowLength - 1], "Snow"))
  clean_and_consolidate_iterations(get(DTnameVector[i]),
                                   HistSnowPercent,
                                   calibdataset,
                                   nkdeRepository = camelDTinternal,
                                   compileCamels = TRUE)
}
# change column name to Years_B1950, because already converted
setnames(allDates, "Best_Est_Age_yr", "Years_B1950")

# Resample the age distributions
allDates[, Resampled_IntAge :=
    as.integer(round(Years_B1950 + Internal_SD_yr * Z_Factor))]
allDates[, Resampled_ExtAge :=
    as.integer(round(Years_B1950 + External_SD_yr * Z_Factor))]

# WARNING: Export results--------------------------------------------------------
fwrite(allDates, "allDates1.txt", sep = "\t")
fwrite(camelDTinternal, "camelDTinternal.txt", sep = "\t")


# WARNING: Reimport results--------------------------------------------------
allDates <- fread("allDates1.txt")
# allDates[, CalibDataset := NULL]
camelDTint <- fread("camelDTinternal.txt",
                         colClasses = "character")
oldNames <- names(camelDTint)
newNames <- gsub("-", "_", oldNames)
setnames(camelDTint, oldNames, newNames)
camelDTint[, ':=' (Year = as.integer(Year),
                   PercentSnow = as.integer(PercentSnow),
                   C2_Boulder1 = as.numeric(C2_Boulder1),
                   C2_Boulder2 = as.numeric(C2_Boulder2),
                   LoLy_11_08 = as.numeric(LoLy_11_08),
                   LyCy_1 = as.numeric(LyCy_1),
                   LyCy_3 = as.numeric(LyCy_3),
                   LyCy_4 = as.numeric(LyCy_4),
                   LyCy_5 = as.numeric(LyCy_5),
                   TM14_01 = as.numeric(TM14_01),
                   TM14_02 = as.numeric(TM14_02),
                   TM14_03 = as.numeric(TM14_03),
                   TM14_04 = as.numeric(TM14_04),
                   TM14_05 = as.numeric(TM14_05),
                   TM14_06 = as.numeric(TM14_06),
                   TM14_07 = as.numeric(TM14_07),
                   TM14_11 = as.numeric(TM14_11),
                   TM14_12 = as.numeric(TM14_12),
                   TM14_13 = as.numeric(TM14_13),
                   TM14_15 = as.numeric(TM14_15),
                   TM14_16 = as.numeric(TM14_16),
                   TM14_17 = as.numeric(TM14_17),
                   TM14_18 = as.numeric(TM14_18),
                   UpLy_12_10 = as.numeric(UpLy_12_10))]


# WARNING: compile camels for external uncertainties------------------------
compile_camels()
fwrite(camelDTexternal, "camelDTexternal.txt", sep = "\t")
camelDText <- fread("camelDTexternal.txt")


# WARNING: Convert to tidy data------------------------------------------------
# melt data into 3 columns: Sample_Name, Year, and nkde
meltedCamelInt <- melt(camelDTint, c("Year", "PercentSnow", "CalibDataset"))
meltedCamelExt <- melt(camelDText, c("Year", "PercentSnow", "CalibDataset"))
# rename columns
setnames(meltedCamelInt, c("variable", "value"), c("Sample_Name", "nkde"))
setnames(meltedCamelExt, c("variable", "value"), c("Sample_Name", "nkde"))
# change underscores in "Sample_Name" to dashes
meltedCamelInt[, Sample_Name :=
                 gsub("_", "-", meltedCamelInt[, Sample_Name])]


# WARNING: Calculate Percentiles------------------------------------------------
IntPercentiles <- find_camel_percentiles(meltedCamelInt, "IntPercentiles")
ExtPercentiles <- find_camel_percentiles(meltedCamelExt, "ExtPercentiles")
fwrite(IntPercentiles, "IntPercentiles.txt", sep = "\t")
fwrite(ExtPercentiles, "ExtPercentiles.txt", sep = "\t")


# Reload percentiles and other files--------------------------------
percentiles <- fread("IntPercentiles0and100.txt")
ExtPercentiles <- fread("ExtPercentiles0and100.txt")

# spread percentile values out into new columns
percentiles <- as.data.table(spread(percentiles, Percentile, Age_yr))
ExtPercentiles <- as.data.table(spread(ExtPercentiles, Percentile, Age_yr))
# rename columns
oldNames <- names(percentiles)
oldNames <- oldNames[3:7]
newNames <- rep(as.character(NA), length(oldNames))
for (i in 1:length(newNames)) {
  newNames[i] <- paste0("Percentile_", oldNames[i])
}
setnames(percentiles, oldNames, newNames)
setnames(ExtPercentiles, oldNames, newNames)

# add boulder height
boulder_height <- fread("Boulder_Height.txt")
for (i in 1:length(SampleNameVector)) {
  percentiles[Sample_Name == SampleNameVector[i],
              Boulder_Height_m := 
                boulder_height[Sample_Name == SampleNameVector[i],
                               Boulder_Height_m]]
}

# calc r-squared of median age vs boulder height
ShortSnowPercentVector <- c(0L, 100L)
for (i in 1:length(ShortSnowPercentVector)) {
  model <- lm(Percentile_0.5 ~ Boulder_Height_m,
              percentiles[PercentSnow == ShortSnowPercentVector[i] &
                            Exclude == FALSE])
  glanceModel <- glance(model)
  percentiles[PercentSnow == ShortSnowPercentVector[i] &
                Exclude == FALSE,
              r.squared := glanceModel$r.squared]
  percentiles[PercentSnow == ShortSnowPercentVector[i] &
                Exclude == FALSE,
              pearson := cor(Boulder_Height_m, Percentile_0.5)]
}

# prob pearson due to stochastic variation
# expected dist is 0 +/- 1 / sqrt(n - 3)
NumIncluded <- percentiles[Exclude == FALSE,
                           sum(!is.na(unique(Sample_Name)))]
percentiles[, p_pearson := pnorm(pearson, 0, (1 / sqrt(NumIncluded - 3)))]
percentiles[pearson >= 0, p_pearson := 1 - p_pearson]
# double pearson's r values to account for two-tailed test
percentiles[, p_pearson := p_pearson * 2]

# calc avg SD
percentiles[, AvgIntSD := (Percentile_0.841 - Percentile_0.159) / 2]
ExtPercentiles[, AvgExtSD := (Percentile_0.841 - Percentile_0.159) / 2]

# set keys and copy AvgExtSD into percentiles
setkey(percentiles, PercentSnow, Sample_Name)
setkey(ExtPercentiles, PercentSnow, Sample_Name)
percentiles[, AvgExtSD := ExtPercentiles[, AvgExtSD]]

# convert AvgIntSD and AvgExtSD from numeric to integer
percentiles[, AvgIntSD := as.integer(round(AvgIntSD))]
percentiles[, AvgExtSD := as.integer(round(AvgExtSD))]

# calc inverse variance
percentiles[, InverseVarInt := 1 / AvgIntSD ^ 2]
percentiles[, InverseVarExt := 1 / AvgExtSD ^ 2]

# Add column for Location
# Tuolumne Meadows
TMvector <- c("TM14-11", "TM14-12", "TM14-13", "TM14-15", "TM14-16",
              "TM14-17", "TM14-18")
percentiles[Sample_Name %in% TMvector, Location := "TM"]
# Lower Lyell Canyon
LLCvector <- c("TM14-02", "TM14-03", "TM14-04", "TM14-05", "TM14-06",
               "LoLy-11-08")
percentiles[Sample_Name %in% LLCvector, Location := "LLC"]
# Upper Lyell Canyon
ULCvector <- c("C2-Boulder1", "C2-Boulder2", "UpLy-12-10")
percentiles[Sample_Name %in% ULCvector, Location := "ULC"]
# Duhnforth et al dates from upper- and mid-Lyell Canyon
DuhnVector <- c("LyCy-1", "LyCy-3", "LyCy-4", "LyCy-5")
percentiles[Sample_Name %in% DuhnVector, Location := "Duhnforth"]
# lower mid-Lyell Canyon
percentiles[Sample_Name == "TM14-01", Location := "LMLC"]
# TM and LLC
percentiles[Sample_Name == "TM14-07", Location := "TMLLC"]

# Define LocationsVector
LocationsVector <- percentiles[, unique(Location)]

# Add column for TMLocation
PDVector <- c("TM14-11", "TM14-12", "TM14-13")
LMVector <- "TM14-15"
LDVector <- c("TM14-16", "TM14-17", "TM14-18")
percentiles[Sample_Name %in% PDVector,
            TMLocation := "PD"]
percentiles[Sample_Name %in% LMVector,
            TMLocation := "LM"]
percentiles[Sample_Name %in% LDVector,
            TMLocation := "LD"]

# Define TMLocationsVector
TMLocationsVector <- percentiles[, unique(TMLocation)]
TMLocationsVector <- TMLocationsVector[2:4]

# calc weighted means
percentiles[Exclude == FALSE,
  WMeanAgeAll :=
    as.integer(round(weighted.mean(Percentile_0.5, InverseVarInt))),
            by = "PercentSnow"]
# calc Location WMean
for (i in 1:length(LocationsVector)) {
  percentiles[Exclude == FALSE & Location == LocationsVector[i],
              LocWMean := 
    as.integer(round(weighted.mean(Percentile_0.5, InverseVarInt))),
            by = "PercentSnow"]
}
# calc TMLocation WMean
for (i in 1:length(TMLocationsVector)) {
  percentiles[Exclude == FALSE & TMLocation == TMLocationsVector[i],
              TMLocWMean :=
    as.integer(round(weighted.mean(Percentile_0.5, InverseVarInt))),
            by = "PercentSnow"]
}

# calc MSWDs
NumIncluded <- percentiles[PercentSnow == 0, sum(!is.na(WMeanAgeAll))]
percentiles[Exclude == FALSE,
  MSWDall := sum((Percentile_0.5 - WMeanAgeAll) ^ 2 * InverseVarInt)
                / (NumIncluded - 1),
            by = "PercentSnow"]
# calc MSWD by Location
NumByLocation <- rep.int(NA, length(LocationsVector))
for (i in 1:length(NumByLocation)) {
  NumByLocation[i] <- percentiles[Exclude == FALSE &
                                    Location == LocationsVector[i],
                                  length(unique(Sample_Name))]
}
for (i in 1:length(LocationsVector)) {
  if (NumByLocation[i] <= 1L) {
    next()
  }
  percentiles[Exclude == FALSE & Location == LocationsVector[i],
    LocMSWD := sum((Percentile_0.5 - LocWMean) ^ 2 * InverseVarInt) /
      (NumByLocation[i] - 1L),
    by = "PercentSnow"]
}
# calc MSWD by TMLocation
NumByTMLocation <- rep.int(NA, length(TMLocationsVector))
for (i in 1:length(NumByTMLocation)) {
  NumByTMLocation[i] <- percentiles[Exclude == FALSE &
                                      TMLocation == TMLocationsVector[i],
                                    length(unique(Sample_Name))]
}
for (i in 1:length(TMLocationsVector)) {
  if (NumByTMLocation[i] <= 1L) {
    next()
  }
  percentiles[Exclude == FALSE & TMLocation == TMLocationsVector[i],
    TMLocMSWD := sum((Percentile_0.5 - TMLocWMean) ^ 2 * InverseVarInt) /
      (NumByTMLocation[i] - 1L), 
    by = "PercentSnow"]
}

# Probability from a single normal distribution
percentiles[Exclude == FALSE,
            P_MSWDall := pchisq(MSWDall * (NumIncluded - 1),
                                (NumIncluded - 1),
                                lower.tail = FALSE)]
# Prob from single norm by location
for (i in 1:length(LocationsVector)) {
  if (NumByLocation[i] <= 1L) {
    next()
  }
  percentiles[Exclude == FALSE & Location == LocationsVector[i],
              P_LocMSWD := pchisq(LocMSWD * (NumByLocation[i] - 1L),
                                  NumByLocation[i] - 1L,
                                  lower.tail = FALSE)]
}
# Prob from single normal by TMLocation
for (i in 1:length(TMLocationsVector)) {
  if (NumByTMLocation[i] <= 1L) {
    next()
  }
  percentiles[Exclude == FALSE & TMLocation == TMLocationsVector[i],
              P_TMLocMSWD := pchisq(TMLocMSWD * (NumByTMLocation[i] - 1L),
                                    NumByTMLocation[i] - 1L,
                                    lower.tail = FALSE)]
}

# calculate internal uncertainties on weighted mean calculation,
#  following Douglass et al., 2006
percentiles[Exclude == FALSE,
            WMeanAgeAll_IntSD :=
              as.integer(round(sqrt(1 / sum(InverseVarInt)))),
            by = "PercentSnow"]
# internal uncertainties by location
for (i in 1:length(LocationsVector)) {
  if (NumByLocation[i] == 0L) {
    next()
  }
  if (NumByLocation[i] >= 1L) {
    percentiles[Exclude == FALSE & Location == LocationsVector[i],
                LocWMean_IntSD := 
              as.integer(round(sqrt(1 / sum(InverseVarInt)))),
              by = "PercentSnow"]
  }
}
# internal uncertainties by TMLocation
for (i in 1:length(TMLocationsVector)) {
  percentiles[Exclude == FALSE & TMLocation == TMLocationsVector[i],
              TMLocWMean_IntSD :=
                as.integer(round(sqrt(1 / sum(InverseVarInt)))),
              by = "PercentSnow"]
}
# adjust uncertainties upward for MSWD > 1 cases
percentiles[Exclude == FALSE & MSWDall > 1,
            WMeanAgeAll_IntSD := as.integer(round(WMeanAgeAll_IntSD
                                                  * sqrt(MSWDall)))]
percentiles[Exclude == FALSE & LocMSWD > 1,
            LocWMean_IntSD := 
              as.integer(round(LocWMean_IntSD * sqrt(LocMSWD)))]
percentiles[Exclude == FALSE & TMLocMSWD > 1,
            TMLocWMean_IntSD :=
              as.integer(round(TMLocWMean_IntSD * sqrt(TMLocMSWD)))]

# calculate external uncertainties
percentiles[Exclude == FALSE,
            WMeanAgeAll_ExtSD := 
              as.integer(round(sqrt(1 / sum(InverseVarExt)))),
            by = "PercentSnow"]
# calc ext uncertainty by location
for (i in 1:length(LocationsVector)) {
  if (NumByLocation[i] == 0L) {
    next()
  }
  if (NumByLocation[i] >= 1L) {
    percentiles[Exclude == FALSE & Location == LocationsVector[i],
                LocWMean_ExtSD := 
                  as.integer(round(sqrt(1 / sum(InverseVarExt)))),
                by = "PercentSnow"]
  }
}
# calc External uncertainty by TMLocation
for (i in 1:length(TMLocationsVector)) {
  percentiles[Exclude == FALSE & TMLocation == TMLocationsVector[i],
              TMLocWMean_ExtSD :=
                as.integer(round(sqrt(1 / sum(InverseVarExt)))),
              by = "PercentSnow"]
}
# adjust uncertainty upward for MSWD > 1 cases
percentiles[Exclude == FALSE & MSWDall > 1,
            WMeanAgeAll_ExtSD := as.integer(round(WMeanAgeAll_ExtSD *
                                                    sqrt(MSWDall)))]
percentiles[Exclude == FALSE & LocMSWD > 1,
            LocWMean_ExtSD := as.integer(round(LocWMean_ExtSD *
                                                 sqrt(LocMSWD)))]
percentiles[Exclude == FALSE & TMLocMSWD > 1,
            TMLocWMean_ExtSD := as.integer(round(TMLocWMean_ExtSD *
                                                   sqrt(TMLocMSWD)))]


# Create and populate DT for host Calculator Comparison------------------
CalculatorVector <- c("Standard Balco", "Monte Carlo Balco",
                      "CRONUScalc", "CREp")
UncertTypeVector <- c("Int", "Ext")
CalcCompare <- as.data.table(expand.grid(
  Sample_Name = c(SampleNameVector[1:3], SampleNameVector[8:22]),
  PercentSnow = c(0L, 100L),
  UncertType = UncertTypeVector,
  Calculator = CalculatorVector,
  ProdRate = as.character(NA),
  ScalingModel = as.character(NA),
  Atmosphere = as.character(NA),
  MagField = as.character(NA),
  Age = as.integer(NA),
  Uncert = as.integer(NA)))

# Construct iteration vectors
SampleNames <- CalcCompare[, unique(Sample_Name)]
ShortSnowVector <- CalcCompare[, unique(PercentSnow)]

# Populate CalcCompare with details about Balco calculator
CalcCompare[Calculator == "Standard Balco" |
              Calculator == "Monte Carlo Balco", ':=' (
  ProdRate = "CRONUS-Earth primary",
  ScalingModel = "LSDn",
  Atmosphere = "ERA40",
  MagField = "Lifton2016")]
# populate CalcCompare with Monte Carlo Balco ages
for (i in 1:length(SampleNames)) {
  for (j in 1:length(ShortSnowVector)) {
    for (k in 1:length(UncertTypeVector)) {
      # get name of uncertainty column
      if (UncertTypeVector[k] == "Int") {
        uncertName <- "AvgIntSD"
      } else if (UncertTypeVector[k] == "Ext") {
        uncertName <- "AvgExtSD"
      } else {
        stop("UncertType is not 'Int' or 'Ext'", call. = FALSE)
      }
      # extract values, add to CalcCompare
      CalcCompare[Calculator == "Monte Carlo Balco" &
                    Sample_Name == SampleNames[i] &
                    PercentSnow == ShortSnowVector[j] &
                    UncertType == UncertTypeVector[k], ':=' (
                      Age = percentiles[Sample_Name == SampleNames[i] &
                                          PercentSnow == ShortSnowVector[j],
                                        Percentile_0.5],
                      Uncert = percentiles[Sample_Name == SampleNames[i] &
                                             PercentSnow == ShortSnowVector[j],
                                           get(uncertName)])]
    }
  }
}

# Populate CalcCompare with SimpleBalco values
# 1- load simpleBalco
simpleBalco <- fread("simpleBalcoResults.txt")
# 2- convert ages in simpleBalco to dates benchmarked against 1950 CE
#  load adjustmentsFile
#  adjustmentsFile includes a column named UncertaintyAdjustmentFactor.
#  Ignore that column here, I've already updated the spreadsheets from
#  which the SimpleBalco ages were calculated with the new [10Be], 
#  so no ad-hoc correction is required here.
# The "adjustment" is putting "simple Balco" ages onto the BP (1950 CE)
#  timescale.
adjustmentsFile <- fread("adjustments.txt")
for (i in 1:length(SampleNames)) {
  simpleBalco[Sample_Name == SampleNames[i],
              Years_B1950 := LSDn_Age_yr -
                (adjustmentsFile[Sample_Name == SampleNames[i],
                                 SamplingYear] - 1950L)]
}
simpleBalco[, LSDn_Age_yr := NULL]
# 3- populate CalcCompare with standard Balco ages
for (i in 1:length(SampleNames)) {
  for (j in 1:length(ShortSnowVector)) {
    for (k in 1:length(UncertTypeVector)) {
      # get name of uncertainty column
      if (UncertTypeVector[k] == "Int") {
        uncertName <- "LSDn_Interr_yr"
      } else if (UncertTypeVector[k] == "Ext") {
        uncertName <- "LSDn_Exterr_yr"
      } else {
        stop("UncertType is not 'Int' or 'Ext'", call. = FALSE)
      }
      CalcCompare[Calculator == "Standard Balco" &
                    Sample_Name == SampleNames[i] &
                    PercentSnow == ShortSnowVector[j] &
                    UncertType == UncertTypeVector[k], ':=' (
                      Age = simpleBalco[Sample_Name == SampleNames[i] &
                                          PercentSnow == ShortSnowVector[j],
                                        Years_B1950],
                      Uncert = simpleBalco[Sample_Name == SampleNames[i] &
                                             PercentSnow == ShortSnowVector[j],
                                           get(uncertName)])]
    }
  }
}

# Populate CalcCompare with Marrero et al. / CRONUScalc
# Add details to CalcCompare about how ages were calculated
CalcCompare[Calculator == "CRONUScalc", ':=' (
  ProdRate = "CRONUS-Earth primary",
  ScalingModel = "LSDn",
  Atmosphere = "ERA40",
  MagField = "Lifton2014"
)]
shasta <- fread("ShastalatorResults.txt")
# convert ages from ka units to years
shasta[, ':=' (Age_B2010 = as.integer(Age_B2010 * 1000),
               TotalUncert = as.integer(TotalUncert * 1000),
               InternalUncert = as.integer(InternalUncert * 1000))]
# convert Age_B2010 into Years_B1950
shasta[, Years_B1950 := Age_B2010 - 60L]
# populate CalcCompare
for (i in 1:length(SampleNames)) {
  for (j in 1:length(ShortSnowVector)) {
    for (k in 1:length(UncertTypeVector)) {
      # get name of uncertainty column
      if (UncertTypeVector[k] == "Int") {
        uncertName <- "InternalUncert"
      } else if (UncertTypeVector[k] == "Ext") {
        uncertName <- "TotalUncert"
      } else {
        stop("UncertType is not 'Int' or 'Ext'", call. = FALSE)
      }
      CalcCompare[Calculator == "CRONUScalc" &
                    Sample_Name == SampleNames[i] &
                    PercentSnow == ShortSnowVector[j] &
                    UncertType == UncertTypeVector[k], ':=' (
                      Age = shasta[Sample_Name == SampleNames[i] &
                                     PercentSnow == ShortSnowVector[j],
                                   Years_B1950],
                      Uncert = shasta[Sample_Name == SampleNames[i] &
                                        PercentSnow == ShortSnowVector[j],
                                      get(uncertName)])]
    }
  }
}

# Populate CalcCompare with CREp results
# Add details to CalcCompare about how ages were calculated
CalcCompare[Calculator == "CREp", ':=' (
  ProdRate = "WorldWideMean",
  ScalingModel = "LSDn",
  Atmosphere = "ERA40",
  MagField = "Lifton2016")]
CREpResults <- fread("CREpResults.txt")
# CREp ages are relative to 2010 CE, convert to 1950 CE
CREpResults[, ':=' (Years_B1950 = as.integer((Age_ka * 1000) - 60),
                    IntErr = as.integer(IntErr * 1000L),
                    ExtErr = as.integer(ExtErr * 1000L))]
CREpResults[, Age_ka := NULL]
# populate CalcCompare
for (i in 1:length(SampleNames)) {
  for (j in 1:length(ShortSnowVector)) {
    for (k in 1:length(UncertTypeVector)) {
      # get name of uncertainty column
      if (UncertTypeVector[k] == "Int") {
        uncertName <- "IntErr"
      } else if (UncertTypeVector[k] == "Ext") {
        uncertName <- "ExtErr"
      } else {
        stop("UncertType is not 'Int' or 'Ext'", call. = FALSE)
      }
      CalcCompare[Calculator == "CREp" &
                    Sample_Name == SampleNames[i] &
                    PercentSnow == ShortSnowVector[j] &
                    UncertType == UncertTypeVector[k], ':=' (
                      Age = CREpResults[Sample_Name == SampleNames[i] &
                                          PercentSnow == ShortSnowVector[j],
                                        Years_B1950],
                      Uncert = CREpResults[Sample_Name == SampleNames[i] &
                                             PercentSnow == ShortSnowVector[j],
                                           get(uncertName)])]
    }
  }
}


# Calc group stats for Calc Comparison by location------------------------
# 1- Define sample location vectors
ULCvector <- c("C2-Boulder1", "C2-Boulder2", "UpLy-12-10")
LLCvector <- c("TM14-02", "TM14-03", "TM14-04", "TM14-05", "TM14-06",
               "LoLy-11-08")
PDVector <- c("TM14-11", "TM14-12", "TM14-13")
LDVector <- c("TM14-16", "TM14-17", "TM14-18")

# 2- Add Location info to CalcCompare
CalcCompare[Sample_Name %in% ULCvector, Location := "ULC"]
CalcCompare[Sample_Name %in% LLCvector, Location := "LLC"]
CalcCompare[Sample_Name == "TM14-01",   Location := "MLC"]
CalcCompare[Sample_Name == "TM14-07",   Location := "TM-LC"]
CalcCompare[Sample_Name %in% LDVector,  Location := "LD"]
CalcCompare[Sample_Name %in% PDVector,  Location := "PD"]
CalcCompare[Sample_Name == "TM14-15",   Location := "LM"]

# 3- Exclude outliers
CalcCompare[, Outlier := F]
samplesToExclude <- c("TM14-13")
CalcCompare[Sample_Name %in% samplesToExclude, Outlier := TRUE]

# 4- Calculate WMeans by Location
CalcCompare[Outlier == FALSE, 
            WMean := as.integer(round(weighted.mean(Age, (1 / Uncert ^ 2)))),
            by = .(PercentSnow, UncertType, Calculator, Location)]

# 5- Calculate MSWDs by Location
# count samples per group, SpG
CalcCompare[Outlier == FALSE, 
            SpG := sum(!is.na(Sample_Name)),
            by = .(PercentSnow, UncertType, Calculator, Location)]
CalcCompare[Outlier == FALSE & UncertType == "Int" & SpG >= 2,
            MSWD := sum((Age - WMean) ^ 2 * (1 / Uncert ^ 2) /
                          (SpG - 1)),
            by = .(PercentSnow, Calculator, Location)]
MSWDs <- CalcCompare[UncertType == "Int", MSWD]
CalcCompare[UncertType == "Ext", MSWD := MSWDs]

# 6- Calculate probability from single normal distribution
CalcCompare[Outlier == FALSE,
            P_MSWD := pchisq(MSWD * (SpG - 1), (SpG - 1),
                             lower.tail = FALSE)]

# 7- Calculate uncertainties on weighted mean ages
# Follows Douglass et al. 2006
CalcCompare[Outlier == FALSE,
            WMean_SD := as.integer(round(sqrt(1 / sum(1 / Uncert ^ 2)))),
            by = .(PercentSnow, UncertType, Calculator, Location)]

# 8- Expand uncertainties for MSWD > 1
CalcCompare[Outlier == FALSE & MSWD > 1,
            WMean_SD := as.integer(round(WMean_SD * sqrt(MSWD)))]

# 9- Find average WMean over both Int and Ext
CalcCompare[, WMean := as.integer(round(mean(WMean, na.rm = TRUE))),
            by = .(PercentSnow, Calculator, Location)]

# 10- Construct and populate a DT to host the results of the
#  weighted mean calculations, for later plotting.
# LocValue <- c(0, 4, 6, 13, 15, 23)
ShortLocVector <- c("ULC", "LLC", "LD", "PD")
WMeanBoxes <- data.table(expand.grid(
  PercentSnow = ShortSnowVector,
  UncertType = UncertTypeVector,
  Calculator = CalculatorVector,
  Location = ShortLocVector,
  xMin = as.integer(NA),
  xMax = as.integer(NA),
  yMin = as.integer(NA),
  yMax = as.integer(NA)))

for (h in 1:length(ShortSnowVector)) {
  for (i in 1:length(UncertTypeVector)) {
    for (j in 1:length(CalculatorVector)) {
      for (k in 1:length(ShortLocVector)) {
        WMeanBoxes[PercentSnow == ShortSnowVector[h] &
                     UncertType == UncertTypeVector[i] &
                     Calculator == CalculatorVector[j] &
                     Location == ShortLocVector[k], ':=' (
          xMin = CalcCompare[PercentSnow == ShortSnowVector[h] &
                               UncertType == UncertTypeVector[i] &
                               Calculator == CalculatorVector[j] &
                               Location == ShortLocVector[k],
                             min(WMean - WMean_SD, na.rm = TRUE)],
          xMax = CalcCompare[PercentSnow == ShortSnowVector[h] &
                               UncertType == UncertTypeVector[i] &
                               Calculator == CalculatorVector[j] &
                               Location == ShortLocVector[k],
                             max(WMean + WMean_SD, na.rm = TRUE)])]
      }
    }
  }
}

# Populate WMeanBoxes with ymin and ymax
WMeanBoxes[Location == "ULC", ':=' (
  yMin = 0L,
  yMax = 4L)]
WMeanBoxes[Location == "LLC", ':=' (
  yMin = 6L,
  yMax = 13L)]
WMeanBoxes[Location == "LD", ':=' (
  yMin = 15L,
  yMax = 19L)]
WMeanBoxes[Location == "PD", ':=' (
  yMin = 19L,
  yMax = 23L)]

# Add boulder height to CalcCompare
for (i in 1:length(SampleNames)) {
  CalcCompare[Sample_Name == SampleNames[i],
              Boulder_Height_m := 
                boulder_height[Sample_Name == SampleNames[i],
                               Boulder_Height_m]]
}

# Add Location Order to CalcCompare
LocOrder <- fread("LocationOrder.txt")
for (i in 1:length(SampleNames)) {
  CalcCompare[Sample_Name == SampleNames[i],
              LocationOrder := LocOrder[Sample_Name == SampleNames[i],
                                             LocationOrder]]
}

# Add elevation info
marrero <- fread("Marrero_simple_YESerosion_NOsnow_TMandLC.txt")
for (i in 1:length(SampleNames)) {
  CalcCompare[Sample_Name == SampleNames[i],
              Elevation_m := marrero[Sample_Name == SampleNames[i],
                                     Elevation_m]]
}

# Export CalcCompare
fwrite(CalcCompare, "Ch2CalcCompare.txt", sep = "\t")


# define theme for figures in publications-------------------------------
# define ggplot2 theme for use in publication
publicationTheme <- theme(
  panel.border = element_rect(fill = NA, color = "black", linetype = 1),
  panel.spacing.x = unit(4L, "mm"),
  panel.spacing.y = unit(0L, "mm"),
  strip.background = element_rect(fill = NA),
  strip.text = element_text(size = 7L),
  axis.ticks.x = element_line(color = "black", linetype = 1),
  axis.ticks.y = element_line(color = "black", linetype = 1),
  axis.text = element_text(color = "black", size = 7L),
  axis.title = element_text(size = 7L),
  legend.position = "none")


# MAKE Fig. 5 - Age vs Boulder Height ----------------------------------
Fig5 <- ggplot(CalcCompare[UncertType == "Int"]) +
  geom_errorbar(mapping = aes(x = Boulder_Height_m,
                              ymin = (Age - Uncert) / 1000,
                              ymax = (Age + Uncert) / 1000)) +
  geom_point(data = CalcCompare[Sample_Name != "TM14-13"],
             mapping = aes(x = Boulder_Height_m,
                           y = Age / 1000),
             shape = 21, color = "black", fill = "black") +
  geom_point(data = CalcCompare[Sample_Name == "TM14-13"],
             mapping = aes(x = Boulder_Height_m,
                           y = Age / 1000),
             shape = 21, color = "black", fill = "white") +
  geom_smooth(data = CalcCompare[Sample_Name != "TM14-13"],
              mapping = aes(x = Boulder_Height_m,
                            y = Age / 1000),
              method = "lm") +
  facet_grid(PercentSnow ~ Calculator) +
  scale_y_continuous(name = "Age (ka BP)",
                     limits = c(10, 25),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Boulder Height (m)",
                     breaks = seq(0, 3, 1),
                     limits = c(0, 3),
                     expand = c(0, 0)) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(color = "white"),
        panel.spacing = unit(0.5, "cm"),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(color = "black"))

# Export results
postscript(file = "Fig 5 - Results.eps",
           width = 6.5, height = 3.25, pointsize = 7L,
           horizontal = FALSE, family = "Helvetica",
           paper = "special")
multiplot(Fig5, cols = 1)
dev.off()

# Calculate regression stats for Figure 5 - Results
included <- CalcCompare[Outlier == FALSE, sum(!is.na(unique(Sample_Name)))]
CalcCompare[Sample_Name != "TM14-13",
            Pearson_r := cor(Boulder_Height_m, Age),
            by = .(PercentSnow, Calculator)]
CalcCompare[!is.na(Pearson_r), r_squared := Pearson_r ^ 2]
CalcCompare[!is.na(r_squared) & PercentSnow == 100,
            mean(r_squared)]
CalcCompare[!is.na(r_squared) & PercentSnow == 0,
            mean(Pearson_r)]
# calc prob stochastic can explain
CalcCompare[!is.na(Pearson_r), 
            Prob_Stochastic := pnorm(Pearson_r, 0, (1 / sqrt(included - 3)))]
# trim probs greater than 0.5
CalcCompare[Prob_Stochastic > 0.5, 
            Prob_Stochastic := 1 - Prob_Stochastic]
# double probs for two-tail probability
CalcCompare[!is.na(Prob_Stochastic), 
            Prob_Stochastic := Prob_Stochastic * 2]
# find average prob
CalcCompare[!is.na(Prob_Stochastic) & PercentSnow == 0,
            mean(Prob_Stochastic)]


# MAKE Fig. 6 - Ages by Location---------------------------------
# add background heatmap to reflect probability of deglaciation
# find minYear
minYear <- CalcCompare[, min(Age)]
minYear <- CalcCompare[UncertType == "Ext" & Age == minYear, Age - Uncert]
minYear2 <- round(minYear, -3)
if (minYear > minYear2) {
  minYear <- minYear2
} else {
  minYear <- minYear2 - 1000L
}
# find maxYear
maxYear <- CalcCompare[Sample_Name != "TM14-13", max(Age)]
maxYear <- CalcCompare[UncertType == "Ext" & Age == maxYear, Age + Uncert]
maxYear2 <- round(maxYear, -3)
if (maxYear < maxYear2) {
  maxYear <- maxYear2
} else {
  maxYear <- maxYear2 + 1000L
}
# make backgroundC14 - Timing and duration of Tioga 4 deglac from C-14
startYear <- 12000L
endYear <- 20000L
yearIncrement <- 25L
vectorOfYears <- seq.int(startYear, endYear, yearIncrement)
# create DT
Fig6Bckgrnd14C <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = 15750L,
  SDvalue = 500L,
  yAxis = seq(0, 26, 1),
  Calculator = CalculatorVector,
  UncertType = UncertTypeVector,
  Prob = as.numeric(NA)))
# calclate probabilities
Fig6Bckgrnd14C[UncertType == "Ext",
               Prob := dnorm(Year, meanValue, SDvalue)]
Fig6Bckgrnd14C[UncertType == "Int", Prob := 0]

# POSSIBILITY 1: facet_grid with Calculator and UncertType
Fig6 <- ggplot() +
  coord_cartesian(xlim = c(minYear / 1000, 20), ylim = c(0, 25),
                  expand = FALSE) +
  geom_raster(data = Fig6Bckgrnd14C,
              mapping = aes(x = Year / 1000, y = yAxis, fill = Prob),
              vjust = 0, hjust = 1, interpolate = TRUE) +
  facet_grid(Calculator ~ UncertType) +
  scale_fill_gradient(low = "#FFFFFF", high = "#FFEF00") +
  scale_x_continuous(name = "Age (ka BP)", trans = "reverse",
                     breaks = seq.int(20, 12),
                     limits = c(20, 12)) +
  scale_y_continuous(name = "Location within Field Area", trans = "reverse", 
                     breaks = c(0, 4, 6, 13, 15, 19, 23, 25),
                     labels = NULL) +
  geom_rect(data = WMeanBoxes[PercentSnow == 100],
            mapping = aes(xmin = xMin / 1000,
                          xmax = xMax / 1000,
                          ymin = yMin,
                          ymax = yMax),
            inherit.aes = FALSE,
            fill = "#cccccc") +
  facet_grid(Calculator ~ UncertType) +
  geom_errorbarh(data = CalcCompare[PercentSnow == 0 &
                                      (Sample_Name == "TM14-01" |
                                         Sample_Name == "TM14-07")],
                 mapping = aes(x = Age / 1000,
                               xmin = (Age - Uncert) / 1000,
                               xmax = (Age + Uncert) / 1000,
                               y = LocationOrder),
                 color = "#636363", height = 1) +
  geom_point(data = CalcCompare[PercentSnow == 0 &
                                  (Sample_Name == "TM14-01" |
                                     Sample_Name == "TM14-07")],
             mapping = aes(x = Age / 1000, y = LocationOrder),
             shape = 21, color = "#636363", fill = "#636363") +
  geom_errorbarh(data = CalcCompare[PercentSnow == 100],
                mapping = aes(x = Age / 1000,
                              xmin = (Age - Uncert) / 1000,
                              xmax = (Age + Uncert) / 1000,
                              y = LocationOrder),
                height = 1) +
  geom_point(data = CalcCompare[PercentSnow == 100 &
                                  Outlier == FALSE],
             mapping = aes(x = Age / 1000, y = LocationOrder),
             shape = 21, color = "black", fill = "black") +
  geom_point(data = CalcCompare[PercentSnow == 100 &
                                  Outlier == TRUE],
             mapping = aes(x = Age / 1000, y = LocationOrder),
             shape = 21, color = "black", fill = "white") +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black"),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#f0f0f0"),
        panel.grid.minor.y = element_blank(),
        panel.ontop = TRUE,
        strip.background = element_rect(fill = NA, color = NA))
# export Fig6
postscript(file = "Fig 6 - ResultsByLocation.eps",
           width = 7.4, height = 8.0, pointsize = 7L,
           horizontal = FALSE, family = "Helvetica",
           paper = "special")
multiplot(Fig6, cols = 1)
dev.off()


# Prep files for Ages vs Distance and Elevation Fig------------------
# add CirqueHeadwallDist
CalcCompare[Location == "ULC", CirqueHeadwallDist_km := 1.3]
CalcCompare[Location == "MLC", CirqueHeadwallDist_km := 4.0]
CalcCompare[Location == "LLC", CirqueHeadwallDist_km := 6.1]
CalcCompare[Location == "TM-LC", CirqueHeadwallDist_km := 14.5]
CalcCompare[Location == "LD", CirqueHeadwallDist_km := 18.4]
CalcCompare[Location == "PD", CirqueHeadwallDist_km := 22.4]
CalcCompare[Location == "LM", CirqueHeadwallDist_km := 23.5]

# calc avgLocElev
CalcCompare[, AvgLocElev := mean(Elevation_m), by = "Location"]

# Find first samples
FirstSamples <- CalcCompare[Outlier == FALSE, .SD[1, Sample_Name],
                            by = "Location"]
FirstSamples <- FirstSamples[, V1]

# create background for age vs. dist figure
Fig7DistBckgrnd <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = 15750L,
  SDvalue = 500L,
  xAxisDist = seq(0, 25, 1),
  Calculator = CalculatorVector,
  UncertType = UncertTypeVector,
  Prob = as.numeric(NA)))
# populate
Fig7DistBckgrnd[UncertType == "Ext", 
                Prob := dnorm(Year, meanValue, SDvalue)]
Fig7DistBckgrnd[UncertType == "Int", Prob := 0]

# create background for age vs. elevation figure
Fig7ElevBckgrnd <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = 15750L,
  SDvalue = 500L,
  xAxisElev = seq(2500, 3500, 100),
  Calculator = CalculatorVector,
  UncertType = UncertTypeVector,
  Prob = as.numeric(NA)))
# pop
Fig7ElevBckgrnd[UncertType == "Ext",
                Prob := dnorm(Year, meanValue, SDvalue)]
Fig7ElevBckgrnd[UncertType == "Int", Prob := 0]


# MAKE Fig. 7 - Ages by Distance and Elevation----------------------
Fig7A <- ggplot(CalcCompare[Calculator == "CRONUScalc" &
                              PercentSnow == 100 &
                              Sample_Name %in% FirstSamples]) +
  geom_raster(data = Fig7DistBckgrnd,
              mapping = aes(x = xAxisDist, y = Year / 1000, fill = Prob),
                            vjust = 0, hjust = 1, interpolate = TRUE) +
  scale_fill_gradient(low = "#FFFFFF", high = "#FFEF00") +
  geom_errorbar(mapping = aes(x = CirqueHeadwallDist_km,
                              ymin = (WMean - WMean_SD) / 1000,
                              ymax = (WMean + WMean_SD) / 1000),
                width = 0) +
  geom_point(data = CalcCompare[Calculator == "CRONUScalc" &
                                  PercentSnow == 100 &
                                  Sample_Name %in% FirstSamples &
                                  (Location == "MLC" |
                                     Location == "TM-LC")],
             mapping = aes(x = CirqueHeadwallDist_km,
                           y = WMean / 1000),
             shape = 21L, color = "black", fill = "white") +
  geom_point(data = CalcCompare[Calculator == "CRONUScalc" &
                                  PercentSnow == 100 &
                                  Sample_Name %in% FirstSamples &
                                  (Location != "MLC" &
                                     Location != "TM-LC")],
             mapping = aes(x = CirqueHeadwallDist_km,
                           y = WMean / 1000),
             shape = 21L, color = "black", fill = "black") +
  facet_grid(.~UncertType) +
  scale_x_continuous(name = "Distance from Cirque Headwall (km)",
                     limits = c(0, 25),
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Location Weighted Mean Age (ka)",
                     limits = c(13, 19),
                     expand = c(0, 0)) +
  publicationTheme

# plot of location weighted mean age vs elevation
Fig7B <- ggplot(CalcCompare[Calculator == "CRONUScalc" &
                              PercentSnow == 100 &
                              Sample_Name %in% FirstSamples]) +
  geom_raster(data = Fig7ElevBckgrnd,
              mapping = aes(x = xAxisElev, y = Year / 1000, fill = Prob),
              vjust = 0, hjust = 1, interpolate = TRUE) +
  scale_fill_gradient(low = "#FFFFFF", high = "#FFEF00") +
  geom_errorbar(mapping = aes(x = AvgLocElev,
                              ymin = (WMean - WMean_SD) / 1000,
                              ymax = (WMean + WMean_SD) / 1000),
                width = 0) +
  geom_point(data = CalcCompare[Calculator == "CRONUScalc" &
                                  PercentSnow == 100 &
                                  Sample_Name %in% FirstSamples &
                                  (Location == "MLC" |
                                     Location == "TM-LC")],
             mapping = aes(x = AvgLocElev,
                           y = WMean / 1000),
             shape = 21L, color = "black", fill = "white") +
  geom_point(data = CalcCompare[Calculator == "CRONUScalc" &
                                  PercentSnow == 100 &
                                  Sample_Name %in% FirstSamples &
                                  (Location != "MLC" &
                                     Location != "TM-LC")],
             mapping = aes(x = AvgLocElev,
                           y = WMean / 1000),
             shape = 21L, color = "black", fill = "black") +
  facet_grid(.~UncertType) +
  scale_x_continuous(name = "Average Location Elevation (m)",
                     limits = c(2500, 3500),
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Location Weighted Mean Age (ka)",
                     limits = c(13, 19),
                     expand = c(0, 0)) +
  publicationTheme
# export plots
ggsave("Fig 2-7 WMeans vs Distance and Elevation.eps",
       ggarrange(Fig7A, Fig7B, ncol = 1),
       width = unit(6, "in"),
       height = unit(8.3 * (2.25/4), "in"))


# Monte Carlo model for rate of ice-thickness loss----------------------
sims <- 100000L
IceRate <- data.table(TLC = rnorm(sims,
                                  CalcCompare[PercentSnow == 100 &
                                                UncertType == "Int" &
                                                Calculator == "CRONUScalc" &
                                                Location == "ULC",
                                              unique(WMean)],
                                  CalcCompare[PercentSnow == 100 &
                                                UncertType == "Int" &
                                                Calculator == "CRONUScalc" &
                                                Location == "ULC",
                                              unique(WMean_SD)]),
                      MLCsnow = rnorm(sims,
                                  CalcCompare[PercentSnow == 100 &
                                                UncertType == "Int" &
                                                Calculator == "CRONUScalc" &
                                                Sample_Name == "TM14-01",
                                              Age],
                                  CalcCompare[PercentSnow == 100 &
                                                UncertType == "Int" &
                                                Calculator == "CRONUScalc" &
                                                Sample_Name == "TM14-01",
                                              Uncert]),
                      MLCnoSnow = rnorm(sims,
                                      CalcCompare[PercentSnow == 0 &
                                                    UncertType == "Int" &
                                                  Calculator == "CRONUScalc" &
                                                    Sample_Name == "TM14-01",
                                                  Age],
                                      CalcCompare[PercentSnow == 0 &
                                                    UncertType == "Int" &
                                                  Calculator == "CRONUScalc" &
                                                    Sample_Name == "TM14-01",
                                                  Uncert]),
                      BLC = rnorm(sims,
                                  CalcCompare[PercentSnow == 100 &
                                                UncertType == "Int" &
                                                Calculator == "CRONUScalc" &
                                                Location == "LLC",
                                              unique(WMean)],
                                  CalcCompare[PercentSnow == 100 &
                                                UncertType == "Int" &
                                                Calculator == "CRONUScalc" &
                                                Location == "LLC",
                                              unique(WMean_SD)]),
                      TMLCsnow = rnorm(sims,
                                   CalcCompare[PercentSnow == 100 &
                                                 UncertType == "Int" &
                                                 Calculator == "CRONUScalc" &
                                                 Sample_Name == "TM14-07",
                                               Age],
                                   CalcCompare[PercentSnow == 100 &
                                                 UncertType == "Int" &
                                                 Calculator == "CRONUScalc" &
                                                 Sample_Name == "TM14-07",
                                               Uncert]),
                      TMLCnoSnow = rnorm(sims,
                                   CalcCompare[PercentSnow == 0 &
                                                 UncertType == "Int" &
                                                 Calculator == "CRONUScalc" &
                                                 Sample_Name == "TM14-07",
                                               Age],
                                   CalcCompare[PercentSnow == 0 &
                                                 UncertType == "Int" &
                                                 Calculator == "CRONUScalc" &
                                                 Sample_Name == "TM14-07",
                                               Uncert]),
                      LD = rnorm(sims,
                                 CalcCompare[PercentSnow == 100 &
                                               UncertType == "Int" &
                                               Calculator == "CRONUScalc" &
                                               Location == "LD",
                                             unique(WMean)],
                                 CalcCompare[PercentSnow == 100 &
                                               UncertType == "Int" &
                                               Calculator == "CRONUScalc" &
                                               Location == "LD",
                                             max(WMean_SD, na.rm = TRUE)]),
                      PD = rnorm(sims,
                                 CalcCompare[PercentSnow == 100 &
                                               UncertType == "Int" &
                                               Calculator == "CRONUScalc" &
                                               Location == "PD",
                                             unique(WMean)],
                                 CalcCompare[PercentSnow == 100 &
                                               UncertType == "Int" &
                                               Calculator == "CRONUScalc" &
                                               Location == "PD",
                                             max(WMean_SD, na.rm = TRUE)]),
                      LM = rnorm(sims,
                                 CalcCompare[PercentSnow == 100 &
                                               UncertType == "Int" &
                                               Calculator == "CRONUScalc" &
                                               Sample_Name == "TM14-15",
                                             Age],
                                 CalcCompare[PercentSnow == 100 &
                                               UncertType == "Int" &
                                               Calculator == "CRONUScalc" &
                                               Sample_Name == "TM14-15",
                                             Uncert]))

# calculate thinning rates...
IceRate[, LCDeglacDuration := TLC - BLC]
ggplot(IceRate, aes(x = LCDeglacDuration)) +
  geom_histogram()
IceRate[, mean(LCDeglacDuration)]
IceRate[, sd(LCDeglacDuration)]
IceRate[, LCRate_mYr := (3407L - 2741L) / LCDeglacDuration]
IceRate[, FullDuration := LCDeglacDuration * (766 / 666)]

# average ice-thinning rate and duration
IceRate[, quantile(LCRate_mYr, c(0.159, 0.5, 0.841))]
IceRate[, quantile(LCDeglacDuration, c(0.159, 0.5, 0.841))]
IceRate[LCDeglacDuration > 0, quantile(LCRate_mYr, c(0.159, 0.5, 0.841))]
IceRate[LCDeglacDuration > 0, 
        quantile(LCDeglacDuration, c(0.159, 0.5, 0.841))]

# calc true duration of Tioga 4 deglaciation
IceRate[FullDuration > 0, quantile(FullDuration, c(0.159, 0.5, 0.841))]

# calc timing of Tioga 4 deglaciation onset in Lyell Canyon
IceRate[, Tioga4Start := BLC + FullDuration]
IceRate[FullDuration > 0, quantile(Tioga4Start, c(0.159, 0.5, 0.841))]
# calc timing with external uncertainties
IceRate[, BLCext := rnorm(sims, 
                          CalcCompare[PercentSnow == 100 &
                                              UncertType == "Ext" &
                                              Calculator == "CRONUScalc" &
                                              Location == "LLC",
                                            unique(WMean)],
                          CalcCompare[PercentSnow == 100 &
                                        UncertType == "Ext" &
                                        Calculator == "CRONUScalc" &
                                        Location == "LLC",
                                      unique(WMean_SD)])]
IceRate[, Tioga4StartExt := BLCext + FullDuration]
IceRate[FullDuration > 0, quantile(Tioga4StartExt, c(0.159, 0.5, 0.841))]

# probability that TM14-01 with snow is older than TLC samples with snow
IceRate[MLCsnow > TLC, .N / sims]
# probability that TM14-01 without snow is younger than BLC samples with snow
IceRate[MLCnoSnow < BLC, .N / sims]


# MAKE Fig. 8 - LC Ice-Thinning Rate-----------------------
# TLC - BLC age diff
Fig8A <- ggplot(IceRate, aes(x = LCDeglacDuration)) +
  geom_histogram(binwidth = 50, boundary = 0, fill = "#bdbdbd") +
  scale_x_continuous(
    name = "Age Difference between the TLC and BLC samples (yrs)",
    limits = c(-1000, 2000), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 8000),
                     expand = c(0, 0), labels = NULL) +
  geom_histogram(data = IceRate[FullDuration > 0],
                 mapping = aes(x = LCDeglacDuration),
                 binwidth = 50, boundary = 0, fill = "#636363") +
  geom_vline(xintercept = IceRate[FullDuration > 0,
                                  quantile(LCDeglacDuration, 
                                           c(0.159, 0.5, 0.841))]) +
  geom_vline(xintercept = 0) +
  publicationTheme

# Avg. Ice-Thinning Rate
Fig8B <- ggplot(IceRate[FullDuration > 0], aes(x = LCRate_mYr)) +
  geom_histogram(binwidth = 0.05, boundary = 0, fill = "#636363") +
  scale_x_continuous(name = "Average Ice-Thinning Rate (m/yr)",
                     limits = c(0, 5), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 5000),
                     labels = NULL) +
  geom_vline(xintercept = 
               IceRate[FullDuration > 0,
                       quantile(LCRate_mYr, c(0.159, 0.5, 0.841))]) +
  publicationTheme
  
# Estimated duration of Tioga 4 deglaciation in Lyell Canyon
Fig8C <- ggplot(IceRate[FullDuration > 0], aes(x = FullDuration)) +
  geom_histogram(binwidth = 50, boundary = 0, fill = "#636363") +
  scale_x_continuous(
   name = "Calculated Duration of the Tioga 4 Deglaciation (yrs)",
   limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 6000),
                     expand = c(0, 0), labels = NULL) +
  geom_vline(xintercept = IceRate[FullDuration > 0,
                                  quantile(FullDuration,
                                           c(0.159, 0.5, 0.841))]) +
  publicationTheme
    
# export figure
postscript(file = "Fig 2-8 - LC Ice-Thinning Rate.eps",
           width = 3.5, height = 8.3 * (3/4), pointsize = 7L,
           horizontal = FALSE, family = "Helvetica",
           paper = "special")
multiplot(Fig8A, Fig8B, Fig8C, cols = 1)
dev.off()


# MAKE Fig. 9 - WMean Ages vs HaboveTM---------------------------
# add height above TM to CalcCompare
CalcCompare[Location == "LD", 
            HaboveTM_m := Elevation_m - 2627]
CalcCompare[Location == "PD",
            HaboveTM_m := Elevation_m - 2608]
CalcCompare[Location == "LM",
            HaboveTM_m := Elevation_m - 2566]
# avg height above TM
CalcCompare[Location == "LD", 
            AvgHaboveTM_m := AvgLocElev - 2627]
CalcCompare[Location == "PD",
            AvgHaboveTM_m := AvgLocElev - 2608]
CalcCompare[Location == "LM",
            AvgHaboveTM_m := AvgLocElev - 2566]

# plot ages vs height above TM
Fig9A <- ggplot(CalcCompare[PercentSnow == 100 & UncertType == "Int" &
                     Calculator == "CRONUScalc" & Outlier == FALSE &
                     Location %in% TMLocationsVector],
               aes(x = HaboveTM_m, y = Age / 1000)) +
  geom_point() +
  geom_errorbar(mapping = aes(x = HaboveTM_m,
                              ymin = (Age - Uncert) / 1000,
                              ymax = (Age + Uncert) / 1000),
                width = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(
    name = "Height Above Tuolumne Meadows (m)",
    limits = c(0, 250), expand = c(0, 0)) +
  scale_y_continuous(
    name = "Individual Exposure Age (ka)",
    limits = c(14, 17), expand = c(0, 0)) +
  publicationTheme

# plot weighted mean ages
Fig9B <- ggplot(CalcCompare[PercentSnow == 100 & UncertType == "Int" &
                              Calculator == "CRONUScalc" & Outlier == FALSE &
                              Location %in% TMLocationsVector &
                              Sample_Name %in% FirstSamples],
                aes(x = AvgHaboveTM_m, y = WMean / 1000)) +
  geom_point() +
  geom_errorbar(mapping = aes(x = AvgHaboveTM_m,
                              ymin = (WMean - WMean_SD) / 1000,
                              ymax = (WMean + WMean_SD) / 1000),
                width = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(
    name = "Average Height Above Tuolumne Meadows (m)",
    limits = c(0, 250), expand = c(0, 0)) +
  scale_y_continuous(
    name = "Weighted-Mean Location Age (ka)",
    limits = c(14, 17), expand = c(0, 0)) +
  publicationTheme

# export plots
ggsave("Fig 2-9 WMeans vs H above TM.eps",
       ggarrange(Fig9A, Fig9B, ncol = 1),
       width = unit(3, "in"),
       height = unit(8.3 * (1.9/4), "in"))



# Ice-margin retreat from TM to BLC-----------------------
# calc duration
IceRate[, ':=' (
  LDtoBLCdur_yr = LD - BLC,
  PDtoBLCdur_yr = PD - BLC,
  LMtoBLCdur_yr = LM - BLC,
  TMLCtoBLCdur_yr = TMLCsnow - BLC)]

# pull new columns into tidy data
IceRateTidy <- as.data.table(gather(
  IceRate, "Location", "Duration", LDtoBLCdur_yr:TMLCtoBLCdur_yr))

# clean location column
IceRateTidy[Location == "LDtoBLCdur_yr", Location := "LD"]
IceRateTidy[Location == "PDtoBLCdur_yr", Location := "PD"]
IceRateTidy[Location == "LMtoBLCdur_yr", Location := "LM"]
IceRateTidy[Location == "TMLCtoBLCdur_yr", Location := "TM-LC"]

# calc rates
IceRateTidy[Location == "LD",
            Rate_myr := (18.4 - 6.1) * 1000 / Duration]
IceRateTidy[Location == "PD", 
            Rate_myr := (22.4 - 6.1) * 1000 / Duration]
IceRateTidy[Location == "LM", 
            Rate_myr := (23.5 - 6.1) * 1000 / Duration]
IceRateTidy[Location == "TM-LC",
            Rate_myr := (14.5 - 6.1) * 1000 / Duration]

# set Location to factor
LocLevels <- IceRateTidy[, as.character(unique(Location))]
IceRateTidy[, Location := factor(Location,
                                 levels = LocLevels)]

# construct DT of quantiles
IceRatePercentiles <- as.data.table(expand.grid(
  Location = LocLevels[1:3],
  Variable = c("Duration", "Rate"),
  P159 = as.numeric(NA),
  P500 = as.numeric(NA),
  P841 = as.numeric(NA)))
for (i in 1:3) {
  IceRatePercentiles[Location == LocLevels[i] & 
                       Variable == "Duration", ':=' (
    P159 = IceRateTidy[Location == LocLevels[i] & Duration > 0,
                       quantile(Duration, 0.159)],
    P500 = IceRateTidy[Location == LocLevels[i] & Duration > 0,
                       quantile(Duration, 0.5)],
    P841 = IceRateTidy[Location == LocLevels[i] & Duration > 0,
                       quantile(Duration, 0.841)])]
  IceRatePercentiles[Location == LocLevels[i] &
                       Variable == "Rate", ':=' (
    P159 = IceRateTidy[Location == LocLevels[i] & Rate_myr > 0,
                       quantile(Rate_myr, 0.159)],
    P500 = IceRateTidy[Location == LocLevels[i] & Rate_myr > 0,
                       quantile(Rate_myr, 0.5)],
    P841 = IceRateTidy[Location == LocLevels[i] & Rate_myr > 0,
                       quantile(Rate_myr, 0.841)])]
}


# MAKE Fig. 10 - Duration and Rate of Retreat from TM to LC-----------
# left side, durations
Fig10L <- ggplot() +
  geom_histogram(data = IceRateTidy[Duration < 0 & Location != "TM-LC"],
                 mapping = aes(x = Duration),
                 fill = "#bdbdbd",
                 boundary = 0,
                 binwidth = 50L) +
  geom_histogram(data = IceRateTidy[Duration > 0 & Location != "TM-LC"],
                 mapping = aes(x = Duration),
                 fill = "#636363",
                 binwidth = 50L,
                 boundary = 0) +
  geom_vline(xintercept = 0) +
  geom_vline(data = IceRatePercentiles[Variable == "Duration"],
             mapping = aes(xintercept = P159)) +
  geom_vline(data = IceRatePercentiles[Variable == "Duration"],
             mapping = aes(xintercept = P500)) +
  geom_vline(data = IceRatePercentiles[Variable == "Duration"],
             mapping = aes(xintercept = P841)) +
  facet_grid(Location ~ .) +
  scale_x_continuous(
    name = "Age Difference, Locations Above minus BLC Age (yr)",
    limits = c(-1500, 2500), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 6000),
                     labels = NULL, expand = c(0, 0)) +
  publicationTheme +
  theme(panel.spacing.y = unit(3, "mm"))
# right side, rates
Fig10R <- ggplot(IceRateTidy[Rate_myr > 0 & Location != "TM-LC"]) +
  geom_histogram(aes(x = Rate_myr),
                 fill = "#636363",
                 boundary = 0,
                 binwidth = 1) +
  geom_vline(data = IceRatePercentiles[Variable == "Rate"],
             mapping = aes(xintercept = P159)) +
  geom_vline(data = IceRatePercentiles[Variable == "Rate"],
             mapping = aes(xintercept = P500)) +
  geom_vline(data = IceRatePercentiles[Variable == "Rate"],
             mapping = aes(xintercept = P841)) +
  facet_grid(Location ~ .) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0),
                     minor_breaks = seq(0, 100, 5),
                     name = "Average Ice-Margin Retreat Rate (m/yr)") +
  scale_y_continuous(limits = c(0, 6000), expand = c(0, 0),
                     name = NULL, labels = NULL) +
  publicationTheme +
  theme(panel.spacing.y = unit(3, "mm"))
# export plots
ggsave("Fig 2-8 TM Retreat Rate.eps",
       ggarrange(Fig10L, Fig10R, ncol = 2),
       width = unit(6, "in"),
       height = unit(5, "in"))
  

# create DT for camel plot of CRONUS dates----------------------------
CronusCamel <- data.table(expand.grid(
  Sample_Name = SampleNames,
  Year = seq.int(10000, 30000, 25),
  nkde = as.numeric(NA)))

# populate CronusCamel
for (i in 1:length(SampleNames)) {
  uncert <- CalcCompare[PercentSnow == 100 & UncertType == "Int" &
                          Calculator == "CRONUScalc" &
                          Sample_Name == SampleNames[i], Uncert]
  age <- CalcCompare[PercentSnow == 100 & UncertType == "Int" &
                       Calculator == "CRONUScalc" &
                       Sample_Name == SampleNames[i], Age]
  CronusCamel[Sample_Name == SampleNames[i],
              nkde := (1 / sqrt(2 * pi *  uncert ^ 2)) *
                exp((-(Year - age) ^ 2) / (2 * uncert ^ 2))]
}
# add true/false for TM
TMsamples <- c("TM14-11", "TM14-12", "TM14-13", "TM14-15", "TM14-16",
               "TM14-17", "TM14-18")
CronusCamel[Sample_Name %in% TMsamples, TMsample := TRUE]
CronusCamel[is.na(TMsample), TMsample := FALSE]
# add outlier identification
rejects <- c("TM14-07", "TM14-13", "TM14-16")
CronusCamel[Sample_Name %in% rejects, Outlier := TRUE]
CronusCamel[is.na(Outlier), Outlier := FALSE]
# add column for allTMnkde
CronusCamel[TMsample == TRUE, allTMnkde := sum(nkde),
            by = "Year"]
# add column for noTMrejects
CronusCamel[TMsample == TRUE & Outlier == FALSE,
            noTMrejects := sum(nkde),
            by = "Year"]
# add column for TLC and TM samples
StartLocations <- c("ULC", "LD", "PD", "LM")
TLCandTMsamples <- CalcCompare[Location %in% StartLocations,
                               unique(Sample_Name)]
CronusCamel[Sample_Name %in% TLCandTMsamples,
            TLCorTMsample := TRUE]
CronusCamel[is.na(TLCorTMsample), TLCorTMsample := FALSE]
CronusCamel[TLCorTMsample == TRUE,
            allTLCandTMnkde := sum(nkde),
            by = "Year"]
CronusCamel[TLCorTMsample == TRUE & Outlier == FALSE,
            noTLCorTMrejects := sum(nkde),
            by = "Year"]


# Load and calc WMean ages for Bishop Creek Tioga 4 dates----------
# load file
BishCreekAges <- fread("BishCreekCl36Ages.txt")

# create Outlier column
BishCreekAges[, Outlier := FALSE]

# gather IntErr and ExtErr into single column
BishCreekAges <- as.data.table(gather(BishCreekAges, "UncertType",
                                      "Uncert", c(IntErr, ExtErr)))
# shorten UncertType names
BishCreekAges[UncertType == "IntErr", UncertType := "Int"]
BishCreekAges[UncertType == "ExtErr", UncertType := "Ext"]

# reorder columns
oldOrder <- names(BishCreekAges)
newOrder <- c(oldOrder[1:8], oldOrder[11], oldOrder[9], oldOrder[12], 
              oldOrder[10])
setcolorder(BishCreekAges, newOrder)

# calc weighted mean group stats,
#  if p < 5%, calc arith mean and reject largest absolute deviator
#  repeat calcs

# calc WMeans
BishCreekAges[Outlier == FALSE,
              WMean := 
                as.integer(round(weighted.mean(Age_B1950, (1 / Uncert ^ 2)))),
              by = .(GroupNum, UncertType, SnowModel)]
BishCreekAges[Outlier == FALSE,
              WMean := as.integer(round(mean(WMean))),
              by = .(Sample_Name, SnowModel)]
# add col for WMeanSD
BishCreekAges[, WMeanSD := as.integer(NA)]
# add col for original samples per group
BishCreekAges[Outlier == FALSE,
              OSpG := sum(!is.na(unique(Sample_Name))),
              by = .(GroupNum, SnowModel)]
# add col for retained samples per group
BishCreekAges[Outlier == FALSE,
              SpG := OSpG,
              by = .(GroupNum, SnowModel)]
# calc MSWD
BishCreekAges[Outlier == FALSE & UncertType == "Int",
              MSWD := sum((Age_B1950 - WMean) ^ 2 * (1 / Uncert ^ 2) /
                            (SpG - 1)),
              by = .(GroupNum, SnowModel)]
MSWDs <- BishCreekAges[Outlier == FALSE & !is.na(MSWD),
                       MSWD]
BishCreekAges[Outlier == FALSE & is.na(MSWD),
              MSWD := MSWDs]
# calc prob single normal
BishCreekAges[Outlier == FALSE,
              P_MSWD := pchisq(MSWD * (SpG - 1L),
                               SpG - 1L,
                               lower.tail = FALSE)]
# calc WMeanSD
BishCreekAges[Outlier == FALSE,
              WMeanSD := as.integer(round(sqrt(1 / sum(1 / Uncert ^ 2)))),
              by = .(UncertType, GroupNum, SnowModel)]
# expand WMeanSD for MSWD > 1
BishCreekAges[Outlier == FALSE & MSWD > 1,
              WMeanSD := as.integer(round(sqrt(MSWD) * WMeanSD))]

# as long as there are groups with p < 5% and not more than
#  half the dataset is reject...
#  1- calc arith mean for each group
#  2- calc abs(deviates from arith mean for each sample)
#  3- reject most extreme deviator
#  4- recalc group stats
# Run auto-reject code...
maxCounter <- 1L
while(maxCounter < 20 &
      BishCreekAges[P_MSWD < 0.05 & ((SpG - 1) / OSpG) > 0.5, .N] > 0) {
  # 1- ID and reject most extreme outlier...
  # calc arith mean for deviant groups
  BishCreekAges[P_MSWD < 0.05 & 
                  ((SpG - 1) / OSpG) > 0.5 &
                  Outlier == FALSE,
                ArithMean := as.integer(round(mean(Age_B1950))),
                by = .(GroupNum, SnowModel)]
  # calcs abs(deviation)
  BishCreekAges[!is.na(ArithMean), AbsDev := abs(Age_B1950 - ArithMean)]
  # find most extreme samples
  ExtVals <- BishCreekAges[!is.na(AbsDev), max(AbsDev),
                           by = .(GroupNum, SnowModel)]
  # set these samples to Outlier
  for(i in 1:ExtVals[, .N]) {
    BishCreekAges[GroupNum == ExtVals[i, GroupNum] &
                    SnowModel == ExtVals[i, SnowModel] &
                    AbsDev == ExtVals[i, V1],
                  ':=' (Outlier = TRUE,
                        WMean = as.integer(NA),
                        SpG = as.integer(NA),
                        MSWD = as.numeric(NA),
                        P_MSWD = as.numeric(NA),
                        WMeanSD = as.integer(NA),
                        ArithMean = as.integer(NA),
                        AbsDev = as.integer(NA))]
  }
  
  # 2 - recalc group stats...
  # calc WMean
  BishCreekAges[!is.na(AbsDev),
                WMean := 
                  as.integer(round(
                    weighted.mean(Age_B1950, (1 / Uncert ^ 2)))),
                by = .(GroupNum, UncertType, SnowModel)]
  BishCreekAges[!is.na(AbsDev),
                WMean := as.integer(round(mean(WMean))),
                by = .(Sample_Name, SnowModel)]
  # update SpG
  BishCreekAges[!is.na(AbsDev),
                SpG := sum(!is.na(unique(Sample_Name))),
                by = .(GroupNum, SnowModel)]
  # calc MSWD
  BishCreekAges[!is.na(AbsDev) & UncertType == "Int",
                MSWD := sum((Age_B1950 - WMean) ^ 2 * (1 / Uncert ^ 2) /
                              (SpG - 1)),
                by = .(GroupNum, SnowModel)]
  MSWDs <- BishCreekAges[!is.na(AbsDev) & UncertType == "Int",
                         MSWD]
  BishCreekAges[!is.na(AbsDev) & UncertType == "Ext",
                MSWD := MSWDs]
  # calc prob single normal
  BishCreekAges[!is.na(AbsDev),
                P_MSWD := pchisq(MSWD * (SpG - 1L),
                                 SpG - 1L,
                                 lower.tail = FALSE)]
  # calc WMeanSD
  BishCreekAges[!is.na(AbsDev),
                WMeanSD := as.integer(round(sqrt(1 / sum(1 / Uncert ^ 2)))),
                by = .(UncertType, GroupNum, SnowModel)]
  # expand WMeanSD for MSWD > 1
  BishCreekAges[!is.na(AbsDev) & MSWD > 1,
                WMeanSD := as.integer(round(sqrt(MSWD) * WMeanSD))]
  
  # 3- DELETE outlier selection columns
  BishCreekAges[, ':=' (ArithMean = NULL,
                        AbsDev = NULL)]
  
  # advance maxCounter
  maxCounter <- maxCounter + 1
}
fwrite(BishCreekAges, "BishCreekAges--AutoOutlierRejection.txt", sep = "\t")

# add info for later plotting use...
# All the recalculated Cl-36 ages from Bishop Creek
Cl36SampleNames <- BishCreekAges[, unique(Sample_Name)]
# add plotting number for moraine samples
for (i in 1:BishCreekAges[GroupNum == 1,
                          length(unique(Sample_Name))]) {
  BishCreekAges[Sample_Name == Cl36SampleNames[i],
                PlotNum := i]
}
# add plotting number for valley samples
for (i in 7:BishCreekAges[GroupNum == 2,
                          length(unique(Sample_Name)) + 6]) {
  BishCreekAges[Sample_Name == Cl36SampleNames[i - 1L],
                PlotNum := i]
}
# add plotting number for Humphrey Basin samples
for (i in 24:BishCreekAges[GroupNum == 3,
                           length(unique(Sample_Name)) + 23]) {
  BishCreekAges[Sample_Name == Cl36SampleNames[i - 2L],
                PlotNum := i]
}
# shift all PlotNum 0.5 to the left (minus)
BishCreekAges[, PlotNum := PlotNum - 0.5]

# order samples by relative age (increasing) within each group
BishCreekAges[, RelAgeOrder := as.numeric(NA)]
baseValue <- 0L
FirstCl36Samples <- as.character(rep(NA, 3))
for (i in 1:3) {
  # get list of sample names in this GroupNum
  GroupNumSamples <- BishCreekAges[GroupNum == i, unique(Sample_Name)]
  FirstCl36Samples[i] <- GroupNumSamples[1]
  # add xmin value for this group of samples
  BishCreekAges[Sample_Name %in% GroupNumSamples,
                xmin := baseValue]
  for (j in 1:length(GroupNumSamples)) {
    # find name of youngest sample in group
    youngestAge <- BishCreekAges[GroupNum == i & is.na(RelAgeOrder),
                                    min(Age_B1950)]
    youngestName <- BishCreekAges[GroupNum == i & is.na(RelAgeOrder) &
                                    Age_B1950 == youngestAge,
                                  unique(Sample_Name)]
    # set that sample's RelAgeOrder value
    BishCreekAges[Sample_Name == youngestName[1],
                  RelAgeOrder := j + baseValue]
  }
  baseValue <- BishCreekAges[!is.na(RelAgeOrder), 
                             max(RelAgeOrder)]
  # set xmax
  BishCreekAges[Sample_Name %in% GroupNumSamples,
                xmax := baseValue]
}
# subtract 0.5 from all RelAgeOrder so samples plot in middle of number
BishCreekAges[, RelAgeOrder := RelAgeOrder - 0.5]


# Monte Carlo model of Bishop Creek Ages-------------------------------  
BishCreekMC <- data.table(
  trial = seq.int(1, sims),
  moraine = rnorm(sims,
                  BishCreekAges[GroupNum == 1L &
                                  SnowModel == "Phillips", 
                                max(WMean, na.rm = TRUE)],
                  BishCreekAges[GroupNum == 1L & 
                                  UncertType == "Int" &
                                  SnowModel == "Phillips",
                                max(WMeanSD, na.rm = TRUE)]),
  valley = rnorm(sims,
                 BishCreekAges[GroupNum == 2L &
                                 SnowModel == "Phillips", 
                               max(WMean, na.rm = TRUE)],
                 BishCreekAges[GroupNum == 2L & 
                                 UncertType == "Int" &
                                 SnowModel == "Phillips",
                               max(WMeanSD, na.rm = TRUE)]),
  basin = rnorm(sims,
                BishCreekAges[GroupNum == 3L &
                                SnowModel == "Phillips",
                              max(WMean, na.rm = TRUE)],
                BishCreekAges[GroupNum == 3L & 
                                UncertType == "Int" &
                                SnowModel == "Phillips",
                              max(WMeanSD, na.rm = TRUE)]))

# identify valid simulations (moraine oldest)
BishCreekMC[moraine > valley & moraine > basin, Valid := TRUE]
BishCreekMC[is.na(Valid), Valid := FALSE]

# identify duration of deglaciation for each trial
BishCreekMC[, Duration := moraine - min(.SD), by = "trial", 
            .SDcols = c("valley", "basin")]
ggplot(BishCreekMC, aes(x = Duration)) + geom_histogram()
BishCreekMC[Valid == TRUE, quantile(Duration, c(0.159, 0.5, 0.841))]

# What if we only require the valley bottom to be youngest?
BishCreekMC[valley < moraine, ValleyYounger := T]
BishCreekMC[is.na(ValleyYounger), ValleyYounger := F]
BishCreekMC[ValleyYounger == T, .N / sims]
BishCreekMC[, AltDuration := max(.SD) - valley, by = "trial",
            .SDcols = c("moraine", "basin")]
BishCreekMC[ValleyYounger == T, 
            quantile(AltDuration, c(0.159, 0.5, 0.841))]


# MAKE Fig. 11 - Cl-36 data from Bishop Creek------------------------
# create nkde DT for Tioga4moraine
Tioga4moraineSamples <- BishCreekAges[Group == "moraine",
                                      unique(Sample_Name)]
BishCrkNKDE <- data.table(expand.grid(
  Sample_Name = c(Tioga4moraineSamples, "WMean"),
  Year = seq.int(10000, 30000, 25),
  nkde = as.numeric(NA)))

# populate BishCrkNKDE
for (i in 1:length(Tioga4moraineSamples)) {
  uncert <- BishCreekAges[Sample_Name == Tioga4moraineSamples[i] &
                            UncertType == "Ext" &
                            SnowModel == "Phillips", 
                          Uncert]
  age <- BishCreekAges[Sample_Name == Tioga4moraineSamples[i] &
                         UncertType == "Ext" &
                         SnowModel == "Phillips",
                       Age_B1950]
  BishCrkNKDE[Sample_Name == Tioga4moraineSamples[i],
              nkde := (1 / sqrt(2 * pi *  uncert ^ 2)) *
                exp((-(Year - age) ^ 2) / (2 * uncert ^ 2))]
}

# add summary column
BishCrkNKDE[, nkdeSum := sum(nkde, na.rm = T), by = "Year"]

# pop with nkde for WMean
uncert <- BishCreekAges[Sample_Name == "BpCR97-8" &
                          SnowModel == "Phillips" &
                          UncertType == "Ext",
                        WMeanSD]
age <- BishCreekAges[Sample_Name == "BpCR97-8" &
                       SnowModel == "Phillips" &
                       UncertType == "Ext",
                     WMean]
BishCrkNKDE[Sample_Name == "WMean",
            nkde := (1 / sqrt(2 * pi *  uncert ^ 2)) *
              exp((-(Year - age) ^ 2) / (2 * uncert ^ 2))]

# create figure showing weighted mean age for Tioga 4 moraine
# make plot of individual Cl-36 ages by group
Fig11A <- ggplot() +
  geom_rect(data = BishCreekAges[Sample_Name %in% FirstCl36Samples &
                                   UncertType == "Int" &
                                   SnowModel == "Phillips"],
            mapping = aes(xmin = xmin, xmax = xmax,
                          ymin = (WMean - WMeanSD) / 1000,
                          ymax = (WMean + WMeanSD) / 1000),
            color = "#bdbdbd", fill = "#bdbdbd") +
  geom_errorbar(data = BishCreekAges[UncertType == "Int" &
                                       SnowModel == "Phillips"],
                mapping = aes(x = RelAgeOrder,
                              ymin = (Age_B1950 - Uncert) / 1000,
                              ymax = (Age_B1950 + Uncert) / 1000),
                width = 0.0) +
  geom_point(data = BishCreekAges[UncertType == "Int" &
                                    Sample_Type == "Boulder" &
                                    Outlier == FALSE &
                                    SnowModel == "Phillips"],
             mapping = aes(x = RelAgeOrder, y = Age_B1950 / 1000),
             shape = 21, color = "black", fill = "black") +
  geom_point(data = BishCreekAges[UncertType == "Int" &
                                    Sample_Type == "Boulder" &
                                    Outlier == TRUE &
                                    SnowModel == "Phillips"],
             mapping = aes(x = RelAgeOrder, y = Age_B1950 / 1000),
             shape = 21, color = "black", fill = "white") +
  geom_point(data = BishCreekAges[UncertType == "Int" &
                                    Sample_Type == "Bedrock" &
                                    Outlier == FALSE &
                                    SnowModel == "Phillips"],
             mapping = aes(x = RelAgeOrder, y = Age_B1950 / 1000),
             shape = 22, color = "black", fill = "black") +
  geom_point(data = BishCreekAges[UncertType == "Int" &
                                    Sample_Type == "Bedrock" &
                                    Outlier == TRUE &
                                    SnowModel == "Phillips"],
             mapping = aes(x = RelAgeOrder, y = Age_B1950 / 1000),
             shape = 22, color = "black", fill = "white") +
  scale_x_continuous(
    name = "Phillips et al. (2009) Tioga 4 Cl-36 Sample by Location", 
    breaks = c(0, 5, 21, 26),
    expand = c(0, 0), minor_breaks = seq.int(0, 26, 1),
    labels = NULL, limits = c(0, 26)) +
  scale_y_continuous(
    name = "CRONUScalc Age (ka BP)", 
    limits = c(11, 19), expand = c(0, 0), 
    breaks = seq(11, 19, 1), minor_breaks = NULL) +
  geom_vline(xintercept = c(5, 21)) +
  publicationTheme

# Tioga 4 moraine camel plot
upperYlimit <- BishCrkNKDE[, max(nkde) * 1.25]
Fig11B <- ggplot() +
  geom_rect(data = BishCreekAges[Sample_Name == "BpCR97-8" &
                                   SnowModel == "Phillips" &
                                   UncertType == "Ext"],
            mapping = aes(xmin = (WMean - WMeanSD) / 1000,
                          xmax = (WMean + WMeanSD) / 1000,
                          ymin = 0, ymax = upperYlimit),
            color = "#bdbdbd", fill = "#bdbdbd") +
  scale_x_continuous(limits = c(20, 12), trans = "reverse",
                     name = "Age (ka BP)", expand = c(0, 0),
                     breaks = seq(20, 12, -1)) +
  scale_y_continuous(limits = c(0, upperYlimit), expand = c(0, 0),
                     name = "Relative Probability", labels = NULL) +
  geom_line(data = BishCrkNKDE[Sample_Name != "WMean"],
            mapping = aes(x = Year / 1000, y = nkde, group = Sample_Name),
            color = "black", size = 0.5) +
  geom_line(data = BishCrkNKDE[Sample_Name == "WMean"],
            mapping = aes(x = Year / 1000, y = nkde),
            color = "black", size = 1) + 
  publicationTheme  

# plot results of Monte Carlo model
Fig11C <- ggplot() +
  geom_histogram(data = BishCreekMC,
                 mapping = aes(x = Duration),
                 boundary = 0, binwidth = 50, fill = "#bdbdbd") +
  geom_histogram(data = BishCreekMC[Valid == TRUE],
                 mapping = aes(x = Duration),
                 boundary = 0, binwidth = 50, fill = "#636363") +
  scale_x_continuous(
    name = "Calculated Duration of the Tioga 4 Deglaciation at Bishop Creek",
    limits = c(-1000, 2500), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, labels = NULL, expand = c(0, 0),
                     limits = c(0, 5000)) +
  geom_vline(xintercept = BishCreekMC[Valid == TRUE,
                                      quantile(Duration,
                                               c(0.159, 0.5, 0.841))]) +
  publicationTheme

# export files
postscript(file = "Fig 2-11A - Cl36 age results.eps",
           width = 6.3, height = (8.3 * 1 / 4), pointsize = 7L,
           horizontal = FALSE, family = "Helvetica",
           paper = "special")
multiplot(Fig11A, cols = 1)
dev.off()
postscript(file = "Fig 2-11BandC - T4moraine and MC results.eps",
           width = 6.3, height = (8.3 * 1 / 4), pointsize = 7L,
           horizontal = FALSE, family = "Helvetica",
           paper = "special")
multiplot(Fig11B, Fig11C, cols = 2)
dev.off()  


# Load and analyze 26Al data from Old Man Mtn--------------------
OldMan <- fread("OldManMtn.txt")

# let's calculate some WMeans...
# create Outlier column
OldMan[, Outlier := FALSE]

# gather IntErr and ExtErr into single column
OldMan <- as.data.table(gather(
  OldMan, "UncertType", "Uncert", c(IntErr, ExtErr)))
# shorten UncertType names
OldMan[UncertType == "IntErr", UncertType := "Int"]
OldMan[UncertType == "ExtErr", UncertType := "Ext"]

# reorder cols
oldOrder <- names(OldMan)
newOrder <- c(oldOrder[1:10], oldOrder[13:14], oldOrder[12], oldOrder[15], 
              oldOrder[11])
setcolorder(OldMan, newOrder)
OldMan[, RawAge := NULL]

# combine the 10Be and 26 Al dates for BB-1B into a single age
# calc mean age
OldMan[UncertType == "Ext", 
       CombinedSampleAge := 
         as.integer(round(weighted.mean(Age_B1950, (1 / Uncert ^ 2)))),
       by = "Sample_Name"]
# set up col for uncert
OldMan[, CombinedSampleUncert := as.integer(NA)]
# count replicate measurements per sample
OldMan[UncertType == "Ext", 
       SpG := sum(!is.na(Age_B1950)), by = "Sample_Name"]
# calc MSWD for samples with two or more measurements
OldMan[SpG >= 2, CombinedSampleMSWD := 
         sum((Age_B1950 - CombinedSampleAge) ^ 2 * (1 / Uncert ^ 2) /
               (SpG - 1)),
       by = "Sample_Name"]
# calc uncert
OldMan[UncertType == "Ext", 
       CombinedSampleUncert :=
         as.integer(round(sqrt(1 / sum(1 / Uncert ^ 2)))),
       by = "Sample_Name"]
OldMan[CombinedSampleMSWD > 1,
       CombinedSampleUncert := 
         CombinedSampleUncert * sqrt(CombinedSampleMSWD)]
# clean up unneeded columns
OldMan[, c("SpG", "CombinedSampleMSWD") := NULL]

# What if the highest elevation sample from the Old Man Mtn
#  transect is rejected?
OldMan[Sample_Name == "97-40", Outlier := T]


# calc group stats for Old Man Mtn samples (not BB-1B)
OldMan[Outlier == FALSE, 
       WMean := 
         as.integer(round(weighted.mean(Age_B1950, (1 / Uncert ^ 2)))),
       by = .(Location, UncertType)]
# calc avg WMean for each location across uncertainty type
OldMan[Outlier == FALSE, 
       WMean := as.integer(round(mean(WMean))), 
       by = "Location"]
# add col for WMeanSD
OldMan[, WMeanSD := as.integer(NA)]
# count samples per group
OldMan[Outlier == FALSE,
       SpG := sum(!is.na(unique(Sample_Name))),
       by = .(Location, UncertType)]
# calc MSWD
OldMan[Outlier == FALSE & UncertType == "Int" & SpG >= 2,
       MSWD := sum((Age_B1950 - WMean) ^ 2 * (1 / Uncert ^ 2) /
                     (SpG - 1)),
       by = "Location"]
MSWDs <- OldMan[!is.na(MSWD), MSWD]
OldMan[Outlier == FALSE & UncertType == "Ext" & SpG >= 2,
       MSWD := MSWDs]
# calc P_SingleNorm
OldMan[Outlier == FALSE,
       P_SingleNorm := pchisq(MSWD * (SpG - 1L),
                              SpG - 1L,
                              lower.tail = FALSE)]
# calc WMeanSD
OldMan[Outlier == FALSE,
       WMeanSD := as.integer(round(sqrt(1 / sum(1 / Uncert ^ 2)))),
       by = .(Location, UncertType)]
OldMan[MSWD > 1, WMeanSD := WMeanSD * sqrt(MSWD)]


# MAKE Fig. 12 Old Man Mtn dates------------------------------------
Fig12 <- ggplot(OldMan[Location == "OldManMountainElevationTransect" &
                UncertType == "Int"]) +
  geom_errorbarh(mapping = aes(x = Age_B1950 / 1000,
                               xmin = (Age_B1950 - Uncert) / 1000,
                               xmax = (Age_B1950 + Uncert) / 1000,
                               y = Elevation_m)) +
  geom_point(mapping = aes(x = Age_B1950 / 1000, 
                           y = Elevation_m)) +
  scale_x_continuous(name = "Age (ka BP)", trans = "reverse", 
                     expand = c(0, 0), limits = c(21, 12),
                     breaks = seq(21, 12, -1)) +
  scale_y_continuous(name = "Elevation (m)", limits = c(1800, 2300),
                     expand = c(0, 0)) +
  publicationTheme
ggsave("Fig 2-12 Old Man Mountain.eps",
       ggarrange(Fig12, ncol = 1),
       width = unit(3.15, "in"),
       height = unit(8.3 * (1/4), "in"))


# Create Monte Carlo model of Al-26 dates-------------------------
sims <- 1E5
simNames <- OldMan[Sample_Name != "BB-1B", unique(Sample_Name)]

# create DT to host Monte Carlo sims
OldManSim <- data.table(expand.grid(
  Trial = seq.int(1, sims),
  Sample_Name = simNames))

# pop DT with ages
OldManSim[, Age := as.integer(NA)]
for (i in 1:length(simNames)) {
  bestEst <- OldMan[Sample_Name == simNames[i] &
                      UncertType == "Int",
                    Age_B1950]
  uncertEst <- OldMan[Sample_Name == simNames[i] &
                        UncertType == "Int",
                      Uncert]
  OldManSim[Sample_Name == simNames[i],
            Age := as.integer(round(rnorm(sims, bestEst, uncertEst)))]
}

# ID valid sims
WideOldMan <- as.data.table(spread(OldManSim, Sample_Name, Age))
# change names
oldNames <- names(WideOldMan)
oldNames <- oldNames[2:9]
newNames <- c("Elev1910", "Elev2000", "Elev2120", "Elev2140", 
              "Elev2160", "Elev2190", "Elev2210", "Elev2240")
setnames(WideOldMan, oldNames, newNames)
WideOldMan[, Valid := FALSE]
WideOldMan[Elev2240 > Elev2210 & Elev2210 > Elev2190 &
             Elev2190 > Elev2160 & Elev2160 > Elev2140 &
             Elev2140 > Elev2120 & Elev2120 > Elev2000 &
             Elev2000 > Elev1910, Valid := T]

# convert wide DT into long DT of valid sims
LongOldMan <- as.data.table(gather(
  WideOldMan[Valid == T], Elev, Age, Elev1910:Elev2240))
LongOldMan[, Valid := NULL]
# change values of Elev
LongOldMan[, Elevation_m := as.integer(NA)]
for (i in 1:length(newNames)) {
  elVal <- sub("Elev", "", newNames[i])
  elVal <- as.integer(elVal)
  LongOldMan[Elev == newNames[i], Elevation_m := elVal]
}
LongOldMan[, Elev := NULL]


# plot successful trials-----------------------------------------
ggplot(LongOldMan) +
  geom_line(aes(x = Age / 1000, y = Elevation_m, group = Trial),
            color = "#636363") +
  geom_errorbarh(data = 
                   OldMan[Location == "OldManMountainElevationTransect" &
                                 UncertType == "Int"],
                 mapping = aes(x = Age_B1950 / 1000,
                               xmin = (Age_B1950 - Uncert) / 1000,
                               xmax = (Age_B1950 + Uncert) / 1000,
                               y = Elevation_m)) +
  geom_point(data = OldMan[Location == "OldManMountainElevationTransect" &
                             UncertType == "Int"],
             mapping = aes(x = Age_B1950 / 1000,
                           y = Elevation_m)) +
  scale_x_continuous(name = "Age (ka BP)", trans = "reverse", 
                     expand = c(0, 0), limits = c(24, 9),
                     breaks = seq(24, 9, -1)) +
  scale_y_continuous(name = "Elevation (m)", limits = c(1800, 2300),
                     expand = c(0, 0)) +
  publicationTheme


# What if we exclude the youngest sample?------------------------
WideOldMan[, ValidWithoutYoungest := F]
WideOldMan[Elev2240 > Elev2210 & Elev2210 > Elev2190 &
             Elev2190 > Elev2160 & Elev2160 > Elev2140 &
             Elev2140 > Elev2000 & Elev2000 > Elev1910, 
           ValidWithoutYoungest := T]

# convert wide DT into long DT of valid sims
LongOldManWithoutYoungest <- as.data.table(gather(
  WideOldMan[ValidWithoutYoungest == T], Elev, Age, Elev1910:Elev2240))
LongOldManWithoutYoungest[, c("Valid", "ValidWithoutYoungest") := NULL]
# change values of Elev
LongOldManWithoutYoungest[, Elevation_m := as.integer(NA)]
for (i in 1:length(newNames)) {
  elVal <- sub("Elev", "", newNames[i])
  elVal <- as.integer(elVal)
  LongOldManWithoutYoungest[Elev == newNames[i], Elevation_m := elVal]
}
LongOldManWithoutYoungest[, Elev := NULL]


# plot successful trials------------------------------------------
ggplot(LongOldManWithoutYoungest[Elevation_m != 2120]) +
  geom_line(aes(x = Age / 1000, y = Elevation_m, group = Trial),
            color = "#636363") +
  geom_errorbarh(data = 
                   OldMan[Location == "OldManMountainElevationTransect" &
                            UncertType == "Int"],
                 mapping = aes(x = Age_B1950 / 1000,
                               xmin = (Age_B1950 - Uncert) / 1000,
                               xmax = (Age_B1950 + Uncert) / 1000,
                               y = Elevation_m)) +
  geom_point(data = OldMan[Location == "OldManMountainElevationTransect" &
                             UncertType == "Int" &
                             Sample_Name != "97-35"],
             mapping = aes(x = Age_B1950 / 1000,
                           y = Elevation_m),
             shape = 21L, color = "black", fill = "black") +
  geom_point(data = OldMan[UncertType == "Int" &
                             Sample_Name == "97-35"],
             mapping = aes(x = Age_B1950 / 1000,
                           y = Elevation_m),
             shape = 21L, color = "black", fill = "white") +
  scale_x_continuous(name = "Age (ka BP)", trans = "reverse", 
                     expand = c(0, 0), limits = c(24, 9),
                     breaks = seq(24, 9, -1)) +
  scale_y_continuous(name = "Elevation (m)", limits = c(1800, 2300),
                     expand = c(0, 0)) +
  publicationTheme


# calc ice-thinning rates-----------------------------------------
uniqueTrials <- LongOldMan[, unique(Trial)] 
uniqueElevs <- LongOldMan[, unique(Elevation_m)]
expander <- (length(uniqueElevs) - 1) * 2
OldManRate <- as.data.table(expand.grid(
  temp = 1:expander,
  Trial = uniqueTrials))
OldManRate[, ':=' (Age = as.integer(NA),
                   Rate = as.numeric(NA))]
for (i in 1:length(uniqueTrials)) {
  for (j in 1:expander) {
    if ((j %% 2) == 1) {
      k <- (-0.5 * j) + length(uniqueElevs) + 0.5
      elev1 <- uniqueElevs[k]
      elev2 <- uniqueElevs[k - 1]
      elev3 <- elev1 - elev2
      age1 <- LongOldMan[Trial == uniqueTrials[i] &
                           Elevation_m == elev1, Age]
      age2 <- LongOldMan[Trial == uniqueTrials[i] &
                           Elevation_m == elev2, Age]
      age3 <- age1 - age2
      OldManRate[Trial == uniqueTrials[i] & temp == j,
                 ':=' (Age = age1, Rate = elev3 / age3)]
      OldManRate[Trial == uniqueTrials[i] & temp == j + 1,
                 ':=' (Age = age2, Rate = elev3 / age3)]
    } else {
      next()
    }
  }
}
    
# plot ice-thinning rate
ggplot(OldManRate) +
  geom_line(aes(x = Age / 1000, y = Rate, group = Trial)) +
  scale_x_continuous(name = "Age (ka BP)", trans = "reverse", 
                     expand = c(0, 0), limits = c(24, 9),
                     breaks = seq(24, 9, -1)) +
  scale_y_continuous(name = "Ice-Thinning Rate (m/yr)",
                     trans = "log10",
                     limits = c(0.004, 30),
                     breaks = c(0.001, 0.01, 0.1, 1, 10),
                     minor_breaks = c(seq(0.001, 0.009, 0.001),
                                      seq(0.01, 0.09, 0.01),
                                      seq(0.1, 0.9, 0.1),
                                      seq(1, 9, 1),
                                      seq(10, 90, 10))) +
  publicationTheme


# repeat calcs for without youngest--------------------------
uniqueTrials <- LongOldManWithoutYoungest[, unique(Trial)] 
uniqueElevs <- LongOldMan[Elevation_m != 2120, unique(Elevation_m)]
expander <- (length(uniqueElevs) - 1) * 2
OldManRateWithoutYoungest <- as.data.table(expand.grid(
  temp = 1:expander,
  Trial = uniqueTrials))
OldManRateWithoutYoungest[, ':=' (Age = as.integer(NA),
                   Rate = as.numeric(NA))]
for (i in 1:length(uniqueTrials)) {
  for (j in 1:expander) {
    if ((j %% 2) == 1) {
      k <- (-0.5 * j) + length(uniqueElevs) + 0.5
      elev1 <- uniqueElevs[k]
      elev2 <- uniqueElevs[k - 1]
      elev3 <- elev1 - elev2
      age1 <- LongOldManWithoutYoungest[Trial == uniqueTrials[i] &
                           Elevation_m == elev1, Age]
      age2 <- LongOldManWithoutYoungest[Trial == uniqueTrials[i] &
                           Elevation_m == elev2, Age]
      age3 <- age1 - age2
      OldManRateWithoutYoungest[Trial == uniqueTrials[i] & temp == j,
                 ':=' (Age = age1, Rate = elev3 / age3)]
      OldManRateWithoutYoungest[Trial == uniqueTrials[i] & temp == j + 1,
                 ':=' (Age = age2, Rate = elev3 / age3)]
    } else {
      next()
    }
  }
}

# plot ice-thinning rate
ggplot(OldManRateWithoutYoungest) +
  geom_line(aes(x = Age / 1000, y = Rate, group = Trial)) +
  scale_x_continuous(name = "Age (ka BP)", trans = "reverse", 
                     expand = c(0, 0), limits = c(24, 9),
                     breaks = seq(24, 9, -1)) +
  scale_y_continuous(name = "Ice-Thinning Rate (m/yr)",
                     trans = "log10",
                     limits = c(0.004, 30),
                     breaks = c(0.001, 0.01, 0.1, 1, 10),
                     minor_breaks = c(seq(0.001, 0.009, 0.001),
                                      seq(0.01, 0.09, 0.01),
                                      seq(0.1, 0.9, 0.1),
                                      seq(1, 9, 1),
                                      seq(10, 90, 10))) +
  publicationTheme  
    
    
# What if I plot the rates based on time since start of deglac?-------    
OldManRate[, MaxTrialAge := max(Age), by = "Trial"]
OldManRate[, TimeSinceStartOfDeglac := MaxTrialAge - Age]
# plot
ggplot(OldManRate) +
  geom_line(aes(x = TimeSinceStartOfDeglac, y = Rate, group = Trial)) +
  scale_y_continuous(name = "Ice-Thinning Rate (m/yr)",
                     trans = "log10",
                     limits = c(0.004, 30),
                     breaks = c(0.001, 0.01, 0.1, 1, 10),
                     minor_breaks = c(seq(0.001, 0.009, 0.001),
                                      seq(0.01, 0.09, 0.01),
                                      seq(0.1, 0.9, 0.1),
                                      seq(1, 9, 1),
                                      seq(10, 90, 10))) +
  publicationTheme  

# after rejecting the youngest sample...
OldManRateWithoutYoungest[, MaxTrialAge := max(Age), by = "Trial"]  
OldManRateWithoutYoungest[, TimeSinceStartOfDeglac := MaxTrialAge - Age]
# plot
ggplot(OldManRateWithoutYoungest) +
  geom_line(aes(x = TimeSinceStartOfDeglac, y = Rate, group = Trial)) +
  scale_y_continuous(name = "Ice-Thinning Rate (m/yr)",
                     trans = "log10",
                     limits = c(0.004, 30),
                     breaks = c(0.001, 0.01, 0.1, 1, 10),
                     minor_breaks = c(seq(0.001, 0.009, 0.001),
                                      seq(0.01, 0.09, 0.01),
                                      seq(0.1, 0.9, 0.1),
                                      seq(1, 9, 1),
                                      seq(10, 90, 10))) +
  publicationTheme      
  

# Create new age-depth model for Swamp Lake using Bchron---------------
# load Swamp Lake TOC
SwampLakeTOC <- fread("SwampLakeTOC.txt")
SwampLakeTOC[, Years_B1950 := as.integer(round(Years_B1950, 0))]
setkey(SwampLakeTOC, Years_B1950, CoreDepth_cm)

# load Swamp Lake Mag
SwampLakeMag <- fread("SwampLakeMag.txt")
SwampLakeMag[, Years_B1950 := as.integer(round(Years_B1950, 0))]
setkey(SwampLakeMag, Years_B1950, CoreDepth_cm)

# load Swamp Lake 14C dates / ages
SwampLakeAges <- fread("SwampLake14CAgesCALIB71.txt")
SwampLakeAges[, V9 := NULL]
SwampLakeAges[, CalCurve := "intcal13"]

# calc Bchron ages, compare with CALIB5 and CALIB7 ages
SLBchronAges <- BchronCalibrate(SwampLakeAges[, Age_14Cyr_B1950],
                                SwampLakeAges[, SD_14Cyr_B1950],
                                SwampLakeAges[, CalCurve])

# find median Bchron ages
for (i in 1:SwampLakeAges[, .N]) {
  DT <- data.table(
    ages  = SLBchronAges[[i]][4]$ageGrid,
    probs = SLBchronAges[[i]][5]$densities)
  for (j in DT[, .N]:1) {
    DT[ages <= DT[j, ages], CumProb := sum(probs)]
  }
  SwampLakeAges[i, MedianBchronAge := DT[CumProb >= 0.5, min(ages)]]
}

# difference between Bchron and CALIB 5
SwampLakeAges[, BchronDiffFromCALIB5 := MedianAge_calYr - MedianBchronAge]

# extract and clean Swamp Lake Measurement depths
SwampLakeMeasurementDepths <- c(SwampLakeTOC[, CoreDepth_cm], 
                                SwampLakeMag[, CoreDepth_cm])
SwampLakeMeasurementDepths <- unique(SwampLakeMeasurementDepths)
SwampLakeMeasurementDepths <- sort(SwampLakeMeasurementDepths)

# construct age-depth model for Swamp Lake TOC samples
AgeDepthModel <- Bchronology(SwampLakeAges[, Age_14Cyr_B1950],
                             SwampLakeAges[, SD_14Cyr_B1950],
                             SwampLakeAges[, Composite_Core_Depth_cm],
                             predictPositions = SwampLakeMeasurementDepths)
# construct sampleDepths DT
sampleDepths <- data.table(
  Depth_cm = AgeDepthModel[[6]],
  BchronAge = as.integer(NA),
  BchronAgeSD = as.integer(NA))
for (i in 1:sampleDepths[, .N]) {
  temp <- rep(as.integer(NA), 1000)
  for (j in 1:1000) {
    temp[j] <- AgeDepthModel[[5]][j, i]
  }
  sampleDepths[i, BchronAge := as.integer(round(mean(temp)))]
  sampleDepths[i, BchronAgeSD := as.integer(round(sd(temp)))]
}
# transfer age estimates to SwampLakeTOC and SwampLakeMag
SwampLakeTOC[, CalAgeSD := as.integer(NA)]
SwampLakeMag[, CalAgeSD := as.integer(NA)]
for (i in 1:SwampLakeTOC[, .N]) {
  set(SwampLakeTOC, i, "Years_B1950",
      sampleDepths[Depth_cm == SwampLakeTOC[i, CoreDepth_cm],
                   BchronAge])
  set(SwampLakeTOC, i, "CalAgeSD",
      sampleDepths[Depth_cm == SwampLakeTOC[i, CoreDepth_cm],
                   BchronAgeSD])
}
for (i in 1:SwampLakeMag[, .N]) {
  set(SwampLakeMag, i, "Years_B1950",
      sampleDepths[Depth_cm == SwampLakeMag[i, CoreDepth_cm],
                   BchronAge])
  set(SwampLakeMag, i, "CalAgeSD",
      sampleDepths[Depth_cm == SwampLakeMag[i, CoreDepth_cm],
                   BchronAgeSD])
}


# Load and prep files for paleoclimate figure---------------------
NGRIP <- fread("NGRIP_d18OandDust.txt")
NGRIP[, Years_B1950 := Years_B2000 - 50]
# adjust dust into millions of grains per ml
NGRIP[, DustCount_E6PERml := DustCount_PERml / 1E6]
# convert dust into d18O "units"
NGRIP.d18Omin <- NGRIP[, min(d18O, na.rm = T)]
NGRIP.d18Omax <- NGRIP[, max(d18O, na.rm = T)]
NGRIP.d18Orange <- NGRIP.d18Omax - NGRIP.d18Omin
dustMin <- NGRIP[, min(DustCount_E6PERml, na.rm = T)]
dustMax <- NGRIP[, max(DustCount_E6PERml, na.rm = T)]
dustRange <- dustMax - dustMin
NGRIP[, Dust_d18Ounits := NGRIP.d18Omin + 
        (DustCount_E6PERml - dustMin) *
        (NGRIP.d18Orange / dustRange)]

# calculating running average of data, there'se too many points...
NGRIP[, Years_B2000 := NULL]
# calc 20 yr running mean
NGRIPrunning <- data.table(
  startStep = seq.int(9460, 27980, 20),
  midStep   = seq.int(9470, 27990, 20),
  endStep   = seq.int(9480, 28000, 20),
  d18O      = as.numeric(NA),
  Dust_d18Ounits = as.numeric(NA))
for (i in 1:NGRIPrunning[, .N]) {
  stepBottom <- NGRIPrunning[i, startStep]
  stepTop    <- NGRIPrunning[i, endStep]
  NGRIPrunning[i, ':=' (
    d18O = NGRIP[Years_B1950 >= stepBottom &
                      Years_B1950 < stepTop,
                    mean(d18O, na.rm = T)],
    Dust_d18Ounits = NGRIP[Years_B1950 >= stepBottom &
                             Years_B1950 < stepTop,
                           mean(Dust_d18Ounits, na.rm = T)])]
}

# North Atlantic IRD
IRD <- fread("NorthAtlanticIRDdata_SternLisiecki.txt")
ggplot(IRD, aes(x = Age_kyr, y = IRD_Stack_norm)) +
  geom_area()
# scale to NGRIP d18O
IRD[, IRDscaled := (IRD_Stack_norm * 10) - 50]
IRDmin <- IRD[, min(IRD_Stack_norm, na.rm = T)]
IRDmax <- IRD[, max(IRD_Stack_norm, na.rm = T)]
IRDrange <- IRDmax - IRDmin
IRD[, IRD_NGRIP.d18O.units := NGRIP.d18Omin +
      (IRD_Stack_norm - IRDmin) *
      (NGRIP.d18Orange / IRDrange)]

# Load Hendy et al. (2002) data...
Hendy02 <- fread("Hendy2002_forR.txt")
# Load and clean Hendy et al. (2003) data...
Hendy2003 <- fread("Hendy2003_forR.txt")
Hendy2003[, Years_B1950 := Years_B2000 - 50L]

# plot Hendy02 d18O records on our timescale
ggplot(Hendy02) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  # G. bulloides is a surface dweller
  geom_line(mapping = aes(x = HendyAge_yrs / 1000, y = bull.d18O)) +
  # N. pachyderma is a thermocline dweller
  geom_line(mapping = aes(x = HendyAge_yrs / 1000, y = pach.d18O),
            color = "red") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  publicationTheme
# d13C on our timescale
ggplot(Hendy02) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  # G. bulloides is a surface dweller
  geom_line(mapping = aes(x = HendyAge_yrs / 1000, y = bull.d13C)) +
  # N. pachyderma is a thermocline dweller
  geom_line(mapping = aes(x = HendyAge_yrs / 1000, y = pach.d13C),
            color = "red") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  scale_y_continuous(trans = "reverse") +
  publicationTheme

# plot Hendy02 on Hendy02 timescale
# d18O
ggplot() +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  # G. bulloides is a surface dweller
  geom_line(data = Hendy02[HendyAge_yrs >= 16000],
            mapping = aes(x = HendyAge_yrs / 1000, y = bull.d18O)) +
  geom_line(data = Hendy02[HendyAge_yrs <= 16000],
            mapping = aes(x = HendyAge_yrs / 1000, y = bull.d18O)) +
  # N. pachyderma is a thermocline dweller
  # geom_line(data = Hendy02[HendyAge_yrs >= 16000],
  #           mapping = aes(x = HendyAge_yrs / 1000, y = pach.d18O),
  #           color = "red") +
  # geom_line(data = Hendy02[HendyAge_yrs <= 16000],
  #           mapping = aes(x = HendyAge_yrs / 1000, y = pach.d18O),
  #           color = "red") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  publicationTheme

# load global temp record
Temp <- fread("GlobalTempRecord.txt")
setkey(Temp, Years_B1950)
ggplot(Temp, aes(x = Years_B1950, y = Temp_C)) + geom_line() +
  scale_x_continuous(trans = "reverse")

# load CO2 record
IceCore <- fread("IceCoreCO2.txt")
setkey(IceCore, Years_B1950)
ggplot(IceCore, aes(x = Years_B1950, y = CO2_ppm)) + 
  geom_line() +
  scale_x_continuous(trans = "reverse") +
  scale_y_continuous(limits = c(0, 410))
                          
# Gulf of CA SSTs
# UK37 inferred SSTs
GofCA_UK37 <- fread("McClymontEtAl2012_GulfOfCA_UK37_SST_forR.txt")
GofCA_UK37[, Years_B1950int := as.integer(round(Years_B1950))]
GofCA_TEXH86 <- fread("McClymontEtAl2012_GulfOfCA_TEXH86_SST_forR.txt")
GofCA_TEXH86[, Years_B1950int := as.integer(round(Years_B1950))]
GofCA_Opal <- fread("McClymontEtAl2012_GulfOfCA_Opal_R.txt")
GofCA_Opal[, Years_B1950int := as.integer(round(Years_B1950))]
GofCA_Tgrad <- fread("McClymontEtAl2012_GulfOfCA_DeltaT_forR.txt")
GofCA_Tgrad[, Years_B1950int := as.integer(round(Years_B1950))]
# explore data
ggplot(GofCA_UK37) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  geom_line(data = GofCA_UK37,
            mapping = aes(x = Years_B1950 / 1000,
                          y = SST_C)) +
  geom_line(data = GofCA_TEXH86,
            mapping = aes(x = Years_B1950 / 1000,
                          y = SST_C),
            color = "red") +
  # geom_line(data = GofCA_Opal,
  #           mapping = aes(x = Years_B1950 / 1000,
  #                         y = Opal_percent),
  #           color = "#bdbdbd") +
  # geom_smooth(data = GofCA_Opal,
  #           mapping = aes(x = Years_B1950 / 1000,
  #                         y = Opal_percent),
  #           color = "#636363") +
  # geom_line(data = GofCA_Tgrad,
  #           mapping = aes(x = Years_B1950 / 1000,
  #                         y = SST_Gradient_UKminusTEX),
  #           color = "#8856a7") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  publicationTheme
# BIT index from Gulf of CA
ggplot(GofCA_UK37) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  geom_line(data = GofCA_TEXH86[!is.na(Branched_GDGT_conc_ppm)],
            mapping = aes(x = Years_B1950 / 1000,
                          y = Branched_GDGT_conc_ppm),
            color = "#2ca25f") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  publicationTheme
# temp gradient, degree of upwelling and mixing
ggplot(GofCA_UK37) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  geom_line(data = GofCA_Tgrad,
            mapping = aes(x = Years_B1950 / 1000,
                          y = SST_Gradient_UKminusTEX),
            color = "#8856a7") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  publicationTheme
# reduction in upwelling between 17-16 ka
# This dataset has potential for a comparision with the Sierra Nevada
# Perhaps write, "Unfortunately ODP core 893A from the Santa Barbara
#  basin has a core gap during this time interval (sup. fig.), but
#  core XXX from the Gulf of CA shows a Y degree change in SST at this
#  time."

# Analysis of the MyClymont et al. (2012) data
# average SST before/after 16.4
UK37after <- GofCA_UK37[Years_B1950 <= 16400 & Years_B1950 >= 14400, 
                    mean(SST_C)]
UK37before <- GofCA_UK37[Years_B1950 >= 16400 & Years_B1950 <= 18400, 
                     mean(SST_C)]
UK37after - UK37before
TEXH86after <- GofCA_TEXH86[Years_B1950 <= 16400 & Years_B1950 >= 14400, 
                    mean(SST_C)]
TEXH86before <- GofCA_TEXH86[Years_B1950 >= 16400 & Years_B1950 <= 18400, 
                     mean(SST_C)]
TEXH86after - TEXH86before

# Load Moaning Cave d18O and 13C record from Oster et al., 2009
Moaning <- fread("MoaningCaveCA.txt")
setnames(Moaning, c("d13C", "d18O"), c("Moaning.d13C", "Moaning.d18O"))
setkey(Moaning, "Years_B1950")
# load McLean Cave
McLean <- fread("McLean.txt")
McLean[, depth_mm := NULL]
setkey(McLean, "Years_B1950")
localCaves <- merge(Moaning, McLean, all = T)

# scale McLean to have the same range as Moaning
MoanMin <- localCaves[, min(Moaning.d18O, na.rm = T)]
MoanMax <- localCaves[, max(Moaning.d18O, na.rm = T)]
MoanRange <- MoanMax - MoanMin
McLMin <- localCaves[, min(McLean.d18O, na.rm = T)]
McLMax <- localCaves[, max(McLean.d18O, na.rm = T)]
McLRange <- McLMax - McLMin
localCaves[, McLeanToMoaning.d18O := 
             MoanMin + (McLean.d18O - McLMin) *
             (MoanRange / McLRange)]

# explore data
setnames(Moaning, names(Moaning)[2:3], c("d13C", "d18O"))
ggplot(Moaning) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  geom_line(mapping = aes(x = Years_B1950 / 1000, y = d18O)) +
  geom_line(mapping = aes(x = Years_B1950 / 1000, y = d13C),
            color = "red") +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), breaks = seq.int(20, 10, -1)) +
  publicationTheme

# zoom in on 15.5-14.5 ka to isolate time of rapid change
ggplot(Moaning[Years_B1950 >= 14500 & Years_B1950 <= 15500]) +
  geom_line(mapping = aes(x = Years_B1950 / 1000, y = d18O)) +
  geom_line(mapping = aes(x = Years_B1950 / 1000, y = d13C),
            color = "red") +
  scale_x_continuous(limits = c(15.5, 14.5), trans = "reverse",
                     expand = c(0, 0), breaks = seq(15.5, 14.5, -0.1)) +
  publicationTheme
# calc magnitude of change
moan1 <- Moaning[Years_B1950 >= 15050 & Years_B1950 <= 15500,
                 .(d13C = mean(d13C), d18O = mean(d18O))]
moan2 <- Moaning[Years_B1950 >= 14000 & Years_B1950 <= 15050,
                 .(d13C = mean(d13C), d18O = mean(d18O))]
moanChange <- moan2 - moan1

# Load Fort Stanton d18O record from Asmerom et al. 2010
FortStanton <- fread("AsmeromEtAl2010_FortStantonCaveNM.txt")
# convert Years_B2008 to Years_B1950
FortStanton[, Years_B1950 := Years_B2008 - 58L]
# plot record
ggplot(FortStanton) +
  geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  geom_line(mapping = aes(x = Years_B1950 / 1000, y = d18O_VPDB)) +
  scale_x_continuous(limits = c(20, 10), trans = "reverse",
                     expand = c(0, 0), 
                     breaks = seq.int(20, 10, -1)) +
  publicationTheme
# calculate magnitude of 16.6-16.3 ka change in terms of temp 
d18Oval1 <- FortStanton[Years_B1950 == 16575, d18O_VPDB]
d18Oval2 <- FortStanton[Years_B1950 == 16226, d18O_VPDB]
TempFraction <- (1 / 3)
minTempEst <- ((d18Oval2 - d18Oval1) * TempFraction) / 0.69
maxTempEst <- ((d18Oval2 - d18Oval1) * TempFraction) / 0.5
altMinTempEst <- ((d18Oval2 - d18Oval1) * TempFraction) / (0.69 - 0.24)
altMaxTempEst <- ((d18Oval2 - d18Oval1) * TempFraction) / (0.5 - 0.24)
# calc temp relative to modern, assuming
# 1- a d18O value for the precipitation (range: 0.5-0.69)
precip_d18O <- 0.6
# 2- that 1/2 the change can be attributed to temp
PercentChangeDueToTemp <- 1/3
# 3- that 32% of modern precip comes from the Pacific, -11 o/oo d18O
# 4- that 68% comes from GoM, -2 per mil
FortStanton[, Modern_d18O := (0.32 * -11) + (0.68 * -2)]
FortStanton[, d18O_diffFromModern := d18O_VPDB - Modern_d18O]

FortStanton[, ChangeDuToTemp := d18O_diffFromModern * 
                                  PercentChangeDueToTemp]
FortStanton[, TempRelToModern := ChangeDuToTemp / (precip_d18O - 0.24)]
Temp1 <- FortStanton[Years_B1950 == 16575, TempRelToModern]
Temp2 <- FortStanton[Years_B1950 == 16226, TempRelToModern]
TempChange <- Temp2 - Temp1
StandardLapseRate <- 6.5 # degrees C per km
# calculate implied ELA rise
(TempChange / StandardLapseRate) * 1000

# remove influence of temp from d18O
FortStanton[, d18OvalueDuToPrecip := d18O_VPDB - ChangeDuToTemp]
# calc percent pacific moisture
FortStanton[, PercentPacific := (-2 / 9) - (d18OvalueDuToPrecip / 9)]
PacPercent1 <- FortStanton[Years_B1950 == 16575, PercentPacific]
PacPercent2 <- FortStanton[Years_B1950 == 16226, PercentPacific]
PercentReduction <- (PacPercent2 - PacPercent1) / PacPercent1

# Load Cave of the Bells
CaveOfTheBells <- fread("CaveOfTheBells.txt")
setkey(CaveOfTheBells, Years_B1950)
# reload FortStanton
FortStanton <- fread("AsmeromEtAl2010_FortStantonCaveNM.txt")
FortStanton[, Years_B1950 := Years_B2008 - 58L]
FortStanton[, Years_B2008 := NULL]
setkey(FortStanton, Years_B1950)
speleo <- merge(FortStanton, CaveOfTheBells, all = T)
setnames(speleo, c("d18O_VPDB", "d18O_IceVolCorrected"),
                 c("FortStanton.d18O", "CaveOfTheBells.d18O"))
# scale Cave of the Bells to Fort Stanton
# scale Cave of the Bells d18O record to Fort Stanton
FSmin <- speleo[, min(FortStanton.d18O, na.rm = T)]
FSmax <- speleo[, max(FortStanton.d18O, na.rm = T)]
FSrange <- FSmax - FSmin
CBmin <- speleo[, min(CaveOfTheBells.d18O, na.rm = T)]
CBmax <- speleo[, max(CaveOfTheBells.d18O, na.rm = T)]
CBrange <- CBmax - CBmin
speleo[, CofTheB_scaledTo_FS := FSmin + (CaveOfTheBells.d18O - CBmin) *
         (FSrange / CBrange)]

# Biological Productivity from core TN57-13PC from the Southern Ocean
# See Ref 18 in Marcott et al (2014) Nature
BioProd53S <- fread("MarcottEtAl2014Fig2_coreTN57_13PC.txt")
ggplot(BioProd53S, aes(x = MarineCal13Age,
                       y = MarineCal13_230Th_Normalized_Flux_gcm2ka)) +
  geom_line()

# Biological productivity from core E27-23 from the southern ocean
BioProd60S <- fread("MarcottEtAl2014Fig2_coreE27_23.txt")
ggplot(BioProd60S, aes(x = MarineCal13Age,
                       y = MarineCal13_230Th_Normalized_Flux_gcm2ka)) +
  geom_line()

# Master splice of Partin et al. 2007 speleothem
Borneo <- fread("PartinEtAl2007_Borneo_4N.txt")
ggplot(Borneo, aes(x = Years_B1950, y = d18O)) +
  geom_line()

# load Hulu file
Hulu <- fread("WuEtAl_HuluCave.txt")
ggplot(Hulu, aes(x = Years_B1950, y = d18O)) +
  geom_line() +
  scale_x_continuous(breaks = seq.int(10000, 20000, 1000))


# ASSEMBLE all paleoclimate records into one DT-------------------------
paleo <- as.data.table(expand.grid(
  Year = seq.int((1950L - 2018L), 65000L, 1L),
  CO2 = as.numeric(NA),
  Methane = as.numeric(NA),
  GlobalTemp = as.numeric(NA),
  SwampTOC = as.numeric(NA),
  SwampMag = as.numeric(NA),
  FtStanton = as.numeric(NA),
  Moaning.d18O = as.numeric(NA),
  Moaning.d13C = as.numeric(NA),
  Hendy02.pach.d18O = as.numeric(NA),
  Hendy02.pach.d13C = as.numeric(NA),
  Hendy02.bull.d18O = as.numeric(NA),
  Hendy02.bull.d13C = as.numeric(NA),
  UK37 = as.numeric(NA),
  TEXH86 = as.numeric(NA),
  Opal27N = as.numeric(NA),
  Tgrad = as.numeric(NA),
  IRDetritus = as.numeric(NA),
  Opal53S = as.numeric(NA),
  Opal60S = as.numeric(NA),
  Borneo.d18O = as.numeric(NA),
  Hulu.d18O = as.numeric(NA)))

# copy over CO2 records
for (i in 1:IceCore[, .N]) {
  paleo[Year == IceCore[i, Years_B1950],
        CO2 := IceCore[i, CO2_ppm]]
}

# copy over methane records
methane <- fread("Methane.txt")
methane[, Years_B1950 := as.integer(round(Age_yrsB1950))]
methane[, Age_yrsB1950 := NULL]
for (i in 1:methane[, .N]) {
  paleo[Year == methane[i, Years_B1950],
        Methane := methane[i, CH4_ppb]]
}

# copy over Temp records
for (i in 1:Temp[, .N]) {
  paleo[Year == Temp[i, Years_B1950],
        GlobalTemp := Temp[i, Temp_C]]
}

# copy over Swamp Lake TOC records
for (i in 1:SwampLakeTOC[, .N]) {
  paleo[Year == SwampLakeTOC[i, Years_B1950],
        SwampTOC := SwampLakeTOC[i, TOC_wtPercent]]
}

# copy over Swamp Lake Mag Sus records
for (i in 1:SwampLakeMag[, .N]) {
  paleo[Year == SwampLakeMag[i, Years_B1950],
        SwampMag := SwampLakeMag[i, MagSus]]
}

# copy over Fort Stanton d18O record
for (i in 1:FortStanton[, .N]) {
  paleo[Year == FortStanton[i, Years_B1950],
        FtStanton := FortStanton[i, d18O_VPDB]]
}

# copy over Moaning Cave d18O and d13C records
for (i in 1:Moaning[, .N]) {
  paleo[Year == Moaning[i, Years_B1950], ':=' (
    Moaning.d18O = Moaning[i, d18O],
    Moaning.d13C = Moaning[i, d13C])]
}

# copy over Hendy02 records
for (i in 1:Hendy02[, .N]) {
  paleo[Year == Hendy02[i, HendyAge_yrs], ':=' (
    Hendy02.pach.d18O = Hendy02[i, pach.d18O],
    Hendy02.pach.d13C = Hendy02[i, pach.d13C],
    Hendy02.bull.d18O = Hendy02[i, bull.d18O],
    Hendy02.bull.d13C = Hendy02[i, bull.d13C])]
}

# copy over Gulf of Mexico records
for (i in 1:GofCA_UK37[, .N]) {
  paleo[Year == GofCA_UK37[i, Years_B1950int],
        UK37 := GofCA_UK37[i, SST_C]]
}
for (i in 1:GofCA_TEXH86[, .N]) {
  paleo[Year == GofCA_TEXH86[i, Years_B1950int],
        TEXH86 := GofCA_TEXH86[i, SST_C]]
}
for (i in 1:GofCA_Opal[, .N]) {
  paleo[Year == GofCA_Opal[i, Years_B1950int],
        Opal27N := GofCA_Opal[i, Opal_percent]]
}
for (i in 1:GofCA_Tgrad[, .N]) {
  paleo[Year == GofCA_Tgrad[i, Years_B1950int],
        Tgrad := GofCA_Tgrad[i, SST_Gradient_UKminusTEX]]
}

# copy over IRD
for (i in 1:IRD[, .N]) {
  paleo[Year == IRD[i, Age_yrs_BP],
        IRDetritus := IRD[i, IRD_Stack_norm]]
}

# copy over Biological Productivity record from 53 S
for (i in 1:BioProd53S[, .N]) {
  paleo[Year == BioProd53S[i, MarineCal13Age],
        Opal53S := BioProd53S[i, MarineCal13_230Th_Normalized_Flux_gcm2ka]]
}

# copy over Biological Productivity record from 60 S
for (i in 1:BioProd60S[, .N]) {
  paleo[Year == BioProd60S[i, MarineCal13Age],
        Opal60S := BioProd60S[i, MarineCal13_230Th_Normalized_Flux_gcm2ka]]
}

# copy over master splice of d18O from Borneo
for (i in 1:Borneo[, .N]) {
  paleo[Year == Borneo[i, Years_B1950],
        Borneo.d18O := Borneo[i, d18O]]
}

# copy d18O over from Hulu
for (i in 1:Hulu[, .N]) {
  paleo[Year == Hulu[i, Years_B1950],
        Hulu.d18O := Hulu[i, d18O]]
}

# eliminate empty rows
paleo <- paleo[!is.na(CO2) | !is.na(Methane) | !is.na(GlobalTemp) | 
                 !is.na(SwampTOC) | !is.na(SwampMag) | !is.na(FtStanton) |
                 !is.na(Moaning.d18O) | 
                 !is.na(Hendy02.pach.d18O) |
                 !is.na(Hendy02.pach.d13C) | 
                 !is.na(Hendy02.bull.d18O) |
                 !is.na(Hendy02.bull.d13C) |
                 !is.na(UK37) | !is.na(TEXH86) | !is.na(Opal27N) |
                 !is.na(Tgrad) | !is.na(IRDetritus) | !is.na(Opal53S) |
                 !is.na(Opal60S) | !is.na(Borneo.d18O) |
                 !is.na(Hulu.d18O)]

# translate GlobalTemp into CO2 units for plotting on single plot
minCO2 <- paleo[, min(CO2, na.rm = TRUE)]
maxCO2 <- paleo[, max(CO2, na.rm = TRUE)]
CO2range <- maxCO2 - minCO2
minTemp <- paleo[, min(GlobalTemp, na.rm = TRUE)]
maxTemp <- paleo[, max(GlobalTemp, na.rm = TRUE)]
TempRange <- maxTemp - minTemp
paleo[, Temp_CO2units := minCO2 + (GlobalTemp - minTemp) * 
        (CO2range / TempRange)]

# translate Methane into CO2 units for plotting on single plot
minMethane <- paleo[, min(Methane, na.rm = TRUE)]
maxMethane <- paleo[, max(Methane, na.rm = TRUE)]
MethaneRange <- maxMethane - minMethane
CO2MethaneRatio <- CO2range / MethaneRange
paleo[, Methane_CO2units := minCO2 + (Methane - minMethane) *
        CO2MethaneRatio]

# translate SwampMag into SwampTOC units for plotting on single figure
minTOC <- paleo[, min(SwampTOC, na.rm = TRUE)]
maxTOC <- paleo[, max(SwampTOC, na.rm = TRUE)]
TOCrange <- maxTOC - minTOC
minMag <- paleo[, min(SwampMag, na.rm = TRUE)]
maxMag <- paleo[, max(SwampMag, na.rm = TRUE)]
MagRange <- maxMag - minMag
TOCmagRatio <- maxTOC / maxMag
paleo[, SwampMag_TOCunits := minTOC + (SwampMag - minMag) * TOCmagRatio]

# translate Borneo.18O onto Hulu.d18O scale for plotting
minHulu <- paleo[, min(Hulu.d18O, na.rm = TRUE)]
maxHulu <- paleo[, max(Hulu.d18O, na.rm = TRUE)]
HuluRange <- maxHulu - minHulu
minBorneo <- paleo[, min(Borneo.d18O, na.rm = TRUE)]
maxBorneo <- paleo[, max(Borneo.d18O, na.rm = TRUE)]
BorneoRange <- maxBorneo - minBorneo
HuluBorneoRatio <- HuluRange / BorneoRange
paleo[, Borneo_HuluUnits := minHulu + (Borneo.d18O - minBorneo) *
                              HuluBorneoRatio]

# translate Opal27N onto UK37 scale for plotting
minUK37 <- paleo[, min(UK37, na.rm = TRUE)]
maxUK37 <- paleo[, max(UK37, na.rm = TRUE)]
UKRange <- maxUK37 - minUK37
minOpal27 <- paleo[, min(Opal27N, na.rm = TRUE)]
maxOpal27 <- paleo[, max(Opal27N, na.rm = TRUE)]
Opal27Range <- maxOpal27 - minOpal27
UKopalRatio <- UKRange / Opal27Range
paleo[, Opal27N_UK37units := minUK37 + (Opal27N - minOpal27) *
                              UKopalRatio]


# Compile cosmo and radiocarbon data-----------------------------
# Create DT to host cosmo data for paleoclimate figure plotting
CosmoWMeans <- data.table(expand.grid(
  Location = c("TLC", "BLC", "LD", "PD", "LM", "BCT4m", "BCVB", "HB",
               "HL", "GL", "EL"),
  Elevation_m = as.integer(NA),
  SampleType = as.character(NA),
  UncertType = "Ext",
  yAxis = as.integer(NA),
  WMeanAge = as.numeric(NA),
  WMeanSD = as.numeric(NA)))

# Populate Locations in CosmoWMeans
Be10Locations <- c("TLC", "BLC", "LD", "PD", "LM")
Cl36Locations <- c("BCT4m", "BCVB", "HB")
CosmoWMeans[Location %in% Be10Locations, SampleType := "Be10"]
CosmoWMeans[Location %in% Cl36Locations, SampleType := "Cl36"]
CosmoWMeans[is.na(SampleType), SampleType := "C14"]

# Populate yAxis in CosmoWMeans
yAxisValues <- c(seq(13, 9), seq(7, 5), seq(3, 1))
CosmoWMeans[, yAxis := yAxisValues]

# Populate WMeanAge and WMeanSD in CosmoWMeans
# Our Locations
CosmoWMeans[Location == "TLC", ':=' (
  Elevation_m = CalcCompare[Location == "ULC",
                            as.integer(round(mean(Elevation_m)))],
  WMeanAge = CalcCompare[PercentSnow == 100 &
                           UncertType == "Ext" &
                           Calculator == "CRONUScalc" &
                           Location == "ULC", 
                         unique(WMean) / 1000],
  WMeanSD = CalcCompare[PercentSnow == 100 &
                          UncertType == "Ext" &
                          Calculator == "CRONUScalc" &
                          Location == "ULC", 
                        unique(WMean_SD) / 1000])]
# Exposure of BLC
CosmoWMeans[Location == "BLC", ':=' (
  Elevation_m = CalcCompare[Location == "LLC",
                            as.integer(round(mean(Elevation_m)))],
  WMeanAge = CalcCompare[PercentSnow == 100 &
                           UncertType == "Ext" &
                           Calculator == "CRONUScalc" &
                           Location == "LLC",
                         unique(WMean) / 1000],
  WMeanSD = CalcCompare[PercentSnow == 100 &
                          UncertType == "Ext" &
                          Calculator == "CRONUScalc" &
                          Location == "LLC",
                        unique(WMean_SD) / 1000])]
# Exposure of LD, PD, and LM
for (i in 3:5) {
  CosmoWMeans[Location == Be10Locations[i], ':=' (
    Elevation_m = CalcCompare[Location == Be10Locations[i],
                              as.integer(round(mean(Elevation_m)))],
    WMeanAge = CalcCompare[Location == Be10Locations[i] &
                             PercentSnow == 100 &
                             UncertType == "Ext" &
                             Calculator == "CRONUScalc",
                           max(WMean, na.rm = T) / 1000],
    WMeanSD = CalcCompare[Location == Be10Locations[i] &
                            PercentSnow == 100 &
                            UncertType == "Ext" &
                            Calculator == "CRONUScalc",
                          max(WMean_SD, na.rm = T) / 1000])]
}
# Exposure of Bishop Creek T4 moraine
CosmoWMeans[Location == "BCT4m", ':=' (
  Elevation_m = BishCreekAges[GroupNum == 1,
                              as.integer(round(mean(Elevation_m)))],
  WMeanAge = BishCreekAges[GroupNum == 1 &
                             UncertType == "Ext" &
                             SnowModel == "Phillips",
                           max(WMean, na.rm = T) / 1000],
  WMeanSD = BishCreekAges[GroupNum == 1 &
                            UncertType == "Ext" &
                            SnowModel == "Phillips",
                          max(WMeanSD, na.rm = T) / 1000])]
# Exposure of the Bishop Creek valley bottoms
CosmoWMeans[Location == "BCVB", ':=' (
  Elevation_m = BishCreekAges[GroupNum == 2,
                              as.integer(round(mean(Elevation_m)))],
  WMeanAge = BishCreekAges[GroupNum == 2 &
                             UncertType == "Ext" &
                             SnowModel == "Phillips" &
                             !is.na(WMean), 
                           unique(WMean) / 1000],
  WMeanSD = BishCreekAges[GroupNum == 2 &
                            UncertType == "Ext" &
                            SnowModel == "Phillips" &
                            !is.na(WMean), 
                          unique(WMeanSD) / 1000])]
# Exposure of HB
CosmoWMeans[Location == "HB", ':=' (
  Elevation_m = BishCreekAges[GroupNum == 3,
                              as.integer(round(mean(Elevation_m)))],
  WMeanAge = BishCreekAges[GroupNum == 3 &
                             UncertType == "Ext" &
                             SnowModel == "Phillips" &
                             !is.na(WMean), 
                           unique(WMean) / 1000],
  WMeanSD = BishCreekAges[GroupNum == 3 &
                            UncertType == "Ext" &
                            SnowModel == "Phillips" &
                            !is.na(WMean), 
                          unique(WMeanSD) / 1000])]
# Radiocarbon date for Highland Lakes
CosmoWMeans[Location == "HL", ':=' (
  Elevation_m = 2625L,
  WMeanAge = 15.94,
  WMeanSD  =  0.29)]
# Radiocarbon date for Greenstone Lake
CosmoWMeans[Location == "GL", ':=' (
  Elevation_m = 3091L,
  WMeanAge = 15.71,
  WMeanSD  =  0.12)]
# Radiocarbon date for East Lake
CosmoWMeans[Location == "EL", ':=' (
  Elevation_m = 2864L,
  WMeanAge = 16.12,
  WMeanSD  =  0.10)]


# Make plot backgrounds---------------------------------
startYear <- 20L
endYear <- 10L
barMin <- 16.0
barMax <- 16.5

# background color choice
bgColor <- "#00FF00"

# make cosmo background probability image 
yearIncrement <- 0.05
vectorOfYears <- seq.int(startYear, endYear, -yearIncrement)
# create DT
BCT4mBckgrnd_Elev <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = CosmoWMeans[Location == "BCT4m", WMeanAge],
  SDvalue = CosmoWMeans[Location == "BCT4m", WMeanSD],
  Elevation_m = seq(1500, 4000, 50),
  Prob = as.numeric(NA)))
# calclate probabilities
BCT4mBckgrnd_Elev[, Prob := dnorm(Year, meanValue, SDvalue)]

# Bishop Creek Tioga 4 probabilities for Swamp Lake figure
BCT4mBckgrnd_TOC <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = CosmoWMeans[Location == "BCT4m", WMeanAge],
  SDvalue = CosmoWMeans[Location == "BCT4m", WMeanSD],
  TOC = seq(-2, 30),
  Prob = as.numeric(NA)))
BCT4mBckgrnd_TOC[, Prob := dnorm(Year, meanValue, SDvalue)]

# Bishop Creek Tioga 4 probs for y axes with d18O
BCT4mBckgrnd_d18O <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = CosmoWMeans[Location == "BCT4m", WMeanAge],
  SDvalue = CosmoWMeans[Location == "BCT4m", WMeanSD],
  d18O = seq(-13, 5),
  Prob = as.numeric(NA)))
BCT4mBckgrnd_d18O[, Prob := dnorm(Year, meanValue, SDvalue)]

# Bishop Creek Tioga 4 probs for y axes with SST
BCT4mBckgrnd_SST <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = CosmoWMeans[Location == "BCT4m", WMeanAge],
  SDvalue = CosmoWMeans[Location == "BCT4m", WMeanSD],
  SST = seq(14, 31),
  Prob = as.numeric(NA)))
BCT4mBckgrnd_SST[, Prob := dnorm(Year, meanValue, SDvalue)]

# NGRIP prob background
BCT4mBckgrnd_NGRIP <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = CosmoWMeans[Location == "BCT4m", WMeanAge],
  SDvalue = CosmoWMeans[Location == "BCT4m", WMeanSD],
  d18O = seq(-44, -34),
  Prob = as.numeric(NA)))
BCT4mBckgrnd_NGRIP[, Prob := dnorm(Year, meanValue, SDvalue)]

# GHG prob background
BCT4mBckgrnd_GHG <- data.table(expand.grid(
  Year = vectorOfYears,
  meanValue = CosmoWMeans[Location == "BCT4m", WMeanAge],
  SDvalue = CosmoWMeans[Location == "BCT4m", WMeanSD],
  ppm = seq(175, 305, 5),
  Prob = as.numeric(NA)))
BCT4mBckgrnd_GHG[, Prob := dnorm(Year, meanValue, SDvalue)]


# MAKE Fig 13 - Regional Paleoclimate--------------------------------
# Cosmo and radiocarbon vs. elevation
Fig13A <- ggplot() +
  coord_cartesian(xlim = c(endYear, startYear), 
                  ylim = c(1750, 3750),
                  expand = FALSE) +
  geom_raster(data = BCT4mBckgrnd_Elev,
              mapping = aes(x = Year, y = Elevation_m, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = "#FFFFFF") +
  # geom_rect(mapping = aes(xmin = barMin, xmax = barMax,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "#bcbddc", color = "#FFFFFF") +
  geom_errorbarh(data = CosmoWMeans,
                 mapping = aes(x = WMeanAge,
                               xmin = WMeanAge - WMeanSD,
                               xmax = WMeanAge + WMeanSD,
                               y = Elevation_m)) +
  geom_errorbarh(data = OldMan[Sample_Name != "BB-1B" &
                                 Sample_Name != "97-40" &
                                 UncertType == "Ext"],
                 mapping = aes(x = Age_B1950 / 1000,
                               xmin = (Age_B1950 - Uncert) / 1000,
                               xmax = (Age_B1950 + Uncert) / 1000,
                               y = Elevation_m)) +
  geom_point(data = OldMan[Sample_Name != "BB-1B" &
                             Sample_Name != "97-40" &
                             UncertType == "Ext"],
             mapping = aes(x = Age_B1950 / 1000,
                           y = Elevation_m),
             shape = 22, fill = "red") +
  geom_point(data = CosmoWMeans[SampleType == "Be10"],
             mapping = aes(x = WMeanAge, y = Elevation_m),
             shape = 21L, fill = "blue") +
  geom_point(data = CosmoWMeans[SampleType == "Cl36"],
             mapping = aes(x = WMeanAge, y = Elevation_m),
             shape = 21, fill = "#00FF00") +
  geom_point(data = CosmoWMeans[SampleType == "C14"],
             mapping = aes(x = WMeanAge, y = Elevation_m),
             shape = 24, fill = "yellow") +
  scale_x_continuous(trans = "reverse", limits = c(startYear, endYear),
                     expand = c(0, 0), breaks = seq(startYear, endYear),
                     name = "Age (ka)",
                     sec.axis = sec_axis(~., breaks = seq(20, 10, -1),
                                         labels = NULL)) +
  scale_y_continuous(
    name = "Elevation (m)",
    sec.axis = sec_axis(~., name = "Elevation (m)")) +
  publicationTheme

# Plot Swamp Lake Figure
Fig13B <- ggplot()  +
  geom_rect(mapping = aes(xmin = barMin, xmax = barMax,
                          ymin = -Inf, ymax = +Inf),
            fill = "#bcbddc", color = "#FFFFFF") +
  geom_raster(data = BCT4mBckgrnd_TOC,
              mapping = aes(x = Year, y = TOC, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  coord_cartesian(xlim = c(startYear, endYear), 
                  ylim = c(-2, 30)) +
  scale_x_continuous(name = "Age (ka)", trans = "reverse",
                     expand = c(0, 0), 
                     breaks = seq(startYear, endYear, -1),
                     sec.axis = 
                       sec_axis(~., breaks = seq(startYear, endYear),
                                         labels = NULL)) +
  scale_y_continuous(name = "Swamp Lake TOC (%C)",
                     sec.axis = sec_axis(
                       trans = ~(.-(minTOC))*(MagRange/TOCrange)+minMag,
                       name = "Magnetic Susceptibility"),
                     limits = c(-2, 30),
                     expand = c(0, 0)) +
  geom_line(data = paleo[!is.na(SwampTOC)],
            mapping = aes(x = Year / 1000, y = SwampTOC),
            color = "black") +
  geom_line(data = paleo[!is.na(SwampMag_TOCunits)],
            mapping = aes(x = Year / 1000, y = SwampMag_TOCunits),
            color = "blue") +
  geom_errorbarh(data = SwampLakeAges[Start1SD_calYr > 10000],
                 mapping = aes(x = MedianAge_calYr / 1000,
                               xmin = Start1SD_calYr / 1000,
                               xmax = End1SD_calYr / 1000,
                               y = -1)) +
  geom_point(data = SwampLakeAges[Start1SD_calYr > 10000],
             mapping = aes(x = MedianAge_calYr / 1000,
                           y = -1),
             shape = 24, color = "black", fill = "yellow") +
  publicationTheme

# Fort Stanton and Cave of the Bells
Fig13C <- ggplot() +
  geom_raster(data = BCT4mBckgrnd_d18O,
              mapping = aes(x = Year, y = d18O, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  # # add rectangle for YD
  # # changed start from 12.9 to 12.8 so it doesn't overlap with B/A
  # geom_rect(mapping = aes(xmin = 11.7, xmax = 12.8,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "light blue") +
  # # add rectangle for B/A
  # # changed end from 12.7 to 12.8 so it doesn't overlap with YD
  # geom_rect(mapping = aes(xmin = 12.8, xmax = 14.7,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "pink") +
  geom_line(data = speleo[!is.na(FortStanton.d18O)],
            mapping = aes(x = Years_B1950 / 1000,
                          y = FortStanton.d18O)) +
  geom_line(data = speleo[!is.na(CofTheB_scaledTo_FS)],
            mapping = aes(x = Years_B1950 / 1000,
                          y = CofTheB_scaledTo_FS),
            color = "blue") +
  coord_cartesian(xlim = c(startYear, endYear)) +
  scale_x_continuous(name = NULL, trans = "reverse",
                     breaks = seq(startYear, endYear, -1),
                     expand = c(0, 0),
                     sec.axis = 
                       sec_axis(~., breaks = seq(startYear, endYear, -1),
                                labels = NULL)) +
  scale_y_continuous(name = "Fort Stanton d18O (o/oo)",
                     sec.axis = sec_axis(
                       trans = ~(.-(FSmin))*(CBrange/FSrange)+CBmin,
                       name = "Cave of the Bells d18O (o/oo)"),
                     limits = c(-12, -5), expand = c(0, 0)) +
  publicationTheme

# plot local caves
Fig13D <- ggplot()  +
  geom_raster(data = BCT4mBckgrnd_d18O,
              mapping = aes(x = Year, y = d18O, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  # geom_rect(mapping = aes(xmin = barMin, xmax = barMax,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "#bcbddc", color = "#FFFFFF") +
  coord_cartesian(xlim = c(startYear, endYear),
                  ylim = c(-9.5, -5.5)) +
  scale_x_continuous(
    name = "Age (ka)", trans = "reverse", expand = c(0, 0),
    breaks = seq(startYear, endYear, -1),
    sec.axis = sec_axis(~., breaks = seq(startYear, endYear, -1),
                                         labels = NULL)) +
  scale_y_continuous(
    name = "Moaning Cave d18O (per mil)",
    sec.axis = sec_axis(~(.-(MoanMin))*(McLRange/MoanRange)+McLMin, 
                 name = "McLean Cave d18O (per mil)")) +
  geom_line(data = localCaves[!is.na(McLeanToMoaning.d18O)],
            mapping = aes(x = Years_B1950 / 1000, y = McLeanToMoaning.d18O),
            color = "blue") +
  geom_line(data = localCaves[!is.na(Moaning.d18O)],
            mapping = aes(x = Years_B1950 / 1000, y = Moaning.d18O),
            color = "black") +
  publicationTheme

# plot Santa Barbara Basin d18O
Fig13E <- ggplot()  +
  geom_raster(data = BCT4mBckgrnd_d18O,
              mapping = aes(x = Year, y = d18O, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  # geom_rect(mapping = aes(xmin = barMin, xmax = barMax,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "#bcbddc", color = "#FFFFFF") +
  coord_cartesian(xlim = c(startYear, endYear), 
                  ylim = c(4, -1)) +
  scale_x_continuous(
    name = "Age (ka)", trans = "reverse", expand = c(0, 0),
    breaks = seq(startYear, endYear),
    sec.axis = sec_axis(~., breaks = seq(startYear, endYear, -1),
                        labels = NULL)) +
  scale_y_continuous(
    name = "Santa Barbara bull. d18O",
    limits = c(4, -1), expand = c(0, 0),
    sec.axis = sec_axis(~., name = "Santa Barbara bull. d18O"),
    trans = "reverse") +
  geom_line(data = paleo[!is.na(Hendy02.bull.d18O) & Year > 16000],
            mapping = aes(x = Year / 1000, y = Hendy02.bull.d18O)) +
  geom_line(data = paleo[!is.na(Hendy02.bull.d18O) & 
                           Year < 16000 & Year > 10000],
            mapping = aes(x = Year / 1000, y = Hendy02.bull.d18O)) +
  # geom_line(data = paleo[!is.na(Hendy02.bull.d13C) & Year > 16000],
  #           mapping = aes(x = Year / 1000, y = Hendy02.bull.d13C + 4),
  #           color = "#1f78b4") +
  # geom_line(data = paleo[!is.na(Hendy02.bull.d13C) &
  #                          Year < 16000 & Year > 10000],
  #           mapping = aes(x = Year / 1000, y = Hendy02.bull.d13C + 4),
  #           color = "#1f78b4") +
  publicationTheme

# Records from the Gulf of CA
Barron <- fread("Barron2003_ODP1019_UK37_SST.txt")
Barron[SST_C == -999, SST_C := NA]

Fig13F <- ggplot()  +
  geom_raster(data = BCT4mBckgrnd_SST,
              mapping = aes(x = Year, y = SST, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  # geom_rect(mapping = aes(xmin = barMin, xmax = barMax,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "#bcbddc", color = "#FFFFFF") +
  coord_cartesian(xlim = c(startYear, endYear),
                  ylim = c(15, 30)) +
  scale_x_continuous(
    name = "Age (ka)", trans = "reverse", expand = c(0, 0),
    breaks = seq(startYear, endYear, -1),
    sec.axis = sec_axis(~., breaks = seq(startYear, endYear, -1),
                        labels = NULL)) +
  scale_y_continuous(
    name = "Gulf of CA SST (C)", 
    sec.axis = sec_axis(~., name = "Gulf of CA SST (C)")) +
  geom_line(data = paleo[!is.na(UK37)],
            mapping = aes(x = Year / 1000, y = UK37)) +
  # geom_line(data = paleo[!is.na(Opal27N_UK37units)],
  #           mapping = aes(x = Year / 1000, y = Opal27N_UK37units),
  #           color = "blue") +
  # geom_line(data = paleo[!is.na(TEXH86)],
  #           mapping = aes(x = Year / 1000, y = TEXH86),
  #           color = "#e41a1c") +
  # geom_line(data = Barron[!is.na(SST_C)],
  #           mapping = aes(x = Years_B1950 / 1000, y = SST_C),
  #           color = "blue") +
  publicationTheme

# NGRIP
Fig13G <- ggplot() +
  geom_raster(data = BCT4mBckgrnd_NGRIP,
             mapping = aes(x = Year, y = d18O, fill = Prob),
             vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  # # add rectangle for YD
  # # changed start from 12.9 to 12.8 so it doesn't overlap with B/A
  # geom_rect(mapping = aes(xmin = 11.7, xmax = 12.8,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "light blue") +
  # # add rectangle for B/A
  # # changed end from 12.7 to 12.8 so it doesn't overlap with YD
  # geom_rect(mapping = aes(xmin = 12.8, xmax = 14.7,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "pink") +
  geom_line(data = NGRIPrunning,
            mapping = aes(x = midStep / 1000,
                          y = d18O)) +
  coord_cartesian(xlim = c(startYear, endYear),
                  ylim = c(-45, -35)) +
  scale_x_continuous(
    name = "Age (ka)", trans = "reverse",
    breaks = seq(startYear, endYear, -1), expand = c(0, 0),
    sec.axis = sec_axis(~., breaks = seq(startYear, endYear, -1),
                        labels = NULL)) +
  scale_y_continuous(
    name = "NGRIP d18O",
    sec.axis = sec_axis(~., name = "NGRIP d18O"),
    expand = c(0, 0)) +
  # geom_line(data = NGRIPrunning,
  #           mapping = aes(x = midStep / 1000,
  #                         y = Dust_d18Ounits),
  #           color = "blue") +
  # geom_ribbon(data = IRD,
  #             mapping = aes(x = Age_kyr,
  #                           ymin = -50,
  #                           ymax = IRDscaled)) +
  publicationTheme

# Greenhouse Gases
Fig13H <- ggplot() +
  geom_raster(data = BCT4mBckgrnd_GHG,
              mapping = aes(x = Year, y = ppm, fill = Prob),
              vjust = 0, hjust = 1, interpolate = T) +
  scale_fill_gradient(low = "#FFFFFF", high = bgColor) +
  # geom_rect(mapping = aes(xmin = 16.0, xmax = 16.5,
  #                         ymin = -Inf, ymax = +Inf),
  #           fill = "#bcbddc", color = "#FFFFFF") +
  coord_cartesian(xlim = c(startYear, endYear), 
                  ylim = c(180, 300)) +
  scale_x_continuous(
    name = "Age (ka)", trans = "reverse",
    breaks = seq(startYear, endYear, -1), expand = c(0, 0),
    sec.axis = sec_axis(~., breaks = seq(startYear, endYear, -1),
                        labels = NULL)) +
  scale_y_continuous(
    name = "CO2 (ppm)", limits = c(180, 300),
    sec.axis = sec_axis(
      trans = ~(.-(minCO2))*(MethaneRange/CO2range)+minMethane,
      name = "Methane (ppb)"), expand = c(0, 0)) +
  geom_line(data = paleo[!is.na(CO2)],
            mapping = aes(x = Year / 1000, y = CO2),
            color = "black") +
  geom_line(data = paleo[!is.na(Methane_CO2units)],
            mapping = aes(x = Year / 1000, y = Methane_CO2units),
            color = "blue") +
  publicationTheme

# assemble the figure
# leave out the Gulf of CA for now
ggsave("Fig 2-13 Paleoclimate Assemblage.eps",
       ggarrange(Fig13A, Fig13E, Fig13B, Fig13F, 
                 Fig13C, Fig13G, Fig13D, Fig13H, ncol = 2),
       width = unit(6.6, "in"),
       height = unit(8.3 * (4/4), "in"))


"End of script"