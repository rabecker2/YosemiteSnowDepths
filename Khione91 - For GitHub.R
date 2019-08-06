# The name of this script is Khione.  It's goal is to estimate the average
# snow depth and density in the Sierra Nevada Mtns by elevation since the
# last deglaciation.

# SET OPTIONS--------------------------------------------------------------
options(scipen = 6L, digits = 9L)


# install and load packages------------------------------------------------

# data.table for the fast loading and analysis of large datasets
# install.packages("data.table")
library(data.table)

# tidyr for the cleaning data
# install.packages("tidyr")
library(tidyr)

# lubridate for easy manipulation of dates and times
# install.packages("lubridate")
library(lubridate)

# dplyr for the easy display of R data
# install.packages("dplyr")
library(dplyr)

# MASS for the truehist function, which produces a normalized histogram
# install.packages("MASS")
library(MASS)

# cars for access to the qqPlot function, to graphically test for
# Gaussian distribution.
# install.packages("car")
library(car)

# ggplot2 for its advanced plotting capacity
# install.packages("ggplot2")
library(ggplot2)

# broom for tidying model objects
# install.packages("broom")
library(broom)

# FUNCTIONS DEFINED HERE---------------------------------------------------

adj_biased_SWE <- function(DT, Station_ID, trusted, biased,
                           repository = Sierra_SWE, n = 1000L) {
  # Purpose:
  #   Adjusts biased SWE measurements (in the statistical sense) into
  #   alignment with more trusted SWE measurements.
  #
  # Args:
  #   DT: A data.table containing a column of trusted measurements and a
  #       column of biased measurements.  The trusted column typical
  #       contains daily measurements of SWE that have been averaged into
  #       monthly values while the biased column contains older
  #       measurements that reflect just a single measurement at some
  #       point in a particular month.
  #   Station_ID: A 3-letter abbrevation for the station location.
  #   trusted:  The name of the column of trusted SWE measurements.
  #   biased:   The name of the column containing the biased measurements.
  #   repository: The data.table that will accumulate the average
  #               difference and SD between the daily and monthly data for
  #               each month. This DT should have already been imported
  #               and should contain a populated column of Station IDs.
  #   n:        The number of iterations to run through the random number
  #             generator, in calculating the new, adjusted SWE.
  #             Default = 1000.
  #
  # Returns:
  #   An updated data.table with a new column, "Adjusted_SWE." 
  #
  #   The function finds the difference between the trusted and biased 
  #   measurements, in their timespan of overlap, and then calculates the
  #   mean and sd of the difference for each month.  The average difference
  #   is then added to each biased measurement, to bring it into alignment
  #   with the trusted measurements.  Because this process can result in
  #   adjusted SWEs of less than zero (not possible), the script generates
  #   a series of random numbers (default = 1000) with the mean and sd of
  #   the difference for each month, assuming a normal distribution.  The
  #   qqPlots support this as a reasonable assumption. Values of less than
  #   zero in this series are changed to zero and then the mean of this
  #   series is calculated.  The new SWE is the mean of this series, unless
  #   there is a trusted measurement for a month, in which case the
  #   trusted value is recorded as the SWE.
  #
  # Error checking:
  if (!is.data.table(DT)) {
    stop("DT needs to be a data.table.", call. = FALSE)
  }
  if (!is.data.table(repository)) {
    stop("'repository' needs to be a data.table.", call. = FALSE)
  }
  if (nchar(Station_ID) != 3L) {
    stop("The Station_ID must be three letters long.", call. = FALSE)
  }
  # Main body:
  # find individual differences
  DT[, Difference := .(get(as.character(trusted)) -
                         get(as.character(biased)))]
  # construct a table of differences
  start_yr <- DT[!is.na(Difference), min(Year)]
  end_yr   <- DT[!is.na(Difference), max(Year)]
  tbl_of_diffs <- data.table(Year = seq(start_yr, end_yr, 1L))
  first_mo <- min(DT[Year >= start_yr & Year <= end_yr &
                    !is.na(get(as.character(biased))), Month])
  last_mo  <- max(DT[Year >= start_yr & Year <= end_yr &
                       !is.na(get(as.character(biased))), Month])
  for (i in first_mo:last_mo) {
    heading <- month.name[i]
    tbl_of_diffs[, (heading) := 
                DT[Year >= start_yr & Year <= end_yr & Month == i,
                   Difference]]
  }
  # output the table of differences as a tab delimited text file
  fwrite(tbl_of_diffs,
         as.character(paste0(Station_ID, "_Differences.txt")), 
         sep = "\t")
  # plot differences versus year for each month
  x <- tbl_of_diffs[, Year]
  lower_ylim <- min(tbl_of_diffs[, lapply(.SD, min, na.rm = TRUE),
                                 .SDcols = 2:length(tbl_of_diffs)])
  upper_ylim <- max(tbl_of_diffs[, lapply(.SD, max, na.rm = TRUE),
                                 .SDcols = 2:length(tbl_of_diffs)])
  # convert ylim into cm
  lower_ylim <- floor(lower_ylim * 2.54)
  upper_ylim <- ceiling(upper_ylim * 2.54)
  # round to the next 10's position, up or down, depending...
  lower_ylim2 <- round(lower_ylim, -1)
  if (lower_ylim2 < lower_ylim) {
    lower_ylim <- lower_ylim2
  } else {
    lower_ylim <- lower_ylim2 - 10L
  }
  upper_ylim2 <- round(upper_ylim, -1)
  if (upper_ylim2 > upper_ylim) {
    upper_ylim <- upper_ylim2
  } else {
    upper_ylim <- upper_ylim2 + 10L
  }
  # set up tiff call
  figure_length <- (last_mo - 1L) * 1.7
  tiff(as.character(paste0(Station_ID, " Figure.tif")),
       compression = "none",
       width = 6.3, height = figure_length, units = "in",
       res = 144)
  nrows <- last_mo - 1L
  par(mfcol = c(nrows, 2))
  fig_margins <- c(0.3, 0.6, 0.25, 0.1)
  for (i in 2:last_mo) {
    month_i <- month.name[i]
    y <- tbl_of_diffs[, get(as.character(month_i))]
    # convert y to cm
    y <- y * 2.54
    # plot differences and omit x axis label
    par(mai = fig_margins)
    plot(x, y, type = "o",
         main = month_i,
         xlab = "", ylab = "Difference (cm)",
         ylim = c(lower_ylim, upper_ylim),
         pch = 15, las = 1)
      abline(h = 0L, lty = 2, lwd = 2)
  }
  # make qqPlots of each month's differences
  for (i in 2:last_mo) {
    month_i <- month.name[i]
    x <- tbl_of_diffs[, get(as.character(month_i))]
    # convert x to cm
    x <- x * 2.54
    # plot qqPlots with no x axis labels
    par(mai = fig_margins)
    qqPlot(x, main = as.character(month_i),
           ylab = "Difference (cm)",
           xlab = "", las = 1, cex = 1.5)
  }
  dev.off()
  # find average differences by month, sd is SAMPLE standard deviation
  DT[, ':=' (Avg_diff_inches = mean(Difference, na.rm = TRUE),
             Avg_diff_SD     = sd  (Difference, na.rm = TRUE),
             Avg_diff_n      = sum(!is.na(Difference))),
                keyby = .(Month)]
  # record Avg_diff_inches, Avg_diff_SD, and Avg_diff_n
  #  in the repository DT
  for (i in first_mo:last_mo) {
    month_i <- month.name[i]
    mean_value <- mean(tbl_of_diffs[, get(as.character(month_i))],
                       na.rm = TRUE)
    value_SD   <-   sd(tbl_of_diffs[, get(as.character(month_i))],
                       na.rm = TRUE)
    value_n    <- tbl_of_diffs[, sum(!is.na(get(as.character(month_i))))]
    sta_name <- Station_ID
    repository[Station_ID == sta_name & Month == month_i,
               ':=' (Avg_diff_inches = mean_value,
                     Avg_diff_SD = value_SD,
                     Avg_diff_n = value_n)]
  }
  # calculate adjustment
  DT[, Adjusted_SWE := as.numeric(NA)]
  for (i in 1:DT[, .N]) {
    trusted_SWE <- DT[i, get(as.character(trusted))]
    # if there's a "trusted" value, use it.
    if (!is.na(trusted_SWE)) {
      DT[i, Adjusted_SWE := trusted_SWE]
      next()
    }
    old_SWE <- DT[i, get(as.character(biased))]
    # if there's no "biased" value, there's nothing to adjust
    if (is.na(old_SWE)) {
      next()
    }
    # already know "trusted" *is* NA and "biased" *isn't* NA... test if
    # there's a correction.  If there isn't, just apply the "biased"
    # measurement as is.
    if (is.na(DT[i, Avg_diff_inches]) & is.na(DT[i, Avg_diff_SD])) {
      DT[i, Adjusted_SWE := old_SWE]
      next()
    }
    # if the calibration/adjustment consists of only a single measurement,
    # so there's an Avg_diff but no SD, just apply the Avg_diff
    # correction.
    if (!is.na(DT[i, Avg_diff_inches]) & is.na(DT[i, Avg_diff_SD])) {
      DT[i, Adjusted_SWE := old_SWE + Avg_diff_inches]
      if (DT[i, Adjusted_SWE < 0]) {
        DT[i, Adjusted_SWE := 0]
      }
      next()
    }
    temp <- data.table(Adjustment =
              rnorm(n, DT[i, Avg_diff_inches], DT[i, Avg_diff_SD]))
    temp[, Adjustment := old_SWE + Adjustment]
    temp[Adjustment < 0, Adjustment := 0]
    new_SWE <- mean(temp[, Adjustment])
    set(DT, i, "Adjusted_SWE", new_SWE)
  }
}

est_missing_SWE <- function(DT, Station_ID, SWE = "Adjusted_SWE",
                            first_mo = 2L, last_mo = 4L) {
  # Purpose:
  #   Predict missing SWEs in a DT from the relationship between the
  #   months that do have data.
  #
  # Args:
  #   DT: A data.table containing the following columns: "Year", "Month",
  #       and a column of snow water equivalent measurements.
  #   Station_ID: A 3-letter abbrevation for the station location.
  #   SWE:  Name of the snow water equivalent column.
  #   first_mo: The first month that should be used as a perdictor of
  #             other months. (Default = February)
  #   last_mo:  The last month that should be used as a predictor of
  #             other months. (Default = April)
  #
  # Returns:
  #   An updated DT with a column of final estimated SWEs.
  #
  #   This script ingests a data.table and two months, first_mo and last_mo.
  #   The months between these two months, inclusive, are used to predict
  #   the SWE values of the other months.  For example, in Tuolumne Meadows
  #   there's data for every month starting in December, 1979, but for only
  #   some Februarys, Marchs, Aprils, and Mays between 1930 and 1979.  The
  #   goal of this script is to use the relationships between these 4
  #   winter/spring months and the other months in order to predict the
  #   missing values between 1930 and 1979.
  #
  # Error checking:
  if (!is.data.table(DT)) {
    stop("DT needs to be a data.table.", call. = FALSE)
  }
  if (!is.integer(first_mo)) {
    stop("'first_mo' needs to be an integer.", call. = FALSE)
  }
  if (!is.integer(last_mo)) {
    stop("'last_mo' needs to be an integer.", call. = FALSE)
  }
  if (first_mo <= 0L | last_mo <= 0L | first_mo >= 13L |
      last_mo >= 13L) {
    stop("'first_mo' and 'last__est_mo' need to be between 1L and 12L",
         call. = FALSE)
  }
  if (last_mo < first_mo) {
    stop("first_mo must be less than last_mo.", call. = FALSE)
  }
  if ("Year" %in% names(DT) == FALSE) {
    stop("The DT needs a 'Year' column.", call. = FALSE)
  }
  if ("Month" %in% names(DT) == FALSE) {
    stop("The DT needs a 'Month' column.", call. = FALSE)
  }
  if (nchar(Station_ID) != 3L) {
    stop("The Station_ID must be three letters long.", call. = FALSE)
  }
  # Main body:
  # create a local copy of the relevant columns from DT
  DTlocal <- data.table(Year  = DT[, as.integer(Year)], 
                        Month = DT[, Month], 
                        SWE   = DT[, get(as.character(SWE))])
  # add water years and months, to make the identifying of relevant months
  # eaiser. The water year runs from the beginning of October through the
  # following September.
  DTlocal[Month >= 10L, ':=' (Water_Year  = Year + 1L,
                              Water_Month = Month - 9L)]
  DTlocal[Month < 10L, ':=' (Water_Year = Year,
                             Water_Month = Month + 3L)]
  setkey(DTlocal, "Water_Year", "Water_Month")
  # Delete columns "Year" and "Month"
  DTlocal[, c("Year", "Month") := NULL]
  # Expand Water_Month into columns
  DTlocal <- spread(DTlocal, Water_Month, SWE)
  # Reorder columns to get water month 1 in the first column, water month 2
  # in the second column, etc.
  setcolorder(DTlocal, c(1:12, "Water_Year"))
  # Create water month conversion table
  WM_Conversion <- data.table(Water_Month = 1:12,
                    Name = c("Oct", "Nov", "Dec", "Jan", "Feb",
                             "Mar", "Apr", "May", "Jun", "Jul",
                             "Aug", "Sep"))
  setnames(DTlocal, as.character(WM_Conversion[, Water_Month]),
           WM_Conversion[, Name])
  # convert the input months to water months
  first_mo <- first_mo + 3L
  last_mo  <- last_mo  + 3L
  # 
  # create the lookup table
  n <- (last_mo - first_mo + 1L) * 11L
  lookup <- data.table(Predictor_Mo = rep(as.integer(NA), n),
                       Predicted_Mo = rep(as.integer(NA), n),
                       Faktor       = rep(as.numeric(NA), n),
                       Variance     = rep(as.numeric(NA), n))
  # Discover relationships between months, populate lookup table
  n <- 1L
  for (i in first_mo:last_mo) {
    for (j in 1:12) {
      if (i == j) {
        next()
      }
      month_i <- WM_Conversion[i, Name]
      month_j <- WM_Conversion[j, Name]
      results <- DTlocal[(!is.na(get(month_i)) & !is.na(get(month_j))),
                        (get(month_j)) / (get(month_i))]
      lookup[n, ':=' (Predictor_Mo = i,
                      Predicted_Mo = j,
                      Faktor   = mean(results[is.finite(results)]),
                      Variance = var(results[is.finite(results)]))]
      n <- n + 1L
      }
  }
  # create a copy of the lookup table with month names for output
  output <- data.table(lookup)
  for (i in first_mo:last_mo) {
    output[Predictor_Mo == i,
           Predictor_Month := WM_Conversion[i, Name]]
  }
  for (j in 1:12) {
    output[Predicted_Mo == j,
           Predicted_Month := WM_Conversion[j, Name]]
  }
  output[, c("Predictor_Mo", "Predicted_Mo") := NULL]
  setcolorder(output, c("Predictor_Month", "Predicted_Month", "Faktor",
                             "Variance"))
  # output "output" as a tab-delimited text file
  fwrite(output, as.character(paste0(Station_ID, "_Lookup.txt")),
         sep = "\t")
  # Create data.tables to host the estimated values
  for (i in first_mo:last_mo) {
    month_i <- WM_Conversion[i, Name]
    assign(paste0(month_i, "_predictions"), 
           data.table(Oct = as.numeric(NA),
                      Nov = as.numeric(NA),
                      Dec = as.numeric(NA),
                      Jan = as.numeric(NA),
                      Feb = as.numeric(NA),
                      Mar = as.numeric(NA),
                      Apr = as.numeric(NA),
                      May = as.numeric(NA),
                      Jun = as.numeric(NA),
                      Jul = as.numeric(NA),
                      Aug = as.numeric(NA),
                      Sep = as.numeric(NA),
                      Water_Year = DTlocal[, Water_Year]))
    }
  # Fill tables with estimated values
  for (i in first_mo:last_mo) {
    month_i <- WM_Conversion[i, Name]
    for (j in 1:12) {
      if (i == j) {
        next()
      }
      month_j <- WM_Conversion[j, Name]
      Faktor  <- lookup[Predictor_Mo == i & Predicted_Mo == j, Faktor]
      results <- Faktor * DTlocal[, .(get(as.character(month_i)))]
      get(paste0(month_i, "_predictions"))[, (j) := results]
    }
  }
  # output prediction tables with estimated values
  for (i in first_mo:last_mo) {
    month_i <- WM_Conversion[i, Name]
    fwrite(get(paste0(month_i, "_predictions")),
      as.character(paste0(Station_ID, "_", month_i, "_predictions.txt")),
      sep = "\t")
  }
  # Create a "Best_estimates" DT
  Best_estimates <- data.table(Oct = as.numeric(NA),
                               Nov = as.numeric(NA),
                               Dec = as.numeric(NA),
                               Jan = as.numeric(NA),
                               Feb = as.numeric(NA),
                               Mar = as.numeric(NA),
                               Apr = as.numeric(NA),
                               May = as.numeric(NA),
                               Jun = as.numeric(NA),
                               Jul = as.numeric(NA),
                               Aug = as.numeric(NA),
                               Sep = as.numeric(NA),
                               Water_Year = DTlocal[, Water_Year])
  # Populate "Best_estimates" with values
  # create a file for a particular month
  for (j in 1:12) {
    month_j <- WM_Conversion[j, Name]
    assign(paste0(month_j, "_estimated"), 
           data.table(Water_Year = DTlocal[, Water_Year]))
    # merge all the estimates for a particular month into a single DT
    for (i in first_mo:last_mo) {
      month_i <- WM_Conversion[i, Name]
      get(paste0(month_j, "_estimated"))[, (month_i) 
        := get(paste0(month_i, "_predictions"))
        [, .(get(as.character(month_j)))]]
    }
    assign(paste0(month_j, "_estimated"), as.data.table(gather(
      get(paste0(month_j, "_estimated")), Month, Est_SWE, -Water_Year)))
    setkey(get(paste0(month_j, "_estimated")), "Water_Year")
    # create a column for inverse variance and populate it
    for (i in first_mo:last_mo) {
      if (i == j) {
        next()
      }
      month_i <- WM_Conversion[i, Name]
      get(paste0(month_j, "_estimated"))[Month == (month_i), 
        Inv_Var :=
          lookup[Predictor_Mo == i & Predicted_Mo == j, 1 / Variance]]
    }
    # calculate the inverse-variance weighted average of the SWE estimates
    estimates <- get(paste0(month_j, "_estimated"))[, 
      weighted.mean(Est_SWE, Inv_Var, na.rm = TRUE), by = Water_Year]
    setkey(estimates, "Water_Year")
    # store the results in a "Best_estimates" DT
    Best_estimates[, (month_j) := estimates[, V1]]
  }
  # output Best_estimates file
  fwrite(Best_estimates,
         as.character(paste0(Station_ID, "_Best_estimates.txt")),
         sep = "\t")
  # merge results into DT
  Best_estimates <- as.data.table(gather(Best_estimates, Month,
                                         Est_SWE, -Water_Year))
  # convert "Month" from abbreviation to integer
  Cal_Mo <- data.table(Number = 1:12,
                       Name = c("Jan", "Feb", "Mar", "Apr", "May",
                                "Jun", "Jul", "Aug", "Sep", 
                                "Oct", "Nov", "Dec"))
  n <- Best_estimates[, .N]
  Best_estimates[, Cal_Month := rep(as.integer(NA), n)]
  for (j in 1:12) {
    month_j <- Cal_Mo[j, Name]
    Best_estimates[Month == (month_j), Cal_Month := j]
  }
  # delete "Month" (abbreviations), rename "Cal_Month" to "Month",
  # reorder columns
  Best_estimates[, Month := NULL]
  setnames(Best_estimates, "Cal_Month", "Month")
  # convert "Water_Year" to calendar "Year"
  Best_estimates[Month >= 10L, Year := (Water_Year - 1L)]
  Best_estimates[Month <= 9L , Year := Water_Year]
  # delete "Water_Year" and reorder columns
  Best_estimates[, Water_Year := NULL]
  setcolorder(Best_estimates, c("Year", "Month", "Est_SWE"))
  setkey(Best_estimates, "Year", "Month")
  # merge "Est_SWE" into the original DT
  DT <- DT[Best_estimates, on = c("Year", "Month")]
  # create "Final_SWE_Inches" column and populate it
  DT[, Final_SWE_Inches := Est_SWE]
  DT[!is.na(Adjusted_SWE), Final_SWE_Inches := Adjusted_SWE]
  # return original DT with column of final SWE
  return(DT)
}

monthly_to_final <- function(monthly, Station_ID, repository = Sierra_SWE,
                             monthly_diffs = all_monthly_diffs,
                             n = 1000L) {
  # Purpose:
  #   Calculates the average snow water equivalent for every month in a
  #   monthly record.
  #
  # Args:
  #   monthly: A two column data.table, with one column labeled "Date" and
  #            the other "SWE_Inches".
  #   Station_ID: A 3-letter abbrevation for the station location.
  #   repository: The data.table that will accumulate the average SWE for
  #               all months and all stations.  This DT should already have
  #               been imported and should contain a populated column of
  #               Station IDs.
  #   monthly_diffs: A data.table hosting all the individual differences
  #                  between the daily and monthly data. For simplicity in
  #                  this particular application, these data were gathered
  #                  in Excel from the indiviual "_Differences" files and
  #                  exported.
  #   n: The number of iterations to run through the random number
  #      generator, in calculating the new, adjusted SWE. Default = 1000. 
  #
  # Returns:
  #   Returns the repository DT with the average SWE for each month in the
  #   monthly record.
  #
  # Error checking:
  if (!is.data.table(monthly)) {
    stop("'monthly' needs to be a data.table.", call. = FALSE)
  }
  if (!is.data.table(repository)) {
    stop("'repository' needs to be a data.table.", call. = FALSE)
  }
  if (!is.data.table(monthly_diffs)) {
    stop("'monthly_diffs' needs to be a data.table.", call. = FALSE)
  }
  if ("Date" %in% names(monthly) == FALSE) {
    stop("The DT needs a 'Date' column.", call. = FALSE)
  }
  if ("SWE_Inches" %in% names(monthly) == FALSE) {
    stop("'monthly' needs a 'SWE_Inches' column.", call. = FALSE)
  }
  if (nchar(Station_ID) != 3L) {
    stop("The Station_ID must be three letters long.", call. = FALSE)
  }
  # Main body:
  # convert negative SWEs to NA (if any present)
  if (monthly[SWE_Inches < 0, .N] > 0L) {
    monthly[SWE_Inches < 0, SWE_Inches = NA]
  }
  # create new columns, Year and Month
  monthly[, c("Year", "Month") := .(year(Date), month(Date))]
  # reorder columns to (Date, Year, Month, SWE_Inches)
  setcolorder(monthly, c("Date", "Year", "Month", "SWE_Inches"))
  # set a two-column key, Year and Month
  setkey(monthly, Year, Month)
  # delete "Date" column
  monthly[, Date := NULL]
  monthly_avg <- monthly[, mean(SWE_Inches, na.rm = TRUE), by = "Month"]
  setkey(monthly_avg, "Month")
  monthly_avg[, Month := month.name]
  # the monthly data being processed here doesn't have a daily counterpart.
  # where daily counterparts are available there are consistent differnces
  #   between the daily data, averaged into months, and the monthly data
  #   (see graphs). To adjust the monthly data here into better alignment
  #   with the other data, I'm going to average all the differences (from
  #   all the stations with both daily and monthly data) for a particular
  #   month and then apply that average adjustment to every monthly record
  #   without a daily counterpart.
  avg_diff <- monthly_diffs[, lapply(.SD, mean, na.rm = TRUE), 
                            .SDcols = month.name[1:5]]
  SD <-       monthly_diffs[, lapply(.SD, sd, na.rm = TRUE), 
                            .SDcols = month.name[1:5]]
  # gather the wide DTs into long DTs
  avg_diff <- as.data.table(gather(avg_diff, Month, SWE_Adjustment))
  SD <- as.data.table(gather(SD, Month, SD))
  # apply adjustment to monthly data
  for (i in 1:5) {
    mean_val <- avg_diff[i, SWE_Adjustment]
    sd_val <- SD[i, SD]
    temp <- data.table(Random_Series = rnorm(n, mean_val, sd_val))
    temp[, Random_Series := Random_Series + monthly_avg[i, V1]]
    if (temp[Random_Series < 0L, .N] > 0L) {
      temp[Random_Series < 0L, Random_Series := 0L]
    }
    new_val <- temp[, mean(Random_Series)]
    monthly_avg[i, V1 := new_val]
  }
  repository[Station_ID, "Avg_SWE_Inches" := monthly_avg[, V1]]
  #
  # Count the number of records going into each average
  number <- monthly[, sum(!is.na(SWE_Inches)), by = "Month"]
  setkey(number, "Month")
  repository[Station_ID, "SWE_Inches_n" := number[, V1]]
}

daily_and_monthly_to_final <- function(daily, monthly, Station_ID,
                  repository = Sierra_SWE, first_mo = 2L, last_mo = 5L,
                  counting_threshold = 15L) {
  # Purpose:
  #   Combines a daily and a monthly record of SWE to (hopefully) produce a
  #   longer and more accurate record of SWE.
  #
  # Args:
  #   daily: A daily record of SWE at a given location, in DT format.
  #          It should have two columns: Date and SWE_Inches.
  #          Additional columns (Snow_Depth_Inches and Snow_Depth_Code)
  #          are permissible.
  #   monthly: A monthly record of SWE at the same location as the daily
  #            record. It should also have two columns: Date and SWE_Inches.
  #   Station_ID: A 3-letter abbrevation for the station location.
  #   repository: The data.table that will accumulate the average SWE for
  #               all months and all stations.  This DT should already have
  #               been imported and should contain a populated column of
  #               Station IDs.
  #   first_mo: the first month to use in estimating missing months with
  #             the est_missing_SWE function. Default = February.
  #   last_mo:  the last month to use in estimating missing months with the
  #             est_missing_SWE function. Default = May.
  #   counting_threshold: The minimum number of records in a month for a
  #                       monthly average to be calculated.
  #
  # Dependencies:
  #   This function requires the following functions:
  #     adj_biased_SWE
  #     est_missing_SWE
  #
  # Returns:
  #   Outputs a DT with the average SWE for each month and updates a DT
  #   with all the snow depths and SWEs from the daily stations.
  #
  # Error checking:
  if (!is.data.table(daily)) {
    stop("'daily' needs to be a data.table.", call. = FALSE)
  }
  if (!is.data.table(monthly)) {
    stop("'monthly' needs to be a data.table.", call. = FALSE)
  }
  if (!is.data.table(repository)) {
    stop("'repository' needs to be a data.table.", call. = FALSE)
  }
  if (nchar(Station_ID) != 3L) {
    stop("The Station_ID must be three letters long.", call. = FALSE)
  }
  if (!is.integer(first_mo)) {
    stop("'first_mo' needs to be an integer.", call. = FALSE)
  }
  if (!is.integer(last_mo)) {
    stop("'last_mo' needs to be an integer.", call. = FALSE)
  }
  if ("Date" %in% names(daily) == FALSE) {
    stop("The DT 'daily' needs a 'Date' column.", call. = FALSE)
  }
  if ("SWE_Inches" %in% names(daily) == FALSE) {
    stop("The DT 'daily' needs a column titled exactly 'SWE_Inches'.",
         call. = FALSE)
  }
  if ("Date" %in% names(monthly) == FALSE) {
    stop("The DT 'monthly' needs a 'Date' column.", call. = FALSE)
  }
  if ("SWE_Inches" %in% names(monthly) == FALSE) {
    stop("The DT 'monthly' needs a column titled exactly 'SWE_Inches'.",
         call. = FALSE)
  }
  if ("Station_ID" %in% names(repository) == FALSE) {
    stop("The repository DT must contain a column named 'Station_ID'.",
         call. = FALSE)
  }
  if("snow_depth.txt" %in% dir() == FALSE) {
    stop("The working directory must contain the file 'snow_depth.txt'.",
         call. = FALSE)
  }
  if (length(monthly) != 2L) {
    stop("The DT 'monthly' must have exactly two columns.", call. = FALSE)
  }
  # MAIN BODY:
  #
  # Read in the "snow_depth" file
  snow_depth <- fread("snow_depth.txt")
    # Error checking in "snow_depth"
    if ("Station_ID" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Station_ID'.",
           call. = FALSE)
    }
    if ("Record_Type" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Record_Type'.",
           call. = FALSE)
    }
    if ("Latitude" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Latitude'.",
          call. = FALSE)
    }
    if ("Elevation_ft" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Elevation_ft'.",
          call. = FALSE)
    }
    if ("Date" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Date'.",
          call. = FALSE)
    }
    if ("Month" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Month'.",
          call. = FALSE)
    }
    if ("SWE_Inches" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'SWE_Inches'.",
          call. = FALSE)
    }
    if ("SWE_Code" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'SWE_Code'.",
         call. = FALSE)
    }
    if ("Snow_Depth_Inches" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 
           'Snow_Depth_Inches'.", call. = FALSE)
    }
    if ("Snow_Depth_Code" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 
         'Snow_Depth_Code'.", call. = FALSE)
    }
    if (length(names(snow_depth)) != 10L) {
      stop("snow_depth.txt must have exactly 10 columns.",
           call. = FALSE)
    }
  # format columns
  snow_depth[, Station_ID := as.character(Station_ID)]
  snow_depth[, Record_Type := as.character(Record_Type)]
  snow_depth[, Latitude := as.numeric(Latitude)]
  snow_depth[, Elevation_ft := as.integer(Elevation_ft)]
  snow_depth[, Date := ymd(Date)]
  snow_depth[, Month := as.integer(Month)]
  snow_depth[, SWE_Inches := as.numeric(SWE_Inches)]
  snow_depth[, SWE_Code := as.character(SWE_Code)]
  snow_depth[, Snow_Depth_Inches := as.integer(Snow_Depth_Inches)]
  snow_depth[, Snow_Depth_Code := as.character(Snow_Depth_Code)]
  # break snow_depth into individual vectors and add the new data
  lat <- repository[Station_ID, Latitude]
  ele <- repository[Station_ID, Elevation_ft]
    # for the first column, "Station_ID"
      vector1Station_ID <- snow_depth[, Station_ID]
      vector2Station_ID <- rep_len(Station_ID, daily[, .N])
      vector3Station_ID <- c(vector1Station_ID, vector2Station_ID)
    # for the second column, "Record_Type"
      vector1Record_Type <- snow_depth[, Record_Type]
      vector2Record_Type <- rep_len("daily", daily[, .N])
      vector3Record_Type <- c(vector1Record_Type, vector2Record_Type)
    # for the third column, "Latitude"
      vector1Latitude <- snow_depth[, Latitude]
      vector2Latitude <- rep_len(lat[1], daily[, .N])
      vector3Latitude <- c(vector1Latitude, vector2Latitude)
    # for the fourth column, "Elevation_ft"
      vector1Elevation_ft <- snow_depth[, Elevation_ft]
      vector2Elevation_ft <- rep_len(ele[1], daily[, .N])
      vector3Elevation_ft <- c(vector1Elevation_ft, vector2Elevation_ft)
    # for the fifth column, "Date"
      vector1Date <- snow_depth[, Date]
      vector2Date <- daily[, Date]
      vector3Date <- c(vector1Date, vector2Date)
    # for the sixth column, "Month"
      vector1Month <- snow_depth[, Month]
      vector2Month <- daily[, Month]
      vector3Month <- c(vector1Month, vector2Month)
    # for the seventh column, "SWE_Inches"
      vector1SWE_Inches <- snow_depth[, SWE_Inches]
      vector2SWE_Inches <- daily[, SWE_Inches]
      vector3SWE_Inches <- c(vector1SWE_Inches, vector2SWE_Inches)
    # for the eighth column, "SWE_Code"
      vector1SWE_Code <- snow_depth[, SWE_Code]
      vector2SWE_Code <- daily[, SWE_Code]
      vector3SWE_Code <- c(vector1SWE_Code, vector2SWE_Code)
    # for the ninth column, "Snow_Depth_Inches" 
      vector1Snow_Depth_Inches <- snow_depth[, Snow_Depth_Inches]
      vector2Snow_Depth_Inches <- daily[, Snow_Depth_Inches]
      vector3Snow_Depth_Inches <- c(vector1Snow_Depth_Inches,
                                         vector2Snow_Depth_Inches)
    # for the tenth column, "Snow_Depth_Code"
      vector1Snow_Depth_Code <- snow_depth[, Snow_Depth_Code]
      vector2Snow_Depth_Code <- daily[, Snow_Depth_Code]
      vector3Snow_Depth_Code <- c(vector1Snow_Depth_Code,
                                       vector2Snow_Depth_Code)
  # assemble the vector3's into a new DT with the old snow_depth name
  snow_depth <- data.table(Station_ID = vector3Station_ID,
                          Record_Type = vector3Record_Type,
                          Latitude = vector3Latitude,
                          Elevation_ft = vector3Elevation_ft,
                          Date = vector3Date,
                          Month = vector3Month,
                          SWE_Inches = vector3SWE_Inches,
                          SWE_Code = vector3SWE_Code,
                          Snow_Depth_Inches = vector3Snow_Depth_Inches,
                          Snow_Depth_Code = vector3Snow_Depth_Code)        
  # export snow_depth as a .txt file
  fwrite(snow_depth, "snow_depth.txt", sep = "\t")
  #
  # Preparing the daily record for analysis:
  # count number of daily SWE records for each month
  daily[!is.na(SWE_Inches), Records := 1L]
  daily[is.na(SWE_Inches),  Records := 0L]
  daily[, Records := sum(Records, na.rm = TRUE), by = .(Year, Month)]
  # calculate average SWE for each year and month,
    # a month must have at least "counting_threshold" number of daily SWE
    # observations (default = 15) for it to be included in the average
  YearMonth <- daily[Records >= counting_threshold,
                     .(trusted_SWE = mean(SWE_Inches, na.rm = TRUE)),
                     keyby = .(Year, Month)]
  # Export 'YearMonth' as a tab-delimited text file
    YearMonthExport <- as.data.table(YearMonth)
    # Convert Month column from numbers to names
    for (j in 1:12) {
      YearMonthExport[Month == j, MonthName := month.name[j]]
    }
    # delete 'Month' column
    YearMonthExport[, Month := NULL]
    # spread month names into column headings
    YearMonthExport <- spread(YearMonthExport, MonthName, trusted_SWE)
    # set column order
    setcolorder(YearMonthExport, c("Year", month.name))
    # export file to tab-delimited
    fwrite(YearMonthExport,
           as.character(paste0(Station_ID, "_YearMonth.txt")),
           sep = "\t")
  #  
  # Preparing the monthly records:
  # add columns for year and month
  monthly[, c("Year", "Month") := .(year(Date), month(Date))]
  # set column order
  setcolorder(monthly, c("Date", "Year", "Month", "SWE_Inches"))
  # set a two-column key, Year and Month
  setkey(monthly, Year, Month)
  # delete "Date" column
  monthly[, Date := NULL] 
  # rename "SWE_Inches"
  setnames(monthly, "SWE_Inches", "biased_SWE")
  # create a new DT of dates by month, "combined"
  end_date <- Sys.Date()
  if (monthly[1, Month <= 9L]){
    yr <- monthly[1, Year - 1L]
    start_date <- as.Date(as.character(paste0(yr, "-10-01")))
  } else {
    yr <- monthly[1, Year]
    start_date <- as.Date(as.character(paste0(yr, "-10-01")))
  }
  combined <- data.table(Date = seq(start_date, end_date, by = "month"))
  # break the "Date" column of the new DT into "Year" and "Month" columns
  combined[, c("Year", "Month") := .(year(Date), month(Date))]
  # set a two-column key
  setkey(combined, Year, Month)
  # delete the "Date" column from "combined" DT
  combined[, Date := NULL]
  #
  # Merge the YearMonth and Monthly records
  combined <- YearMonth[combined]
  combined <- monthly[combined]
  #
  # Adjust the "biased" monthly measurements (in the statistical sense)
  # into alignment with the daily measurements.
  adj_biased_SWE(combined, Station_ID, "trusted_SWE", "biased_SWE")
  #
  # Use the adjusted SWE measurements and the relationship between
  # various months to impute the missing months of data.
  combined <- est_missing_SWE(combined, Station_ID)
  # Export "combined" DT
  fwrite(combined, as.character(paste0(Station_ID, "_combined.txt")),
         sep = "\t")
  # calculate final SWE averages
    avg_SWE <- combined[, mean(Final_SWE_Inches, na.rm = TRUE),
                        by = "Month"]
    # set key in "final" records
    setkey(avg_SWE, "Month")
    # assign calendar names to the months
    avg_SWE[, Month := month.name]
    # add final values to the repository
    repository[Station_ID, "Avg_SWE_Inches" := avg_SWE[, V1]]
  # calculate the SD on the final SWE average
    SWE_SD <- combined[, sd(Final_SWE_Inches, na.rm = TRUE),
                       by = "Month"]
    setkey(SWE_SD, "Month")
    SWE_SD[, Month := month.name]
    repository[Station_ID, "SWE_Inches_SD" := SWE_SD[, V1]]
  # count the number of records going into each SWE average
    SWE_n <- combined[, sum(!is.na(Final_SWE_Inches)), by = "Month"]
    setkey(SWE_n, "Month")
    SWE_n[, Month := month.name]
    repository[Station_ID, "SWE_Inches_n" := SWE_n[, V1]]
}

only_daily_to_final <- function(daily, Station_ID,
                                repository = Sierra_SWE,
                                counting_threshold = 15L) {
  # Purpose:
  #   Calculates the average SWE for each month in a daily record of SWE.
  #
  # Args:
  #   daily: A daily record of SWE at a given location, in DT format. It
  #          should have two columns: Date and SWE_Inches.
  #   Station_ID: A 3-letter abbrevation for the station location.
  #   repository: The data.table that will accumulate the average SWE for
  #               all months and all stations.  This DT should already
  #               have been imported and should contain a populated column
  #               of Station IDs.
  #   counting_threshold: The minimum number of records in a month for a
  #                       monthly average to be calculated.
  #
  # Returns:
  #   Outputs a DT with the average SWE for each month.
  #
  # Error checking:
  if (!is.data.table(daily)) {
    stop("'daily' needs to be a data.table.", call. = FALSE)
  }
  if (!is.data.table(repository)) {
    stop("'repository' needs to be a data.table.", call. = FALSE)
  }
  if (nchar(Station_ID) != 3L) {
    stop("The Station_ID must be three letters long.", call. = FALSE)
  }
  if ("Date" %in% names(daily) == FALSE) {
    stop("The DT 'daily' needs a 'Date' column.", call. = FALSE)
  }
  if ("SWE_Inches" %in% names(daily) == FALSE) {
    stop("The DT 'daily' needs a column titled exactly 'SWE_Inches'.",
         call. = FALSE)
  }
  if ("Station_ID" %in% names(repository) == FALSE) {
    stop("The repository DT must contain a column named 'Station_ID'.")
  }
  # MAIN BODY:
  #
  # Read in the "snow_depth" file
  snow_depth <- fread("snow_depth.txt")
  # Error checking in "snow_depth"
    if ("Station_ID" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Station_ID'.",
          call. = FALSE)
    }
    if ("Record_Type" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Record_Type'.",
          call. = FALSE)
    }
    if ("Latitude" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Latitude'.",
          call. = FALSE)
    }
    if ("Elevation_ft" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Elevation_ft'.",
          call. = FALSE)
    }
    if ("Date" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Date'.",
          call. = FALSE)
    }
    if ("Month" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'Month'.",
          call. = FALSE)
    }
    if ("SWE_Inches" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'SWE_Inches'.",
          call. = FALSE)
    }
    if ("SWE_Code" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 'SWE_Code'.",
          call. = FALSE)
    }
    if ("Snow_Depth_Inches" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named
           'Snow_Depth_Inches'.", call. = FALSE)
    }
    if ("Snow_Depth_Code" %in% names(snow_depth) == FALSE) {
      stop("snow_depth.txt must contain a column named 
          'Snow_Depth_Code'.", call. = FALSE)
    }
    if (length(names(snow_depth)) != 10L) {
      stop("snow_depth.txt must have exactly 10 columns.",
           call. = FALSE)
    }
  # format columns
    snow_depth[, Station_ID := as.character(Station_ID)]
    snow_depth[, Record_Type := as.character(Record_Type)]
    snow_depth[, Latitude := as.numeric(Latitude)]
    snow_depth[, Elevation_ft := as.integer(Elevation_ft)]
    snow_depth[, Date := ymd(Date)]
    snow_depth[, Month := as.integer(Month)]
    snow_depth[, SWE_Inches := as.numeric(SWE_Inches)]
    snow_depth[, SWE_Code := as.character(SWE_Code)]
    snow_depth[, Snow_Depth_Inches := as.integer(Snow_Depth_Inches)]
    snow_depth[, Snow_Depth_Code := as.character(Snow_Depth_Code)]
  # break snow_depth into individual vectors and add the new data
  lat <- repository[Station_ID, Latitude]
  ele <- repository[Station_ID, Elevation_ft]
  # for the first column, "Station_ID"
    vector1Station_ID <- snow_depth[, Station_ID]
    vector2Station_ID <- rep_len(Station_ID, daily[, .N])
    vector3Station_ID <- c(vector1Station_ID, vector2Station_ID)
  # for the second column, "Record_Type"
    vector1Record_Type <- snow_depth[, Record_Type]
    vector2Record_Type <- rep_len("daily", daily[, .N])
    vector3Record_Type <- c(vector1Record_Type, vector2Record_Type)
  # for the third column, "Latitude"
    vector1Latitude <- snow_depth[, Latitude]
    vector2Latitude <- rep_len(lat[1], daily[, .N])
    vector3Latitude <- c(vector1Latitude, vector2Latitude)
  # for the fourth column, "Elevation_ft"
    vector1Elevation_ft <- snow_depth[, Elevation_ft]
    vector2Elevation_ft <- rep_len(ele[1], daily[, .N])
    vector3Elevation_ft <- c(vector1Elevation_ft, vector2Elevation_ft)
  # for the fifth column, "Date"
    vector1Date <- snow_depth[, Date]
    vector2Date <- daily[, Date]
    vector3Date <- c(vector1Date, vector2Date)
  # for the sixth column, "Month"
    vector1Month <- snow_depth[, Month]
    vector2Month <- daily[, Month]
    vector3Month <- c(vector1Month, vector2Month)
  # for the seventh column, "SWE_Inches"
    vector1SWE_Inches <- snow_depth[, SWE_Inches]
    vector2SWE_Inches <- daily[, SWE_Inches]
    vector3SWE_Inches <- c(vector1SWE_Inches, vector2SWE_Inches)
  # for the eighth column, "SWE_Code"
    vector1SWE_Code <- snow_depth[, SWE_Code]
    vector2SWE_Code <- daily[, SWE_Code]
    vector3SWE_Code <- c(vector1SWE_Code, vector2SWE_Code)
  # for the ninth column, "Snow_Depth_Inches" 
    vector1Snow_Depth_Inches <- snow_depth[, Snow_Depth_Inches]
    vector2Snow_Depth_Inches <- daily[, Snow_Depth_Inches]
    vector3Snow_Depth_Inches <- c(vector1Snow_Depth_Inches,
                                vector2Snow_Depth_Inches)
  # for the tenth column, "Snow_Depth_Code"
    vector1Snow_Depth_Code <- snow_depth[, Snow_Depth_Code]
    vector2Snow_Depth_Code <- daily[, Snow_Depth_Code]
    vector3Snow_Depth_Code <- c(vector1Snow_Depth_Code,
                              vector2Snow_Depth_Code)
  # assemble the vector3's into a new DT with the old snow_depth name
    snow_depth <- data.table(Station_ID = vector3Station_ID,
                            Record_Type = vector3Record_Type,
                            Latitude = vector3Latitude,
                            Elevation_ft = vector3Elevation_ft,
                            Date = vector3Date,
                            Month = vector3Month,
                            SWE_Inches = vector3SWE_Inches,
                            SWE_Code = vector3SWE_Code,
                            Snow_Depth_Inches = vector3Snow_Depth_Inches,
                            Snow_Depth_Code = vector3Snow_Depth_Code)        
  # export snow_depth as a .txt file
  fwrite(snow_depth, "snow_depth.txt", sep = "\t")
  #
  # Preparing the daily record:
  # count number of daily SWE records for each month
  daily[!is.na(SWE_Inches), Records := 1L]
  daily[is.na(SWE_Inches),  Records := 0L]
  daily[, Records := sum(Records, na.rm = TRUE), by = .(Year, Month)]
  # calculate average SWE for each year and month,
    # a month must have at least "counting_threshold" number of daily SWE
    # observations (default = 15) for it to be included in the average
  YearMonth <- daily[Records >= counting_threshold,
                     .(SWE_Inches = mean(SWE_Inches, na.rm = TRUE)),
                     keyby = .(Year, Month)]
  # The DAN record, and perhaps others, contains a row of NAs because of
  # the years we wiped out, due to data quality concerns.
  # Eliminate a row of NAS, if present
  YearMonth <- YearMonth[!is.na(Year)]
  # Export 'YearMonth' as a tab-delimited text file
    YearMonthExport <- as.data.table(YearMonth)
    # Convert Month column from numbers to names
    for (j in 1:12) {
      YearMonthExport[Month == j, MonthName := month.name[j]]
    }
    # delete 'Month' column
    YearMonthExport[, Month := NULL]
    # spread month names into column headings
    YearMonthExport <- spread(YearMonthExport, MonthName, SWE_Inches)
    # set column order
    setcolorder(YearMonthExport, c("Year", month.name))
    # export file to tab-delimited
    fwrite(YearMonthExport,
           as.character(paste0(Station_ID, "_YearMonth.txt")),
           sep = "\t")
  # calculate final SWE averages
    avg_SWE <- YearMonth[, mean(SWE_Inches, na.rm = TRUE),
                         by = "Month"]
    # set key in "final" records
    setkey(avg_SWE, "Month")
    # assign calendar names to the months
    avg_SWE[, Month := month.name]
    # add final values to the repository
    repository[Station_ID, "Avg_SWE_Inches" := avg_SWE[, V1]]
  # calculate the SD on the final SWE averages
    SWE_SD <- YearMonth[, sd(SWE_Inches, na.rm = TRUE),
                        by = "Month"]
    setkey(SWE_SD, "Month")
    SWE_SD[, Month := month.name]
    repository[Station_ID, "SWE_Inches_SD" := SWE_SD[, V1]]
  # Count the number of records going into each average
    number <- YearMonth[, sum(!is.na(SWE_Inches)), by = "Month"]
    setkey(number, "Month")
    number[, Month := month.name]
    repository[Station_ID, "SWE_Inches_n" := number[, V1]]
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


# LOAD TUOLUMNE RECORDS-----------------------------------------------

# THE FOLLOWING SNOW WATER EQUIVALENT RECORDS ARE ORGANIZED FIRST BY
# DRAINAGE BASIN, FROM NORTH TO SOUTH, AND THEN BY DECREASING ELEVATION.

# A daily SWE station at Tioga Pass is listed on the California
# Department of Water Resources website and supposedly it hosts a
# record extending from 3Nov2003 to present (31Dec2017), but when
# downloaded it only consists of 3 measurements: March 1-3, 2005.
# This comment documents that I noted the record, but it isn't
# otherwise considered here.

# DAN_daily = post-1980 daily record (revised) of SWE from Dana Meadows
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=DAN
# The record runs from 10/26/80 to present (5/26/17).
DAN_daily <- fread("DAN_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
DAN_daily[, Date := ymd(Date)]
  
# DAN_monthly = post-1926 monthly record (revised) of SWE from Dana
# Meadows
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=DAN
DAN_monthly <- fread("DAN_monthly.txt")
DAN_monthly[, Date := ymd(Date)]

# RFM_monthly = post-1948 monthly record (revised) of SWE from Rafferty
# Meadows in Yosemite National Park.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=RFM
RFM_monthly <- fread("RFM_monthly.txt")
RFM_monthly[, Date := ymd(Date)]

# BNP_monthly = post-1948 monthly record (revised) of SWE from Bond Pass.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=BNP
BNP_monthly <- fread("BNP_monthly.txt")
BNP_monthly[, Date := ymd(Date)]

# SLI_daily = post-1982 daily record (revised) of SWE from Slide Canyon
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SLI
# The record runs from 10/21/82 to present (5/26/17).
# A string of zeros from 4/16/97 to 5/9/97 is sandwiched between SWCs of
# 40-50", this string of zeros was replaced with NAs.
SLI_daily <- fread("SLI_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
SLI_daily[, Date := ymd(Date)]

# NGM_monthly = post-1966 monthly record (revised) of SWE in New Grace
# Meadow in Yosemite National Park.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=NGM
NGM_monthly <- fread("NGM_monthly.txt")
NGM_monthly[, Date := ymd(Date)]

# TUM_daily = post-1979 daily record of SWE from Tuolumne Meadows
#   at: http://cdec.water.ca.gov/cgi-progs/stationInfo?station_id=TUM
# The record runs from 11/25/79 to present (5/26/17).
TUM_daily <- fread("TUM_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
TUM_daily[, Date := ymd(Date)]

# TUM_monthly = post-1930 manually recorded monthly record (revised) of
#               SWE from Tuolumne Meadows.
#   at: http://cdec.water.ca.gov/cgi-progs/stationInfo?station_id=TUM
TUM_monthly <- fread("TUM_monthly.txt")
TUM_monthly[, Date := ymd(Date)]

# HRS_daily = post-1984 daily recorded (revised) SWE from Horse Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=HRS
# The record runs from 11/1/84 to present (5/26/17).
HRS_daily <- fread("HRS_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
HRS_daily[, Date := ymd(Date)]

# HRS_monthly = post-1948 monthly record (revised) of SWE in Horse
# Meadow.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=HRS
HRS_monthly <- fread("HRS_monthly.txt")
HRS_monthly[, Date := ymd(Date)]

# WLW_monthly = post-1946 monthly record (revised) of SWE at Wilma Lake.
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=WLW
WLW_monthly <- fread("WLW_monthly.txt")
WLW_monthly[, Date := ymd(Date)]

# SAS_monthly = post-1948 monthly record (revised) of SWE at Sachse
# Springs.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SAS
SAS_monthly <- fread("SAS_monthly.txt")
SAS_monthly[, Date := ymd(Date)]

# WHW_daily = post-2007 daily record (revised) SWE from White Wolf
#   at: http://cdec.water.ca.gov/cgi-progs/stationInfo?station_id=WHW
# The record runs from 11/1/2007 to present (11/30/2007).
WHW_daily <- fread("WHW_daily_clean.txt")
WHW_daily[, Date := ymd(Date)]

# SPF_monthly = post-1948 monthly record (revised) of SWE at Spotted Fawn.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SPF
SPF_monthly <- fread("SPF_monthly.txt")
SPF_monthly[, Date := ymd(Date)]

# HCL_monthly = post-1948 manually recorded monthly SWE (revised) from
# Huckleberry Lake.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=HCL
HCL_monthly <- fread("HCL_monthly.txt")
HCL_monthly[, Date := ymd(Date)]

# PDS_daily = post-1980 daily record (revised) of SWE in Paradise Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PDS
# The record runs from 11/10/80 to present (5/26/17).
PDS_daily <- fread("PDS_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
PDS_daily[, Date := ymd(Date)]

# PDS_monthly = post-1946 monthly record (revised) of SWE in Paradise
# Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PDS
PDS_monthly <- fread("PDS_monthly.txt")
PDS_monthly[, Date := ymd(Date)]

# KRC_monthly = post-1961 monthly record (revised) of SWE at Kerrick
# Corral.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=KRC
KRC_monthly <- fread("KRC_monthly.txt")
KRC_monthly[, Date := ymd(Date)]

# VNN_monthly = post-1947 monthly record (revised) of SWE at Vernon Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=VNN
VNN_monthly <- fread("VNN_monthly.txt")
VNN_monthly[, Date := ymd(Date)]

# LKB_monthly = post-1937 monthly record (revised) of SWE at Lower
# Kibbie Ridge
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=LKB
LKB_monthly <- fread("LKB_monthly.txt")
LKB_monthly[, Date := ymd(Date)]

# UKR_monthly = post-1947 monthly record (revised) of SWE at Upper
# Kibbie Ridge
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=UKR
UKR_monthly <- fread("UKR_monthly.txt")
UKR_monthly[, Date := ymd(Date)]

# BEM_monthly = post-1947 monthly record (revised) of SWE at Bell Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=BEM
BEM_monthly <- fread("BEM_monthly.txt")
BEM_monthly[, Date := ymd(Date)]

# BHV_monthly = post-1930 monthly record (revised) of SWE at Beehive
# Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=BHV
BHV_monthly <- fread("BHV_monthly.txt")
BHV_monthly[, Date := ymd(Date)]

# load Merced records----------------------------------------------------

# THE FOLLOWING SNOW WATER EQUIVALENT RECORDS ARE ORGANIZED FIRST BY
# DRAINAGE BASIN, FROM NORTH TO SOUTH, AND THEN BY DECREASING ELEVATION.

# SNF_daily = 1988-1999 daily record (revised) of SWE at Snow Flat
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SNF
# The record runs from 11/24/88 to 10/27/98.
# March 18, 19, and 20, 1997 were manually converted from zero to NA.
SNF_daily <- fread("SNF_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
SNF_daily[, Date := ymd(Date)]

# SNF_monthly = post-1930 monthly record (revised) of SWE at Snow Flat
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SNF
SNF_monthly <- fread("SNF_monthly.txt")
SNF_monthly[, Date := ymd(Date)]

# STR_daily = post-1988 daily record (revised) of SWE at Ostrander Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=STR
# The record runs from 12/1/88 to present (5/26/17).
STR_daily <- fread("STR_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
STR_daily[, Date := ymd(Date)]

# STR_monthly = post-1938 monthly record (revised) of SWE at Ostrander
# Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=STR
STR_monthly <- fread("STR_monthly.txt")
STR_monthly[, Date := ymd(Date)]

# TNY_daily = post-1998 daily record (revised) of SWE at Tenaya Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=TNY
# The record runs from 11/1/98 to present (5/26/17).
TNY_daily <- fread("TNY_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
TNY_daily[, Date := ymd(Date)]

# TNY_monthly = post-1930 monthly record (revised) of SWE at Tenaya Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=TNY
TNY_monthly <- fread("TNY_monthly.txt")
TNY_monthly[, Date := ymd(Date)]

# PGM_monthly = post-1931 monthly record (revised) of SWE at Peregoy
# Meadows
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PGM
PGM_monthly <- fread("PGM_monthly.txt")
PGM_monthly[, Date := ymd(Date)]

# GFL_monthly = post-1930 monthly record (revised) of SWE at Gin Flat
# (course)
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GFL
GFL_monthly <- fread("GFL_monthly.txt")
GFL_monthly[, Date := ymd(Date)]


# LOAD SAN JOAQUIN RECORDS-----------------------------------------------

# THE FOLLOWING SNOW WATER EQUIVALENT RECORDS ARE ORGANIZED FIRST BY
# DRAINAGE BASIN, FROM NORTH TO SOUTH, AND THEN BY DECREASING ELEVATION.

# MNP_monthly = post-1950 monthly record (revised) of SWE from Mono Pass
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=MNP
MNP_monthly <- fread("MNP_monthly.txt")
MNP_monthly[, Date := ymd(Date)]

# PPS_monthly = post-1930 monthly record (revised) of SWE from Piute Pass
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PPS
PPS_monthly <- fread("PPS_monthly.txt")
PPS_monthly[, Date := ymd(Date)]

# EML_monthly = post-1944 monthly record (revised) of SWE from Emerald
# Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=EML
EML_monthly <- fread("EML_monthly.txt")
EML_monthly[, Date := ymd(Date)]

# PNB_monthly = post-1949 monthly record (revised) of SWE from Pioneer
# Basin.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=PNB
PNB_monthly <- fread("PNB_monthly.txt")
PNB_monthly[, Date := ymd(Date)]

# HRT_monthly = post-1940 monthly record (revised) of SWE from Heart
# Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HRT
HRT_monthly <- fread("HRT_monthly.txt")
HRT_monthly[, Date := ymd(Date)]

# VLC_daily = post-1989 daily record (revised) of SWE at Volcanic Knob
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=VLC
# The record runs from 11/29/89 to present (5/26/17)
VLC_daily <- fread("VLC_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
VLC_daily[, Date := ymd(Date)]

# VLC_monthly = post-1946 monthly record (revised) of SWE at Volcanic
# Knob
#   at: at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=VLC
VLC_monthly <- fread("VLC_monthly.txt")
VLC_monthly[, Date := ymd(Date)]

# RMR_monthly = post-1946 monthly record (revised) of SWE at Rose Marie
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=RMR
RMR_monthly <- fread("RMR_monthly.txt")
RMR_monthly[, Date := ymd(Date)]

# CBM_monthly = post-1944 monthly record (revised) of SWE at Colby Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CBM
CBM_monthly <- fread("CBM_monthly.txt")
CBM_monthly[, Date := ymd(Date)]

# AGP_daily = post-1989 daily record (revised) of SWE at Agnew Pass
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=AGP
# The record runs from 11/24/1989 to 6/5/2017.
AGP_daily <- fread("AGP_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
AGP_daily[, Date := ymd(Date)]

# AGP_monthly = post-1930 monthly record (revised) of SWE at Agnew Pass
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=AGP
AGP_monthly <- fread("AGP_monthly.txt")
AGP_monthly[, Date := ymd(Date)]

# KSP_daily = post-1970 (!) daily record of SWE at Kaiser Point
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=KSP
# The record runs from 1/6/1970 to 12/14/2017
# The KSP_daily record (Kaiser Point) will NOT be combined with the
#  KSR_monthly (Kaiser Pass) record because the two locations are
#  ~600 m (horizontally) and 100 ft (vertically) apart.
KSP_daily <- fread("KSP_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
KSP_daily[, Date := ymd(Date)]

# DTL_monthly = post-1938 monthly record (revised) of SWE at Dutch Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DTL
DTL_monthly <- fread("DTL_monthly.txt")
DTL_monthly[, Date := ymd(Date)]

# KSR_monthly = post-1930 monthly record (revised) of SWE at Kaiser Pass
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=KSR
KSR_monthly <- fread("KSR_monthly.txt")
KSR_monthly[, Date := ymd(Date)]

# CYT_monthly = post-1946 monthly record (revised) of SWE at Coyote Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CYT
CYT_monthly <- fread("CYT_monthly.txt")
CYT_monthly[, Date := ymd(Date)]

# CRA_monthly = post-1939 monthly record (revised) of SWE at Cora Lakes
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CRA
CRA_monthly <- fread("CRA_monthly.txt")
CRA_monthly[, Date := ymd(Date)]

# BDF_monthly = post-1960 monthly record (revised) of SWE at Badger Flat
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BDF
BDF_monthly <- fread("BDF_monthly.txt")
BDF_monthly[, Date := ymd(Date)]

# NLL_monthly = post-1990 monthly record (revised) of SWE at Nellie Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=NLL
NLL_monthly <- fread("NLL_monthly.txt")
NLL_monthly[, Date := ymd(Date)]

# GRM_daily = post-1972 daily record (revised) of SWE at Green Mtn
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GRM
# The record runs from 10/1/1972 to 11/30/2017
GRM_daily <- fread("GRM_daily_clean.txt")
GRM_daily[, Date := ymd(Date)]

# THE_monthly = post-1958 monthly record (revised) of SWE at Lake Thomas
# Edison
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=THE
THE_monthly <- fread("THE_monthly.txt")
THE_monthly[, Date := ymd(Date)]

# DPO_daily = post-2007 daily record (revised) of SWE at Devil's Postpile
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DPO
# The record runs from 10/1/2007 to 6/5/2017
DPO_daily <- fread("DPO_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
DPO_daily[, Date := ymd(Date)]

# The website http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DPO
# claims there's a post-2013 monthly record of SWE at Devil's Postpile,
# but it consists entirely of missing data.  Not sure what's up with that,
# but I'm going to process the daily record as is, without a monthly partner.

# TMR_daily = post-1980 daily record (revised) of SWE at Tamarack Summit
#   at: http://cdec.water.ca.gov/cgi-progs/stationInfo?station_id=TMR
# The record runs from 12/3/1980 to 11/30/2017.
TMR_daily <- fread("TMR_daily_clean.txt", colClasses = c("Date",
              "integer", "integer", "numeric", "character", "integer",
              "character"))
TMR_daily[, Date := ymd(Date)]

# CKT_monthly = post-1990 monthly record (revised) of SWE at Chilkoot Lake.
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CKT
CKT_monthly <- fread("CKT_monthly.txt")
CKT_monthly[, Date := ymd(Date)]

# CHM_daily = post-1985 daily record of SWE (revised) at Chilkoot Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CHM
# The record runs from 11/1/1985 to 11/30/2017
CHM_daily <- fread("CHM_daily_clean.txt")
CHM_daily[, Date := ymd(Date)]

# HNT_daily = post-1987 daily record (revised) of SWE at Huntington Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HNT
# The record runs from 11/4/1987 to 11/30/2017
# The HNT_daily record is going to be combined with the HTT_monthly
# record, both from Huntington Lake.  The two stations are far enough
# apart (100 m) that the CA Dept of Water Resources gives them
# different Station Codes, but they're at the same elevation and
# latitude, and have the same Station Name (Huntington Lake), which
# makes them equivalent for our purposes.
HNT_daily <- fread("HNT_daily_clean.txt")
HNT_daily[, Date := ymd(Date)]

# HTT_monthly = post-1930 monthly record (revised) of SWE at Huntington
# Lake.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=HTT
HTT_monthly <- fread("HTT_monthly.txt")
HTT_monthly[, Date := ymd(Date)]

# CLM_monthly = post-1939 monthly record (revised) of SWE at Clover Meadow.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=CLM
CLM_monthly <- fread("CLM_monthly.txt")
CLM_monthly[, Date := ymd(Date)]

# JCM_monthly = post-1939 monthly record (revised) of SWE at Jackass Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=JCM
JCM_monthly <- fread("JCM_monthly.txt")
JCM_monthly[, Date := ymd(Date)]

# GRV_daily = post-1982 daily record (revised) of SWE at
#  Graveyard Meadow
#  at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GRV
# The record runs from 3/4/1982 to 10/31/2016
GRV_daily <- fread("GRV_daily_clean.txt")
GRV_daily[, Date := ymd(Date)]

# PSR_daily = post-1970 daily record (SWE) revised at Poison Ridge
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PSR
# The record runs from 1/1/1970 to 12/14/2017
PSR_daily <- fread("PSR_daily_clean.txt")
PSR_daily[, Date := ymd(Date)]

# CHQ_monthly = post-1939 monthly record (revised) of SWE at Chiquito
# Creek.
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=CHQ
CHQ_monthly <- fread("CHQ_monthly.txt")
CHQ_monthly[, Date := ymd(Date)]

# PMD_monthly = post-1944 monthly record (revised) of SWE at Poison Meadow.
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PMD
PMD_monthly <- fread("PMD_monthly.txt")
PMD_monthly[, Date := ymd(Date)]


# load/create files to accumulate snow information--------------------------
# 
# 1- Load the text file created in Excel for accumulating average SWEs for
# each month and station. The R code expects the 'Station_ID' field to
# already be populated. The code will be searching for specific station IDs.
Sierra_SWE <- fread("Sierra_Nevada_SWE.txt")
Sierra_SWE[, month.name[1:12] := as.numeric(NA)]
Sierra_SWE <- as.data.table(gather(Sierra_SWE, Month, Avg_SWE_Inches,
                                   January:December))
 # loop over the relevant columns and transfer the values to the
 # Insolation... column
col_num   <- which(names(Sierra_SWE) == "Jan_Insol_MJperM2")
for (j in 1:12) {
  # pull vector of column names
  col_names <- names(Sierra_SWE)
  # transfer values
  Sierra_SWE[Month == month.name[j], 
            Insolation_MJperM2 := get(col_names[col_num])]
  # delete old column
  Sierra_SWE[, col_names[col_num] := NULL]
}
 # reorder columns
 name_vec <- names(Sierra_SWE)
 new_order <- c(name_vec[1:9], name_vec[11], name_vec[10])
 setcolorder(Sierra_SWE, new_order)
# create other needed columns
 Sierra_SWE[, ':=' (SWE_Inches_SD   = as.numeric(NA),
                    SWE_Inches_n    = as.integer(NA),
                    Avg_diff_inches = as.numeric(NA),
                    Avg_diff_SD     = as.numeric(NA),
                    Avg_diff_n      = as.integer(NA))]
setkey(Sierra_SWE, "Station_ID")

# 2- Load a text file populated with the individual differences between the 
# monthly and daily SWE data. I attempted to do this programmatically in R,
# but it was too much for me. Instead I've gathered the relevant data in
# Excel and exported it as a text file for importing here.
all_monthly_diffs <- fread("all_the_monthly_differences.txt")

# 3- Create a DT for the snow depth data and export it as a text file. Each 
# function call to "daily_and_monthly_to_final" and "only_daily_to_final"
# will then import it, modify it, and re-export it. I can't (quickly) figure
# out a better way of accumulating this info.
snow_depth <- data.table(Station_ID   = as.factor(NA),
                         Record_Type  = as.factor(NA),
                         Latitude     = as.numeric(NA),
                         Elevation_ft = as.integer(NA),
                         Date         = as.Date(NA),
                         Month        = as.integer(NA),
                         SWE_Inches   = as.numeric(NA),
                         SWE_Code     = as.factor(NA),
                         Snow_Depth_Inches = as.integer(NA),
                         Snow_Depth_Code = as.factor(NA))
fwrite(snow_depth, "snow_depth.txt", sep = "\t")


# Process SWE RECORDS----------------------------------------------------

# processing monthly records paired with a daily record
# TUOLUMNE DRAINAGE
daily_and_monthly_to_final(DAN_daily, DAN_monthly, "DAN")
daily_and_monthly_to_final(TUM_daily, TUM_monthly, "TUM")
daily_and_monthly_to_final(HRS_daily, HRS_monthly, "HRS")
daily_and_monthly_to_final(PDS_daily, PDS_monthly, "PDS")
# MERCED DRAINAGE
daily_and_monthly_to_final(SNF_daily, SNF_monthly, "SNF")
daily_and_monthly_to_final(STR_daily, STR_monthly, "STR")
daily_and_monthly_to_final(TNY_daily, TNY_monthly, "TNY")
# SAN JOAQUIN DRAINAGE
daily_and_monthly_to_final(VLC_daily, VLC_monthly, "VLC")
daily_and_monthly_to_final(AGP_daily, AGP_monthly, "AGP")
daily_and_monthly_to_final(HNT_daily, HTT_monthly, "HTT")
   # the "HNT" vs "HTT" usage above is deliberate and correct

# processing of monthly records without a daily counterpart
# TUOLUMNE DRAINAGE
monthly_to_final(RFM_monthly, "RFM")
monthly_to_final(BNP_monthly, "BNP")
monthly_to_final(NGM_monthly, "NGM")
monthly_to_final(WLW_monthly, "WLW")
monthly_to_final(SAS_monthly, "SAS")
monthly_to_final(SPF_monthly, "SPF")
monthly_to_final(HCL_monthly, "HCL")
monthly_to_final(KRC_monthly, "KRC")
monthly_to_final(VNN_monthly, "VNN")
monthly_to_final(LKB_monthly, "LKB")
monthly_to_final(UKR_monthly, "UKR")
monthly_to_final(BEM_monthly, "BEM")
monthly_to_final(BHV_monthly, "BHV")
# MERCED DRAINAGE
monthly_to_final(PGM_monthly, "PGM")
monthly_to_final(GFL_monthly, "GFL")
# SAN JOAQUIN DRAINAGE
monthly_to_final(MNP_monthly, "MNP")
monthly_to_final(PPS_monthly, "PPS")
monthly_to_final(EML_monthly, "EML")
monthly_to_final(PNB_monthly, "PNB")
monthly_to_final(HRT_monthly, "HRT")
monthly_to_final(RMR_monthly, "RMR")
monthly_to_final(CBM_monthly, "CBM")
monthly_to_final(DTL_monthly, "DTL")
monthly_to_final(KSR_monthly, "KSR")
monthly_to_final(CYT_monthly, "CYT")
monthly_to_final(CRA_monthly, "CRA")
monthly_to_final(BDF_monthly, "BDF")
monthly_to_final(NLL_monthly, "NLL")
monthly_to_final(THE_monthly, "THE")
monthly_to_final(CKT_monthly, "CKT")
monthly_to_final(CLM_monthly, "CLM")
monthly_to_final(JCM_monthly, "JCM")
monthly_to_final(CHQ_monthly, "CHQ")
monthly_to_final(PMD_monthly, "PMD")

# processing daily records without a monthly counterpart
# TUOLUMNE DRAINAGE
only_daily_to_final(SLI_daily, "SLI")
only_daily_to_final(WHW_daily, "WHW")
# SAN JOAQUIN DRAINAGE
only_daily_to_final(DPO_daily, "DPO")
only_daily_to_final(TMR_daily, "TMR")
only_daily_to_final(CHM_daily, "CHM")
only_daily_to_final(KSP_daily, "KSP")
only_daily_to_final(GRM_daily, "GRM")
only_daily_to_final(GRV_daily, "GRV")
only_daily_to_final(PSR_daily, "PSR")


# EXPORT REPOSITORY FILE-------------------------------------------------
fwrite(Sierra_SWE, "repository.txt", sep = "\t")

# re-import Sierra_SWE---------------------------------------------------

# exporting and then re-importing Sierra_SWE enables me/us to quickly
#  rerun the calculations below without redoing the time-consuming
#  calculations above.
Sierra_SWE <- fread("repository.txt")
  # set Month to class factor with levels
  Sierra_SWE[, Month := factor(Month, levels = month.name[1:12],
                                ordered = TRUE)]
  # convert conventional units into SI and reorder DT
  Sierra_SWE[, Avg_SWE_m := Avg_SWE_Inches * 0.0254]
  Sierra_SWE[, SWE_m_SD  := SWE_Inches_SD  * 0.0254]
  setnames(Sierra_SWE, "SWE_Inches_n", "SWE_n")
  Sierra_SWE[, c("Avg_SWE_Inches", "SWE_Inches_SD") := NULL]
  Sierra_SWE[, Avg_diff_cm    := Avg_diff_inches * 2.54]
  Sierra_SWE[, Avg_diff_SD := Avg_diff_SD * 2.54]
  Sierra_SWE[, Avg_diff_inches := NULL]
  names_vector <- names(Sierra_SWE)
  setcolorder(Sierra_SWE, c(names_vector[1:10],
                            names_vector[16],
                            names_vector[12:15],
                            names_vector[11]))
  

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
    axis.title = element_text(size = 7L))
  
  
# analyze snow depth and density------------------------------------------

# reload snow_depth.txt
snow_depth <- fread("snow_depth.txt")
snow_depth[, Date := ymd(Date)]
# convert elevations into SI, round to nearest 10 m
snow_depth[, Elevation_m := round(Elevation_ft * 0.3048, -1)]
  # round DPO to nearest 1 m, it has a more precise elevation
  snow_depth[Station_ID == "DPO",
             Elevation_m := round(Elevation_ft * 0.3048, 0)]
snow_depth[, SWE_m := SWE_Inches * 0.0254]
snow_depth[, Snow_Depth_m := Snow_Depth_Inches * 0.0254]
# delete conventional unit columns
snow_depth[, c("Elevation_ft", "SWE_Inches", "Snow_Depth_Inches")
           := NULL]
setcolorder(snow_depth, c("Station_ID", "Record_Type", "Latitude",
                          "Elevation_m", "Date", "Month", "SWE_m",
                          "SWE_Code", "Snow_Depth_m",
                          "Snow_Depth_Code"))
# calculate snow density
snow_depth[SWE_m > 0.1 & Snow_Depth_m > 0.1 &
             SWE_Code != "e" & Snow_Depth_Code != "e",
           Snow_Density := (SWE_m / Snow_Depth_m)]
# plot histogram of snow densities
p0 <- ggplot(snow_depth[!is.na(Snow_Density)],
       aes(x = Snow_Density, y = ..density..)) +
  geom_histogram(binwidth = 0.01) +
  publicationTheme +
  xlab("Avg. Snowpack Density (g/cm3)") +
  ylab("Rel. Frequency") +
  xlim(c(0, 4))
# overwrite snow densities greater than solid ice to NA 
density_of_ice <- 0.9167
snow_depth[Snow_Density > density_of_ice, Snow_Density := NA]
ggplot(snow_depth[!is.na(Snow_Density)],
       aes(x = Snow_Density, y = ..density..)) +
  geom_histogram(binwidth = 0.01) +
  publicationTheme +
  xlab("Avg. Snowpack Density") +
  ylab("Relative Frequency") +
  xlim(c(0, 1))
# small peak in histogram near zero suggests some problems
# overwrite all snow densities less than 0.04 to NA
snow_depth[Snow_Density < 0.04, Snow_Density := NA]
p1 <- ggplot(snow_depth[!is.na(Snow_Density)],
       aes(x = Snow_Density, y = ..density..)) +
  geom_histogram(binwidth = 0.01) +
  publicationTheme +
  scale_x_continuous(name = "Average Snowpack Density (g/cm3)",
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Relative Frequency")
# calculate mean snow density and sd
# plot snow density vs. SWE
p2 <- ggplot(snow_depth[!is.na(Snow_Density)],
       aes(x = SWE_m, y = Snow_Density)) +
  geom_point() +
  publicationTheme +
  scale_x_continuous(name = "Snow Water Equivalent (m)",
                     limits = c(0, 3)) +
  scale_y_continuous(name = "Snow Density (g/cm3)",
                     limits = c(0, 1))
#
# Calculate average snow density as a function of snow depth
max_SWE <- snow_depth[, max(SWE_m, na.rm = TRUE)]
seq_of_num <- seq(0, ceiling(max_SWE), 0.02)
mean_snow_density <- data.table(from_SWE_m = seq_of_num,
                                to_SWE_m = seq_of_num + 0.02,
                                avg_snow_density = as.numeric(NA),
                                snow_density_std_dev = as.numeric(NA),
                                n = as.integer(NA))
# Calculate average snow density for every 2 cm of SWE,
for(i in 1:mean_snow_density[, .N]) {
  set(mean_snow_density, i, "avg_snow_density", 
      snow_depth[SWE_m >= mean_snow_density[i, from_SWE_m] &
                   SWE_m <  mean_snow_density[i, to_SWE_m],
                 mean(Snow_Density, na.rm = TRUE)])
  # calculate the sd in snow density for every 2 cm of SWE
  set(mean_snow_density, i, "snow_density_std_dev",
      snow_depth[SWE_m >= mean_snow_density[i, from_SWE_m] &
                   SWE_m <  mean_snow_density[i, to_SWE_m],
                 sd(Snow_Density, na.rm = TRUE)])
  # calculate the n going into every average snow density calc
  set(mean_snow_density, i, "n",
      snow_depth[SWE_m >= mean_snow_density[i, from_SWE_m] &
                   SWE_m <  mean_snow_density[i, to_SWE_m],
                 sum(!is.na(Snow_Density))])
}
# Plot mean snow density as a function of SWE_m
#   minimum number of snow density observations for plots and regression
  n_min <- 10L
SWE_m <-            mean_snow_density[n >= n_min, from_SWE_m + 0.01]
avg_snow_density <- mean_snow_density[n >= n_min, avg_snow_density]
cor(SWE_m, avg_snow_density, use = "pairwise.complete.obs")
# the correlation of x and y above is 0.954.
# calculate linear regression
msd_lm <- lm(avg_snow_density ~ SWE_m)  
  # r-squared is 0.910.
# plot mean snow density as a function of SWE_m  in ggplot2
p3 <- ggplot(mean_snow_density[n >= n_min],
       aes(x = from_SWE_m + 0.01,
           y = avg_snow_density)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  publicationTheme +
  scale_x_continuous(name = "Snow Water Equivalent (m)",
                     limits = c(0, 3)) +
  scale_y_continuous(name = "Snow Density (g/cm3)",
                     limits = c(0, 1)) +
  geom_smooth(method = "lm", color = "blue", fill = "blue")

# plot Fig. 3 - Snow Density vs. SWE as .eps file
postscript(file = "Fig 4 - Snow Density vs. SWE.eps",
           width = 3.54, height = 5.625, pointsize = 7L,
           horizontal = FALSE, family = "Times")
multiplot(p1, p2, p3, cols = 1)
dev.off()

x <- mean_snow_density[n >= n_min, from_SWE_m + 0.01]
y <- mean_snow_density[n >= n_min, avg_snow_density]
glance(lm(y ~ x))

# plot relationships in "Sierra_SWE"--------------------------------------

# CONTROL VARIABLES FOR PLOTS:
# What is the minimum number of months in a monthly average for the
# monthly average to be included in the plots and regression analysis?
      min_months <- 10L

# calculate upper and lower xlims in m
lower_xlim <- min(Sierra_SWE[, Elevation_m], na.rm = TRUE)
upper_xlim <- max(Sierra_SWE[, Elevation_m], na.rm = TRUE)

# PLOT: SWE vs. Elevation

# calculate regression stats for every month
 # create DT to collect comparative stats
 regressionStats <- data.table(Month = month.name[1:12],
                               lm1_Elevation_R2 = as.numeric(NA),
                               lm2_Insolation_R2 = as.numeric(NA),
                               lm3_Both_R2 = as.numeric(NA),
                               lm4_Full_R2 = as.numeric(NA),
                               lm1_Elevation_AdjR2 = as.numeric(NA),
                               lm2_Insolation_AdjR2 = as.numeric(NA),
                               lm3_Both_AdjR2 = as.numeric(NA),
                               lm4_Full_AdjR2 = as.numeric(NA),
                               lm1_Elevation_pValue = as.numeric(NA),
                               lm2_Insolation_pValue = as.numeric(NA),
                               lm3_Both_pValue = as.numeric(NA),
                               lm4_Full_pValue = as.numeric(NA))
for (j in 1:12) {
  month_j <- month.name[j]
  # linear regression analysis
  linear1 <- lm(Avg_SWE_m ~ Elevation_m,
               data = Sierra_SWE[Month == month_j &
                                   SWE_n >= min_months])
  linear2 <- lm(Avg_SWE_m ~ Insolation_MJperM2,
                data = Sierra_SWE[Month == month_j &
                                    SWE_n >= min_months])
  linear3 <- lm(Avg_SWE_m ~ Elevation_m + Insolation_MJperM2,
               data = Sierra_SWE[Month == month_j &
                                   SWE_n >= min_months])
  linear4 <- lm(Avg_SWE_m ~ Elevation_m * Insolation_MJperM2,
                data = Sierra_SWE[Month == month_j &
                                    SWE_n >= min_months])
  # Calculate pearson correlation coefficients
  Sierra_SWE[Month == month_j,
             Cor_SWEvsElevation := cor(Avg_SWE_m, Elevation_m,
                                     use = "pairwise.complete.obs")]
  Sierra_SWE[Month == month_j,
             Cor_SWEvsInsolation := cor(Avg_SWE_m, Insolation_MJperM2,
                                        use = "pairwise.complete.obs")]
  # calculate r-squared
  glance_linear1 <- glance(linear1)
  glance_linear2 <- glance(linear2)
  glance_linear3 <- glance(linear3)
  glance_linear4 <- glance(linear4)
  
  # populate regressionStats
  regressionStats[Month == month.name[j],
                  ':=' (lm1_Elevation_R2 =  glance_linear1[1, 1],
                        lm2_Insolation_R2 = glance_linear2[1, 1],
                        lm3_Both_R2 =       glance_linear3[1, 1],
                        lm4_Full_R2 =       glance_linear4[1, 1],
                        lm1_Elevation_AdjR2 =  glance_linear1[1, 2],
                        lm2_Insolation_AdjR2 = glance_linear2[1, 2],
                        lm3_Both_AdjR2 =       glance_linear3[1, 2],
                        lm4_Full_AdjR2 =       glance_linear4[1, 2],
                        lm1_Elevation_pValue =  glance_linear1[1, 5],
                        lm2_Insolation_pValue = glance_linear2[1, 5],
                        lm3_Both_pValue =       glance_linear3[1, 5],
                        lm4_Full_pValue =       glance_linear4[1, 5])]
  
  # COMMENT ON MODELING APPROACH:
  # After examining the regression statistics, it is clear that
  # adding insolation as an explanatory variable is NOT useful.
  
  # COMMENT ON LACK OF WEIGHTING:
  # I'm pulling out all the weighting by SWE_n. Whether the 
  # regressions are weighted or not doesn't seem to make a big
  # difference in their slopes or intercepts, but it does
  # seem to greatly increase the uncertainty of their predictions
  # (unreasonably so, many negative SWE values in simulations).
  
  Sierra_SWE[Month == month_j, R_squared := glance_linear1[1, 1]]
  
  # output correlation details
  output <- as.data.table(suppressWarnings(augment(linear1)))
  output_names_vec <- names(output)
  sta_ids <- Sierra_SWE[Month == month_j & SWE_n >= min_months,
                        Station_ID]
  sta_names <- Sierra_SWE[Month == month_j & SWE_n >= min_months,
                          Station_Name]
  output[, ':=' (Station_ID = sta_ids,
                 Station_Name = sta_names)]
  setcolorder(output, c("Station_ID", "Station_Name",
                        output_names_vec[1:9]))
  fwrite(output, as.character(paste0("SLRmodel ", month_j, ".txt")),
         sep = "\t")
}

# Export regressionStats
fwrite(regressionStats, "RegressionStats.txt", sep = "\t") 
 
# Plot Average SWE vs. Elevation for each month with ggplot2
upper_ylim <- 1.5

# plot data
postscript(file = "Fig 3 - SWE vs Elevation.eps",
           width = 7.48, height = 4.62, pointsize = 7L,
           horizontal = FALSE, family = "Times")
ggplot(data = Sierra_SWE[SWE_n >= min_months],
         aes(x = Elevation_m,
             y = Avg_SWE_m)) +
    theme(panel.border = element_rect(fill = NA, color = "black",
                                      linetype = 1),
          panel.spacing.x = unit(4L, "mm"),
          panel.spacing.y = unit(0L, "mm"),
          strip.background = element_rect(fill = NA),
          strip.text = element_text(size = 7L),
          axis.ticks.x = element_line(color = "black", linetype = 1),
          axis.text = element_text(color = "black", size = 7L),
          axis.title = element_text(size = 7L)) +
    scale_x_continuous(name = "Elevation (m)",
                       limits = c(lower_xlim, upper_xlim)) +
    scale_y_continuous(name = "Snow Water Equivalent (m)",
                       limits = c(0, upper_ylim)) +
    geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE,
                se = FALSE) +
    geom_point(shape = 22, size = 1, color = "black",
               fill = "black") +
    facet_wrap(~Month)
dev.off()

# # Plot Average SWE vs. Total Monthly Insolation (from ArcGIS)
#   ggplot(data = Sierra_SWE[SWE_n >= min_months],
#          aes(x = Insolation_MJperM2,
#              y = Avg_SWE_m,
#              weight = SWE_n)) +
#     ggtitle("Average SWE vs. Insolation") +
#     theme(plot.title = element_text(hjust = 0.5),
#           panel.border = element_rect(fill = NA, color = "black",
#                                       linetype = 1)) +
#     xlab("Insolation (MJ/sq-meter)") +
#     ylab("Snow Water Equivalent (m)") +
#     xlim(c(0, 1000)) +
#     ylim(c(0, upper_ylim)) +
#     geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE) +
#     geom_point(shape = 22, size = 0.5, color = "black",
#                fill = "black") +
#     facet_wrap(~Month)
  

# PLOT: Avg_diff vs. Elevation for each month
upper_ylim <- ceiling(Sierra_SWE[, max(Avg_diff_cm, na.rm = TRUE)])
upper_ylim2 <- round(upper_ylim, -1)
if (upper_ylim2 > upper_ylim) {
  upper_ylim <- upper_ylim2
} else {
  upper_ylim <- upper_ylim2 + 10
}

lower_ylim <- floor(Sierra_SWE[, min(Avg_diff_cm, na.rm = TRUE)])
lower_ylim2 <- round(lower_ylim, -1)
if (lower_ylim > lower_ylim2) {
  lower_ylim <- lower_ylim2
} else {
  lower_ylim <- lower_ylim - 10
}
# set up the tiff call
figure_length <- 4 * 1.7
tiff("Fig B10 - Average Differences by Month.tiff",
     compression = "none",
     width = 3.15, height = figure_length, units = "in",
     res = 144)
par(mfcol = c(4, 1))
fig_margins <- c(0.3, 0.6, 0.25, 0.1)
# plot figures in tiff
for (j in 2:5) {
  x <- Sierra_SWE[Month == month.name[j] & Avg_diff_n >= 5,
                  Elevation_m]
  y <- Sierra_SWE[Month == month.name[j] & Avg_diff_n >= 5,
                  Avg_diff_cm]
  par(mai = fig_margins)
  plot(x, y, xlim = c(lower_xlim, upper_xlim),
       ylim = c(lower_ylim, upper_ylim),
       xlab = "Elevation (m)", ylab = "Avg. Diff. (cm)",
       main = as.character(month.name[j]), pch = 15, las = 1)
  abline(h = 0L, lty = 2, lwd = 2)
}
dev.off()


# CALCULATE SNOW SHIELDING CORRECTIONS----------------------------------

# 1- injest text file of sample info in CRONUScalc format
  marrero <- fread("Marrero_simple_YESerosion_NOsnow_TMandLC.txt") 
  
# 2- set number of monte carlo sims for Balculator
  sims <- 1000L
  
# 3- set the minimum number of records in a monthly average for it to be
  #   included in the regression.
  #  See "min_months", defined above, on (or near) line 1818.
  
# 4- load boulder_height text file
  boulder_height <- fread("Boulder_Height.txt")  
  
# 5- Set the coefficient to multiply the reconstructed SWEs by...
  #  This enables the model to explore the sensitivie of our tentative
  #   conclusions (that the historical snow record in the Sierra
  #   Nevada is a good proxy for average postglacial snow depths)
  #   to different snow depths.
  #  What if long-term average snow depths were 95% the historical
  #   average? This variable will help us find out.
  historical_snow_coef <- c(1.0)
  proportionalVariability <- TRUE
  
# LOOP OVER THE CALCULATIONS BELOW AND CALCULATE SNOW CORRECTIONS
#  FOR A VARIETY OF SNOW CONDITIONS
for (h in 1:length(historical_snow_coef)) {
    
# 6- create sample_data DT
  sample_data <- data.table(Sample_Name = marrero[, Sample_Name],
                            Latitude_DD = marrero[, Latitude_DD],
                            Longitude_DD = marrero[, Longitude_DD],
                            Elevation_m  = marrero[, Elevation_m],
                            Boulder_Height_m = as.numeric(NA))
  # Add months 1:12 as columns and then gather them into a single column,
  #   this will replicate the other data as needed.
  sample_data[, month.name[1:12] := as.numeric(NA)]
  # gather the months into a single column
  sample_data <- as.data.table(gather(sample_data, Month,
                                  Est_Avg_SWE_m, January:December))
  
  # populate sample_data with the other column names
  sample_data[, ':=' (Est_Avg_SWE_m             = as.numeric(NA),
                      Est_SWE_SD                = as.numeric(NA),
                      Est_Avg_Snow_Density      = as.numeric(NA),
                      Est_Snow_Density_SD       = as.numeric(NA),
                      Est_Avg_Snow_Depth_m      = as.numeric(NA),
                      Est_Snow_Depth_SD         = as.numeric(NA),
                      Snow_Surf_Rel_to_Sample_m = as.numeric(NA),
                      monthly_Ssnow             = as.numeric(NA),
                      monthly_Ssnow_SD          = as.numeric(NA),
                      annual_Ssnow              = as.numeric(NA),
                      annual_Ssnow_SD           = as.numeric(NA))]
  
# 7- assemble a DT for a quick and simple approach to version 3
#     of the Balculator
  simpleBalco <- data.table(Sample_Name = marrero[, Sample_Name],
              Latitude_DD         = marrero[, Latitude_DD],
              Longitude_DD        = marrero[, Longitude_DD],
              Elevation_m         = marrero[, Elevation_m],
              Elevation_Flag      = as.character(NA),
              Sample_Thickness_cm = marrero[, Sample_Thickness_cm],
              Sample_Density_gcm3 = marrero[, Bulk_Density_gcm3],
              Shielding           = marrero[, Shielding_Factor],
              Erosion_Rate_cmyr   = marrero[, Erosion_Rate_mmkyr / 10000],
              Collection_Year     = marrero[, Year_Collected_AD],
              End_of_Line1        = as.character(NA),
              Sample_Name2        = marrero[, Sample_Name],
              Nuclide             = as.character(NA),
              Mineral             = as.character(NA),
              Nuclide_Conc        = marrero[, Conc_10Be],
              Nuclide_Uncert      = marrero[, Conc_10Be_Uncertainty],
              Standard            = marrero[, `10Be_Standardization`],
              End_of_Line2        = as.character(NA))
  
  # populate unfilled fields of simple Balco
  simpleBalco[, ':=' (Elevation_Flag = "std",
                      End_of_Line1   = ";",
                      Nuclide        = "Be-10",
                      Mineral        = "quartz",
                      End_of_Line2   = ";")]
  
  
# 8.0- build text files of "sims" length, one for each sample in "marrero"
  for (i in 1:marrero[, .N]) {
  
    # populate the sample name vector
    sample_name <- marrero[i, Sample_Name]
    iteration <- 1:sims
    vector_sample_name <-
      rep_len(paste0(sample_name, "--", iteration), sims)
    
    # create latitude vector
    latitude <- marrero[i, Latitude_DD]
    lat_uncert <- marrero[i, Latitude_Uncertainty]
    vector_latitude <- rnorm(sims, latitude, lat_uncert)
    
    # create longitude vector
    longitude <- marrero[i, Longitude_DD]
    long_uncert <- marrero[i, Longitude_Uncertainty]
    vector_longitude <- rnorm(sims, longitude, long_uncert)
    
    # create elevation vector
    elevation <- marrero[i, Elevation_m]
    el_uncert <- marrero[i, Elevation_Uncertainty]
    vector_elevation <- rnorm(sims, elevation, el_uncert)
    
    # create Elevation/Pressure Flag vector
    vector_flag <- rep_len("std", sims)
    
    # create sample thickness vector
    thickness <- marrero[i, Sample_Thickness_cm]
    thickness_uncert <- marrero[i, Thickness_Uncertainty]
    vector_thickness <- rnorm(sims, thickness, thickness_uncert)
    
    # create sample density vector
    sampleDensity <- marrero[i, Bulk_Density_gcm3]
    density_uncert <- marrero[i, Density_Uncertainty]
    vector_density <- rnorm(sims, sampleDensity, density_uncert)
    
    # create shielding vector
    shielding <- marrero[i, Shielding_Factor]
    shielding_uncert <- marrero[i, Shielding_Uncertainty]
    vector_shielding <- rnorm(sims, shielding, shielding_uncert)
    # wipe values greater than 1 to 1
    shielding_index <- vector_shielding > 1
    vector_shielding[shielding_index] <- 1.0
    
    # create erosion rate vector
    erosion <- marrero[i, Erosion_Rate_mmkyr / 10000]
    erosion_uncert <- marrero[i, Erosion_Rate_Uncertainty / 10000]
    vector_erosion <- rnorm(sims, erosion, erosion_uncert)
    # convert negative erosion rates to zero
    erosion_index <- vector_erosion < 0
    vector_erosion[erosion_index] <- 0
    
    # create collection year vector
    year <- marrero[i, Year_Collected_AD]
    year_uncert <- marrero[i, Year_Collected_Uncertainty]
    vector_year <- rnorm(sims, year, year_uncert)
    vector_year <- round(vector_year)
    
    # create end-of-line vector
    vector_endOfLine <- rep_len(";", sims)
    
    # sample name vector will be repeated
    
    # create nuclide vector
    vector_nuclide <- rep_len("Be-10", sims)
    
    # create mineral vector
    vector_mineral <- rep_len("quartz", sims)
    
    # create concentration vector
    vector_Be10conc <- rep_len(marrero[i, Conc_10Be], sims)
    
    # create concentration uncertainty vector
    vector_Be10uncert <- rep_len(marrero[i, Conc_10Be_Uncertainty], sims)
    
    # create standardization vector
    vector_standard <- rep_len(marrero[i, `10Be_Standardization`], sims)
    
    # repeat end-of-line column
    
    # combine vectors into a data.table for an ad hoc monte carlo
    #  approach to the balculator v. 3
    balco <- data.table(Sample_Name1     = vector_sample_name,
                        Latitude         = vector_latitude,
                        Longitude        = vector_longitude,
                        Elevation        = vector_elevation,
                        Elevation_Flag   = vector_flag,
                        Sample_Thickness = vector_thickness,
                        Sample_Density   = vector_density,
                        Shielding_Corr   = vector_shielding,
                        Erosion_Rate     = vector_erosion,
                        Sample_Collection_Year = vector_year,
                        End_of_Line1     = vector_endOfLine,
                        Sample_Name2     = vector_sample_name,
                        Nuclide          = vector_nuclide,
                        Mineral          = vector_mineral,
                        Nuclide_Conc     = vector_Be10conc,
                        Nuclide_Uncert   = vector_Be10uncert,
                        Standardization  = vector_standard,
                        End_of_Line2     = vector_endOfLine)
    
    # SNOW CORRECTION
    
    # Add Months and gather them into a single column
    balco[, month.name[1:12] := as.numeric(NA)]
    balco <- as.data.table(gather(balco, Month, SWE_m,
                                  January:December))
    # Populate balco with the other column names
    balco[, ':=' (SWE_m             = as.numeric(NA),
                  Snow_Density      = as.numeric(NA),
                  Snow_Depth_m      = as.numeric(NA),
                  Boulder_Height_m  = as.numeric(NA),
                  Snow_Surf_Rel_to_Sample_m = as.numeric(NA),
                  Attenuation_Length = as.numeric(NA),
                  monthly_Ssnow     = as.numeric(NA),
                  annual_Ssnow      = as.numeric(NA))]
    
    
# 8.1- Loop through each month and estimate SWE and density
    # convert "elevation" from line 2053 (or so) into a DT
    sampleElevation <- data.table(Elevation_m = elevation)
    
    # for each month...  
    for (j in 1:12) {
      
      # Develop linear model for SWE as function of elevation
      linear_model <- lm(Avg_SWE_m ~ Elevation_m,
                         data = Sierra_SWE[Month == month.name[j] &
                                             SWE_n >= min_months])
      # Predict average SWE at sampling site
      Avg_SWE <- as.data.table(predict.lm(linear_model,
                 sampleElevation, interval = "predict", level = 0.6827))
      
      # Adjust predicted Avg_SWE by the historical snow depth
      #  coefficient. This lets us model different snow
      #  accumulation scenarios (0% modern, 80% modern, etc.).
      # NOTE: The constantUncert variable lets us switch between
      #  scaling the variability with the historical_snow_coef
      #  (so, 50% historical snow is not only half as much, but
      #  half as variable in depth) and assuming that the 
      #  variability remains constant at observed (100% modern)
      #  levels.
      if (proportionalVariability == TRUE) {
        Avg_SWE <- Avg_SWE * historical_snow_coef[h]
      } else {
        sdSWE   <- Avg_SWE[, upr - fit]
        Avg_SWE[, ':=' (fit = fit * historical_snow_coef,
                        upr = fit + sdSWE,
                        lwr = fit - sdSWE)]
      }
      
      # assign the predicted SWE values to balco
      meanSWE <- Avg_SWE[, fit]
      sdSWE   <- Avg_SWE[, upr - fit]
      balco[Month == month.name[j],
            SWE_m := rnorm(sims, meanSWE, sdSWE)]
      
      # assign reasonable snow density values to balco
      # convert meanSWE into a DT
      meanSWE <- data.table(SWE_m = meanSWE)
      snowDensity <- as.data.table(predict.lm(msd_lm, meanSWE,
                      interval = "predict", level = 0.6827))
      meanSnowDensity <- snowDensity[, fit]
      sdSnowDensity <- snowDensity[, upr - fit]
      balco[Month == month.name[j],
            Snow_Density := rnorm(sims, meanSnowDensity,
                                  sdSnowDensity)]
    # end of j loop
    }
    
    # Adjust SWE for elevations below 4000 ft (1220 m) to 0.
    balco[Elevation < 1220, SWE_m := 0]
    
    # Linearly adjust SWE for elevations between 4000 ft (1220 m)
    # and 6500 ft (1980 m) toward zero.  Avg SWE at 4000 ft will be
    # zero, Avg SWE at 6500 ft will be the full calculated value.
    
    # Without an adjustment, the code predicts an annual snow shielding
    # correction of 0.905 for a bedrock surface at sea level.  This is
    # equivalent to a 3.54 m snowpack that lasts 6 months.  This is an
    # unreasonable average for California in the Holocene.
    
    # While doing field work in the Sierra Nevada during 2016, Will
    # Montz and I (Richard Becker) observed that the lowest elevation
    # snow stakes along CA highway 168 between Lake Edison and Fresno
    # were at about 4,000 ft above sea level.  California native John Hora
    # confirmed that this was the approximate lower limit of winter snow.
    
    # The script therefore assumes that the snow shielding
    # correction goes to zero at 4,000 ft.  Snow shielding values will be
    # linearly interpolated from the calculated value at the elevation
    # of the lowest-elevation snow survey sites, Bell and Beehive
    # Meadows at 6500' (1980 m), to zero at 4,000' (1220 m).
    
    # All the samples from  Tuolumne Meadows and Lyell Canyon are above
    # 6500 ft, so the snow correction is unadjusted for them. For the
    # samples in chapter 3 of Becker's (2018) dissertation, however,
    # some low elevations snow corrections were adjusted.
    balco[Elevation >= 1220 & Elevation < 1980,
            SWE_m := SWE_m * (Elevation - 1220) / 760]
    
    # Record reconstructed SWE in sample_data
    results <- balco[, mean(SWE_m, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Est_Avg_SWE_m := results[, V1]]
    results <- balco[, sd(SWE_m, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Est_SWE_SD := results[, V1]]
  
    # convert all snow densities less than 0.10 to 0.10
    #  and all densities greater than solid ice to 0.9167
    # (This is rarely if ever necessary.)
    balco[Snow_Density < 0.10, Snow_Density := .10]
    balco[Snow_Density > 0.9167, Snow_Density := 0.9167]
    
    # Record reconstructed snow densities in sample_data
    results <- balco[, mean(Snow_Density, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Est_Avg_Snow_Density := results[, V1]]
    results <- balco[, sd(Snow_Density, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Est_Snow_Density_SD := results[, V1]]
    
    
# 8.2- Convert estimated average SWE and snow density into snow depths    
    balco[, Snow_Depth_m := SWE_m / Snow_Density]
    
    # Many summertime predicted snow depths are negative,
    #  because mean summer snow depths are low but variable.
    # Convert negative snow depths to 0.
    balco[Snow_Depth_m < 0, Snow_Depth_m := 0]
    
    # # UNCOMMENT this code to make a histogram
    # ggplot(balco[Month == "April"], aes(Snow_Depth_m)) +
    #   geom_histogram(binwidth = 0.1)
    
    # Record reconstructed snow depths in sample_data
    results <- balco[, mean(Snow_Depth_m, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Est_Avg_Snow_Depth_m := results[, V1]]
    results <- balco[, sd(Snow_Depth_m, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Est_Snow_Depth_SD := results[, V1]]
    
    
# 8.3- Calculate snow depth relative to sample surface.
    # create distribution of boulder height
    meanBoulderHeight <-
      boulder_height[Sample_Name == marrero[i, Sample_Name],
                     Boulder_Height_m]
    # save measured boulder height to sample_data
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Boulder_Height_m := meanBoulderHeight]
    # calculate sd of boulder height
    sdBoulderHeight <-
      boulder_height[Sample_Name == marrero[i, Sample_Name],
                                      Boulder_Height_SD]
    # assign values to balco
    balco[, Boulder_Height_m := rnorm(sims, meanBoulderHeight,
                                      sdBoulderHeight)]
    # export boulder height file
    output <- data.table(Sample_Name = balco[, Sample_Name1],
                    Boulder_Height_m = balco[, Boulder_Height_m])
     # calculate snow name
     snow_coef <- as.character(historical_snow_coef[h] * 100)
     # calculate uncertainty type (variable | constant)
     if (proportionalVariability == TRUE) {
       varType <- "proportionalVariability_"
     } else {
       varType <- "historicalVariability_"
     }
    fwrite(output, paste0("exportBoulderHeight_",
                          snow_coef,
                          "PercentHistoricalSnow_",
                          varType,
                          marrero[i, Sample_Name],
                          ".txt"), sep = "\t")
    # Calculate height of snow surface relative to sample
    balco[, Snow_Surf_Rel_to_Sample_m := Snow_Depth_m - Boulder_Height_m]
    # Set negative depths to zero
    balco[Snow_Surf_Rel_to_Sample_m < 0,
          Snow_Surf_Rel_to_Sample_m := 0]
    
    # Record reconstructed snow surface heights relative to
    #  the sample in sample_data
    results <- balco[, mean(Snow_Surf_Rel_to_Sample_m, na.rm = TRUE),
                     by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                Snow_Surf_Rel_to_Sample_m := results[, V1]]
    # uncertainty in the height of the snow relative to the
    #  sample is the same as uncertainty in snow depth
    
    
# 8.4- Calculate monthly snow shielding.
    # see Eq. 3.76 in Gosse & Phillips, 2001:
    # "Terrestrial in situ cosmogenic nuclides: theory and application"
    # QSR, v. 20, p. 1475-1560    
    
    # create distribution of attentuation lengths
    meanAttLength <- marrero[i, Attenuation_Length_gcm2]
    sdAttLength <- marrero[i, Attenuation_Length_Uncertainty]
    balco[, Attenuation_Length :=
            rnorm(sims, meanAttLength, sdAttLength)]
    
    # calculate monthly snow shielding correction
    balco[, monthly_Ssnow := exp(-(Snow_Surf_Rel_to_Sample_m * 100 *
                                     Snow_Density / Attenuation_Length))]
    
    # UNCOMMENT to plot histogram
    # ggplot(balco[Month == "April"], aes(monthly_Ssnow)) +
    #   geom_histogram()
    
    # Record monthly snow shielding factors in sample_data
    results <- balco[, mean(monthly_Ssnow, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                monthly_Ssnow := results[, V1]]
    results <- balco[, sd(monthly_Ssnow, na.rm = TRUE), by = "Month"]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                monthly_Ssnow_SD := results[, V1]]
    
    
# 8.5- Calculate average correction over the course of the entire year    
    balco[, annual_Ssnow := mean(monthly_Ssnow), by = "Sample_Name1"]
    
    # UNCOMMENT to plot histogram
    # ggplot(balco[!is.na(annual_Ssnow)], aes(annual_Ssnow)) +
    #   geom_histogram(binwidth = 0.0025)
    
    # Record annual snow shielding factors in sample_data
    #  and in simpleBalco
    results <- balco[, mean(annual_Ssnow, na.rm = TRUE)]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                annual_Ssnow := results]
    simpleBalco[i, Shielding := Shielding * results]
    results <- balco[, sd(annual_Ssnow, na.rm = TRUE)]
    sample_data[Sample_Name == marrero[i, Sample_Name],
                annual_Ssnow_SD := results]
    
    
# 8.6- Merge snow correction with topographic shielding correction
    balco[, Shielding_Corr := Shielding_Corr * annual_Ssnow]
    
    # UNCOMMENT to plot histogram
    # ggplot(balco[!is.na(Shielding_Corr)], aes(Shielding_Corr)) +
    #   geom_histogram(binwidth = 0.0025)
    
# 8.7- Copy balco and clean copy so it is compatible with version 3
    # of the balculator.
    output <- balco[Month == "January" & !is.na(Shielding_Corr)]
    outputNameVec <- names(output)
    output[, outputNameVec[19:27] := NULL]
    
# 8.8- Export "monte carlo" file for each sample
    fwrite(output, paste0("monte_carlo_", snow_coef,
                          "PercentHistoricalSnow_", varType,
                          marrero[i, Sample_Name], ".txt"), sep = "\t")
    
# 8.9- Close the "i" loops    
  }
  
# 8.10- Plot graphs
  # # annual average snow corr. vs. Elevation
  # x <- sample_data[Month == "January", Elevation_m]
  y <- sample_data[Month == "January", annual_Ssnow]
  # plot(x, y, xlab = "Elevation (m)",
  #      ylab = "Annual average snow correction",
  #      main = paste0(snow_coef, " Snow Correction vs. Elevation"))
  
  #  annual average snow corr. vs. sample height (m)
  x <- sample_data[Month == "January", Boulder_Height_m]
  plot(x, y, xlab = "Boulder/Sample Height (m)",
       ylab = "Annual average snow correction",
       xlim = c(-1.0, 4.0),
       ylim = c(0.7, 1.0),
       main = paste0(snow_coef, "% Average Snow Corr. vs. Boulder Height"))

# close the h loop
}  
  
  
# EXPORT files-------------------------------------------------
  
  # Output results as text files
  # output detailed file
  fwrite(sample_data, paste0("sample_data_",
                             snow_coef,
                             "PercentHistoricalSnow_",
                             varType,
                             "output_details.txt"), sep = "\t")
  
  # output summary file
  sample_summary <- data.table(
    Sample_Name = sample_data[Month == "January", Sample_Name],
    Elevation_m = sample_data[Month == "January", Elevation_m],
    Boulder_Height_m = sample_data[Month == "January", Boulder_Height_m],
    Snow_Shielding = sample_data[Month == "January", annual_Ssnow],
    Snow_Shielding_SD = sample_data[Month == "January", annual_Ssnow_SD])
  fwrite(sample_summary, paste0("sample_data_",
                                snow_coef,
                                "PercentHistoricalSnow_",
                                varType,
                                "output_summary.txt"), sep = "\t")
  
  # output final copy of Sierra_SWE
  fwrite(Sierra_SWE, "Sierra_SWE_finaloutput.txt", sep = "\t")
  
  # output simpleBalco
  fwrite(simpleBalco, paste0("simpleBalco_",
                             varType,
                             snow_coef,
                             "PercentHistoricalSnow.txt"), sep = "\t")
    
  
# End of script