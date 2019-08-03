# This script cleans daily snow water equivalent (SWE) records from the
# Sierra Nevada Mountains (USA) and prepares them for analysis.

# install and load packages------------------------------------------------

# data.table for the fast loading and analysis of large datasets
# install.packages("data.table")
library(data.table)

# lubridate for easy manipulation of dates and times
# install.packages("lubridate")
library(lubridate)


# FUNCTIONS DEFINED HERE---------------------------------------------------

clean_record <- function(DT, Station_ID, first_mo = 5L, last_mo = 11L,
                         threshold = 14L) {
  # Purpose:
  #   This function does 10 things to clean the snow records:
  #     1- It breaks the Date column into three new columns, Year, Month,
  #        and MDay.
  #     2- It converts negative SWEs and snow depths to NA.
  #     3- It checks for error code "N" and converts those measurements, if
  #        any to NA.
  #     4- It estimates missing SWE measurements by interpolation (if the
  #        gap is equal to or less than "threshold").
  #     5- It estimates missing snow depth measurements by interpolation
  #        (if the gap is less than or equal to "threshold").
  #     6- It assumes missing SWE values are 0 between
  #        first_mo and last_mo (inclusive; May and Nov are the defaults).
  #     7- It wipes all snow depth measurements between first_mo and
  #        last_mo to NA, unless otherwise instructed.  There are too
  #        many erroneous snow depths during the summer to bother sorting
  #        them out.  The goal of the snow depth record is to discover a
  #        relationship between SWE and snow density, so this should
  #        improve the script.
  #     8- Replaces "" (no character) in the SWE_Code and Snow_Depth_Code
  #        columns with "_". This prevents warning messages from
  #        popping up in the Khione code when the file snow_depth.txt is
  #        repeatedly read in. Otherwise, R will repeatedly discover on
  #        row 9180 of the Snow_Depth_Code column of the DAN record that
  #        the column should actually be character.  Similar "discoveries"
  #        are also make for the other daily records, at other row numbers.
  #     9- It "spot cleans" specific parts of specific records as defined
  #        below.
  #     10- It deletes MDay and exports a "clean" copy of the daily data.
  #
  # Args:
  #   DT: a data.table with a column of dates, labeled "Date", and a column
  #       of snow water equivalent (SWE) measurements in numeric format
  #       labeled "SWE_Inches".
  #   Station_ID: a 3-letter code for the station location.
  #   first_mo: first month to change the NAs to zero (default = May)
  #   last_mo:  last month to change NAs to zero (default = November)
  #   threshold:  gaps in the SWE and snow depth records equal to or
  #               shorter than this will be filled by interpolation.
  #
  # Returns:
  #   An updated data.table.
  #
  # Error checking:
  if(!is.data.table(DT)) {
    stop("DT needs to be a data.table", call. = FALSE)
  }
  if ("Date" %in% names(DT) == FALSE) {
    stop("The data.table needs a 'Date' column.", call. = FALSE)
  }
  if (!is.Date(DT[, Date])) {
    stop("The DT's 'Date' column must be in date format.", call. = FALSE)
  }
  if ("SWE_Inches" %in% names(DT) == FALSE) {
    stop("The DT needs a column titled exactly 'SWE_Inches'.",
         call. = FALSE)
  }
  if (!is.numeric(DT[, SWE_Inches])) {
    stop("The snow water equivalent column needs to be numeric.",
         call. = FALSE)
  }
  if (nchar(Station_ID) != 3L) {
    stop("The Station_ID must be three letters long.", call. = FALSE)
  }
  if (!is.integer(threshold)) {
    stop("The threshold value must be an integer.", call. = FALSE)
  }
  # Main body of function:
  # 
  # STEP ONE: Split Date column into three new columns: Year, Month, and
  #            MDay. Then reorder the columns.
    DT[, c("Year", "Month", "MDay") :=
         .(year(Date), month(Date), mday(Date))]
    setcolorder(DT, c("Date", "Year", "Month", "MDay", "SWE_Inches",
                      "SWE_Code", "Snow_Depth_Inches", "Snow_Depth_Code"))
  #
  # STEP TWO: Convert negative values to NA.
    # check for negative SWE values
    if (DT[SWE_Inches < 0L, .N] > 0L) {
      # if present, convert to NA
      DT[SWE_Inches < 0L, SWE_Inches := NA]
    }
    # check for negative snow depths
    if (DT[Snow_Depth_Inches < 0L, .N] > 0L) {
      # if present, convert to NA
      DT[Snow_Depth_Inches < 0L, Snow_Depth_Inches := NA]
    }
  # 
  # STEP THREE: Check for code "N", measurements that the data collectors
  #           identified as erroneous, and convert to NA.
    # check for code "N" in the SWE data
    if ("N" %in% levels(DT[, SWE_Code])) {
      # if present, wipe those SWE values to NA
      DT[SWE_Code == "N", SWE_Inches := NA]
    }
    # check for code "N" in the Snow Depth data
    if ("N" %in% levels(DT[, Snow_Depth_Code])) {
     # if present, wipe those snow depths to NA
     DT[Snow_Depth_Code == "N", Snow_Depth_Inches := NA]
    }
    # A snow_depth of 195" seems associated with the "N" error code (see
    # the DAN and TNY records, for example). Therefore, the snow_depth is
    # rewritten to NA when snow_depth is recorded as 195".
    DT[Snow_Depth_Inches == 195L, Snow_Depth_Inches := NA]
    #
    # Snow depths of 219" and 220" are clearly used as an error code in
    # the White Wolf (WHW) record. There are no "N" codes for the WHW
    # record. Convert all snow depths of 219 and 220 inches to NA.
    DT[Snow_Depth_Inches == 219L, Snow_Depth_Inches := NA]
    DT[Snow_Depth_Inches == 220L, Snow_Depth_Inches := NA]
    #
    # 176" is clearly used in lieu of an error code in the HRS
    # (Horse Meadow) record. Wipe all 176" to NA.
    DT[Snow_Depth_Inches == 176, Snow_Depth_Inches := NA]
  #
  # STEP FOUR: Fill small holes in the SWE column.
    # Add a column for # of consecutive days of NA,
      # if there's a SWE measurement, assign 0L to days_of_NA
      DT[!is.na(SWE_Inches), days_of_NA := 0L]
      # if there isn't a SWE measurement, assign 1L to days_of_NA
      DT[is.na(SWE_Inches), days_of_NA := 1L]
    # identify the first line in the DT *WITH* a SWE measurement
    for (i in 1:DT[, .N]) {
      if (DT[i, is.na(SWE_Inches)]) {
        next()
      }
      # if the SWE measurement in row i isn't NA,
      # assign i + 1 as the starting row
      starting_row <- i + 1L
      break()
    }
    # determine # of consecutive days of NA   
    for (i in starting_row:DT[, .N]) {
      # if days_of_NA = 0, go to next line
      if (DT[i, days_of_NA == 0L]) {
        next()
      } 
      # otherwise, days_of_NA = previous "days_of_NA" plus 1
      DT[i, days_of_NA := (DT[i - 1L, days_of_NA] + 1L)]
    }
    # identify the last line in DT with a SWE measurement
    for (i in DT[, .N]:starting_row) {
      if (DT[i, is.na(SWE_Inches)]) {
        next()
      }
      # if the SWE measurement in row i isn't NA,
      # assign i - 1 as the ending row
      ending_row <- i - 1L
      break()
    }
    # carry values of "days_of_NA" upward, so blocks of time with missing
    # SWE measurements all have the same "days_of_NA"
    for (i in ending_row:starting_row) {
      if (DT[i, days_of_NA == 0L]) {
        j <- 1L
        next()
      }
      # if j = 1, assign j = 0 and go to next,
      # this prevents 0's from being carried upward
      if (j == 1L) {
        j <- 0L
        next()
      }
      # otherwise, row i's days_of_NA = row i + 1's days_of_NA
      DT[i, days_of_NA := DT[i + 1L, days_of_NA]]
    }
    # if days_of_NA is equal to or less than threshold, interpolate SWE  
    # initialize gap_length as zero
    gap_length <- 0L
    for (i in starting_row:ending_row) {
      # this if loop enables the code to skip over gaps bigger than
      # "threshold" and over gaps that have already been filled in
      if (gap_length > 0L) {
        gap_length <- gap_length - 1L
        next()
      }
      # if there's a SWE measurement, go to next
      if (DT[i, days_of_NA == 0L]) {
        next()
      }
      # if the gap is greater than "threshold", reset gap_length and go to
      # next
      if (DT[i, days_of_NA > threshold]) {
        gap_length <- DT[i, days_of_NA]
        next()
      }
      # interpolate SWE over all other gaps (less than or equal to
      # "threshold")
      gap_length <- DT[i, days_of_NA]
      starting_SWE <- DT[i - 1L, SWE_Inches]
      ending_SWE <- DT[i + gap_length, SWE_Inches]
      for (j in 1:gap_length) {
        DT[(i + j - 1L), SWE_Inches := starting_SWE +
           (ending_SWE - starting_SWE) * (j / (gap_length + 1L))]
      }
    }
    # delete the column "days_of_NA"
    DT[, days_of_NA := NULL]
  #
  # STEP FIVE: Fill small holes in the Snow_Depth_Inches column.
    # First check to see if there are any Snow_Depth measurements,
    # if not, skip this entire step.
    if (DT[!is.na(Snow_Depth_Inches), .N] > 0L) {
      # Add a column for # of consecutive days of NA,
      # if there's a Snow_Depth measurement, assign 0L to days_of_NA
      DT[!is.na(Snow_Depth_Inches), days_of_NA := 0L]
      # if there isn't a Snow_Depth measurement, assign 1L to days_of_NA
      DT[is.na(Snow_Depth_Inches), days_of_NA := 1L]
      # identify the first line in the DT *WITH* a Snow_Depth measurement
      for (i in 1:DT[, .N]) {
        if (DT[i, is.na(Snow_Depth_Inches)]) {
          next()
        }
        # if the SWE measurement isn't NA, assign i + 1 as the starting row
        starting_row <- i + 1L
        break()
      }
      # determine # of consecutive days of NA   
      for (i in starting_row:DT[, .N]) {
        # if days_of_NA = 0, go to next line
        if (DT[i, days_of_NA == 0L]) {
          next()
        } 
        # otherwise, days_of_NA = previous "days_of_NA" plus 1
        DT[i, days_of_NA := (DT[i - 1L, days_of_NA] + 1L)]
      }
      # identify the last line in DT with a SWE measurement
      for (i in DT[, .N]:starting_row) {
        if (DT[i, is.na(Snow_Depth_Inches)]) {
          next()
        }
        # if the SWE measurement isn't NA, assign i - 1 as the ending row
        ending_row <- i - 1L
        break()
      }
      # carry values of "days_of_NA" upward, so blocks of time with missing
      # SWE measurements all have the same "days_of_NA"
      for (i in ending_row:starting_row) {
        if (DT[i, days_of_NA == 0L]) {
          j <- 1L
          next()
        }
        # if j = 1, assign j = 0 and go to next,
        # this prevents 0's from being carried upward
        if (j == 1L) {
          j <- 0L
          next()
        }
        # otherwise, row i's days_of_NA = row i + 1's days_of_NA
        DT[i, days_of_NA := DT[i + 1L, days_of_NA]]
      }
      # if days_of_NA is equal to or less than threshold, interpolate
      # Snow_Depth  
      # initialize gap_length as zero
      gap_length <- 0L
      for (i in starting_row:ending_row) {
        # this if loop enables the code to skip over gaps bigger than 
        # "threshold" and over gaps that have already been filled in
        if (gap_length > 0L) {
          gap_length <- gap_length - 1L
          next()
        }
        # if there's a SWE measurement, go to next
        if (DT[i, days_of_NA == 0L]) {
          next()
        }
        # if the gap is greater than "threshold", reset gap_length and go
        # to next
        if (DT[i, days_of_NA > threshold]) {
          gap_length <- DT[i, days_of_NA]
          next()
        }
        # interpolate snow depth over all other gaps (less than or
        # equal to "threshold")
        gap_length <- DT[i, days_of_NA]
        starting_SnowDepth <- DT[i - 1L, Snow_Depth_Inches]
        ending_SnowDepth <- DT[i + gap_length, Snow_Depth_Inches]
        for (j in 1:gap_length) {
          DT[(i + j - 1L), Snow_Depth_Inches :=
               round(starting_SnowDepth +
               (ending_SnowDepth - starting_SnowDepth) *
               (j / (gap_length + 1L)), digits = 0L)]
        }
      }
      # delete the column "days_of_NA"
      DT[, days_of_NA := NULL]
    }
  #
  # STEP SIX: If SWE_Inches is NA between the months of "first_mo"
  #           and "last_mo" (inclusive), replace the NA with a zero.
    DT[Month >= first_mo & Month <= last_mo & is.na(SWE_Inches),
      SWE_Inches := 0L]
  #
  # STEP SEVEN: Wipe all snow depths between "first_mo" and "last_mo"
  #             (inclusive) to NA.
    DT[Month >= first_mo & Month <= last_mo, Snow_Depth_Inches := NA]
  #
  # STEP EIGHT: Replace "" (no character) in the SWE_Code and
  #             Snow_Depth_Code columns with "_". This will enable
  #             data.table to later more easily recognize these columns
  #             as character.
    DT[SWE_Code == "", SWE_Code := "_"] 
    DT[Snow_Depth_Code == "", Snow_Depth_Code := "_"]
  #  
  # STEP NINE: SPOT CLEANING...
  #            The daily SWE records were visually inspected and these
  #            problematic times were identified. In general, they're being
  #            wiped to NA.
  #
  # Spot cleaning the DAN, Dana Meadows, daily SWE record
  if (Station_ID == "DAN") {
    # The DAN daily SWE record is missing the entire 1988 WY (water year),
    # not a problem in general (although certainly not ideal), but the late
    # fall of 1987 and May of 1988 values shouldn't be converted to zero
    # just because they're missing.
    DT[Year == 1987L & Month >= 10L, SWE_Inches := as.numeric(NA)]
    DT[Year == 1988L & Month >= 5L  & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # The Dana Meadows ("DAN") daily record of SWE is sparse in the late
    # spring of 1995 and changing these NAs to zero is misleading.  The
    # last measurement was for >40" of SWE. April through August of 1995
    # in the DAN daily record will be set to NA.
    DT[Year == 1995L & Month >= 4L & Month <= 8L,
       SWE_Inches := as.numeric(NA)]
    # The DAN snow depth record has values that, in comparison to
    # the snow depths for previous and subsequent days, and in
    # comparison to changes (or lack thereof) in the SWE record,
    # are erroneous. Wipe these values to NA.
    # On 1/20/2012, 168" was reported.
    DT[Year == 2012L & Month == 1L & MDay == 20L,
       Snow_Depth_Inches := NA]
    # On 12/1/2013, 48" was reported.
    DT[Year == 2013L & Month == 12L & MDay == 1L,
       Snow_Depth_Inches := NA]
    # On 12/14/2013, 42" was reported.
    DT[Year == 2013L & Month == 12L & MDay == 14L,
       Snow_Depth_Inches := NA]
    # On 12/2/2014, 47" was reported.
    DT[Year == 2014L & Month == 12L & MDay == 2L,
       Snow_Depth_Inches := NA]
    # On 12/4/2014, 174" was reported.
    DT[Year == 2014L & Month == 12L & MDay == 4L,
       Snow_Depth_Inches := NA]
    # On 12/12/2014, 102" was reported.
    DT[Year == 2014L & Month == 12L & MDay == 12L,
       Snow_Depth_Inches := NA]
    # On 1/31/2015, 127" was reported.
    DT[Year == 2015L & Month == 1L & MDay == 31L,
       Snow_Depth_Inches := NA]
    # On 2/2/2015, 169" was reported.
    DT[Year == 2015L & Month == 2L & MDay == 2L,
       Snow_Depth_Inches := NA]
    # On 3/2/2015, 153" was reported.
    DT[Year == 2015L & Month == 3L & MDay == 2L,
       Snow_Depth_Inches := NA]
    # On 3/31/2015, 42" was reported.
    DT[Year == 2015L & Month == 3L & MDay == 31L,
       Snow_Depth_Inches := NA]
    # On 4/1/2015, 41" was reported.
    DT[Year == 2015L & Month == 4L & MDay == 1L,
       Snow_Depth_Inches := NA]
    # On 4/12/2015, 42" was reported.
    DT[Year == 2015L & Month == 4L & MDay == 12L,
       Snow_Depth_Inches := NA]
    # On 12/3/2017, 181" was reported.
    DT[Year == 2017L & Month == 12L & MDay == 3L,
       Snow_Depth_Inches := NA]
  }
  # Spot cleaning the TUM, Tuolumne Meadows, daily SWE record
  if (Station_ID == "TUM") {
    # In the TUM daily SWE record, the springs of 2011, 2013, 2014, and
    # 2016 are sparsely recorded.  As a result, assuming NAs in their
    # Mays equal zero is unwarranted.
    # For 2011, May through October is reset to NA.
    DT[(Year == 2011L & Month >= 5L & Month <= 10L),
       SWE_Inches := as.numeric(NA)]
    # For 2013, May through November is reset to NA.
    DT[(Year == 2013L & Month >= 5L & Month <= 11L),
       SWE_Inches := as.numeric(NA)]
    # For 2014, May through September is reset to NA.
    DT[(Year == 2014L & Month >= 5L & Month <= 9L),
       SWE_Inches := as.numeric(NA)]
    # For 2016, May through October is reset to NA.
    DT[(Year == 2016L & Month >= 5L & Month <= 11L),
       SWE_Inches := as.numeric(NA)]
    # For the snow depth record, 118" is reported on 12/21/2017.
    # 5" and 7" were reported for the days before and after.
    DT[Year == 2017L & Month == 12L & MDay == 21L,
       Snow_Depth_Inches := NA]
  }
  # Spot cleaning the HRS, Horse Meadow, daily SWE record.
  if (Station_ID == "HRS") {
    # In the HRS daily SWE record a large portion of the 1986 WY is
    # missing.
    # Adjust May through November, 1986, to NA.
    DT[Year == 1986L & Month >= 5L & Month <= 11L,
       SWE_Inches := as.numeric(NA)]
    # In 1995 the SWE record ends on 5/23 with 78" w.e.
    # Reset all values from May to August 1995 back to NA.
    DT[Year == 1995L & Month >= 5L & Month <= 8L,
       SWE_Inches := as.numeric(NA)]
    # In 1997 the SWE record ends on 1/10 at 38" and isn't resumed until
    # 4/1.
    # Set all Jan1997 values to NA.
    DT[Year == 1997L & Month == 1L, SWE_Inches := as.numeric(NA)]
    # The entire 2005 WY is missing, set May and June to NA.
    DT[Year == 2005L & Month >= 5L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
    # In 2011, the SWE record ends on 4/3 with 75.21" w.e.
    # Set April through Sep to NA.
    DT[Year == 2011L & Month >= 4L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the WHW, White Wolf, daily SWE record.
  if (Station_ID == "WHW") {
    # the entire 2015 & 2016 WYs are missing, convert all to NA.
    DT[Year == 2014 & Month >= 11, SWE_Inches := as.numeric(NA)]
    DT[Year == 2015              , SWE_Inches := as.numeric(NA)]
    DT[Year == 2016 & Month <= 11, SWE_Inches := as.numeric(NA)]
    # the following days have strange snow depths. For example, the days
    # before and after the day in question might agree, but then the day
    # in question has a snow depth 100" greater with no change in SWE.
    # Wipe these snow depths to NA.
    DT[Year == 2012 & Month == 3 & MDay == 12,
       Snow_Depth_Inches := NA]
    DT[Year == 2013 & Month == 1 & MDay == 24,
       Snow_Depth_Inches := NA]
    DT[Year == 2013 & Month == 3 & MDay == 31,
       Snow_Depth_Inches := NA]
  }  
  # Spot cleaning the PDS, Paradise Meadow, daily SWE record.
  if (Station_ID == "PDS") {
    # the last recorded value in the 1982 WY is 12/20/81,
    # reconvert May through Sep 1982 to NA.
    DT[Year == 1982L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # there is a ~2 yr period of missing data in 1983 and 1984, assuming
    # these summertime values are zero is unwarranted.
    DT[Year == 1983L & Month >= 4L, SWE_Inches := as.numeric(NA)]
    DT[Year == 1984L & Month <= 9L, SWE_Inches := as.numeric(NA)]
    # the last record of the 1986 WY is for 10.6" on 2/14,
    # reconvert the intervening months to NA.
    DT[Year == 1986L & Month >= 2L & Month <= 11L,
       SWE_Inches := as.numeric(NA)]
    # there are no SWE records between 1/87 and 10/87,
    # reconvert the May through October 1987 SWEs to NA.
    DT[Year == 1987L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # there are suspect SWCs between May and Sep 1992, the same value is
    # repeated many times, off and on.
    # overwrite all these months to NA.
    DT[Year == 1992L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # because of missing data in the spring of '93, Jan through July should
    # be rewritten to NA.
    DT[Year == 1993L & Month <= 7L, SWE_Inches := as.numeric(NA)]
    # July through August 1995 has stray positive values,
    # these should be rewritten as NA.
    DT[Year == 1995L & Month >= 7L & Month <= 8L,
       SWE_Inches := as.numeric(NA)]
    # the record for the 1996 WY ends on 5/3 with 35",
    # rewrite May through Sep to NA.
    DT[Year == 1996L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # The daily record for Jan through June 1997 consists of many missing
    # values with a few zeros here and there. Meanwhile, the monthly
    # record reports 60.4" for Feb, 55.1" for April, 35.3" for May. In
    # contrast, the daily reports 12 zeros and 18 NAs for April and 15
    # zeros and 16 NAs for May. The daily record for Feb1997 is entirely
    # NAs.  I think the monthly are more likely right (i.e., there was
    # substantial snow) and the daily values are erroneous for some reason.
    # I'm wiping the daily record for all these months to NAs.
    DT[Year == 1997L & Month <= 9L, SWE_Inches := as.numeric(NA)]
    # there's no SWE record for the spring of 1998,
    # rewrite May through Sep to NA.
    DT[Year == 1998L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # there's also no record of daily SWE for the spring of 2001,
    # rewrite May through Sep to NA.
    DT[Year == 2001L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # there's no record of SWE between 11/2008 and 12/2011,
    # reconvert these summers to NA.
    DT[Year >= 2009L & Year <= 2011L & Month >= 5L &
         Month <= 9L, SWE_Inches := as.numeric(NA)]
    # convert all of Feb2016 to NA, too few records for a meaningful
    # average
    DT[Year == 2016L & Month == 2L, SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the SLI, Slide Canyon, daily SWE record
  if (Station_ID == "SLI") {
    # the last SWE value in the spring of 1986 was 42.7" on 2/17,
    # reconvert May through Sep '86 to NA
    DT[Year == 1986L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the summer of 1994 has many repeated values,
    # reset June through Sep to NA
    DT[Year == 1994L & Month >= 6L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # much of the 2012 & 2013 WYs are missing,
    # reset all values from May2012 to August2013 to NA
    DT[Year == 2012L & Month >= 5L, SWE_Inches := as.numeric(NA)]
    DT[Year == 2013L & Month <= 8L, SWE_Inches := as.numeric(NA)]
    # the last measurement in the 2014 WY was on 11/30/13,
    # reset May through Sep 2014 to NA
    DT[Year == 2014L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the last value in the 2015 WY was 11" SWE on 3/24,
    # reset May through Oct to NA
    DT[Year == 2015L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # the last value in the 2016 water year was recorded on 1/31,
    # reset May through October to NA
    DT[Year == 2016L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the SNF, Snow Flat, daily SWE record
  if (Station_ID == "SNF") {
    # The 1989 water year record ends abruptly in April,
    # reset 4/89 through 10/89 to NA
    DT[Year == 1989L & Month >= 4L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # In the summer of 1990 the SWE record never clearly hits 0 and
    # flatlines at 1.96". Reset July through Oct 1991 to NA.
    DT[Year == 1991L & Month >= 7L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # A clear zero is also never reached in the summers of 1992 & 1993.
    # I suspect the snow pillow is slightly miscalibrated. Reset to NA.
    DT[Year == 1992L & Month >= 6L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    DT[Year == 1993L & Month >= 7L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the last record in the 1996 water year is on 1/15/96,
    # overwrite/reset January through Sep to NA
    DT[Year == 1996L & Month <= 9L, SWE_Inches := as.numeric(NA)]
    # the spring of 1997 record is spotty, except for March
    # set Jan, Feb, Apr, May, and Jun to NA.
    DT[Year == 1997L & Month <= 2L, SWE_Inches := as.numeric(NA)]
    DT[Year == 1997L & Month >= 4L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
    # the SWE records from April to December 1998 look suspect, 
    # overwrite / reset to NA
    DT[Year == 1998L & Month >= 4L, SWE_Inches := as.numeric(NA)]
    # there are no SWE measurements in 1999, so the summer months can't
    # be assumed to be zero.
    DT[Year == 1999L, SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the STR, Ostrander Lake, daily SWE record
  if (Station_ID == "STR") {
    # August 1990 has repeated values of 1.96" interspersed with NA,
    # set all of August 1990 to NA
    DT[Year == 1990L & Month == 8L, SWE_Inches := as.numeric(NA)]
    # July 1991 has repeated values of 1.31" interspersed with NA,
    # set all of July 1991 to NA
    DT[Year == 1991L & Month == 7L, SWE_Inches := as.numeric(NA)]
    # the last value in water year 2009 was recorded on 11/5/08,
    # reset May through Sep 2009 to NA
    DT[Year == 2009L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # no values were recorded for water year 2013,
    # reset May through Sep 2013 to NA
    DT[Year == 2013L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the last value in water year 2017 was recorded on 1/23/17,
    # set all records in Feb through June 2017 to NA
    DT[Year == 2017L & Month >= 2L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the TNY, Tenaya Lake, daily SWE record
  if (Station_ID == "TNY") {
    # the last measurement in the 2011 WY was 16" on 2/10/11,
    # reset May through Sep 2011 to NA
    DT[Year == 2011L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the last measurement in the 2014 WY was 6" on 2/24,
    # reset May through Sep 2014 to NA
    DT[Year == 2014L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the last measurement in the 2015 WY was on 12/3/14 and no
    # measurements were reported for TNY during the 2016 water year.
    # set/reset all of 2015 and up through Sep 2016 to NA.
    DT[Year == 2015L, SWE_Inches := as.numeric(NA)]
    DT[Year == 2016L & Month <= 9L, SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the VLC, Volcanic Knob, daily SWE record
  if (Station_ID == "VLC") {
    # Sept92 has large and erratic changes in supposed SWE,
    # set this month to NA
    DT[Year == 1992L & Month == 9L, SWE_Inches := as.numeric(NA)]
    # the 2009 water year record ends abruptly on 2/25,
    # reset May through Sep 2009 to NA
    DT[Year == 2009L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # the 2011 water year record ends abruptly on 11/30/10,
    # reset May through October to NA
    DT[Year == 2011L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # the 2016 water year record ends abruptly on 2/17,
    # reset May through October 2016 to NA
    DT[Year == 2016L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the AGP, Agnew Pass, daily SWE record
  if (Station_ID == "AGP") {
    # last measurement of 1996 water year was on 4/25/96,
    # reset May through Sep 96 to NA
    DT[Year == 1996L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # there are only a few measurements in the spring of 1997,
    # reset May through July 1997 to NA.
    DT[Year == 1997L & Month >= 5L & Month <= 7L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of 1998 water year was on 1/18/98,
    # reset May through Sep to NA.
    DT[Year == 1998L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of 2005 water year was 44" on 4/28/05,
    # reset May through July to NA.
    DT[Year == 2005L & Month >= 5L & Month <= 7L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of the 2008 water year was 18" on 4/30/08,
    # reset May through September to NA.
    DT[Year == 2008L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of 2009 water year was 22" on 4/24/09,
    # reset May through Sept to NA.
    DT[Year == 2009L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of 2010 water year was 33" on 5/7/10,
    # reset May through October to NA.
    DT[Year == 2010L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of 2011 water year was 40" on 4/27/11,
    # reset May through Sept, and Nov, to NA
    DT[Year == 2011L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    DT[Year == 2011L & Month == 11L, SWE_Inches := as.numeric(NA)]
    # nearly all of the 2012 water year is missing,
    # reset May through Sept to NA
    DT[Year == 2012L & Month >= 5L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # last measurement of 2017 water year was 60" on 4/21/17,
    # reset May & June 2017 to NA
    DT[Year == 2017L & Month >= 5L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the KSP, Kaiser Point, daily SWE record
  if (Station_ID == "KSP") {
    # The 1971 WY is missing, wipe 5/71 - 11/71 to NA.
    DT[Year == 1971L & Month >= 5L & Month <= 11L,
       SWE_Inches := NA]
    # last measurement of 1972 is 10.4" on 4/5/72,
    #  wipe 4/72 to 10/72 to NA
    DT[Year == 1972L & Month >= 4L & Month <= 10L,
       SWE_Inches := NA]
    # last measurement of 1974 is 21.1" on 1/11/74,
    #  wipe 5/74 to 9/74 to NA.
    DT[Year == 1974L & Month >= 5L & Month <= 9L,
       SWE_Inches := NA]
    # snowfall comes late in 1976/77, convert NA to 0 for Dec1976
    DT[Year == 1976L & Month == 12L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # A long string of 1.19" values in July & August 1990 must be
    #  erroneous, wipe to NA
    DT[Year == 1990L & Month >= 7L & Month <= 8L & SWE_Inches == 1.19,
       SWE_Inches := NA]
    # last measurement of 1997 is 42" on 2/24/1997,
    #  wipe 5/97 to NA. 0" is recorded on 6/1/97.
    DT[Year == 1997L & Month == 5L, SWE_Inches := 0]
    # last measurement of 1998 is 27" on 2/4/1998,
    #  wipe 5/98 to 8/98 to NA
    DT[Year == 1998L & Month >= 5L & Month <= 8L,
       SWE_Inches := NA]
    # last measurement of 2009 is 21" on 3/30/2009,
    #  wipe 5/09 to 6/09 to NA
    DT[Year == 2009L & Month >= 5L & Month <= 6L,
       SWE_Inches := NA]
  }
  # Spot cleaning the GRM, Green Mountain, daily SWE record
  if (Station_ID == "GRM") {
    # First measurement of 1977 is 0" on 1/1/1977,
    #  set 12/76 to 0
    DT[Year == 1976L & Month == 12L, SWE_Inches := 0]
    # Last measurement of 1977 is 0" on 4/17/1977,
    #  set missing April values to 0
    DT[Year == 1977L & Month == 4L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Last measurement of 1989 is 0" on 4/17/1989,
    # set missing April values to 0
    DT[Year == 1989L & Month == 4L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Many erroneous values from 6/26/1994 to 8/31/1994,
    #  set this date range to NA
    DT[Year == 1994L & Month == 6L & MDay >= 26L,
       SWE_Inches := NA]
    DT[Year == 1994L & Month >= 7L & Month <= 8L,
       SWE_Inches := NA]
    # Last measurement of 2002 is 15.4" on 1/6/2002,
    #  set 5/02 to 6/02 to NA
    DT[Year == 2002L & Month >= 5L & Month <= 6L,
       SWE_Inches := NA]
  }  
  # Spot cleaning the DPO, Devil's Postpile, daily SWE record
  if (Station_ID == "DPO") {
    # The 2009 WY is missing and the 2010 WY starts abruptly on 10/13/09
    # with 11" of SWE, reset May through October 2010 to NA
    DT[Year == 2009L & Month >= 5L & Month <= 10L,
       SWE_Inches := as.numeric(NA)]
    # The 2011 WY is missing and there are lots of negative values in the
    # summer of 2011, reset May through November 2011 to NA.
    DT[Year == 2011L & Month >= 5L & Month <= 11L,
       SWE_Inches := as.numeric(NA)]
    # The SWE measurements end in March for the 2012 water year, but 2012
    # was dry and this seems to be correct, set missing values in March and
    # April to NA.
    DT[Year == 2012L & Month >= 3L & Month <= 4L & is.na(SWE_Inches),
       SWE_Inches := as.numeric(0)]
    # The 2013 water year ends on 1/11/13 at 14", assuming May SWE is zero
    #   seems reasonable, therefore no change in code.
    # The 2015 water year is largely missing,
    # reset May through November to NA.
    DT[Year == 2015L & Month >= 5L & Month <= 11L,
       SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the TMR, Tamarack Summit, daily SWE record
  if (Station_ID == "TMR") {
    # The TMR record is perhaps the longest and highest quality
    # daily SWE record in this study! There are only two "problem"
    # areas.
    # A string of 0.2" SWE values runs on from May to July 1992,
    #  overwrite these months to zero.
    DT[Year == 1992L & Month >= 5L & Month <= 7L,
       SWE_Inches := as.numeric(0)]
    # The SWE record abruptly ends on 5/8/1993 with 35".
    #  Wipe May and June 1993 to NA.
    DT[Year == 1993L & Month >= 5L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
  } 
  # Spot cleaning the CHM, Chilkoot Meadow, daily SWE record  
  if (Station_ID == "CHM") {
    # First measurement in 1989 wy is 15" on 1/24/89,
    #  set 11/88 and 12/88 to NA
    DT[Year == 1988L & Month >= 11L & Month <= 12L,
       SWE_Inches := NA]
    # Many values of 0.2" from 5/23/92 to 11/30/92,
    #  set 5/92 to 11/92 to NA.
    DT[Year == 1992L & Month >= 5L & Month <= 11L,
       SWE_Inches := NA]
    # First measurement of 1998 wy is 8" on 1/16/98,
    #  set 12/97 to NA
    DT[Year == 1997L & Month == 12L, SWE_Inches := NA]
    # Last measurement of 1998 is 48" on 3/27/1998,
    #  set 4/98 to 7/98 to NA
    DT[Year == 1998L & Month >= 4L & Month <= 7L,
       SWE_Inches := NA]
    # Last measurement of 2011 is 35" on 3/27/2011,
    #  set 4/11 to 7/11 to NA
    DT[Year == 2011 & Month >= 4L & Month <= 7L,
       SWE_Inches := NA]
    # Last measurement of 2014 is 7" on 4/13/2014,
    #  set 4/14 to 7/14 to NA
    DT[Year == 2014 & Month >= 4L & Month <= 7L,
       SWE_Inches := NA]
    # Last measurement of 2015 is 0" on 3/17/2015,
    #  set missing values in March 2015 to 0.
    DT[Year == 2015L & Month == 3L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Anomalous snow depth value of 164", between values of
    #  34" and 44", on 1/24/2009. Reset value to 39"
    DT[Year == 2009L & Month == 1L & MDay == 24L,
       Snow_Depth_Inches := 39L]
  }
  # Spot cleaning the HNT, Huntington Lake, daily SWE record
  if (Station_ID == "HNT") {
    # The daily SWE record from HNT, Huntington Lake, is going to
    # be combined with the monthly SWE record from HTT, Huntington
    # Lake.  These two records, although distinguished as separate
    # locations by the CA Dept of Water Resources, are only 100 m
    # apart (E-W) and have the same elevation, latitude, and general
    # name.  Thus, for the purposes of this work, they are at the 
    # same location.
    # In Apr1988, the snow melts early. Missing values -> zero.
    DT[Year == 1988L & Month == 4L, SWE_Inches := as.numeric(0)]
    # In Nov1988, we don't know that missing values are zero.
    DT[Year == 1988L & Month == 11L, SWE_Inches := as.numeric(NA)]
    # In Apr1989, the snow melts early.
    DT[Year == 1989L & Month == 4L, SWE_Inches := as.numeric(0)]
    # In May and June 1992, there are many SWE's of 0.39", clear
    # errors. Wipe these months to NA.
    DT[Year == 1992L & Month >= 5L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
    # Christmas Eve, 1992, anomalous value
    DT[Year == 1992L & Month == 12L & MDay == 24L,
       SWE_Inches := as.numeric(NA)]
    # July through Sept, 1993, many SWEs of 1". Wipe to NA.
    DT[Year == 1993L & Month >= 7L & Month <= 9L,
       SWE_Inches := as.numeric(NA)]
    # SWE record ends abruptly on 5/4/1995 with 35" SWE.
    # Wipe May and June to NA.
    DT[Year == 1995L & Month >= 5L & Month <= 6L,
       SWE_Inches := as.numeric(NA)]
    # SWE record ends abruptly on 4/22/1996 with 29" SWE.
    # Wipe May 1996 to NA.
    DT[Year == 1996L & Month == 5L, SWE_Inches := as.numeric(NA)]
    # SWE record STARTS abruptly on 3/27/1997. Wipe March to NA.
    DT[Year == 1997L & Month == 3L, SWE_Inches := as.numeric(NA)]
  }
  # Spot cleaning the GRV, Graveyard Meadow, daily SWE record
  if (Station_ID == "GRV") {
    # Last measurement of 1984 is 0" on 3/13/1984,
    #  set missing March and April values to 0
    DT[Year == 1984L & Month >= 3L & Month <= 4L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Last measurement of 1985 is 0" on 4/20/1985,
    #  set missing April values to 0
    DT[Year == 1985L & Month == 4L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # First measurement of 1987 wy is 0" on 1/3/1987,
    #  set 12/1986 to 0
    DT[Year == 1986L & Month == 12L, SWE_Inches := 0]
    # Last measurement of 1987 is 0" on 4/11/1987,
    #  set missing April values to 0
    DT[Year == 1987L & Month == 4L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Last measurement of 1988 is 0" on 3/15/1988,
    #  set missing March values to 0.
    DT[Year == 1988L & Month == 3L & is.na(SWE_Inches),
       SWE_Inches := 0]
    #  set April values to 0
    DT[Year == 1988L & Month == 4L, SWE_Inches := 0]
    # Last measurement of 1989 is 0" on 4/6/1989,
    #  set missing April values to 0
    DT[Year == 1989L & Month == 4L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Long string of erroneous numbers between 5/23/1992 and October,
    #  set this date range to NA
    DT[Year == 1992L & Month == 5L & MDay >= 23L, SWE_Inches := NA]
    DT[Year == 1992L & Month >= 6L & Month <= 10L,
       SWE_Inches := NA]
    # Last measurement of 1996 is 18" on 4/1/1996,
    #  set 4/96 to 6/96 to NA
    DT[Year == 1996L & Month >= 4L & Month <= 6L,
       SWE_Inches := NA]
    # Last measurement of 2006 is 25" on 5/1/2006,
    #  set 5/06 to 6/06 to NA
    DT[Year == 2006 & Month >= 5L & Month <= 6L,
       SWE_Inches := NA]
  }  
  # Spot cleaning the PSR, Poison Ridge, daily SWE record
  if (Station_ID == "PSR") {
    # The 1970 record starts abruptly with 7.4" on 12/1/70 and Nov
    #  can't assumed to be 0. Wipe Nov to NA.
    DT[Year == 1970L & Month == 11L, SWE_Inches := NA]
    # Last measurement of 1971 is 32" on 1/14/71,
    #  wipe 4/71 to 7/71 to NA.
    DT[Year == 1971L & Month >= 4L & Month <= 7L,
       SWE_Inches := NA]
    # Last measurement of 1974 WY is 21" on 3/23/74,
    #  wipe 4/74 to 9/74 to NA.
    DT[Year == 1974L & Month >= 4L & Month <= 9L,
       SWE_Inches := NA]
    # Snow comes late in 1976, set missing Dec values to 0.
    DT[Year == 1976L & Month == 12L & is.na(SWE_Inches),
       SWE_Inches := 0]
    # Strange values (34" and 91") on 7/11 and 7/12/1990,
    #  wipe these days to 0.
    DT[Year == 1990L & Month == 7L & MDay >= 11L & MDay <= 12L,
       SWE_Inches := 0]
    # Many readings of 2.76" between 7/92 and 11/92,
    #  wipe these months to NA.
    DT[Year == 1992L & Month >= 7L & Month <= 11L,
       SWE_Inches := NA]
    # Last measurement of 1995 is 15" on 6/4/1995,
    #  wipe 6/95 to 7/95 to NA.
    DT[Year == 1995L & Month >= 6L & Month <= 7L,
       SWE_Inches := NA]
    # Last measurement of 2004 is 13.5" on 1/30/2004,
    #  wipe 4/04 to NA.
    DT[Year == 2004L & Month == 4L, SWE_Inches := NA]
    # Last measurement of 2006 WY is 0.38" on 12/10/2005,
    #  set 4/06 to 6/06 to NA.
    DT[Year == 2006 & Month >= 4L & Month <= 6L,
       SWE_Inches := NA]
    # Last measurement of 2009 is 7" on 4/28/2009,
    #  set 5/09 to NA.
    DT[Year == 2009L & Month == 5L, SWE_Inches := NA]
    # Last measurement of 2012 is 2.4" on 2/6/2012,
    #  set 2/12 to 6/12 to NA
    DT[Year == 2012 & Month >= 2L & Month <= 6L,
       SWE_Inches := NA]
    # There's a strange snow depth measurement of 192" on 12/5/2017
    #  bracketted by measurements of 3" and 4". Set the 12/5/17 value
    #  to 4" (average of 3" and 4", rounded to integer)
    DT[Year == 2017L & Month == 12L & MDay == 5L,
       Snow_Depth_Inches := 4L]
  }  
  # 
  # STEP TEN: Output a new .txt file with the suffix "_clean" that the
  #            main Khione script will ingest.
  # Delete the "MDay" column.
    DT[, MDay := NULL]
  fwrite(DT, as.character(paste0(Station_ID, "_daily_clean.txt")),
         sep = "\t")
}

add_water_year <- function(DT) {
  # Purpose:
  #   Adds two columns to a DT, "Water_Year" and "Water_Month".
  #
  # Args:
  #   DT: A data.table with a column labeled "Year" and a column labeled
  #       "Month."
  #
  # Returns:
  #   An updated DT with two new columns, "Water_Year" and "Water_Month"
  #
  # Error checking:
  if (!is.data.table(DT)) {
    stop("DT needs to be a data.table", call. = FALSE)
  }
  if ("Year" %in% names(DT) == FALSE) {
    stop("The DT needs a column named 'Year'.", call. = FALSE)
  }
  if ("Month" %in% names(DT) == FALSE) {
    stop("The DT needs a column named 'Month'.", call. = FALSE)
  }
  # Main body:
  DT[Month >= 10L, ':=' (Water_Year  = Year + 1L,
                         Water_Month = Month - 9L)]
  DT[Month < 10L, ':=' (Water_Year = Year,
                        Water_Month = Month + 3L)]
}

# load daily SWE records---------------------------------------------------

# THE FOLLOWING SNOW WATER EQUIVALENT RECORDS ARE ORGANIZED FIRST BY
# DRAINAGE BASIN, FROM NORTH TO SOUTH, AND THEN BY DECREASING ELEVATION.

# DAN_daily = post-1980 daily record (revised) of SWE from Dana Meadows
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=DAN
#   The record runs from 11/1/80 to 12/31/17.
DAN_daily <- fread("DAN_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
DAN_daily[, Date := ymd(Date)]

  # Remove the 2007 to 2009 water years, plus Oct 2009 from the Dana
  # Meadows snow pillow record. A tree was growing between the snow
  # pillow's sensors. See "Diagnosis of insidious data disasters" by
  # Lundquist et al., 2015, WRR
  DAN_daily[, c("Year", "Month") := .(year(Date), month(Date))]
  add_water_year(DAN_daily)
  DAN_daily[Water_Year == 2007 | Water_Year == 2008 | Water_Year == 2009,
            names(DAN_daily) := NA]
  DAN_daily[Year == 2009 & Month == 10, names(DAN_daily) := NA]
  DAN_daily[, ':=' (Year = NULL, Month = NULL, 
                  Water_Year = NULL, Water_Month = NULL)]
  
# SLI_daily = post-1982 daily record (revised) of SWE from Slide Canyon
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SLI
# The record runs from 11/1/82 to 12/31/17.
# A string of zeros from 4/16/97 to 5/9/97 is sandwiched between SWEs of
#   40-50", this string of zeros was replaced with NAs.
SLI_daily <- fread("SLI_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
SLI_daily[, Date := ymd(Date)]

# TUM_daily = post-1979 daily record of SWE from Tuolumne Meadows
#   at: http://cdec.water.ca.gov/cgi-progs/stationInfo?station_id=TUM
# The record runs from 12/1/79 to 12/31/17.
TUM_daily <- fread("TUM_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
TUM_daily[, Date := ymd(Date)]

# HRS_daily = post-1984 daily record (revised) SWE from Horse Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=HRS
# The record runs from 11/1/84 to 12/31/17.
HRS_daily <- fread("HRS_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
HRS_daily[, Date := ymd(Date)]

# WHW_daily = post-2007 daily record (revised) SWE from White Wolf
#   at: http://cdec.water.ca.gov/cgi-progs/stationInfo?station_id=WHW
# The record runs from 11/1/2007 to 12/31/2017.
WHW_daily <- fread("WHW_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
WHW_daily[, Date := ymd(Date)]

# PDS_daily = post-1980 daily record (revised) of SWE in Paradise Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PDS
# The record runs from 11/10/80 to 12/31/17.
PDS_daily <- fread("PDS_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
PDS_daily[, Date := ymd(Date)]

# SNF_daily = 1988-1999 daily record (revised) of SWE at Snow Flat
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SNF
# The record runs from 12/1/88 to 10/27/98.
# March 18, 19, and 20, 1997 were manually converted from zero to NA.
# The SNF record DOES NOT include a daily snow depth! Blank columns are
# given snow_depth names, however, for ease and consistency in the script.
SNF_daily <- fread("SNF_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
SNF_daily[, Date := ymd(Date)]

# STR_daily = post-1988 daily record (revised) of SWE at Ostrander Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=STR
# The record runs from 12/1/88 to 12/31/17).
STR_daily <- fread("STR_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
STR_daily[, Date := ymd(Date)]

# TNY_daily = post-1998 daily record (revised) of SWE at Tenaya Lake
#   at: http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=TNY
# The record runs from 11/1/98 to 12/31/17.
TNY_daily <- fread("TNY_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
TNY_daily[, Date := ymd(Date)]

# VLC_daily = post-1989 daily record (revised) of SWE at Volcanic Knob
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=VLC
# The record runs from 12/1/89 to 12/31/17
VLC_daily <- fread("VLC_daily.txt", colClasses = c("Date", "numeric",
                                    "character", "integer", "character"))
VLC_daily[, Date := ymd(Date)]

# AGP_daily = post-1989 daily record (revised) of SWE at Agnew Pass
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=AGP
# The record runs from 12/1/1989 to 12/31/17.
AGP_daily <- fread("AGP_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
AGP_daily[, Date := ymd(Date)]

# KSP_daily = post-1970 (!) daily record of SWE at Kaiser Point
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=KSP
# The record runs from 1/6/1970 to 12/31/2017
KSP_daily <- fread("KSP_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
KSP_daily[, Date := ymd(Date)]

# GRM_daily = post-1972 daily record (revised) of SWE at Green Mtn
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GRM
# The record runs from 10/1/1972 to 12/31/2017
GRM_daily <- fread("GRM_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
GRM_daily[, Date := ymd(Date)]

# DPO_daily = post-2007 daily record (revised) of SWE at Devil's Postpile
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=DPO
# The record runs from 10/1/2007 to 12/5/2017.
DPO_daily <- fread("DPO_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
DPO_daily[, Date := ymd(Date)]

# TMR_daily = post-1980 daily record (revised) of SWE at Tamarack Summit
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=TMR
# The record runs from 12/3/1980 to 12/31/2017.
TMR_daily <- fread("TMR_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
TMR_daily[, Date := ymd(Date)]

# CHM_daily = post-1985 daily record of SWE (revised) at Chilkoot Meadow
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CHM
# The record runs from 11/1/1985 to 12/31/2017
CHM_daily <- fread("CHM_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
CHM_daily[, Date := ymd(Date)]

# HNT_daily = post-1987 daily record (revised) of SWE at Huntington Lake
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=HNT
# The record runs from 11/4/1987 to 12/31/2017
HNT_daily <- fread("HNT_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
HNT_daily[, Date := ymd(Date)]

# GRV_daily = post-1982 daily record (revised) of SWE at
#  Graveyard Meadow
#  at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=GRV
# The record runs from 5/1/1982 to 12/31/2017
GRV_daily <- fread("GRV_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
GRV_daily[, Date := ymd(Date)]

# PSR_daily = post-1970 daily record (SWE) revised at Poison Ridge
#   at: http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=PSR
# The record runs from 1/1/1970 to 12/31/2017
PSR_daily <- fread("PSR_daily.txt", colClasses = c("Date", "numeric",
                                      "character", "integer", "character"))
PSR_daily[, Date := ymd(Date)]


# CLEAN DAILY RECORDS--------------------------------------------------

clean_record(DAN_daily, "DAN")
clean_record(SLI_daily, "SLI")
clean_record(TUM_daily, "TUM")
clean_record(HRS_daily, "HRS")
clean_record(WHW_daily, "WHW")
clean_record(PDS_daily, "PDS")
clean_record(SNF_daily, "SNF")
clean_record(STR_daily, "STR")
clean_record(TNY_daily, "TNY")
clean_record(VLC_daily, "VLC")
clean_record(AGP_daily, "AGP")
clean_record(KSP_daily, "KSP")
clean_record(GRM_daily, "GRM")
clean_record(DPO_daily, "DPO")
clean_record(TMR_daily, "TMR")
clean_record(CHM_daily, "CHM", first_mo = 4L, last_mo = 12L)
clean_record(HNT_daily, "HNT")
clean_record(GRV_daily, "GRV")
clean_record(PSR_daily, "PSR", first_mo = 4L)


"End of Script"