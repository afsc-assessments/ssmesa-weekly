#===============================================================================
#  2020 partial assessment projections for GOA rougheye/blackspotted rockfish
#===============================================================================

# Demo notes: skip database connection and queries and read data from file

# Development notes: code originally developed by P Hulson, modified by J
# Sullivan in 2023

libs <- c("readr", "dplyr", "tidyr", "lubridate", "keyring", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# The code provided in this script runs the AFSC projection model 
# in order to increase efficiency and reduce transcription errors from
# spreadsheets to text files. This code has been customized to the GOA
# Rougheye/Blackspotted complex (REBS). This code also includes toggles to
# accomodate running the script in an 'on' and 'off' year (REMINDER: if you
# are performing a full assessment in an 'off' year, you will need to change
# parts of this code). To run this code each year a couple of hard-wired
# inputs need to be updated.

#===============================================================================
#  User inputs
#===============================================================================

query_data <- FALSE # change to FALSE once you're satisfied with your queried data.
stock <- 'goa_rebs'
path <- file.path('2020_partial') # changed for demo
species <- 307
group_code <- 'REYE'
endyr <- 2020 # as.numeric(substr(Sys.time(),1,4)) # changed for demo

dir.create(paste0(path, '/catch_data_used'))
dir.create(paste0(path, '/', stock, '_max_out'))
dir.create(paste0(path, '/', stock, '_out'))
dir.create(paste0(path, '/proj_model'))

# Define the most recent 3 full years TAC (i.e., if the current year is X, these
# would be TACs for years X-3, X-2, and X-1):

# 2017, 2018, 2019
TAC <- c(1327, 1444, 1428)

#	Define your AKFIN oracle username and password here (skip for
#	demo):
username = "jsullivan"
password = keyring::key_get("akfin", keyring::key_list("akfin")$username)

#===============================================================================
#  Instructions
#===============================================================================

# The steps that this script works through are as follows:
#	Step 1: This step reads in the current catch data from AKFIN and estimates
#		the catch through the end of the current year and the yield ratio of
#		catch to TAC for the last full 3 years. So that we can look back on
#		the data to investigate any issues the observer and landings raw catch
#		data is written to the folder 'catch data used'
#	Step 2: This step gets the initial data files set up for the projection
#		model. These files include: (1) the base species-specific data file
#		read in from the proj.dat file (REMINDER: for this file, when the 
#		finalized assessment is reached you need to copy-paste the model into
#		the species-specific folder in the 'FINAL assmnts' folder), and (2) the
#		setup.dat file, which only updates the Begin Year for the projection.
#	Step 3: This step runs the projection model for the max F scenario (from 
#		which you get the endyr+1 and +2 catches for the author's F scenario).
#		In this step what is done is (1) the spp_catch.dat/goa_rebs_max_spcat.dat
#		file is compiled (NOTE: the date to which catch is available in the
#		current year is written in this file next to the current year's catch),
#		(2) the projection model is run, (3) the results are read in and written
#		to the 'goa_rebs_max_out' folder and (4) the bigsum.dat file is created
#		(note that the data is now sorted and no sorting in excel is needed)
#	Step 4: This step runs the projection model for the Author's F scenario.
#		In this step what is done is (1) the endyr+1 and +2 catches are
#		estimated from the yield ratio and Max F catch scenario catches and
#		the spp_catch.dat/goa_rebs_spcat.dat file is compiled (NOTE: the date
#		to which catch is available in the current year is written in this
#		file next to the current year's catch), (2) the projection model is run,
#		(3) the results are read in and written to the 'goa_rebs_out' folder
#		and (4) the bigsum.dat file is created
#	Step 5: Now we return to the original way the executive summary and
#		projection alternatives table has been created and copy-paste the 
#		percetiles/bigsum files into the projections table spreadsheet.

#===============================================================================
# Step 1: Get catch data and estimate catch to end of year and yield ratio
#===============================================================================

if(query_data == TRUE) {
  # Set up connection, read in observer/catch landings data
  channel = odbcConnect("akfin", uid = username, pwd = password, believeNRows = FALSE)
  catch_data <- sqlQuery(channel, paste("SELECT   COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR, COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_NAME, 
                                                  COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR, 
                                                  COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA, 
                                                  COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED 
                                       FROM       COUNCIL.COMPREHENSIVE_BLEND_CA 
                                       WHERE      COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA = 'GOA' AND 
                                                  COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ", endyr," AND 
                                                  COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = '", group_code,"'", sep="")) %>% 
    rename_with(tolower)
  
  obs_data <- sqlQuery(channel, paste("SELECT    NORPAC.DEBRIEFED_SPCOMP_MV.YEAR, NORPAC.DEBRIEFED_SPCOMP_MV.HAUL_DATE, NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES, 
                                                 NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA, NORPAC.DEBRIEFED_SPCOMP_MV.EXTRAPOLATED_WEIGHT 
                                    FROM         NORPAC.DEBRIEFED_SPCOMP_MV 
                                    INNER JOIN   NORPAC.DEBRIEFED_HAUL_MV 
                                    ON           NORPAC.DEBRIEFED_SPCOMP_MV.JOIN_KEY = NORPAC.DEBRIEFED_HAUL_MV.JOIN_KEY 
                                    WHERE        NORPAC.DEBRIEFED_SPCOMP_MV.YEAR BETWEEN ", endyr-3," AND ", endyr-1," AND 
                                                 NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA = 'GOA' AND 
                                                 NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES = ", species, sep="")) %>% 
    rename_with(tolower)
  
  
  write_csv(catch_data, paste0(path, "/catch_data_used/catch_data_", group_code, ".csv"))
  write_csv(obs_data, paste0(path, "/catch_data_used/obs_catch_data_", group_code, ".csv"))
}

# Start demo:
catch_data <- read_csv(paste0(path, "/catch_data_used/catch_data_", group_code, ".csv"))
obs_data <- read_csv(paste0(path, "/catch_data_used/obs_catch_data_", group_code, ".csv"))

# Estimate ratio of catch from current date to end of year (Endyr_ratio). This
# value will be use to extrapolate end of year catch in the current year
catch_end_date <- catch_data %>% 
  dplyr::filter(year == endyr) %>% 
  dplyr::summarise(c = max(as.Date(week_end_date))) %>% 
  dplyr::pull()

catch_end_date <- as.Date('2020-10-10') # hard coded for the demo

# flag -- why are we using observer data for this and not catch?
(endyr_ratio <- obs_data %>%
  dplyr::mutate(jday = lubridate::yday(haul_date)) %>%
  dplyr::filter(between(year, endyr-3, endyr-1)) %>% 
  dplyr::summarise(oct_catch = sum(extrapolated_weight[jday <= lubridate::yday(catch_end_date)], na.rm = TRUE),
                   total_catch = sum(extrapolated_weight, na.rm = TRUE)) %>% 
  dplyr::summarise(extrap = 1 + (sum(total_catch) - sum(oct_catch)) / sum(oct_catch)) %>% 
  dplyr::pull())
endyr_ratio

# using catch instead of observer data:
catch_data %>%
  dplyr::mutate(jday = lubridate::yday(week_end_date)) %>%
  dplyr::filter(between(year, endyr-3, endyr-1)) %>%
  dplyr::summarise(oct_catch = sum(weight_posted[jday <= lubridate::yday(catch_end_date)], na.rm = TRUE),
                   total_catch = sum(weight_posted, na.rm = TRUE)) %>%
  dplyr::summarise(extrap = 1 + (sum(total_catch) - sum(oct_catch)) / sum(oct_catch)) %>%
  dplyr::pull()

# compute total catch (including extrapolating the current year's catch) and
# estimate yield ratio of TAC to catch
catchsum <- catch_data %>% 
  dplyr::filter(between(year, endyr-3, endyr)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(catch = sum(weight_posted)) %>% 
  dplyr::mutate(catch = ifelse(year == endyr, catch * endyr_ratio, catch))

(yield_ratio <- mean(catchsum$catch[1:3] / TAC))

#====================================================================================================
# Step 2: Get initial projection model data files set up
#====================================================================================================

# Read in proj.dat and wite to projection model data file
proj <- readLines(paste(path, "/final_assessment_model/proj.dat", sep = ""), warn = FALSE)
dat_name <- paste0(stock, ".dat")
write.table(proj, file = paste(path, "/proj_model/data/", dat_name, sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)

# Get setup.dat file setup
setup <- readLines(paste(path, "/proj_model/setup.dat", sep = ""), warn = FALSE)
L_endyr <- grep("#_Begin Year", setup)

# test if endyr is for a full or partial assessment
# For odd-year full assessment (change to if(endyr %% 2 == 1) for even-year full assessment)
if(endyr %% 2 == 0) { 
  # partial:
  setup[L_endyr] <- paste(endyr-1," #_Begin Year")
} else {
  # full:
  setup[L_endyr] <- paste(endyr," #_Begin Year")
}
write.table(setup, file = paste(path, "/proj_model/setup.dat", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)

#===============================================================================
# Step 3: Run Max F projection scenario
#===============================================================================

# Setup spp_catch.dat file (toggled to test for on/off year)
L_1 <- "#_Number_of_years with specified catch"
if(endyr %% 2 == 0) { # For odd-year full assessment, change to if(endyr %% 2 == 1) for even-year full assessment
  # partial: in a partial year under the max F scenario, we specify two years
  # of catch (updated total catch from previous year, best estimate of current
  # year's total catch)
  L_2 <- 2 
} else {
  # full:  in a full year under the max F scenario, we specify only one year of
  # catch (best estimate of current year's total catch)
  L_2 <- 1 
}
L_3 <- "# Number of species"
L_4 <- 1
L_5 <- "# data files for each species"
L_6 <- paste0("data/", stock, ".dat")
L_7 <- "# ABC Multipliers"
L_8 <- 1
L_9 <- "# Population scalars"
L_10 <- 1000
L_11 <- "# Number of TAC model categories"
L_12 <- 1
L_13 <- "# TAC model indices (for aggregating)"
L_14 <- 1
L_15 <- "# Catch in each future year" # Includes toggle for on/off year
if(endyr %% 2 == 0) { # For odd-year full assessment, change to if(endyr %% 2 == 1){ for even-year full assessment
  # partial:
  L_16 <- paste(paste(endyr-1, round(catchsum$catch[catchsum$year == endyr-1], digits = 4), sep = "\t"), " # Finalized previous year catch", sep = " ")
  L_17 <- paste(paste(endyr, round(catchsum$catch[catchsum$year == endyr], digits = 4), sep = "\t"), "# Estimated from catch thru", catch_end_date, "with expansion factor =", endyr_ratio, sep = " ")
  spp_catch <- c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17)
} else {
  # full:
  L_16 <- paste(paste(endyr, round(catchsum$catch[catchsum$year == endyr], digits = 4), sep = "\t"), "# Estimated from catch thru", catch_end_date, "with expansion factor =", endyr_ratio, sep = " ")
  spp_catch <- c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16)
}
write.table(spp_catch, file = paste0(path, "/proj_model/spp_catch.dat"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(spp_catch, file = paste0(path, "/proj_model/data/", stock, "_max_spcat.dat"), quote = FALSE, row.names = FALSE, col.names = FALSE)

# *FLAG* - Experimental: Change species-specific file Author F Multiplier to run tests of Alt 2
spp_dat <- readLines(paste0(path, "/proj_model/data/", stock, ".dat"), warn = FALSE)
spp_dat[grep('#_Author_F_as_fraction_F_40%', spp_dat)+1] <- paste0(round(yield_ratio, 4))
writeLines(spp_dat, paste0(path, "/proj_model/data/", stock, ".dat"))

# Run model
#	NOTE: there's a warning message that comes up with this, you can ignore it,
#		it has to do with the ADMB version the projection model was originally
#		compiled with but has not effect on results
setwd(paste(path, "/proj_model", sep = ""))
shell("main.exe")
setwd('../..')

# Read results from Max scenario
bigfile_write <- readLines(paste0(path, "/proj_model/bigfile.out"), warn = FALSE)
bigfile <- read.delim(paste0(path, "/proj_model/bigfile.out"), sep = "", header = TRUE)
F_profile <- readLines(paste0(path, "/proj_model/F_profile.out"), warn = FALSE)
means <- readLines(paste0(path, "/proj_model/means.out"), warn = FALSE)
percentdb <- readLines(paste0(path, "/proj_model/percentdb.out"), warn = FALSE)
percentiles <- readLines(paste0(path, "/proj_model/percentiles.out"), warn = FALSE)

# Make bigsum file (average over the simulations)
bigsum <- bigfile %>% 
  dplyr::group_by(Alternative, Spp, Yr) %>% 
  dplyr::summarise_at(vars(-group_cols()), mean)

# Write results from max scenario
write.table(bigsum, file = paste0(path, "/", stock, "_max_out/bigsum.dat"), quote = FALSE, row.names = FALSE, col.names = c("Alt", "Stock", "Year", "ABC", "OFL", "Catch", "SSB", "F", "Total_Biom"))
write.table(bigfile_write, file = paste0(path, "/", stock, "_max_out/bigfile.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(F_profile, file = paste0(path, "/", stock, "_max_out/F_profile.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(means, file = paste0(path, "/", stock, "_max_out/means.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(percentdb, file = paste0(path, "/", stock, "_max_out/percentdb.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(percentiles, file = paste0(path, "/", stock, "_max_out/percentiles.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)

#===============================================================================
# Step 4: Run Author's F scenario
#===============================================================================

# Setup spp_catch.dat file - not that L_1, and L_3-L15 are the same as the Max F
# scenario
if(endyr %% 2 == 0) { # For odd-year full assessment, use if(endyr %% 2 == 1) for even-year full assessment
  # partial: in a partial year under the author's F scenario, we specify four
  # years of catch (updated total catch from previous year, best estimate of
  # current year's total catch, two years of future catches using the TAC/ABC
  # yield ratio)
  L_2 <- 4 
} else {
  # full: in a full year under the author's F scenario, we specify three years
  # of catch (best estimate of current year's total catch, two years of future
  # catches using the TAC/ABC yield ratio)
  L_2 <- 3 
}
# assigning catch: same as the max F scenario for current year (or past +
# current year for partials). for future catches, this methods uses Alternative
# 1 Mean_Catch from the max F scenario 'percentiles.out' file as the catch
# associated with ABC, then multiplies this value by the TAC/ABC yield ratio.
if(endyr %% 2 == 0) { # For odd-year full assessment, use if(endyr %% 2 == 1) for even-year full assessment
  # partial: e.g., if the year is 2022, we assign catches for 2021, 2022, 2023
  # and 2024
  L_16 <- paste(paste(endyr-1, round(catchsum$catch[catchsum$year == endyr-1], digits = 4), sep = "\t"), " # Finalized previous year catch", sep = " ")
  L_17 <- paste(paste(endyr, round(catchsum$catch[catchsum$year == endyr], digits = 4), sep = "\t"), "# Estimated from catch thru", catch_end_date, "with expansion factor =", endyr_ratio, sep = " ")
  # future catches (note the indexing differs for full and partial yrs)
  p1 <- percentiles[grep("Catch", percentiles)[1]:grep("Spawning_Biomass", percentiles)[1]]
  futcatch1 <- round(as.numeric(strsplit(p1[5], split = " ")[[1]][8]) * 1000 * yield_ratio, digits = 4)
  futcatch2 <- round(as.numeric(strsplit(p1[6], split = " ")[[1]][8]) * 1000 * yield_ratio, digits = 4)
  L_18 <- paste(paste(endyr+1, futcatch1, sep = "\t"), "# Estimated as Max F scenario catch*yieldratio =", yield_ratio, sep = " ")
  L_19 <- paste(paste(endyr+2, futcatch2, sep = "\t"), "# Estimated as Max F scenario catch*yieldratio", yield_ratio, sep = " ")
  spp_catch <- c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18,L_19)
} else {
  # full: e.g., if year is 2021, we assign catches for 2021, 2022, and 2023
  L_16 <- paste(paste(endyr, round(catchsum$catch[catchsum$year == endyr], digits = 4), sep = "\t"), "# Estimated from catch thru", catch_end_date, "with expansion factor =", endyr_ratio, sep = " ")
  # future catches (note the indexing differs for full and partial yrs)
  p1 <- percentiles[grep("Catch", percentiles)[1]:grep("Spawning_Biomass", percentiles)[1]]
  futcatch1 <- round(as.numeric(strsplit(p1[4], split = " ")[[1]][8]) * 1000 * yield_ratio, digits = 4)
  futcatch2 <- round(as.numeric(strsplit(p1[5], split = " ")[[1]][8]) * 1000 * yield_ratio, digits = 4)
  L_17 <- paste(paste(endyr+1, futcatch1, sep = "\t"), "# Estimated as Max F scenario catch*yieldratio", yield_ratio, sep = " ")
  L_18 <- paste(paste(endyr+2, futcatch2, sep = "\t"), "# Estimated as Max F scenario catch*yieldratio", yield_ratio, sep = " ")
  spp_catch <- c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)
}
write.table(spp_catch, file = paste0(path, "/proj_model/spp_catch.dat"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(spp_catch, file = paste0(path, "/proj_model/data/", stock, "_spcat.dat"), quote = FALSE, row.names = FALSE, col.names = FALSE)

# Run model
#	NOTE: there's a warning message that comes up with this, you can ignore it,
#		it has to do with the ADMB version the projection model was originally
#		compiled with but has not effect on results
setwd(paste(path, "/proj_model", sep = ""))
shell("main.exe")
setwd('../..')

# Read results from Author's F scenario
bigfile_write <- readLines(paste0(path, "/proj_model/bigfile.out"), warn = FALSE)
bigfile <- read.delim(paste0(path, "/proj_model/bigfile.out"), sep = "", header = TRUE)
F_profile <- readLines(paste0(path, "/proj_model/F_profile.out"), warn = FALSE)
means <- readLines(paste0(path, "/proj_model/means.out"), warn = FALSE)
percentdb <- readLines(paste0(path, "/proj_model/percentdb.out"), warn = FALSE)
percentiles <- readLines(paste0(path, "/proj_model/percentiles.out"), warn = FALSE)

# Make bigsum file (average over the simulations)
bigsum <- bigfile %>% 
  dplyr::group_by(Alternative, Spp, Yr) %>% 
  dplyr::summarise_at(vars(-group_cols()), mean)

# Write results from Author's F scenario
write.table(bigsum, file = paste0(path, "/", stock, "_out/bigsum.dat"), quote = FALSE, row.names = FALSE, col.names = c("Alt", "Stock", "Year", "ABC", "OFL", "Catch", "SSB", "F", "Total_Biom"))
write.table(bigfile_write, file = paste0(path, "/", stock, "_out/bigfile.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(F_profile, file = paste0(path, "/", stock, "_out/F_profile.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(means, file = paste0(path, "/", stock, "_out/means.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(percentdb, file = paste0(path, "/", stock, "_out/percentdb.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(percentiles, file = paste0(path, "/", stock, "_out/percentiles.out"), quote = FALSE, row.names = FALSE, col.names = FALSE)

#===============================================================================
# Proceed to projections spreadsheet or try new projection summary methods below
#===============================================================================

source('functions4proj.R')

# in the future some of these arguments would be defined in a function called
# 'prepare_proj()' and 'run_proj()'

out <- tidy_proj(stock = 'goa_rebs',       # stock name (tells the program what .dat file to look for in the assessment model files)
                 curr_yr = 2020,           # the current assessment year (not equal to the fin_mod_yr in a partial assessment year)
                 fin_mod_yr = 2019,        # the year the assessment model was run (equal to the curr_yr in full assessment years)
                 base_path = path,         # directory to main proj folder for a given assessment
                 proj_path = 'proj_model') # path to projection model relative to base_path
out$exec_tabl
out$scenario_results

# summarize and save catch assumptions used in projections 
(catch_assump <- catchsum %>% 
    dplyr::bind_rows(out$scenario_results %>% 
                       dplyr::filter(Variable == 'Yield (t)' & Year %in% c(endyr+1, endyr+2)) %>% 
                       dplyr::select(year = Year, catch_used = "Author's F (Estimated catches)")) %>% 
    dplyr::mutate(endyr_ratio = endyr_ratio,
                  yield_ratio = yield_ratio,
                  catch_end_date = catch_end_date,
                  catch_used = case_when(year < endyr ~ catch,
                                         year == endyr ~ catch * endyr_ratio, 
                                         year == endyr+1 ~ futcatch1,
                                         year == endyr+2 ~ futcatch2)))

readr::write_csv(catch_assump, paste0(path, '/proj_catch_assumptions.csv'))
