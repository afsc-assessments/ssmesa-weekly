library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# tidy the ADMB proj output
tidy_proj <- function(
    stock = 'goa_rebs', # stock name (tells the program what .dat file to look for in the assessment model files)
    curr_yr = 2022, # the current assessment year (not equal to the fin_mod_yr in a partial assessment year)
    fin_mod_yr = 2021, # the year the assessment model was run (equal to the curr_yr in full assessment years)
    base_path = path,  # base path for the current year's projection
    proj_path = 'proj_model' # path to projection model relative to base_path
){
  
  # stock = 'goa_rebs'; curr_yr = 2022; fin_mod_yr = 2022; base_path = path; proj_path = 'proj_model'
  
  # projection years in executive summary table    
  if(curr_yr == fin_mod_yr) {
    pyr1 = fin_mod_yr + 1
    pyr2 = fin_mod_yr + 2
  } else if(curr_yr < fin_mod_yr) {
    stop('The current year (curr_yr) is less than the year of the last assessment (fin_mod_yr). The curr_yr must be greater than or equal to the fin_mod_yr.')
  } else {
    diff_yr = curr_yr - fin_mod_yr
    pyr1 = fin_mod_yr + 1 + diff_yr
    pyr2 = fin_mod_yr + 2  + diff_yr
  }       
  
  # output files that were previously copy-pasted in the excel sheet
  perc_max <- readLines(paste0(base_path, "/", stock, "_max_out/percentiles.out"), warn = FALSE)
  perc_authf <- readLines(paste0(base_path, "/", stock, "_out/percentiles.out"), warn = FALSE)
  bigsum <- read.delim(paste0(base_path, "/", stock, "_out/bigsum.dat"), sep = "", header = TRUE) 
  bigsum_frac40 <- read.delim(paste0(base_path, "/", stock, "_max_out/bigsum.dat"), sep = "", header = TRUE) 
  
  # for safe executive summary table
  stock_info <-  readLines(paste0(base_path, "/", proj_path, "/data/", stock, ".dat"), warn = FALSE)
  natmat <- read.table(textConnection(stock_info[(grep('_Natural_Mortality', stock_info)[1]+1)]))[1,1]
  recage <- strsplit(stock_info[(grep('_Natural_Mortality', stock_info))], split = ' ')[[1]][2]

  # alternative 1: max F
  yield_alt1 <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[1]+2):(grep('Spawning_Biomass', perc_max)[1]-2)]))
  names(yield_alt1) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt1 <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[1]+2):(grep('Fishing_mortality', perc_max)[1]-2)]))
  names(ssb_alt1) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt1 <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[1]+2):(grep('Total_Biomass', perc_max)[1]-2)]))
  names(fmort_alt1) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  # alternative 2: author's F (use specified catches)
  yield_alt2 <- read.table(textConnection(perc_authf[(grep('Catch', perc_authf)[1]+2):(grep('Spawning_Biomass', perc_authf)[1]-2)]))
  names(yield_alt2) <- strsplit(perc_authf[8], split = ' ')[[1]]
  
  ssb_alt2 <- read.table(textConnection(perc_authf[(grep('Spawning_Biomass', perc_authf)[1]+2):(grep('Fishing_mortality', perc_authf)[1]-2)]))
  names(ssb_alt2) <- strsplit(perc_authf[25], split = ' ')[[1]]
  
  fmort_alt2 <- read.table(textConnection(perc_authf[(grep('Fishing_mortality', perc_authf)[1]+2):(grep('Total_Biomass', perc_authf)[1]-2)]))
  names(fmort_alt2) <- strsplit(perc_authf[42], split = ' ')[[1]]
  
  biomass_alt2 <- read.table(textConnection(perc_authf[(grep('Total_Biomass', perc_authf)[1]+2):(grep('Alternative 2', perc_authf)[1]-1)]))
  names(biomass_alt2) <- strsplit(perc_authf[59], split = ' ')[[1]]
  
  # alternative 2: author's F (fraction of F40)
  yield_alt2b <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[3]+2):(grep('Spawning_Biomass', perc_max)[2]-2)]))
  names(yield_alt2b) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt2b <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[2]+2):(grep('Fishing_mortality', perc_max)[2]-2)]))
  names(ssb_alt2b) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt2b <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[2]+2):(grep('Total_Biomass', perc_max)[2]-2)]))
  names(fmort_alt2b) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  biomass_alt2b <- read.table(textConnection(perc_max[(grep('Total_Biomass', perc_max)[2]+2):(grep('Alternative 3', perc_max)[1]-1)]))
  names(biomass_alt2b) <- strsplit(perc_max[59], split = ' ')[[1]]
  
  # alternative 3: 5 yr avg F
  
  # note the indexing for 'Catch' is unique b/c its recycled more time than the other values
  yield_alt3 <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[5]+2):(grep('Spawning_Biomass', perc_max)[3]-2)]))
  names(yield_alt3) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt3 <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[3]+2):(grep('Fishing_mortality', perc_max)[3]-2)]))
  names(ssb_alt3) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt3 <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[3]+2):(grep('Total_Biomass', perc_max)[3]-2)]))
  names(fmort_alt3) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  # alternative 4: no F
  yield_alt4 <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[7]+2):(grep('Spawning_Biomass', perc_max)[4]-2)]))
  names(yield_alt4) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt4 <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[4]+2):(grep('Fishing_mortality', perc_max)[4]-2)]))
  names(ssb_alt4) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt4 <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[4]+2):(grep('Total_Biomass', perc_max)[4]-2)]))
  names(fmort_alt4) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  # alternative 5: no F
  yield_alt5 <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[9]+2):(grep('Spawning_Biomass', perc_max)[5]-2)]))
  names(yield_alt5) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt5 <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[5]+2):(grep('Fishing_mortality', perc_max)[5]-2)]))
  names(ssb_alt5) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt5 <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[5]+2):(grep('Total_Biomass', perc_max)[5]-2)]))
  names(fmort_alt5) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  # alternative 6: test for overfished
  yield_alt6 <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[11]+2):(grep('Spawning_Biomass', perc_max)[6]-2)]))
  names(yield_alt6) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt6 <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[6]+2):(grep('Fishing_mortality', perc_max)[6]-2)]))
  names(ssb_alt6) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt6 <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[6]+2):(grep('Total_Biomass', perc_max)[6]-2)]))
  names(fmort_alt6) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  # alternative 7: test for overfished
  yield_alt7 <- read.table(textConnection(perc_max[(grep('Catch', perc_max)[13]+2):(grep('Spawning_Biomass', perc_max)[7]-2)]))
  names(yield_alt7) <- strsplit(perc_max[8], split = ' ')[[1]]
  
  ssb_alt7 <- read.table(textConnection(perc_max[(grep('Spawning_Biomass', perc_max)[7]+2):(grep('Fishing_mortality', perc_max)[7]-2)]))
  names(ssb_alt7) <- strsplit(perc_max[25], split = ' ')[[1]]
  
  fmort_alt7 <- read.table(textConnection(perc_max[(grep('Fishing_mortality', perc_max)[7]+2):(grep('Total_Biomass', perc_max)[7]-2)]))
  names(fmort_alt7) <- strsplit(perc_max[42], split = ' ')[[1]]
  
  # proj summary tables ----
  
  yield_sum <- yield_alt1 %>% 
    dplyr::select(Year, `Maximum permissible F` = Mean_Catch) %>% 
    dplyr::left_join(yield_alt2 %>% dplyr::select(Year, `Author's F (Estimated catches)` = Mean_Catch), by = join_by(Year)) %>% 
    dplyr::left_join(yield_alt2b %>% dplyr::select(Year, `Author's F (Fraction of F40%)` = Mean_Catch), by = join_by(Year)) %>% 
    dplyr::left_join(yield_alt3 %>% dplyr::select(Year, `Half maximum F` = Median_Catch), by = join_by(Year)) %>% 
    dplyr::left_join(yield_alt4 %>% dplyr::select(Year, `5-year average F` = Mean_Catch), by = join_by(Year)) %>% 
    dplyr::left_join(yield_alt5 %>% dplyr::select(Year, `No fishing` = Mean_Catch), by = join_by(Year)) %>% 
    dplyr::left_join(yield_alt6 %>% dplyr::select(Year, `Overfished` = Mean_Catch), by = join_by(Year)) %>% 
    dplyr::left_join(yield_alt7 %>% dplyr::select(Year, `Approaching overfished` = Mean_Catch), by = join_by(Year)) %>% 
    dplyr::mutate_at(vars(-'Year'), function(x) x * 1e3) %>% 
    dplyr::mutate(Variable = 'Yield (t)') %>% 
    dplyr::relocate(Variable, .before = Year)
    # readr::write_csv(paste0(base_path, '/proj_scenarios_yield.csv'))
  
  ssb_sum <- ssb_alt1 %>%
    dplyr::select(Year, `Maximum permissible F` = Mean_SSB) %>% 
    dplyr::left_join(ssb_alt2 %>% dplyr::select(Year, `Author's F (Estimated catches)` = Mean_SSB), by = join_by(Year)) %>% 
    dplyr::left_join(ssb_alt2b %>% dplyr::select(Year, `Author's F (Fraction of F40%)` = Mean_SSB), by = join_by(Year)) %>% 
    dplyr::left_join(ssb_alt3 %>% dplyr::select(Year, `Half maximum F` = Median_SSB), by = join_by(Year)) %>% 
    dplyr::left_join(ssb_alt4 %>% dplyr::select(Year, `5-year average F` = Mean_SSB), by = join_by(Year)) %>% 
    dplyr::left_join(ssb_alt5 %>% dplyr::select(Year, `No fishing` = Mean_SSB), by = join_by(Year)) %>% 
    dplyr::left_join(ssb_alt6 %>% dplyr::select(Year, `Overfished` = Mean_SSB), by = join_by(Year)) %>% 
    dplyr::left_join(ssb_alt7 %>% dplyr::select(Year, `Approaching overfished` = Mean_SSB), by = join_by(Year)) %>% 
    dplyr::mutate_at(vars(-'Year'), function(x) x * 1e3) %>% 
    dplyr::mutate(Variable = 'Spawning Biomass (t)') %>% 
    dplyr::relocate(Variable, .before = Year)
    # readr::write_csv(paste0(base_path, '/proj_scenarios_ssb.csv'))
  
  fmort_sum <- fmort_alt1 %>%
    dplyr::select(Year, `Maximum permissible F` = Mean_F) %>% 
    dplyr::left_join(fmort_alt2 %>% dplyr::select(Year, `Author's F (Estimated catches)` = Mean_F), by = join_by(Year)) %>% 
    dplyr::left_join(fmort_alt2b %>% dplyr::select(Year, `Author's F (Fraction of F40%)` = Mean_F), by = join_by(Year)) %>% 
    dplyr::left_join(fmort_alt3 %>% dplyr::select(Year, `Half maximum F` = Median_F), by = join_by(Year)) %>% 
    dplyr::left_join(fmort_alt4 %>% dplyr::select(Year, `5-year average F` = Mean_F), by = join_by(Year)) %>% 
    dplyr::left_join(fmort_alt5 %>% dplyr::select(Year, `No fishing` = Mean_F), by = join_by(Year)) %>% 
    dplyr::left_join(fmort_alt6 %>% dplyr::select(Year, `Overfished` = Mean_F), by = join_by(Year)) %>% 
    dplyr::left_join(fmort_alt7 %>% dplyr::select(Year, `Approaching overfished` = Mean_F), by = join_by(Year)) %>% 
    dplyr::mutate(Variable = 'Fishing Mortality') %>% 
    dplyr::relocate(Variable, .before = Year)
    # readr::write_csv(paste0(base_path, '/proj_scenarios_fmort.csv'))
  
  scenario_results <- yield_sum %>% 
    dplyr::bind_rows(ssb_sum) %>% 
    dplyr::bind_rows(fmort_sum) %>% 
    dplyr::mutate(Variable = factor(Variable, levels = c('Spawning Biomass (t)', 'Fishing Mortality', 'Yield (t)'), ordered = TRUE)) %>% 
    dplyr::arrange(Variable) %>% 
    readr::write_csv(paste0(base_path, '/proj_scenario_results.csv'))

  # Exec table ----
  
  projsum <- bigsum %>% 
    dplyr::filter(Alt == 1 &
                    between(Year, pyr1, pyr2)) %>% 
    dplyr::mutate_at(vars(-c('Alt', 'Stock', 'Year', 'F')), function(x) x * 1e3)
  
  proj_biom <- projsum %>% 
    dplyr::mutate(item = paste0('Projected total (age ', recage, '+) biomass (t)')) %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Total_Biom) %>% 
    dplyr::bind_rows(projsum %>% 
                       dplyr::mutate(item = 'Projected female spawning biomass (t)') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = SSB))
  
  # reference points and tier designations
  brps <- read.table(textConnection(perc_authf[(grep('SB0', perc_max)[1]+1)]))
  names(brps) <- strsplit(perc_authf[2], split = ' ')[[1]]
  
  tier_yr1 <- if(as.numeric(proj_biom[2,2]) > brps$SB40) {'3a'} else {'3b'}
  hcr_mult1 <- if(tier_yr1 == '3a') {1} else { ((as.numeric(proj_biom[2,2]) / (brps$SB40 * 1e3)) - 0.05) / (1 - 0.05) }
  tier_yr2 <- if(as.numeric(proj_biom[2,3]) > brps$SB40) {'3a'} else {'3b'}
  hcr_mult2 <- if(tier_yr2 == '3a') {1} else { ((as.numeric(proj_biom[2,3]) / (brps$SB40 * 1e3)) - 0.05) / (1 - 0.05) }
  
  proj_brps <- data.frame(Year = c(pyr1, pyr2),
                          `B100` = brps$SB0 * 1e3,
                          `B40` = brps$SB40 * 1e3,
                          `B35` = brps$SB35 * 1e3) %>% 
    tidyr::pivot_longer(cols = c('B100', 'B40', 'B35'), names_to = 'item') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value) %>% 
    dplyr::mutate(item = dplyr::case_when(item == 'B100' ~ 'B100%',
                                          item == 'B40' ~ 'B40%',
                                          item == 'B35' ~ 'B35%'))
  
  # FOFL and FABCs
  proj_F <- fmort_alt2 %>% 
    dplyr::filter(between(Year, pyr1, pyr2)) %>% 
    dplyr::mutate(Fofl = Fofl * c(hcr_mult1, hcr_mult2),
                  Fabc = Fabc * c(hcr_mult1, hcr_mult2))
  
  proj_F <- proj_F %>% 
    dplyr::mutate(item = 'FOFL') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Fofl) %>% 
    dplyr::bind_rows(proj_F %>% 
                       dplyr::mutate(item = 'maxFABC') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Fabc)) %>% 
    dplyr::bind_rows(proj_F %>% 
                       dplyr::mutate(item = 'FABC') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Fabc))
  
  # OFL and ABCs
  ofl_abc <- projsum %>%
    dplyr::mutate(item = 'OFL (t)') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = OFL) %>% 
    dplyr::bind_rows(projsum %>% 
                       dplyr::mutate(item = 'maxABC (t)') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = ABC)) %>% 
    dplyr::bind_rows(projsum %>% 
                       dplyr:: mutate(item = 'ABC (t)') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = ABC)) 
  
  # natural mortality
  proj_M <- data.frame(Year = c(pyr1, pyr2),
                       item = 'M (natural mortality)',
                       value = c(natmat, natmat)) %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value)
  
  # tier designation
  proj_tier <- data.frame(Year = c(pyr1, pyr2),
                       item = 'Tier',
                       value = c(tier_yr1, tier_yr2)) %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value)
  
  exec_tabl <- proj_M %>% 
    dplyr::bind_rows(proj_biom) %>% 
    dplyr::bind_rows(proj_brps) %>% 
    dplyr::bind_rows(proj_F) %>% 
    dplyr::bind_rows(ofl_abc) %>% 
    readr::write_csv(paste0(base_path, '/exec_summary_tbl.csv'))

  proj_tier %>%  
    readr::write_csv(paste0(base_path, '/tier_design.csv'))
  
  # alt table using bigsum_frac40 - to test why/if we need to run proj more than
  # once ------
  
  projsum <- bigsum_frac40 %>% 
    dplyr::filter(Alt == 2 &
                    between(Year, pyr1, pyr2)) %>% 
    dplyr::mutate_at(vars(-c('Alt', 'Stock', 'Year', 'F')), function(x) x * 1e3)
  
  proj_biom <- projsum %>% 
    dplyr::mutate(item = paste0('Projected total (age ', recage, '+) biomass (t)')) %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Total_Biom) %>% 
    dplyr::bind_rows(projsum %>% 
                       dplyr::mutate(item = 'Projected female spawning biomass (t)') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = SSB))
  
  # reference points and tier designations
  brps <- read.table(textConnection(perc_max[(grep('SB0', perc_max)[1]+1)]))
  names(brps) <- strsplit(perc_max[2], split = ' ')[[1]]
  
  tier_yr1 <- if(as.numeric(proj_biom[2,2]) > brps$SB40) {'3a'} else {'3b'}
  hcr_mult1 <- if(tier_yr1 == '3a') {1} else { ((as.numeric(proj_biom[2,2]) / (brps$SB40 * 1e3)) - 0.05) / (1 - 0.05) }
  tier_yr2 <- if(as.numeric(proj_biom[2,3]) > brps$SB40) {'3a'} else {'3b'}
  hcr_mult2 <- if(tier_yr2 == '3a') {1} else { ((as.numeric(proj_biom[2,3]) / (brps$SB40 * 1e3)) - 0.05) / (1 - 0.05) }
  
  proj_brps <- data.frame(Year = c(pyr1, pyr2),
                          `B100` = brps$SB0 * 1e3,
                          `B40` = brps$SB40 * 1e3,
                          `B35` = brps$SB35 * 1e3) %>% 
    tidyr::pivot_longer(cols = c('B100', 'B40', 'B35'), names_to = 'item') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value) %>% 
    dplyr::mutate(item = dplyr::case_when(item == 'B100' ~ 'B100%',
                                          item == 'B40' ~ 'B40%',
                                          item == 'B35' ~ 'B35%'))
  
  # FOFL and FABCs
  proj_F <- fmort_alt2b %>% 
    dplyr::filter(between(Year, pyr1, pyr2)) %>% 
    dplyr::mutate(Fofl = Fofl * c(hcr_mult1, hcr_mult2),
                  Fabc = Fabc * c(hcr_mult1, hcr_mult2))
  
  proj_F <- proj_F %>% 
    dplyr::mutate(item = 'FOFL') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Fofl) %>% 
    dplyr::bind_rows(proj_F %>% 
                       dplyr::mutate(item = 'maxFABC') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Fabc)) %>% 
    dplyr::bind_rows(proj_F %>% 
                       dplyr::mutate(item = 'FABC') %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = Fabc))
  
  # OFL and ABCs
  ofl_abc <- projsum %>%
    dplyr::mutate(item = 'OFL (t)') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = OFL) %>% 
    dplyr::bind_rows(projsum %>% 
                       dplyr::mutate(item = 'maxABC (t)',
                                     ABC = ABC / yield_ratio) %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = ABC)) %>% 
    dplyr::bind_rows(projsum %>% 
                       dplyr:: mutate(item = 'ABC (t)',
                                      ABC = ABC / yield_ratio) %>% 
                       tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = ABC)) 
  
  # natural mortality
  proj_M <- data.frame(Year = c(pyr1, pyr2),
                       item = 'M (natural mortality)',
                       value = c(natmat, natmat)) %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value)
  
  # tier designation
  proj_tier <- data.frame(Year = c(pyr1, pyr2),
                       item = 'Tier',
                       value = c(tier_yr1, tier_yr2)) %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value)
  
  compare_methods <- exec_tabl %>% 
    dplyr::mutate(scen2_method = "Current: Run proj twice",
                  description = "Scen 2 based on specified future catches; Future_catches = mean(TAC/ABC) * maxABC [two proj yrs]") %>% 
    dplyr::bind_rows(proj_M %>% 
                       dplyr::bind_rows(proj_biom) %>% 
                       dplyr::bind_rows(proj_brps) %>% 
                       dplyr::bind_rows(proj_F) %>% 
                       dplyr::bind_rows(ofl_abc) %>% 
                       dplyr::mutate(scen2_method = "Proposed: Run proj once",
                                     description = "Scen 2 based on fraction of F40; FABC = mean(TAC/ABC) * maxFABC [all proj yrs]")) %>% 
    dplyr::relocate(scen2_method, .before = item) %>% 
    dplyr::arrange(item) #%>%
    # readr::write_csv(paste0(base_path, '/compare_scen2_methods.csv'))
  
  # proj_tier %>%  
  #   readr::write_csv(paste0(base_path, '/tier_design_frac40.csv'))

  out <- list(tier = proj_tier,
              exec_tabl = exec_tabl,
              scenario_results = scenario_results,
              scen2_methods = compare_methods)

  return(out)  
}

# #
# test_overfishing <- function(current = curr, # current year's exec summary info
#                              past2 = past2, # exec summary tbl from two years ago
#                              catch = catch # catch assumptions
# ) {
#   
# }

# Prepare data for the executive summary table
prep_exec_tbl <- function(
    current = curr, # current year's exec summary info
    current_tier = curr_tier, # current year's tier designations
    past = past, # last year's exec summary info
    past_tier = past_tier # last year's tier designations
){
  # join tables, append overfishing/status results
  tbl_top <- past %>% dplyr::left_join(current, by = join_by(item))
  tbl_bot <- tibble::tibble(Year = c(unique(names(tbl_top)[-1])),
                            blank = rep(NA, 4),
                            Status = c(endyr-2, endyr-1, paste0(endyr-1, '_'), endyr),
                            Overfishing = c('No', 'n/a', tof1, 'n/a'),
                            Overfished = c('n/a', 'No', 'n/a', tof2),
                            'Approaching overfished' = c('n/a', 'No', 'n/a', tof3)) %>% 
    tidyr::pivot_longer(cols = c(blank, 
                                 'Status', 'Overfishing', 'Overfished', `Approaching overfished`), 
                        names_to = 'item') %>% 
    tidyr::pivot_wider(id_cols = item, names_from = Year, values_from = value)
  
  tbl_bot2 <- tbl_bot %>% dplyr::slice(-1)
  
  # format values with commas and the correct number of decimal places
  data <- tbl_top %>% 
    dplyr::mutate_if(is.numeric, ~ifelse(. < 1, format(round(., 3), nsmall = 3), 
                                  prettyNum(round(.,0), trim = TRUE, big.mark = ','))) %>% 
    dplyr::bind_rows(tbl_bot) %>% 
    dplyr::bind_rows(past_tier %>% 
                       dplyr::left_join(current_tier, by = join_by(item))) %>% 
    dplyr::slice(1, 18, 2:17)
  
  return(data)
}

# format the executive summary table using flextable - original code by Ben
# Williams: https://github.com/ben-williams/safe_tbl
exec_tbl <- function(data, # output from prep_exec_tbl
                     endyr, # assessment year
                     tier, # tier 1-6
                     catch # catch assumptions dataframe
                     ){
  
  require(dplyr)
  require(flextable)
  
  flextable(data) %>%
    add_header_row(value = c( "", "a", "b"), colwidths = c(1,2,2)) %>%
    compose(i=2, j=1,
            value = as_paragraph( "Quantity"),
            part = "header") %>%
    compose(i = 1, j = 2:3,
            value = as_paragraph( "As estimated or " , as_i("specified last"), " year for:"),
            part = "header") %>%
    compose(i = 1, j = 4:5,
            value = as_paragraph( "As estimated or " , as_i("recommended this"), " year for:"),
            part = "header") %>%
    compose(i = 2, j = 2, value = as_paragraph(as.character(endyr)), 
            part = "header") %>%
    compose(i = 2, j = 3:4, value = as_paragraph(as_chunk(as.character(endyr + 1))),
            part = "header") %>%
    compose(i = 2, j = 5, value = as_paragraph(as_chunk(as.character(endyr + 2))),
            part = "header") %>%
    align(align = "center", part = "header") %>%
    align(j=2:5, align = "center") %>%
    align(j = 1, part = "header", align="left") %>%
    bold(i = 2, j = 1, part = "header") %>%
    width(j = 1, width = 2.5) %>%
    width(j = 2:5, width = 0.65) %>%
    bg(j = 2, bg = "#f7f7f7", part = "all") %>%
    bg(j = 3, bg = "#f7f7f7", part = "all") %>%
    border_remove() %>%
    hline_top(part = "header") %>%
    hline_top() %>%
    vline_right(part = "body") %>%
    vline_right(part = "header") %>%
    vline_left(part = "body") %>%
    vline_left(part = "header") -> tbl
  
  if(tier == 3){
    tbl %>%
      footnote(i=2, j=4:5, part = "header", ref_symbols = "*",
               value = as_paragraph(as_chunk(paste0("Projections are based on an estimated catch of ",
                                                    prettyNum(round(catch$catch_used[catch$year == endyr], 0), 
                                                              big.mark = ","), 
                                                    " t for ", endyr, " and estimates of ",
                                                    prettyNum(round(catch$catch_used[catch$year == endyr+1], 0), 
                                                              big.mark = ","),
                                                    " t and ", 
                                                    prettyNum(round(catch$catch_used[catch$year == endyr+2], 0), 
                                                              big.mark = ","),
                                                    " t used in place of maximum permissible ABC for ",
                                                    endyr+1, " and ", endyr+2, "."))),
               sep = ".") %>%
      compose(i = 5, j = 1, value = as_paragraph("B", as_sub("100%"))) %>%
      compose(i = 6, j = 1, value = as_paragraph("B", as_sub("40%"))) %>%
      compose(i = 7, j = 1, value = as_paragraph("B", as_sub("35%"))) %>%
      compose(i = 8, j = 1, value = as_paragraph("F", as_sub("OFL"))) %>%
      compose(i = 9, j = 1, value = as_paragraph(as_i("max"),"F", as_sub("ABC"))) %>%
      compose(i = 10, j = 1, value = as_paragraph("F", as_sub("ABC"))) %>%
      compose(i = 12, j = 1, value = as_paragraph(as_i("max"),"ABC (t)")) %>%
      compose(i = 14, j = 1, value = as_paragraph("")) %>%
      compose(i = 14, j = 2:3, 
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 14, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 15, j = 2,
              value = as_paragraph(as_chunk(as.character(endyr - 1)))) %>%
      compose(i = 15, j = 3:4,
              value = as_paragraph(as_chunk(as.character(endyr)))) %>%
      compose(i = 15, j = 5,
              value = as_paragraph(as_chunk(as.character(endyr + 1)))) %>%
      colformat_double(i = c(3:7, 11:13), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=14, part = "body") %>%
      bold(i = 15, j = 1) %>%
      hline(i=13) %>%
      hline(i=15) %>%
      hline(i=18) %>%
      fix_border_issues()
    
    # flextable(tbl_bot) %>% 
    
  } else if(tier==1){
    
    if(nrow(tbl$body$data)>17){
      stop("this is not a tier 1 input, \nmaybe you have a tier 3 stock...")
    }
    
    tier = "1a"
    
    tbl %>%
      footnote(i=2, j=4:5, part = "header", ref_symbols = "*",
               value = as_paragraph(as_chunk(paste0("Projections are based on an estimated catch of ",
                                                    prettyNum(c1, big.mark = ","), " t for ", year,
                                                    " and estimates of ",
                                                    prettyNum(c2, big.mark = ","),
                                                    " t and ",
                                                    prettyNum(c3, big.mark = ","),
                                                    " t used in place of maximum permissible ABC for ",
                                                    year+1, " and ", year+2, "."))),
               sep = ".") %>%
      compose(i = 2, j = 2:5, value = as_paragraph(tier), part = "body") %>%
      compose(i = 4, j = 1, value = as_paragraph("F", as_sub("OFL")), part = "body") %>%
      compose(i = 5, j = 1, value = as_paragraph(as_i("max"),"F", as_sub("ABC")), part = "body") %>%
      compose(i = 6, j = 1, value = as_paragraph("F", as_sub("ABC")), part = "body") %>%
      compose(i = 7, j = 1, value = as_paragraph(as_i("max"),"ABC (t)"), part = "body") %>%
      compose(i = 13, j = 1, value = as_paragraph("")) %>%
      compose(i = 13, j = 2:3,
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 13, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 14, j = 2,
              value = as_paragraph(as_chunk(as.character(year - 1)))) %>%
      compose(i = 14, j = 3:4,
              value = as_paragraph(as_chunk(as.character(year)))) %>%
      compose(i = 14, j = 5,
              value = as_paragraph(as_chunk(as.character(year + 1)))) %>%
      compose(i=15, j=c(2,4), value = as_paragraph("No")) %>%
      compose(i=15, j=c(3,5), value = as_paragraph("n/a")) %>%
      compose(i=16:17, j=c(3,5), value = as_paragraph("No")) %>%
      compose(i=16:17, j=c(2,4), value = as_paragraph("n/a")) %>%
      colformat_double(i = c(3:7, 10:12), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=13, part = "body") %>%
      align(align = "center", part = "header") %>%
      align(j=2:5, align = "center", part = "body") %>%
      align(j = 1, part = "header", align="left") %>%
      bold(i = 2, j = 1, part = "header") %>%
      bold(i = 15, j = 1) %>%
      hline(i=12) %>%
      hline(i=14) %>%
      hline(i=17) %>%
      fix_border_issues()
  } else if(tier == 5){
    
    tier = "5"
    
    tbl %>%
      compose(i = 2, j = 2:5, value = as_paragraph(tier), part = "body") %>%
      compose(i = 4, j = 1, value = as_paragraph("F", as_sub("OFL")), part = "body") %>%
      compose(i = 5, j = 1, value = as_paragraph(as_i("max"),"F", as_sub("ABC")), part = "body") %>%
      compose(i = 6, j = 1, value = as_paragraph("F", as_sub("ABC")), part = "body") %>%
      compose(i = 8, j = 1, value = as_paragraph(as_i("max"),"ABC (t)"), part = "body") %>%
      compose(i = 10, j = 1, value = as_paragraph("")) %>%
      compose(i = 10, j = 2:3,
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 10, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 11, j = 2,
              value = as_paragraph(as_chunk(as.character(year - 1)))) %>%
      compose(i = 11, j = 3:4,
              value = as_paragraph(as_chunk(as.character(year)))) %>%
      compose(i = 11, j = 5,
              value = as_paragraph(as_chunk(as.character(year + 1)))) %>%
      compose(i=12, j=c(2,4), value = as_paragraph("No")) %>%
      compose(i=12, j=c(3,5), value = as_paragraph("n/a")) %>%
      colformat_double(i = c(3,7:9), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=10, part = "body") %>%
      align(align = "center", part = "header") %>%
      align(j=2:5, align = "center", part = "body") %>%
      align(j = 1, part = "header", align="left") %>%
      hline(i=9) %>%
      hline(i=11) %>%
      hline(i=12) %>%
      fix_border_issues()
  } else {
    tier = "6"
    
    tbl %>%
      compose(i = 1, j = 2:5, value = as_paragraph(tier), part = "body") %>%
      compose(i = 3, j = 1, value = as_paragraph(as_i("max"),"ABC (t)"), part = "body") %>%
      compose(i = 5, j = 1, value = as_paragraph("")) %>%
      compose(i = 5, j = 2:3,
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 5, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 6, j = 2,
              value = as_paragraph(as_chunk(as.character(year - 1)))) %>%
      compose(i = 6, j = 3:4,
              value = as_paragraph(as_chunk(as.character(year)))) %>%
      compose(i = 6, j = 5,
              value = as_paragraph(as_chunk(as.character(year + 1)))) %>%
      compose(i=7, j=c(2,4), value = as_paragraph("No")) %>%
      compose(i=7, j=c(3,5), value = as_paragraph("n/a")) %>%
      colformat_double(i = c(2:4), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=5, part = "body") %>%
      align(align = "center", part = "header") %>%
      align(j=2:5, align = "center", part = "body") %>%
      align(j = 1, part = "header", align="left") %>%
      hline(i=4) %>%
      hline(i=6) %>%
      hline(i=7) %>%
      fix_border_issues()
  }
}

