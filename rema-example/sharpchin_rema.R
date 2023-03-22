
# REMA model demo using sharpchin rockfish

# Use the devtools package to install the rema package from github. If you do
# not have devtools installed, you must do that first. 
# install.packages("devtools")
devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)

library(rema)
library(ggplot2)
library(dplyr)
# install.packages('cowplot')
library(cowplot) # helpful plotting utilities

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# (1) Read in existing rwout.rep files, which is the report file generated from
# the ADMB version of the random effects model
?read_admb_re
admb_re <- read_admb_re(filename = 'sharpchin_rwout.rep',
                        biomass_strata_names = c('CGOA', 'EGOA', 'WGOA'),
                        model_name = 'ADMB')

# Remove the duplicated variable names
admb_re <- read_admb_re(filename = 'sharpchin_rwout_fixed.rep',
                        biomass_strata_names = c('CGOA', 'EGOA', 'WGOA'),
                        model_name = 'ADMB')
names(admb_re)
admb_re$biomass_dat

# (2) Prepare REMA model inputs
?prepare_rema_input # note alternative methods for bringing in survey data observations
input <- prepare_rema_input(model_name = 'TMB-strata-PE',
                            admb_re = admb_re)

# open rwout.rep or .dat files to look at zeros. open the re.tpl file to see how
# zeros are treated
input <- prepare_rema_input(model_name = 'TMB-strata-PE',
                            admb_re = admb_re,
                            # explicity define zero assumption ('NA' is the
                            # default)
                            zeros = list(assumption = 'NA'))

names(input) # TMB users will find these inputs familiar

# (3) Fit REMA model
?fit_rema
m1 <- fit_rema(input)

# (4) Get tidied data.frames from the REMA model output
?tidy_rema
output <- tidy_rema(rema_model = m1)
names(output)
output$parameter_estimates # estimated fixed effects parameters

# if you're interested in the parameter estimates in log-space:
names(m1)
m1$sdrep

output$biomass_by_strata %>% View # df of predicted and observed biomass by stratum
output$total_predicted_biomass # total predicted biomass
output$proportion_biomass_by_strata # apportionment

# (5) Generate model plots
?plot_rema
plots <- plot_rema(tidy_rema = output,
                   # optional y-axis label
                   biomass_ylab = 'Biomass (t)')
plots$biomass_by_strata  
plots$biomass_by_strata + 
  facet_wrap(~strata, ncol = 1)

# interested in apportionment?
plots$proportion_biomass_by_strata

# (6) Bridging or model comparisons: TMB vs. ADMB model
?compare_rema_models
compare <- compare_rema_models(rema_models = list(m1),
                               admb_re = admb_re,
                               biomass_ylab = 'Biomass (t)')

compare$plots$biomass_by_strata + 
  facet_wrap(~strata, ncol = 1, scales = 'free_y')
# Woah. These results are NOT the same. WTH, Jane?

# Troubleshooting steps:
# 1 - we've already checked the zero assumption is 'NA', so we know that's not
# it
# 2 - what about the parameter estimates? open the ADMB .par file and compare it
# to the TMB results
output$parameter_estimates

# Ohhhh, the ADMB model shares process error across all areas. 

# (7) New model with shared process error
input <- prepare_rema_input(model_name = 'TMB-share-PE',
                            admb_re = admb_re,
                            # share process error (default is PE_options =
                            # list(pointer_PE_biomass = c(1, 2, 3)))
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            zeros = list(assumption = 'NA'))

m2 <- fit_rema(input)

# add "m2" to the rema_models list
compare <- compare_rema_models(rema_models = list(m1, m2), 
                               admb_re = admb_re,
                               biomass_ylab = 'Biomass (t)')

compare$plots$biomass_by_strata + 
  facet_wrap(~strata, ncol = 1, scales = 'free_y')
# Whew. everything looks good now.

# (8) Can we implement this same model with a data table instead of an rwout.rep
# file? Yes! all we need is a data.frame with the following columns: strata,
# year, biomass, cv. We can get that from the admb_re object for now.
admb_re$biomass_dat

biomass_dat <- admb_re$biomass_dat
biomass_dat

input <- prepare_rema_input(model_name = 'TMB-biomass-dat',
                            biomass_dat = biomass_dat,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            zeros = list(assumption = 'NA'))

m3 <- fit_rema(input)

compare <- compare_rema_models(rema_models = list(m2, m3),
                               admb_re = admb_re,
                               biomass_ylab = 'Biomass (t)')
compare$plots$biomass_by_strata

# (9) Note that the multivariate version of the RE.tpl used the Marlow method
# for variance calculations of total biomass. We recommend the standard delta
# method. The GPT and SSC knows to expect total biomass confidence intervals to
# differ because of this change. 
compare$plots$total_predicted_biomass

# Tweedie ----

input <- prepare_rema_input(model_name = 'TMB-Tweedie',
                            admb_re = admb_re,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            zeros = list(assumption = 'tweedie'))

m4 <- fit_rema(input)

compare <- compare_rema_models(rema_models = list(m2, m4), 
                               biomass_ylab = 'Biomass (t)')

compare$plots$biomass_by_strata + 
  facet_wrap(~strata, ncol = 1, scales = 'free_y')
compare$plots$total_predicted_biomass
compare$aic
compare$output$parameter_estimates
