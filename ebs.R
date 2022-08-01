#swo work

# install.packages("devtools")
devtools::install_github("BenWilliams-NOAA/swo",force=TRUE)

#load packages
library(swo)

yrs = 2002
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740, 30060)
region = 'BS'
afsc_user = 'BRYANM'
afsc_pwd = '!Gt2022Kam!!'

query_data(region, species, yrs, afsc_user, afsc_pwd, nbs=FALSE)

cpue_data <- vroom::vroom(here::here('data', 'cpue_bs.csv'))
lfreq_data <- vroom::vroom(here::here('data', 'lfreq_bs.csv'))
strata_data <- vroom::vroom(here::here('data', 'strata_bs.csv'))
specimen_data <- vroom::vroom(here::here('data', 'specimen_bs.csv'))

iters = 500
#ebs shelf
cpue_data %>% 
  tidytable::filter.(!(species_code %in% c(30060))) -> .cpue
lfreq_data %>% 
  tidytable::filter.(!(species_code %in% c(30060))) -> .lfreq
specimen_data %>% 
  tidytable::filter.(!(species_code %in% c(30060))) -> .specimen

st <- Sys.time()

swo_sim(iters, .lfreq, .specimen, .cpue, strata_data, yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
        boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
        write_comp = FALSE, write_sample = FALSE, region = 'bs', save_orig = TRUE)

end <- Sys.time()

#ebs slope
cpue_data_s <- vroom::vroom(here::here('data', 'cpue_slopebs.csv'))
lfreq_data_s <- vroom::vroom(here::here('data', 'lfreq_slope_bs.csv'))
strata_data <- vroom::vroom(here::here('data', 'strata_bs.csv'))
specimen_data_s <- vroom::vroom(here::here('data', 'specimen_slopebs.csv'))

cpue_data_s %>% 
  tidytable::filter.(species_code %in% c(10112, 10115,30060)) -> .cpue
lfreq_data_s %>% 
  tidytable::filter.(species_code %in% c(10112, 10115,30060)) -> .lfreq
specimen_data_s %>% 
  tidytable::filter.(species_code %in% c(10112, 10115,30060)) -> .specimen

st <- Sys.time()

swo_sim(iters, .lfreq, .specimen, .cpue, strata_data, yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
        boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
        write_comp = FALSE, write_sample = FALSE, region = 'bs', save_orig = TRUE)

end <- Sys.time()