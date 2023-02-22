# swo age tech memo input ss tables 
# pete.hulson@noaa.gov
# 2022-07

# load ----
library(tidyverse)
library(tidytable)

# output/data ----

# species names
spec <- vroom::vroom(here::here('data', 'species_code_name.csv'))

# annual input sample sizes
iss <- vroom::vroom(here::here('output', 'afsc_iss.csv'))

# ann specimen
ann_spec <- vroom::vroom(here::here('data', 'ann_specimen.csv'))

# ann length
ann_lfreq <- vroom::vroom(here::here('data', 'ann_len.csv'))


########################################################################################################
# Input sample size results for tables





iss %>%
  tidytable::left_join.(spec) %>% 
  tidytable::summarize.(iss_age = mean(iss_age),
                        iss_length = mean(iss_length),
                        .by = c(species_code, species_name, comp_type, region)) -> iss_sum
  
ann_spec %>% 
  tidytable::summarize.(nss_age = mean(nss),
                        hls_age = mean(hls),
                        .by = c(species_code, species_name, type, surv)) %>% 
  tidytable::mutate.(region = case_when.(surv == 'AI' ~ 'ai',
                                         surv == 'EBS_SHELF' ~ 'bs_shelf',
                                         surv == 'EBS_SLOPE' ~ 'bs_slope',
                                         surv == 'GOA' ~ 'goa')) %>% 
  tidytable::select.(species_code, type, region, hls_age, nss_age) %>% 
  tidytable::rename.(comp_type = 'type') -> spec_summ
  
ann_lfreq %>% 
  tidytable::summarize.(nss_length = mean(nss),
                        hls_length = mean(hls),
                        .by = c(species_code, species_name, type, surv)) %>% 
  tidytable::mutate.(region = case_when.(surv == 'AI' ~ 'ai',
                                         surv == 'EBS_SHELF' ~ 'bs_shelf',
                                         surv == 'EBS_SLOPE' ~ 'bs_slope',
                                         surv == 'GOA' ~ 'goa')) %>% 
  tidytable::select.(species_code, type, region, hls_length, nss_length) %>% 
  tidytable::rename.(comp_type = 'type') -> lfreq_summ


iss_sum %>% 
  tidytable::left_join.(spec_summ) %>% 
  tidytable::left_join.(lfreq_summ)
  
  









