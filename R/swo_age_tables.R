# swo age tech memo input ss tables 
# pete.hulson@noaa.gov
# 2022-07

# load ----
library(tidyverse)
library(tidytable)

# output/data ----
spec <- vroom::vroom(here::here('data', 'species_code_name.csv'))

# globals ----
region = 'ai'

# bootstrapped input sample sizes
ai_size <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz.csv'))
ai_age <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag.csv'))
ai_size_rebs <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_rebs.csv'))
ai_age_rebs <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_rebs.csv'))

ai_size_rebs %>% 
  mutate.(species_code = 30050) -> .ai_size_rebs
ai_age_rebs %>% 
  mutate.(species_code = 30050) -> .ai_age_rebs

ai_size %>% 
  bind_rows.(.ai_size_rebs) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> iss_sz

iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ai_avg_iss_sz
  
vroom::vroom_write(iss_sz, file = here::here("output", region, paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(ai_avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz.csv")), delim = ",")

ai_age %>% 
  bind_rows.(.ai_age_rebs) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> iss_ag

iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ai_avg_iss_ag

vroom::vroom_write(iss_ag, file = here::here("output", region, paste0("iss_ag.csv")), delim = ",")
vroom::vroom_write(ai_avg_iss_ag, file = here::here("output", region, paste0("avg_iss_ag.csv")), delim = ",")


# globals ----
region = 'ebs'

# bootstrapped input sample sizes
ebs_size <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz.csv'))
ebs_age <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag.csv'))

ebs_size %>%
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> iss_sz

iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ebs_avg_iss_sz

vroom::vroom_write(iss_sz, file = here::here("output", region, paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(ebs_avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz.csv")), delim = ",")

ebs_age %>%
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> iss_ag

iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ebs_avg_iss_ag

vroom::vroom_write(iss_ag, file = here::here("output", region, paste0("iss_ag.csv")), delim = ",")
vroom::vroom_write(ebs_avg_iss_ag, file = here::here("output", region, paste0("avg_iss_ag.csv")), delim = ",")

# globals ----
region = 'goa'

# bootstrapped input sample sizes
goa_size <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz.csv'))
goa_age <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag.csv'))
goa_size_rebs <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_rebs.csv'))
goa_age_rebs <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_rebs.csv'))
goa_size_dusk <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_dusk.csv'))
goa_age_dusk <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_dusk.csv'))

goa_size_rebs %>% 
  mutate.(species_code = 30050) -> .goa_size_rebs
goa_age_rebs %>% 
  mutate.(species_code = 30050) -> .goa_age_rebs

goa_size_dusk %>% 
  mutate.(species_code = 30150) -> .goa_size_dusk
goa_age_dusk %>% 
  mutate.(species_code = 30150) -> .goa_age_dusk

goa_size %>% 
  bind_rows.(.goa_size_rebs, .goa_size_dusk) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> iss_sz

iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> goa_avg_iss_sz

vroom::vroom_write(iss_sz, file = here::here("output", region, paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(goa_avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz.csv")), delim = ",")

goa_age %>% 
  bind_rows.(.goa_age_rebs, .goa_age_dusk) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> iss_ag

iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> goa_avg_iss_ag

vroom::vroom_write(iss_ag, file = here::here("output", region, paste0("iss_ag.csv")), delim = ",")
vroom::vroom_write(goa_avg_iss_ag, file = here::here("output", region, paste0("avg_iss_ag.csv")), delim = ",")

goa_size_rs <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_w_c_egoa_rocksole.csv'))
goa_age_rs <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_w_c_egoa_rocksole.csv'))
goa_size_rex <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_wc_egoa_rex.csv'))
goa_age_rex <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_wc_egoa_rex.csv'))

goa_size_rs %>% 
  bind_rows.(goa_size_rex) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name, region) %>%
  dplyr::distinct(iss) -> iss_sz

iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess, region)) %>% 
  group_by(species_code, ess, species_name, region) %>%
  dplyr::distinct(avg_iss) -> avg_iss_sz

vroom::vroom_write(iss_sz, file = here::here("output", region, paste0("iss_sz_reg.csv")), delim = ",")
vroom::vroom_write(avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz_reg.csv")), delim = ",")

goa_age_rs %>% 
  bind_rows.(goa_age_rex) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name, region) %>%
  dplyr::distinct(iss) -> iss_ag

iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess, region)) %>% 
  group_by(species_code, ess, species_name, region) %>%
  dplyr::distinct(avg_iss) -> avg_iss_ag

vroom::vroom_write(iss_ag, file = here::here("output", region, paste0("iss_ag_reg.csv")), delim = ",")
vroom::vroom_write(avg_iss_ag, file = here::here("output", region, paste0("avg_iss_ag_reg.csv")), delim = ",")

# Comile some results

ai_avg_iss_sz %>% 
  mutate.(region = "ai") -> .ai_avg_iss_sz

ebs_avg_iss_sz %>% 
  mutate.(region = "ebs") -> .ebs_avg_iss_sz

goa_avg_iss_sz %>% 
  mutate.(region = "goa") %>% 
  bind_rows.(.ai_avg_iss_sz, .ebs_avg_iss_sz) -> avg_iss_sz

ai_avg_iss_ag %>% 
  mutate.(region = "ai") -> .ai_avg_iss_ag

ebs_avg_iss_ag %>% 
  mutate.(region = "ebs") -> .ebs_avg_iss_ag

goa_avg_iss_ag %>% 
  mutate.(region = "goa") %>% 
  bind_rows.(.ai_avg_iss_ag, .ebs_avg_iss_ag) -> avg_iss_ag

vroom::vroom_write(avg_iss_sz, file = here::here("output", paste0("avg_iss_sz.csv")), delim = ",")
vroom::vroom_write(avg_iss_ag, file = here::here("output", paste0("avg_iss_ag.csv")), delim = ",")







