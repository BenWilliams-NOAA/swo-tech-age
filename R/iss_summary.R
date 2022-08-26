# swo age tech memo input ss tables 
# pete.hulson@noaa.gov
# 2022-07

# load ----
library(tidyverse)
library(tidytable)

# output/data ----
spec <- vroom::vroom(here::here('data', 'species_code_name.csv'))

########################################################################################################
# Input sample size results for tables
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
  dplyr::distinct(iss) -> ai_iss_sz

ai_iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ai_avg_iss_sz
  
vroom::vroom_write(ai_iss_sz, file = here::here("output", region, paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(ai_avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz.csv")), delim = ",")

ai_age %>% 
  bind_rows.(.ai_age_rebs) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> ai_iss_ag

ai_iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ai_avg_iss_ag

vroom::vroom_write(ai_iss_ag, file = here::here("output", region, paste0("iss_ag.csv")), delim = ",")
vroom::vroom_write(ai_avg_iss_ag, file = here::here("output", region, paste0("avg_iss_ag.csv")), delim = ",")


# globals ----
region = 'bs'

# bootstrapped input sample sizes for shelf survey
ebs_shelf_size <- vroom::vroom(here::here('output', region, 'bs_shelf', 'input_ss_ess_sz.csv'))
ebs_shelf_age <- vroom::vroom(here::here('output', region, 'bs_shelf', 'input_ss_ess_ag.csv'))

ebs_shelf_size %>%
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> ebs_shelf_iss_sz

ebs_shelf_iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ebs_shelf_avg_iss_sz

vroom::vroom_write(ebs_shelf_iss_sz, file = here::here("output", region, 'bs_shelf', paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(ebs_shelf_avg_iss_sz, file = here::here("output", region, 'bs_shelf', paste0("avg_iss_sz.csv")), delim = ",")

ebs_shelf_age %>%
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> ebs_shelf_iss_ag

ebs_shelf_iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ebs_shelf_avg_iss_ag

vroom::vroom_write(ebs_shelf_iss_ag, file = here::here("output", region, 'bs_shelf', paste0("iss_ag.csv")), delim = ",")
vroom::vroom_write(ebs_shelf_avg_iss_ag, file = here::here("output", region, 'bs_shelf', paste0("avg_iss_ag.csv")), delim = ",")

# bootstrapped input sample sizes for slope survey
ebs_slope_size <- vroom::vroom(here::here('output', region, 'bs_slope', 'input_ss_ess_sz.csv'))
ebs_slope_age <- vroom::vroom(here::here('output', region, 'bs_slope', 'input_ss_ess_ag.csv'))

ebs_slope_size %>%
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> ebs_slope_iss_sz

ebs_slope_iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ebs_slope_avg_iss_sz

vroom::vroom_write(ebs_slope_iss_sz, file = here::here("output", region, 'bs_slope', paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(ebs_slope_avg_iss_sz, file = here::here("output", region, 'bs_slope', paste0("avg_iss_sz.csv")), delim = ",")

ebs_slope_age %>%
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> ebs_slope_iss_ag

ebs_slope_iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> ebs_slope_avg_iss_ag

vroom::vroom_write(ebs_slope_iss_ag, file = here::here("output", region, 'bs_slope', paste0("iss_ag.csv")), delim = ",")
vroom::vroom_write(ebs_slope_avg_iss_ag, file = here::here("output", region, 'bs_slope', paste0("avg_iss_ag.csv")), delim = ",")



# globals ----
region = 'goa'

# bootstrapped input sample sizes
goa_size <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz.csv'))
goa_age <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag.csv'))
goa_size_rebs <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_rebs.csv'))
goa_age_rebs <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_rebs.csv'))
goa_size_dusk <- vroom::vroom(here::here('output', region, 'input_ss_ess_sz_dr.csv'))
goa_age_dusk <- vroom::vroom(here::here('output', region, 'input_ss_ess_ag_dr.csv'))

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
  dplyr::distinct(iss) -> goa_iss_sz

goa_iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> goa_avg_iss_sz

vroom::vroom_write(goa_iss_sz, file = here::here("output", region, paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(goa_avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz.csv")), delim = ",")

goa_age %>% 
  bind_rows.(.goa_age_rebs, .goa_age_dusk) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name) %>%
  dplyr::distinct(iss) -> goa_iss_ag

goa_iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess)) %>% 
  group_by(species_code, ess, species_name) %>%
  dplyr::distinct(avg_iss) -> goa_avg_iss_ag

vroom::vroom_write(goa_iss_ag, file = here::here("output", region, paste0("iss_ag.csv")), delim = ",")
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
  dplyr::distinct(iss) -> goa_reg_iss_sz

goa_reg_iss_sz %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess, region)) %>% 
  group_by(species_code, ess, species_name, region) %>%
  dplyr::distinct(avg_iss) -> goa_reg_avg_iss_sz

vroom::vroom_write(goa_reg_iss_sz, file = here::here("output", region, paste0("iss_sz_reg.csv")), delim = ",")
vroom::vroom_write(goa_reg_avg_iss_sz, file = here::here("output", region, paste0("avg_iss_sz_reg.csv")), delim = ",")

goa_age_rs %>% 
  bind_rows.(goa_age_rex) %>% 
  left_join.(spec) %>% 
  mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
          .by = c(year, species_code, ess)) %>% 
  group_by(year, species_code, ess, species_name, region) %>%
  dplyr::distinct(iss) -> goa_reg_iss_ag

goa_reg_iss_ag %>% 
  mutate.(avg_iss = mean(iss), 
          .by = c(species_code, ess, region)) %>% 
  group_by(species_code, ess, species_name, region) %>%
  dplyr::distinct(avg_iss) -> goa_reg_avg_iss_ag

vroom::vroom_write(goa_reg_iss_ag, file = here::here("output", region, paste0("iss_ag_reg.csv")), delim = ",")
vroom::vroom_write(goa_reg_avg_iss_ag, file = here::here("output", region, paste0("avg_iss_ag_reg.csv")), delim = ",")

# Compile some results

ai_avg_iss_sz %>% 
  mutate.(surv = "AI") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") -> .ai_avg_iss_sz

ebs_shelf_avg_iss_sz %>% 
  mutate.(surv = "EBS_SHELF") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") -> .ebs_slope_avg_iss_sz

ebs_slope_avg_iss_sz %>% 
  mutate.(surv = "EBS_SLOPE") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") -> .ebs_shelf_avg_iss_sz

goa_avg_iss_sz %>% 
  mutate.(surv = "GOA") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") %>% 
  bind_rows.(.ai_avg_iss_sz, .ebs_shelf_avg_iss_sz, .ebs_slope_avg_iss_sz) -> avg_iss_sz

ai_avg_iss_ag %>% 
  mutate.(surv = "AI") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") -> .ai_avg_iss_ag

ebs_shelf_avg_iss_ag %>% 
  mutate.(surv = "EBS_SHELF") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") -> .ebs_shelf_avg_iss_ag

ebs_slope_avg_iss_ag %>% 
  mutate.(surv = "EBS_SLOPE") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") -> .ebs_slope_avg_iss_ag

goa_avg_iss_ag %>% 
  mutate.(surv = "GOA") %>% 
  pivot_wider.(names_from = ess, values_from = avg_iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "avg_iss") %>%  
  bind_rows.(.ai_avg_iss_ag, .ebs_shelf_avg_iss_ag, .ebs_slope_avg_iss_ag) -> avg_iss_ag

ai_iss_sz %>% 
  mutate.(surv = "AI") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") -> .ai_iss_sz

ebs_shelf_iss_sz %>% 
  mutate.(surv = "EBS_SHELF") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") -> .ebs_shelf_iss_sz

ebs_slope_iss_sz %>% 
  mutate.(surv = "EBS_SLOPE") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") -> .ebs_slope_iss_sz

goa_iss_sz %>% 
  mutate.(surv = "GOA") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") %>% 
  bind_rows.(.ai_iss_sz, .ebs_shelf_iss_sz, .ebs_slope_iss_sz) -> iss_sz

ai_iss_ag %>% 
  mutate.(surv = "AI") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") -> .ai_iss_ag

ebs_shelf_iss_ag %>% 
  mutate.(surv = "EBS_SHELF") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") -> .ebs_shelf_iss_ag

ebs_slope_iss_ag %>% 
  mutate.(surv = "EBS_SHELF") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") -> .ebs_slope_iss_ag

goa_iss_ag %>% 
  mutate.(surv = "GOA") %>% 
  pivot_wider.(names_from = ess, values_from = iss) %>% 
  rename.(female = ess_f, male = ess_m, total = ess_t) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "iss") %>% 
  bind_rows.(.ai_iss_ag, .ebs_shelf_iss_ag, .ebs_slope_iss_ag) -> iss_ag

vroom::vroom_write(avg_iss_sz, file = here::here("output", paste0("avg_iss_sz.csv")), delim = ",")
vroom::vroom_write(avg_iss_ag, file = here::here("output", paste0("avg_iss_ag.csv")), delim = ",")
vroom::vroom_write(iss_sz, file = here::here("output", paste0("iss_sz.csv")), delim = ",")
vroom::vroom_write(iss_ag, file = here::here("output", paste0("iss_ag.csv")), delim = ",")







