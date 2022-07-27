#Remove oracle_pw ans user name
rm(oracle_pw)
rm(oracle_user)

#ADD oracle username and password
oracle_pw=''
oracle_user=''

# packages
# devtools::install_github("https://github.com/afsc-gap-products/ALKr", force=TRUE)
# devtools::install_github("afsc-gap-products/sumfish", force=TRUE)
library(sumfish)
library(tidyverse)
library(tidytable)
library(vroom)
library(here)
library(purrr)
library(rsample)
library(data.table)
# library(scico)
# library(extrafont)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
# loadfonts(device="win")
# 
# # add fonts to all text (last line)
# ggplot2::theme_set(
#   ggplot2::theme_light() +
#     ggplot2::theme(
#       panel.grid.major = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#       # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
#       strip.background = ggplot2::element_rect(fill = NA, colour = NA),
#       strip.text.x = element_text(colour = "black"),
#       strip.text.y = element_text(colour = "black"),
#       panel.border = element_rect(fill = NA),
#       legend.key.size = grid::unit(0.9, "lines"),
#       legend.key = ggplot2::element_rect(colour = NA, fill = NA),
#       legend.background = ggplot2::element_rect(colour = NA, fill = NA),
#       text = element_text(family = "Times New Roman")
#     )
# )
spec<-vroom::vroom(here::here('data', 'species_code_name.csv')) #species_code and common names

#Species codes and yrs by area
ebs_sp_code=c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
ebs_yr=c(seq(1982, 2019), 2021)

ai_sp_code=c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
ai_yr=c(1980, 1983, 1986, 1991, 1994, 1997, 2000, 2002, 2004, 2006, 2010, 2012, 2014, 2016, 2018)

goa_sp_code=c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
goa_yr=c(1984, 1987, 1990, 1993, 1996, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017,2019,2021)

ebs_slope=c(10115, 10112, 10110,30060)
ebs_slope_yr=c(2002,2004,2008, 2010, 2012, 2016)


#query Racebase - returns list object
#also keep the species of interest for each area
ebs_data=sumfish::getRacebase(year=ebs_yr,surv="EBS_SHELF")

ebs_slope_data=sumfish::getRacebase(year=ebs_slope_yr,surv="EBS_SLOPE")

ai_data=sumfish::getRacebase(year=ai_yr,surv="AI")

goa_data=sumfish::getRacebase(year=goa_yr,surv="GOA")

#Combine objects and filter for area specific species codes
#raw lengths
raw_len=bind_rows(data.frame(ebs_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code),surv="EBS_SHELF"),
                  data.frame(ebs_slope_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% ebs_slope),surv="EBS_SLOPE"),
                  data.frame(ai_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code),surv="AI"),
                  data.frame(goa_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code),surv="GOA"))
# #lengths from length table
# lfreq=bind_rows(data.frame(ebs_data$length %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code),surv="EBS_SHELF"),
#                 data.frame(ebs_slope_data$length %>% rename_all(tolower) %>% filter(species_code %in% ebs_slope),surv="EBS_SLOPE"),
#                 data.frame(ai_data$length %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code),surv="AI"),
#                 data.frame(goa_data$length %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code),surv="GOA"))
# #survey catch
# catch=bind_rows(data.frame(ebs_data$catch %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code),surv="EBS_SHELF"),
#                 data.frame(ebs_slope_data$catch %>% rename_all(tolower) %>% filter(species_code %in% ebs_slope),surv="EBS_SLOPE"),
#                 data.frame(ai_data$catch %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code),surv="AI"),
#                 data.frame(goa_data$catch %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code),surv="GOA"))
# #hauls
# haul=bind_rows(data.frame(ebs_data$haul %>% rename_all(tolower),surv="EBS_SHELF"),
#                data.frame(ebs_slope_data$haul %>% rename_all(tolower),surv="EBS_SLOPE"),
#                 data.frame(ai_data$haul %>% rename_all(tolower),surv="AI"),
#                 data.frame(goa_data$haul %>% rename_all(tolower),surv="GOA"))
#specimen data
specimen=bind_rows(data.frame(ebs_data$specimen %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code), surv="EBS_SHELF"),
               data.frame(ebs_slope_data$specimen %>% rename_all(tolower) %>% filter(species_code %in% ebs_slope), surv="EBS_SLOPE"),
               data.frame(ai_data$specimen %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code), surv="AI"),
               data.frame(goa_data$specimen %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code), surv="GOA"))

#add year column
raw_len2 <- raw_len %>%
  mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
  rename_all(tolower)

# catch2<-catch %>%
#   mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
#   rename_all(tolower)
# 
# lfreq2 <- lfreq %>%
#   mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
#   inner_join(haul, by=c('hauljoin','surv')) %>%
#   rename_all(tolower) %>%
#   dplyr::select(year, species_code, hauljoin, sample_type,sex, surv,length, frequency)

specimen2 <- specimen %>%
  mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
  rename_all(tolower)

#Changing juvenile codes so we can plot raw lengths per haul
raw_len2$species_code[raw_len2$species_code==21741]=21740
raw_len2$species_code[raw_len2$species_code==21742]=21740
raw_len2$species_code[raw_len2$species_code==21721]=21720
raw_len2$species_code[raw_len2$species_code==21722]=21720
raw_len2$species_code[raw_len2$species_code==10209]=10210
raw_len2$species_code[raw_len2$species_code==10116]=10115
raw_len2$species_code[raw_len2$species_code==10263]=10261

#Changing juvenile codes so we can plot raw lengths per haul
specimen2$species_code[specimen2$species_code==21741]=21740
specimen2$species_code[specimen2$species_code==21742]=21740
specimen2$species_code[specimen2$species_code==21721]=21720
specimen2$species_code[specimen2$species_code==21722]=21720
specimen2$species_code[specimen2$species_code==10209]=10210
specimen2$species_code[specimen2$species_code==10116]=10115
specimen2$species_code[specimen2$species_code==10263]=10261

#add species name to raw_len and specimen
raw_len3=full_join(raw_len2,spec,by='species_code')
specimen3=full_join(specimen2,spec,by='species_code')

specimen3=specimen3[specimen3$age>=0,]

# #number caught per haul, species, and survey
# num_ct=catch2 %>% group_by(year,species_code,surv,hauljoin) %>% summarise(num=sum(number_fish))
# #number lengths per haul, species and survey
# len_hl=raw_len3 %>% group_by(year,species_code,species_name,surv,hauljoin) %>% summarise(freq=sum(frequency))
# #number lengths per haul, species, sex, and survey
# len_s=raw_len3 %>% group_by(year,species_code,species_name,surv,sex,hauljoin) %>% summarise(freq=sum(frequency))

#Annual number of lengths by year, haul, species and survey
raw_len3 %>%
  filter(species_name == "REBS rockfish complex") %>% 
  summarise.(freq = sum(frequency), hls = length(unique(hauljoin)),
             .by = c(year, species_name, surv, sex)) %>% 
  mutate.(species_code = 30050,
          .by = c(year, species_name, surv, sex, freq)) -> REBS_len

raw_len3 %>%
  filter(species_name == "Dusky rockfish") %>% 
  summarise.(freq = sum(frequency), hls = length(unique(hauljoin)),
             .by = c(year, species_name, surv, sex)) %>% 
  mutate.(species_code = 30150,
          .by = c(year, species_name, surv, sex, freq)) -> Dusk_len

raw_len3 %>% 
  filter(species_name != "REBS rockfish complex") %>% 
  filter(species_name != "Dusky rockfish") %>% 
  summarise.(freq = sum(frequency), hls = length(unique(hauljoin)),
             .by = c(year, species_code, species_name, surv, sex)) %>% 
  rbind(REBS_len, Dusk_len) %>%
  select.(-hls) %>% 
  pivot_wider.(names_from = sex, values_from = freq, names_prefix = "sex_") %>% 
  mutate.(total = sum(sex_1, sex_2, sex_3, na.rm = TRUE),
          .by = c(year, species_code, species_name, surv)) %>% 
  select.(-sex_3) %>% 
  rename.(male = sex_1, female = sex_2) %>%
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "nss") -> .raw_len3
  
raw_len3 %>% 
  filter(species_name != "REBS rockfish complex") %>% 
  filter(species_name != "Dusky rockfish") %>%
  summarise.(freq = sum(frequency), hls = length(unique(hauljoin)),
             .by = c(year, species_code, species_name, surv, sex)) %>% 
  rbind(REBS_len, Dusk_len) %>%
  select.(-freq) %>% 
  pivot_wider.(names_from = sex, values_from = hls, names_prefix = "sex_") %>% 
  mutate.(total = sum(sex_1, sex_2, sex_3, na.rm = TRUE),
          .by = c(year, species_code, species_name, surv)) %>% 
  select.(-sex_3) %>% 
  rename.(male = sex_1, female = sex_2) %>%
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "hls") %>% 
  left_join.(.raw_len3) -> ann_len
# ann_len_w = spread(ann_len, year, freq)
# write.csv(ann_len_w, file=here::here("data","annLen_byAreaSpecies_rep.csv"), row.names=FALSE)
# write.csv(ann_len, file=here::here("data","annLen_byAreaSpecies.csv"), row.names=FALSE)
# vroom::vroom_write(ann_len_w, file = here::here("data","annLen_byAreaSpecies_rep.csv"), delim = ",")
vroom::vroom_write(ann_len, file = here::here("data","ann_len.csv"), delim = ",")

ann_len %>% 
  mutate.(avg_nss = mean(nss), avg_hls = mean(hls),
          .by = c(species_code, species_name, surv, type)) %>% 
  group_by(species_code, species_name, surv, type) %>%
  dplyr::distinct(avg_nss, avg_hls) -> avg_ann_len
vroom::vroom_write(avg_ann_len, file = here::here("data","avg_ann_len.csv"), delim = ",")


# #3-yr average table
# all_3yrAvgTotLen=ann_len %>% group_by(species_code,species_name,surv) %>% summarise(avg_tot_num=round(mean(freq),digits=0))
# all_3yrAvgTotLen_v2=spread(all_3yrAvgTotLen,surv,avg_tot_num)
# write.csv(all_3yrAvgTotLen_v2,file=here::here("data/avg3yr_byAreaSP_rep.csv"),row.names=FALSE)

#Annual number of otoliths read by species, year, and survey
specimen3 %>%
  filter(species_name == "REBS rockfish complex") %>% 
  summarise.(nages = length(age), hls = length(unique(hauljoin)),
             .by = c(year, species_name, surv, sex)) %>% 
  mutate.(species_code = 30050,
          .by = c(year, species_name, surv, sex, nages)) -> REBS_age

specimen3 %>%
  filter(species_name == "Dusky rockfish") %>% 
  summarise.(nages = length(age), hls = length(unique(hauljoin)),
             .by = c(year, species_name, surv, sex)) %>% 
  mutate.(species_code = 30150,
          .by = c(year, species_name, surv, sex, nages)) -> Dusk_age

specimen3 %>% 
  filter(species_name != "REBS rockfish complex") %>%   
  filter(species_name != "Dusky rockfish") %>%   
  summarise.(nages = length(age), hls = length(unique(hauljoin)),
             .by = c(year, species_code, species_name, surv, sex)) %>% 
  rbind(REBS_age, Dusk_age) %>% 
  select.(-hls) %>% 
  pivot_wider.(names_from = sex, values_from = nages, names_prefix = "sex_") %>% 
  mutate.(total = sum(sex_1, sex_2, sex_3, na.rm = TRUE),
          .by = c(year, species_code, species_name, surv)) %>% 
  select.(-sex_3) %>% 
  rename.(male = sex_1, female = sex_2) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "nss") -> .specimen3

specimen3 %>% 
  filter(species_name != "REBS rockfish complex") %>% 
  filter(species_name != "Dusky rockfish") %>%
  summarise.(nages = length(age), hls = length(unique(hauljoin)),
             .by = c(year, species_code, species_name, surv, sex)) %>% 
  rbind(REBS_age, Dusk_age) %>% 
  select.(-nages) %>% 
  pivot_wider.(names_from = sex, values_from = hls, names_prefix = "sex_") %>% 
  mutate.(total = sum(sex_1, sex_2, sex_3, na.rm = TRUE),
          .by = c(year, species_code, species_name, surv)) %>% 
  select.(-sex_3) %>% 
  rename.(male = sex_1, female = sex_2) %>% 
  pivot_longer.(cols = c(female, male, total),names_to = "type", values_to = "hls") %>% 
  left_join.(.specimen3) -> ann_specimen
# write.csv(ann_specimen,file=here::here("data/ann_specimen.csv"),row.names=FALSE)
vroom::vroom_write(ann_specimen, file = here::here("data","ann_specimen.csv"), delim = ",")

ann_specimen %>% 
  mutate.(avg_nss = mean(nss), avg_hls = mean(hls),
          .by = c(species_code, species_name, surv, type)) %>% 
  group_by(species_code, species_name, surv, type) %>%
  dplyr::distinct(avg_nss, avg_hls) -> avg_ann_specimen
vroom::vroom_write(avg_ann_specimen, file = here::here("data","avg_ann_specimen.csv"), delim = ",")

# compute mean age per haul for pollock vs arrowtooth example

specimen3 %>%
  filter(species_name %in% c("walleye pollock", "arrowtooth flounder")) %>% 
  summarise.(avg_age = mean(age), nages = length(age),
             .by = c(year, species_code, species_name, surv, hauljoin)) %>% 
  mutate.(sex = "t",
          .by = c(year, species_code, species_name, surv, hauljoin, avg_age, nages)) -> tot_age


specimen3 %>%
  filter(species_name %in% c("walleye pollock", "arrowtooth flounder")) %>% 
  summarise.(avg_age = mean(age), nages = length(age),
             .by = c(year, species_code, species_name, surv, sex, hauljoin)) %>% 
  filter(sex != 3) %>% 
  rbind(tot_age) -> age_haul_examp
vroom::vroom_write(age_haul_examp, file = here::here("data","age_haul_examp.csv"), delim = ",")

# #Combine ann_len and all_3yrAvgTotLen
#   ebs_summ=inner_join(spread(ann_len[ann_len$surv=="EBS_SHELF",],year,freq),all_3yrAvgTotLen_v2[,c("species_name","EBS_SHELF")],by="species_name")
#     ebs_summ=select(ebs_summ,-surv)
#     ebs_summ=rename(ebs_summ,Species=species_name, average=EBS_SHELF)
#     write.csv(ebs_summ,file=here::here("data/ebs_samples.csv"),row.names=FALSE)
# 
#   ai_summ=inner_join(spread(ann_len[ann_len$surv=="AI",],year,freq),all_3yrAvgTotLen_v2[,c("species_name","AI")],by="species_name")
#     ai_summ=select(ai_summ,-surv)
#     ai_summ=rename(ai_summ,Species=species_name, average=AI)
#     write.csv(ai_summ,file=here::here("data/ai_samples.csv"),row.names=FALSE)
# 
#   goa_summ=inner_join(spread(ann_len[ann_len$surv=="GOA",],year,freq),all_3yrAvgTotLen_v2[,c("species_name","GOA")],by="species_name")
#     goa_summ=select(goa_summ,-surv)
#     goa_summ=rename(goa_summ,Species=species_name, average=GOA)
#     write.csv(goa_summ,file=here::here("data/goa_samples.csv"),row.names=FALSE)

# #plot the number of sampled lengths per haul v number caught per haul by species and survey
# lfreq_num_plot=function(dat,surv,sex=FALSE,pname="foo")
# {
# 
#   if(sex==TRUE)
#   {
#     ls_plot=ggplot(dat,aes(x=num,y=freq,color=as.factor(sex)))+geom_point(pch=21)+
#             facet_wrap(~species_name,scales="free")+
#             geom_hline(yintercept = 200, colour = "red",linetype="dashed")+
#             xlab("Catch (number) per haul")+ylab("Number of sampled lengths per haul")+
#             scale_color_discrete(name="Sex")
# 
#     png(paste0(surv,"_samples_per_haul_bySex.png"),units="in",width=6.5,height=6,res=300)
#     print(ls_plot)
#     dev.off()
#   }
#   if(sex==FALSE)
#   {
#     ls_plot=ggplot(dat,aes(x=num,y=freq))+geom_point(pch=21)+
#             facet_wrap(~species_name,scales="free")+
#             geom_hline(yintercept = 200, colour = "red",linetype="dashed")+
#             xlab("Catch (number) per haul")+ylab("Number of sampled lengths per haul")
# 
#     png(paste0(surv,"_samples_per_haul_",pname,".png"),units="in",width=6.5,height=6,res=300)
#     print(ls_plot)
#     dev.off()
#   }
# }
# 
# #Plot number of lengths per haul vs. catch numbers per haul
# obs_hl=inner_join(num_ct,len_hl,by=c("year","species_code","surv","hauljoin"))
# obs_hl$prop=obs_hl$freq/obs_hl$num
# 
# ebs=obs_hl[obs_hl$surv=="EBS_SHELF" & obs_hl$species_code %in% ebs_sp_code,]
# lfreq_num_plot(ebs,tolower(unique(ebs$surv)),sex=FALSE,pname="total")
# 
# ai=obs_hl[obs_hl$surv=="AI" & obs_hl$species_code %in% ai_sp_code,]
# lfreq_num_plot(ai,tolower(unique(ai$surv)),sex=FALSE,pname="total")
# 
# goa=obs_hl[obs_hl$surv=="GOA" & obs_hl$species_code %in% goa_sp_code,]
# lfreq_num_plot(goa,tolower(unique(goa$surv)),sex=FALSE,pname="total")
# 
# #Plot number of female+male lengths per haul by the total number caught
# len_hl_mf<-len_s %>% filter(sex!=3) %>%
#   group_by(year,species_code,species_name,surv,hauljoin) %>%
#   summarise(freq=sum(freq))
# 
# obs_hl_mf=inner_join(num_ct,len_hl_mf,by=c("year","species_code","surv","hauljoin"))
# 
# ebs_mf=obs_hl_mf[obs_hl_mf$surv=="EBS_SHELF",]
# lfreq_num_plot(ebs_mf,tolower(unique(ebs_mf$surv)),sex=FALSE,pname="MF")
# 
# ai_mf=obs_hl_mf[obs_hl_mf$surv=="AI",]
# lfreq_num_plot(ai_mf,tolower(unique(ai_mf$surv)),sex=FALSE,pname="MF")
# 
# goa_mf=obs_hl_mf[obs_hl_mf$surv=="GOA",]
# lfreq_num_plot(goa_mf,tolower(unique(goa_mf$surv)),sex=FALSE,pname="MF")

#Remove oracle_pw ans user name before saving workspace
rm(oracle_pw)
rm(oracle_user)

