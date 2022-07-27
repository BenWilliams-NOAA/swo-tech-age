
# packages
library(tidyverse)
library(tidytable)
library(vroom)
library(here)
library(purrr)
library(rsample)
library(data.table)
library(scico)
library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
loadfonts(device="win")

# add fonts to all text (last line)
ggplot2::theme_set(
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA),
      text = element_text(family = "Times New Roman")
    )
)
spec<-vroom::vroom(here::here('data', 'species_code_name.csv')) #species_code and common names

nss_len<-vroom::vroom(here::here('data', 'ann_len.csv'))
nss_age<-vroom::vroom(here::here('data', 'ann_specimen.csv'))
avg_nss_len<-vroom::vroom(here::here('data', 'avg_ann_len.csv'))
avg_nss_age<-vroom::vroom(here::here('data', 'avg_ann_specimen.csv'))
iss_len<-vroom::vroom(here::here('output', 'iss_sz.csv'))
iss_age<-vroom::vroom(here::here('output', 'iss_ag.csv'))
avg_iss_len<-vroom::vroom(here::here('output', 'avg_iss_sz.csv'))
avg_iss_age<-vroom::vroom(here::here('output', 'avg_iss_ag.csv'))



#plot the number of sampled lengths v iss by species and survey

nss_len %>% 
  left_join.(iss_len) %>% 
  filter(species_code %in% c(10110, 10112, 10115, 10130, 10180, 10210, 10261, 10285, 10262, 10200)) %>% 
  mutate.(group = "flatfish") -> flatfish

nss_len %>% 
  left_join.(iss_len) %>% 
  filter(species_code %in% c(20510, 21720, 21740, 21921)) %>% 
  mutate.(group = "roundfish") -> roundfish
  
nss_len %>% 
  left_join.(iss_len) %>% 
  filter(species_code %in% c(30060, 30420, 30050, 30150)) %>% 
  mutate.(group = "rockfish") %>% 
  rbind(flatfish, roundfish) %>% 
  mutate.(avg_nss = nss/hls, avg_iss = iss/hls) -> plot_dat

ggplot(plot_dat,aes(x=avg_nss,y=avg_iss,color=as.factor(type)))+
  geom_point(pch=21)+
  facet_grid(surv~group,scales="free")+
  geom_abline(slope = 1, intercept = 0, colour = "red",linetype="dashed")+
  xlab("Fish per sampled haul")+ylab("Input sample size per sampled haul")+
  scale_color_discrete(name="Sex")


#plot the number of sampled ages v iss by species and survey

nss_age %>% 
  left_join.(iss_age) %>% 
  filter(species_code %in% c(10110, 10112, 10115, 10130, 10180, 10210, 10261, 10285, 10262, 10200)) %>% 
  mutate.(group = "flatfish") -> flatfish

nss_age %>% 
  left_join.(iss_age) %>% 
  filter(species_code %in% c(20510, 21720, 21740, 21921)) %>% 
  mutate.(group = "roundfish") -> roundfish

nss_age %>% 
  left_join.(iss_age) %>% 
  filter(species_code %in% c(30060, 30420, 30050, 30150)) %>% 
  mutate.(group = "rockfish") %>% 
  rbind(flatfish, roundfish) %>% 
  mutate.(avg_nss = nss/hls, avg_iss = iss/hls) -> plot_dat

ggplot(plot_dat,aes(x=avg_nss,y=avg_iss,color=as.factor(type)))+
  geom_point(pch=21)+
  facet_grid(surv~group,scales="free")+
  geom_abline(slope = 1, intercept = 0, colour = "red",linetype="dashed")+
  xlab("Fish per sampled haul")+ylab("Input sample size per sampled haul")+
  scale_color_discrete(name="Sex")




