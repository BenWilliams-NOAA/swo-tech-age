
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
library(cowplot)
library(egg)
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

spec <- vroom::vroom(here::here('data', 'species_code_name.csv')) #species_code and common names

iss <- vroom::vroom(here::here('output', 'afsc_iss.csv')) #species_code and common names


## plot example of annual iss for walleye pollock across regions

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 21740 & comp_type == 'total') %>% 
  ggplot(., aes(x = year, y = iss_age)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  facet_grid(region ~ .) +
  xlab("Year") +
  ylab("Age composition input sample size") +
  theme(legend.position = "none",
        strip.text.y = element_blank()) -> p1

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 21740 & comp_type == 'total') %>% 
  ggplot(., aes(x = species_name, y = iss_age)) +
  geom_boxplot(fill = "grey") +
  facet_grid(region ~ .) +
  xlab("") +
  ylab("none") +
  theme(legend.position = "none",
        axis.title.y = element_blank()) -> p2

ggarrange(p1 + 
            theme(plot.margin = margin(r = 1)),
          p2 + 
            theme(plot.margin = margin(l = 1),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()),
          nrow = 1,
          widths = c(4,1))


## plot example of annual iss for yellowfin sole across sexes

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 10210) %>% 
  ggplot(., aes(x = year, y = iss_age)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  facet_grid(comp_type ~ .) +
  xlab("Year") +
  ylab("Age composition input sample size") +
  theme(legend.position = "none",
        strip.text.y = element_blank()) -> p1

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 10210) %>% 
  ggplot(., aes(x = species_name, y = iss_age)) +
  geom_boxplot(fill = "grey") +
  facet_grid(comp_type ~ .) +
  xlab("") +
  ylab("none") +
  theme(legend.position = "none",
        axis.title.y = element_blank()) -> p2

ggarrange(p1 + 
            theme(plot.margin = margin(r = 1)),
          p2 + 
            theme(plot.margin = margin(l = 1),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()),
          nrow = 1,
          widths = c(4,1))

## plot for all species
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)) %>%
  ggplot(., aes(x = species_name, y = iss_age, fill = comp_type)) +
  geom_boxplot() +
  facet_wrap(vars(region), scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(vjust = 4)) +
  xlab("Stock") +
  ylab("Age composition input sample size")


iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)) %>%
  ggplot(., aes(x = species_name, y = iss_length, fill = comp_type)) +
  geom_boxplot() +
  facet_wrap(vars(region), scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Stock") +
  ylab("Length composition input sample size")


# plot relationships with hauls


vroom::vroom(here::here('data', 'ann_specimen.csv')) %>% 
  tidytable::mutate.(region = case_when.(surv == 'AI' ~ 'ai',
                                         surv == 'EBS_SHELF' ~ 'bs_shelf',
                                         surv == 'EBS_SLOPE' ~ 'bs_slope',
                                         surv == 'GOA' ~ 'goa')) %>% 
  tidytable::rename.(comp_type = 'type',
                     hls_age = 'hls',
                     nss_age = 'nss') %>% 
  tidytable::select.(year, species_code, comp_type, region, hls_age, nss_age) -> ann_spec


vroom::vroom(here::here('data', 'ann_len.csv')) %>% 
  tidytable::mutate.(region = case_when.(surv == 'AI' ~ 'ai',
                                         surv == 'EBS_SHELF' ~ 'bs_shelf',
                                         surv == 'EBS_SLOPE' ~ 'bs_slope',
                                         surv == 'GOA' ~ 'goa')) %>% 
  tidytable::rename.(comp_type = 'type',
                     hls_length = 'hls',
                     nss_length = 'nss') %>% 
  tidytable::select.(year, species_code, comp_type, region, hls_length, nss_length) -> ann_len


# iss vs nss
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::left_join(ann_len) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)
                    & species_type != "other") %>% 
ggplot(.,aes(x = nss_age, y = iss_age, color = as.factor(comp_type))) +
  geom_point() +
  facet_grid(region ~ species_type) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  xlab("Number of age samples") +
  ylab("Age composition input sample size") +
  scale_color_discrete(name = "Composition type") +
  geom_smooth(method = 'lm', se = T)

# iss vs hauls
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::left_join(ann_len) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)
                    & species_type != "other") %>% 
  ggplot(.,aes(x = hls_age, y = iss_age, color = as.factor(comp_type))) +
  geom_point() +
  facet_grid(region ~ species_type) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  xlab("Number of hauls sampled for age") +
  ylab("Age composition input sample size") +
  scale_color_discrete(name = "Composition type") +
  geom_smooth(method = 'lm', se = T)

# iss/hls vs nss/hls
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::left_join(ann_len) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)
                    & species_type != "other") %>% 
  ggplot(.,aes(x = nss_age / hls_age, y = iss_age / hls_age, color = as.factor(comp_type))) +
  geom_point() +
  facet_grid(region ~ species_type) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  xlab("Number of age samples per sampled haul") +
  ylab("Age composition input sample size per sampled haul") +
  scale_color_discrete(name = "Composition type") +
  geom_smooth(method = 'lm', se = T)


