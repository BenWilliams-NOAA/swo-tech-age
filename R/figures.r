
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
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
loadfonts(device = "win")

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

surv_labs <- c("Aleutian Isalnds", "Bering Sea Shelf", "Gulf of Alaska", "Bering Sea Slope", "Central GOA", "Eastern GOA", "Western-Central GOA", "Western GOA")
names(surv_labs) <- c("ai", "bs_shelf", "goa", "bs_slope", "cgoa", "egoa", "wcgoa", "wgoa")

# plot example of annual iss for walleye pollock across regions ----


iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 21740 & comp_type == 'total') %>% 
  ggplot(., aes(x = year, y = iss_age)) +
  geom_bar(stat = "identity", color = "black", fill = scico(3, palette = 'roma')[3]) +
  facet_grid(region ~ .,
             labeller = labeller(region = surv_labs)) +
  xlab("Year") +
  ylab("Age composition input sample size") +
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        text = element_text(size = 14)) -> p1

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 21740 & comp_type == 'total') %>% 
  ggplot(., aes(x = species_name, y = iss_age)) +
  geom_boxplot(fill = scico(3, palette = 'roma')[3]) +
  facet_grid(region ~ .,
             labeller = labeller(region = surv_labs)) +
  xlab("") +
  ylab("none") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 14)) -> p2

poll <- ggarrange(p1 + 
                    theme(plot.margin = margin(r = 1)),
                  p2 + 
                    theme(plot.margin = margin(l = 1),
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank()),
                  nrow = 1,
                  widths = c(4,1))

ggsave(here::here("figs", "pollock_examp.png"),
       poll,
       device = "png",
       width = 6,
       height = 5)

# plot example of annual iss for yellowfin sole across sexes ----

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 10210) %>% 
  ggplot(., aes(x = year, y = iss_age, fill = comp_type)) +
  geom_bar(stat = "identity", color = "black") +
  facet_grid(comp_type ~ .) +
  xlab("Year") +
  ylab("Age composition input sample size") +
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        text = element_text(size = 14)) +
  scale_fill_scico_d(palette = 'roma') -> p1

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(species_code == 10210) %>% 
  ggplot(., aes(x = species_name, y = iss_age, fill = comp_type)) +
  geom_boxplot() +
  facet_grid(comp_type ~ .) +
  xlab("") +
  ylab("none") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 14)) +
  scale_fill_scico_d(palette = 'roma') -> p2

yell <- ggarrange(p1 + 
            theme(plot.margin = margin(r = 1)),
          p2 + 
            theme(plot.margin = margin(l = 1),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()),
          nrow = 1,
          widths = c(4,1))

ggsave(here::here("figs", "yellowfin_examp.png"),
       yell,
       device = "png",
       width = 6,
       height = 5)

# plot iss for all species ----

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)) %>%
  ggplot(., aes(x = species_name, y = iss_age, fill = comp_type)) +
  geom_boxplot() +
  facet_wrap(vars(region), 
             scales = "free",
             labeller = labeller(region = surv_labs)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.y = element_text(vjust = 4),
        text = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(t = 0, r = 50, b = 0, l = 15, unit = "pt")) +
  xlab("Stock") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> all_age

ggsave(here::here("figs", "age_iss.png"),
       all_age,
       device = "png",
       width = 7,
       height = 6)

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)) %>%
  ggplot(., aes(x = species_name, y = iss_length, fill = comp_type)) +
  geom_boxplot() +
  facet_wrap(vars(region), 
             scales = "free",
             labeller = labeller(region = surv_labs)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        text = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(t = 0, r = 50, b = 0, l = 15, unit = "pt")) +
  xlab("Stock") +
  ylab("Length composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> all_length

ggsave(here::here("figs", "length_iss.png"),
       all_length,
       device = "png",
       width = 7,
       height = 6)

# plot iss for sub-region special cases in goa ----

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(!(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa')) 
                    & !is.na(species_name)) %>%
  ggplot(., aes(x = species_name, y = iss_length, fill = comp_type)) +
  geom_boxplot() +
  facet_wrap(vars(region), 
             scales = "free",
             labeller = labeller(region = surv_labs)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        text = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(t = 0, r = 50, b = 0, l = 15, unit = "pt")) +
  xlab("Stock") +
  ylab("Length composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> sub_length

ggsave(here::here("figs", "length_iss_subreg.png"),
       sub_length,
       device = "png",
       width = 7,
       height = 6)

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(!(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa')) 
                    & !is.na(species_name)) %>%
  ggplot(., aes(x = species_name, y = iss_age, fill = comp_type)) +
  geom_boxplot() +
  facet_wrap(vars(region), 
             scales = "free",
             labeller = labeller(region = surv_labs)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        text = element_text(size = 14),
        legend.position = "bottom",
        plot.margin = margin(t = 0, r = 50, b = 0, l = 15, unit = "pt")) +
  xlab("Stock") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> sub_age

ggsave(here::here("figs", "age_iss_subreg.png"),
       sub_age,
       device = "png",
       width = 7,
       height = 6)

# plot age iss relationships with sample size ----

vroom::vroom(here::here('data', 'ann_specimen.csv')) %>% 
  tidytable::mutate.(region = case_when.(surv == 'AI' ~ 'ai',
                                         surv == 'EBS_SHELF' ~ 'bs_shelf',
                                         surv == 'EBS_SLOPE' ~ 'bs_slope',
                                         surv == 'GOA' ~ 'goa')) %>% 
  tidytable::rename.(comp_type = 'type',
                     hls_age = 'hls',
                     nss_age = 'nss') %>% 
  tidytable::select.(year, species_code, comp_type, region, hls_age, nss_age) -> ann_spec

# iss vs nss
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)
                    & species_type != "other") %>% 
ggplot(.,aes(x = nss_age, y = iss_age, pch = as.factor(species_name), color = as.factor(comp_type))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  facet_grid(region ~ species_type,
             labeller = labeller(region = surv_labs)) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  xlab("Number of age samples") +
  ylab("Age composition input sample size") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = -45, hjust = 0)) -> iss_nss

ggsave(here::here("figs", "age_iss_nss.png"),
       iss_nss,
       device = "png",
       width = 7,
       height = 6)

# plot age iss relationships with hauls ----


# iss vs hauls
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)
                    & species_type != "other") %>% 
  ggplot(.,aes(x = hls_age, y = iss_age, pch = as.factor(species_name), color = as.factor(comp_type))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  facet_grid(region ~ species_type,
             labeller = labeller(region = surv_labs)) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  xlab("Number of hauls sampled for age") +
  ylab("Age composition input sample size") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = -45, hjust = 0)) -> iss_hls

ggsave(here::here("figs", "age_iss_hls.png"),
       iss_hls,
       device = "png",
       width = 7,
       height = 6)

# iss/hls vs nss/hls (not used in ms) ----
iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') 
                    & !is.na(species_name)
                    & species_type != "other") %>% 
  ggplot(.,aes(x = nss_age / hls_age, y = iss_age / hls_age, pch = as.factor(species_name), color = as.factor(comp_type))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  facet_grid(region ~ species_type,
             labeller = labeller(region = surv_labs)) +
  geom_abline(slope = 1, intercept = 0, colour = "black", xintercept = 1) +
  geom_abline(slope = 0, intercept = 1, colour = "black") +
  xlab("Number of age samples per sampled haul") +
  ylab("Age composition input sample size per sampled haul") +
  labs(pch = "Stock") +
  # geom_smooth(method = 'lm', se = T) +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  theme(text = element_text(size = 14)) -> iss_nss_hls

ggsave(here::here("figs", "age_iss_nss_hl.png"),
       iss_nss_hls,
       device = "png",
       width = 7,
       height = 6)

# compute iss stats (not used in ms) ----

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::select(-iss_length) %>% 
  tidytable::mutate(iss_per = iss_age / nss_age, .by = c(year, species_code, comp_type, region)) %>% 
  tidytable::summarise(iss_per = mean(iss_per, na.rm = TRUE), .by = c(species_type))

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::select(-iss_length) %>% 
  tidytable::mutate(iss_per = iss_age / hls_age, .by = c(year, species_code, comp_type, region)) %>% 
  tidytable::summarise(iss_per = mean(iss_per, na.rm = TRUE), .by = c(species_type))

# produce r-squared tables together ----

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') & !is.na(species_name)) %>% 
  tidytable::drop_na() %>% 
  tidytable::summarise(rsq = summary(lm(iss_age ~ nss_age))$r.squared, .by = c(species_type, comp_type, region)) %>% 
  tidytable::pivot_wider(names_from = comp_type, values_from = rsq) %>% 
  tidytable::rename(species = species_type) %>% 
  tidytable::bind_rows(data_fit %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarise(rsq = summary(lm(iss_age ~ nss_age))$r.squared, .by = c(species_name, comp_type, region)) %>% 
                         tidytable::pivot_wider(names_from = comp_type, values_from = rsq) %>% 
                         tidytable::rename(species = species_name)) -> iss_nss_rsq

iss_nss_rsq %>% 
  tidytable::filter(region %in% c('goa', 'ai')) %>% 
  vroom::vroom_write(here::here("tables", "iss_nss_rsq_goai.csv"), delim = ",")

iss_nss_rsq %>% 
  tidytable::filter(!(region %in% c('goa', 'ai'))) %>% 
  vroom::vroom_write(here::here("tables", "iss_nss_rsq_bs.csv"), delim = ",")

iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::left_join(ann_spec) %>% 
  tidytable::filter(region %in% c('bs_shelf', 'bs_slope', 'ai', 'goa') & !is.na(species_name)) %>% 
  tidytable::drop_na() %>% 
  tidytable::summarise(rsq = summary(lm(iss_age ~ hls_age))$r.squared, .by = c(species_type, comp_type, region)) %>% 
  tidytable::pivot_wider(names_from = comp_type, values_from = rsq) %>% 
  tidytable::rename(species = species_type) %>% 
  tidytable::bind_rows(data_fit %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarise(rsq = summary(lm(iss_age ~ hls_age))$r.squared, .by = c(species_name, comp_type, region)) %>% 
                         tidytable::pivot_wider(names_from = comp_type, values_from = rsq) %>% 
                         tidytable::rename(species = species_name)) -> iss_hls_rsq

iss_hls_rsq %>% 
  tidytable::filter(region %in% c('goa', 'ai')) %>% 
  vroom::vroom_write(here::here("tables", "iss_hls_rsq_goai.csv"), delim = ",")

iss_hls_rsq %>% 
  tidytable::filter(!(region %in% c('goa', 'ai'))) %>% 
  vroom::vroom_write(here::here("tables", "iss_hls_rsq_bs.csv"), delim = ",")






