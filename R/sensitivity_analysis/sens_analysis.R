library(tidyverse)
library(haven)
library(gridExtra)
library(wesanderson)
library(ggnewscale)
library(patchwork)
library(grid)

#### 70, 94 ####

demo <- readRDS("data/processed/demo.rds")
mims <- readRDS("data/processed/mims.rds")
## Use 90th percentile of all MIMS as MVPA cutoff
## Use 70th percentile of all MIMS as LiPA cutoff

lipa_cp <- unname(quantile(mims$MIMS, 0.70)) #14.659
mvpa_cp <- unname(quantile(mims$MIMS, 0.94)) #34.248

mims <- mims %>% mutate(PA  = case_when(Flag == "Wake" & MIMS <= lipa_cp ~ "SeB", 
                                        Flag == "Wake" & between(MIMS, lipa_cp, mvpa_cp) ~ "LiPA", 
                                        Flag == "Wake" & MIMS > mvpa_cp ~ "MVPA", 
                                        .default = Flag))


day_compositions = mims %>%
  group_by(SEQN, DayofWeek) %>%
  mutate(mims = sum(MIMS),
         mvpa = sum(PA == "MVPA"),
         lipa = sum(PA == "LiPA"),
         seb = sum(PA == "SeB"),
         wake_norm = Wake / (Wake + Sleep) * 1440,
         sleep_norm = Sleep / (Wake + Sleep) * 1440,
         rel_mvpa = mvpa / wake_norm,
         rel_lipa = lipa / wake_norm,
         rel_seb = seb / wake_norm,
         rel_mims = mims / wake_norm,
         mvpa_lipa = mvpa / lipa,
         mvpa_seb = mvpa / seb,
         lipa_seb = lipa / seb,
         activity_seb = (mvpa + lipa) / seb) %>%
  select(SEQN, Age, Sex, DayofWeek, Wake, Sleep, wake_norm, sleep_norm, mvpa, lipa, seb, rel_mvpa, rel_lipa, rel_seb,
         mvpa_lipa, mvpa_seb, lipa_seb, activity_seb, mims, rel_mims) %>%
  distinct() %>%
  ungroup()


subject_compositions = day_compositions %>%
  group_by(SEQN) %>%
  mutate(avg_mims = mean(mims),
         avg_sleep = mean(Sleep),
         avg_wake = mean(Wake),
         avg_wake_norm = mean(wake_norm),
         avg_sleep_norm = mean(sleep_norm),
         avg_mvpa = mean(mvpa),
         avg_lipa = mean(lipa),
         avg_seb = mean(seb),
         avg_rel_mvpa = mean(rel_mvpa),
         avg_rel_lipa = mean(rel_lipa),
         avg_rel_seb = mean(rel_seb),
         avg_rel_mims = mean(rel_mims),
         avg_mvpa_lipa = mean(mvpa_lipa),
         avg_mvpa_seb = mean(mvpa_seb),
         avg_lipa_seb = mean(lipa_seb),
         avg_activity_seb = mean(activity_seb)) %>%
  select(SEQN, Age, Sex, avg_sleep, avg_sleep_norm, avg_wake, avg_wake_norm, avg_mvpa, avg_lipa, avg_seb, avg_rel_mvpa, avg_rel_lipa, 
         avg_rel_seb, avg_mvpa_lipa, avg_mvpa_seb, avg_lipa_seb, avg_activity_seb, avg_mims, avg_rel_mims) %>%
  distinct() %>%
  ungroup()


# Adding weights
source("R/functions/get_new_weights.R")
new_weights = get_new_weights(demo)

compositions_weights <- merge(subject_compositions, new_weights, by = "SEQN")

compositions_male <- compositions_weights %>%
  filter(Sex == 1) 

compositions_female <- compositions_weights %>%
  filter(Sex == 2)

source("R/functions/quantile_plot_functions.R")

cent = 50

preds_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa", "new_weights", cent, df = 5)

preds_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_lipa", "new_weights", cent, df = 5)

preds_seb = get_quantile_curves(compositions_male, compositions_female, "avg_seb", "new_weights", cent, df = 5)

preds_rel_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_mvpa", "new_weights", cent, df = 5)

preds_rel_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_lipa", "new_weights", cent, df = 5)

preds_rel_seb = get_quantile_curves(compositions_male, compositions_female, "avg_rel_seb", "new_weights", cent, df = 5)

preds_mims = get_quantile_curves(compositions_male, compositions_female, "avg_mims", "new_weights", cent, df = 5)

preds_rel_mims = get_quantile_curves(compositions_male, compositions_female, "avg_rel_mims", "new_weights", cent, df = 5)

##ggplots

## Plots of medians by Sex

palette_med_plots = c("Female" = "#F98400", "Male" = "#00A08A")

#Figure 2

med_plot_seb_small = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_lipa_small = preds_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA (min)", title = "Absolute LiPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median LiPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_mvpa_small = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA (min)", title = "Absolute MVPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median MVPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))



med_plot_rel_seb_small = preds_rel_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB/Wake", title = "Relative SeB") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_lipa_small = preds_rel_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA/Wake", title = "Relative LiPA") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_mvpa_small = preds_rel_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA/Wake", title = "Relative MVPA") +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))


fig_2_matrix = grid.arrange(med_plot_seb_small, med_plot_lipa_small, med_plot_mvpa_small, med_plot_rel_seb_small, med_plot_rel_lipa_small, med_plot_rel_mvpa_small, nrow = 2)

ggsave("figures/sensitivity_analysis/quantile_plots/figure_2_70_94.png", fig_2_matrix, width = 9, height = 6)
# ggsave("figures/sensitivity_analysis/quantile_plots/figure_2.pdf", fig_2_matrix, width = 9, height = 6, dpi = 1000)
# ggsave("figures/sensitivity_analysis/quantile_plots/figure_2.eps", plot = fig_2_matrix, device = "eps", height = 6, width = 9)



#MIMS plots

med_plot_mims = preds_mims %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MIMS", title = "MIMS") +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))


med_plot_rel_mims = preds_rel_mims %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MIMS / Wake", title = "MIMS / Wake") +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))

mims_plots = gridExtra::grid.arrange(med_plot_mims, med_plot_rel_mims)

ggsave("figures/sensitivity_analysis/quantile_plots/mims_plots.png", mims_plots, height = 10)

#### 70, 96 ####

demo <- readRDS("data/processed/demo.rds")
mims <- readRDS("data/processed/mims.rds")
## Use 96th percentile of all MIMS as MVPA cutoff
## Use 70th percentile of all MIMS as LiPA cutoff

lipa_cp <- unname(quantile(mims$MIMS, 0.70)) #14.659
mvpa_cp <- unname(quantile(mims$MIMS, 0.96)) #38.141

mims <- mims %>% mutate(PA  = case_when(Flag == "Wake" & MIMS <= lipa_cp ~ "SeB", 
                                        Flag == "Wake" & between(MIMS, lipa_cp, mvpa_cp) ~ "LiPA", 
                                        Flag == "Wake" & MIMS > mvpa_cp ~ "MVPA", 
                                        .default = Flag))


day_compositions = mims %>%
  group_by(SEQN, DayofWeek) %>%
  mutate(mvpa = sum(PA == "MVPA"),
         lipa = sum(PA == "LiPA"),
         seb = sum(PA == "SeB"),
         wake_norm = Wake / (Wake + Sleep) * 1440,
         sleep_norm = Sleep / (Wake + Sleep) * 1440,
         rel_mvpa = mvpa / wake_norm,
         rel_lipa = lipa / wake_norm,
         rel_seb = seb / wake_norm,
         mvpa_lipa = mvpa / lipa,
         mvpa_seb = mvpa / seb,
         lipa_seb = lipa / seb,
         activity_seb = (mvpa + lipa) / seb) %>%
  select(SEQN, Age, Sex, DayofWeek, Wake, Sleep, wake_norm, sleep_norm, mvpa, lipa, seb, rel_mvpa, rel_lipa, rel_seb,
         mvpa_lipa, mvpa_seb, lipa_seb, activity_seb) %>%
  distinct() %>%
  ungroup()


subject_compositions = day_compositions %>%
  group_by(SEQN) %>%
  mutate(avg_sleep = mean(Sleep),
         avg_wake = mean(Wake),
         avg_wake_norm = mean(wake_norm),
         avg_sleep_norm = mean(sleep_norm),
         avg_mvpa = mean(mvpa),
         avg_lipa = mean(lipa),
         avg_seb = mean(seb),
         avg_rel_mvpa = mean(rel_mvpa),
         avg_rel_lipa = mean(rel_lipa),
         avg_rel_seb = mean(rel_seb),
         avg_mvpa_lipa = mean(mvpa_lipa),
         avg_mvpa_seb = mean(mvpa_seb),
         avg_lipa_seb = mean(lipa_seb),
         avg_activity_seb = mean(activity_seb)) %>%
  select(SEQN, Age, Sex, avg_sleep, avg_sleep_norm, avg_wake, avg_wake_norm, avg_mvpa, avg_lipa, avg_seb, avg_rel_mvpa, avg_rel_lipa, 
         avg_rel_seb, avg_mvpa_lipa, avg_mvpa_seb, avg_lipa_seb, avg_activity_seb) %>%
  distinct() %>%
  ungroup()


# Adding weights
source("R/functions/get_new_weights.R")
new_weights = get_new_weights(demo)

compositions_weights <- merge(subject_compositions, new_weights, by = "SEQN")

compositions_male <- compositions_weights %>%
  filter(Sex == 1) 

compositions_female <- compositions_weights %>%
  filter(Sex == 2)

source("R/functions/quantile_plot_functions.R")

cent = 50

preds_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa", "new_weights", cent, df = 5)

preds_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_lipa", "new_weights", cent, df = 5)

preds_seb = get_quantile_curves(compositions_male, compositions_female, "avg_seb", "new_weights", cent, df = 5)

preds_rel_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_mvpa", "new_weights", cent, df = 5)

preds_rel_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_lipa", "new_weights", cent, df = 5)

preds_rel_seb = get_quantile_curves(compositions_male, compositions_female, "avg_rel_seb", "new_weights", cent, df = 5)

##ggplots

## Plots of medians by Sex

palette_med_plots = c("Female" = "#F98400", "Male" = "#00A08A")

#Figure 2

med_plot_seb_small = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_lipa_small = preds_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA (min)", title = "Absolute LiPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median LiPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_mvpa_small = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA (min)", title = "Absolute MVPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median MVPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))



med_plot_rel_seb_small = preds_rel_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB/Wake", title = "Relative SeB") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_lipa_small = preds_rel_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA/Wake", title = "Relative LiPA") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_mvpa_small = preds_rel_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA/Wake", title = "Relative MVPA") +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))


fig_2_matrix = grid.arrange(med_plot_seb_small, med_plot_lipa_small, med_plot_mvpa_small, med_plot_rel_seb_small, med_plot_rel_lipa_small, med_plot_rel_mvpa_small, nrow = 2)

ggsave("figures/sensitivity_analysis/quantile_plots/figure_2_70_96.png", fig_2_matrix, width = 9, height = 6)
# ggsave("figures/quantile_plots/figure_2.pdf", fig_2_matrix, width = 9, height = 6, dpi = 1000)
# ggsave("figures/quantile_plots/figure_2.eps", plot = fig_2_matrix, device = "eps", height = 6, width = 9)





#### 75, 95 ####

demo <- readRDS("data/processed/demo.rds")
mims <- readRDS("data/processed/mims.rds")
## Use 95th percentile of all MIMS as MVPA cutoff
## Use 75th percentile of all MIMS as LiPA cutoff

lipa_cp <- unname(quantile(mims$MIMS, 0.75)) #17.678
mvpa_cp <- unname(quantile(mims$MIMS, 0.95)) #35.996

mims <- mims %>% mutate(PA  = case_when(Flag == "Wake" & MIMS <= lipa_cp ~ "SeB", 
                                        Flag == "Wake" & between(MIMS, lipa_cp, mvpa_cp) ~ "LiPA", 
                                        Flag == "Wake" & MIMS > mvpa_cp ~ "MVPA", 
                                        .default = Flag))


day_compositions = mims %>%
  group_by(SEQN, DayofWeek) %>%
  mutate(mvpa = sum(PA == "MVPA"),
         lipa = sum(PA == "LiPA"),
         seb = sum(PA == "SeB"),
         wake_norm = Wake / (Wake + Sleep) * 1440,
         sleep_norm = Sleep / (Wake + Sleep) * 1440,
         rel_mvpa = mvpa / wake_norm,
         rel_lipa = lipa / wake_norm,
         rel_seb = seb / wake_norm,
         mvpa_lipa = mvpa / lipa,
         mvpa_seb = mvpa / seb,
         lipa_seb = lipa / seb,
         activity_seb = (mvpa + lipa) / seb) %>%
  select(SEQN, Age, Sex, DayofWeek, Wake, Sleep, wake_norm, sleep_norm, mvpa, lipa, seb, rel_mvpa, rel_lipa, rel_seb,
         mvpa_lipa, mvpa_seb, lipa_seb, activity_seb) %>%
  distinct() %>%
  ungroup()


subject_compositions = day_compositions %>%
  group_by(SEQN) %>%
  mutate(avg_sleep = mean(Sleep),
         avg_wake = mean(Wake),
         avg_wake_norm = mean(wake_norm),
         avg_sleep_norm = mean(sleep_norm),
         avg_mvpa = mean(mvpa),
         avg_lipa = mean(lipa),
         avg_seb = mean(seb),
         avg_rel_mvpa = mean(rel_mvpa),
         avg_rel_lipa = mean(rel_lipa),
         avg_rel_seb = mean(rel_seb),
         avg_mvpa_lipa = mean(mvpa_lipa, na.rm = T),
         avg_mvpa_seb = mean(mvpa_seb),
         avg_lipa_seb = mean(lipa_seb),
         avg_activity_seb = mean(activity_seb)) %>%
  select(SEQN, Age, Sex, avg_sleep, avg_sleep_norm, avg_wake, avg_wake_norm, avg_mvpa, avg_lipa, avg_seb, avg_rel_mvpa, avg_rel_lipa, 
         avg_rel_seb, avg_mvpa_lipa, avg_mvpa_seb, avg_lipa_seb, avg_activity_seb) %>%
  distinct() %>%
  ungroup()


# Adding weights
source("R/functions/get_new_weights.R")
new_weights = get_new_weights(demo)

compositions_weights <- merge(subject_compositions, new_weights, by = "SEQN")

compositions_male <- compositions_weights %>%
  filter(Sex == 1) 

compositions_female <- compositions_weights %>%
  filter(Sex == 2)

source("R/functions/quantile_plot_functions.R")

cent = 50

preds_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa", "new_weights", cent, df = 5)

preds_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_lipa", "new_weights", cent, df = 5)

preds_seb = get_quantile_curves(compositions_male, compositions_female, "avg_seb", "new_weights", cent, df = 5)

preds_rel_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_mvpa", "new_weights", cent, df = 5)

preds_rel_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_lipa", "new_weights", cent, df = 5)

preds_rel_seb = get_quantile_curves(compositions_male, compositions_female, "avg_rel_seb", "new_weights", cent, df = 5)

##ggplots

## Plots of medians by Sex

palette_med_plots = c("Female" = "#F98400", "Male" = "#00A08A")

#Figure 2

med_plot_seb_small = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_lipa_small = preds_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA (min)", title = "Absolute LiPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median LiPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_mvpa_small = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA (min)", title = "Absolute MVPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median MVPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))



med_plot_rel_seb_small = preds_rel_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB/Wake", title = "Relative SeB") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_lipa_small = preds_rel_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA/Wake", title = "Relative LiPA") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_mvpa_small = preds_rel_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA/Wake", title = "Relative MVPA") +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))


fig_2_matrix = grid.arrange(med_plot_seb_small, med_plot_lipa_small, med_plot_mvpa_small, med_plot_rel_seb_small, med_plot_rel_lipa_small, med_plot_rel_mvpa_small, nrow = 2)

ggsave("figures/sensitivity_analysis/quantile_plots/figure_2_75_95.png", fig_2_matrix, width = 9, height = 6)
# ggsave("figures/quantile_plots/figure_2.pdf", fig_2_matrix, width = 9, height = 6, dpi = 1000)
# ggsave("figures/quantile_plots/figure_2.eps", plot = fig_2_matrix, device = "eps", height = 6, width = 9)


#### 80, 95 ####

demo <- readRDS("data/processed/demo.rds")
mims <- readRDS("data/processed/mims.rds")
## Use 95th percentile of all MIMS as MVPA cutoff
## Use 80th percentile of all MIMS as LiPA cutoff

lipa_cp <- unname(quantile(mims$MIMS, 0.80)) #20.987
mvpa_cp <- unname(quantile(mims$MIMS, 0.95)) #35.996

mims <- mims %>% mutate(PA  = case_when(Flag == "Wake" & MIMS <= lipa_cp ~ "SeB", 
                                        Flag == "Wake" & between(MIMS, lipa_cp, mvpa_cp) ~ "LiPA", 
                                        Flag == "Wake" & MIMS > mvpa_cp ~ "MVPA", 
                                        .default = Flag))


day_compositions = mims %>%
  group_by(SEQN, DayofWeek) %>%
  mutate(mvpa = sum(PA == "MVPA"),
         lipa = sum(PA == "LiPA"),
         seb = sum(PA == "SeB"),
         wake_norm = Wake / (Wake + Sleep) * 1440,
         sleep_norm = Sleep / (Wake + Sleep) * 1440,
         rel_mvpa = mvpa / wake_norm,
         rel_lipa = lipa / wake_norm,
         rel_seb = seb / wake_norm,
         mvpa_lipa = mvpa / lipa,
         mvpa_seb = mvpa / seb,
         lipa_seb = lipa / seb,
         activity_seb = (mvpa + lipa) / seb) %>%
  select(SEQN, Age, Sex, DayofWeek, Wake, Sleep, wake_norm, sleep_norm, mvpa, lipa, seb, rel_mvpa, rel_lipa, rel_seb,
         mvpa_lipa, mvpa_seb, lipa_seb, activity_seb) %>%
  distinct() %>%
  ungroup()


subject_compositions = day_compositions %>%
  group_by(SEQN) %>%
  mutate(avg_sleep = mean(Sleep),
         avg_wake = mean(Wake),
         avg_wake_norm = mean(wake_norm),
         avg_sleep_norm = mean(sleep_norm),
         avg_mvpa = mean(mvpa),
         avg_lipa = mean(lipa),
         avg_seb = mean(seb),
         avg_rel_mvpa = mean(rel_mvpa),
         avg_rel_lipa = mean(rel_lipa),
         avg_rel_seb = mean(rel_seb),
         avg_mvpa_lipa = mean(mvpa_lipa, na.rm = T),
         avg_mvpa_seb = mean(mvpa_seb),
         avg_lipa_seb = mean(lipa_seb),
         avg_activity_seb = mean(activity_seb)) %>%
  select(SEQN, Age, Sex, avg_sleep, avg_sleep_norm, avg_wake, avg_wake_norm, avg_mvpa, avg_lipa, avg_seb, avg_rel_mvpa, avg_rel_lipa, 
         avg_rel_seb, avg_mvpa_lipa, avg_mvpa_seb, avg_lipa_seb, avg_activity_seb) %>%
  distinct() %>%
  ungroup()


# Adding weights
source("R/functions/get_new_weights.R")
new_weights = get_new_weights(demo)

compositions_weights <- merge(subject_compositions, new_weights, by = "SEQN")

compositions_male <- compositions_weights %>%
  filter(Sex == 1) 

compositions_female <- compositions_weights %>%
  filter(Sex == 2)

source("R/functions/quantile_plot_functions.R")

cent = 50

preds_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa", "new_weights", cent, df = 5)

preds_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_lipa", "new_weights", cent, df = 5)

preds_seb = get_quantile_curves(compositions_male, compositions_female, "avg_seb", "new_weights", cent, df = 5)

preds_rel_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_mvpa", "new_weights", cent, df = 5)

preds_rel_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_lipa", "new_weights", cent, df = 5)

preds_rel_seb = get_quantile_curves(compositions_male, compositions_female, "avg_rel_seb", "new_weights", cent, df = 5)

##ggplots

## Plots of medians by Sex

palette_med_plots = c("Female" = "#F98400", "Male" = "#00A08A")

#Figure 2

med_plot_seb_small = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_lipa_small = preds_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA (min)", title = "Absolute LiPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median LiPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_mvpa_small = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA (min)", title = "Absolute MVPA") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median MVPA (hours)"))+
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))



med_plot_rel_seb_small = preds_rel_seb %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB/Wake", title = "Relative SeB") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_lipa_small = preds_rel_lipa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA/Wake", title = "Relative LiPA") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14))

med_plot_rel_mvpa_small = preds_rel_mvpa %>%
  ggplot(aes(Age, quant_50, group = Sex, color = Sex)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA/Wake", title = "Relative MVPA") +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.73),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size =12))


fig_2_matrix = grid.arrange(med_plot_seb_small, med_plot_lipa_small, med_plot_mvpa_small, med_plot_rel_seb_small, med_plot_rel_lipa_small, med_plot_rel_mvpa_small, nrow = 2)

ggsave("figures/sensitivity_analysis/quantile_plots/figure_2_80_95.png", fig_2_matrix, width = 9, height = 6)
# ggsave("figures/quantile_plots/figure_2.pdf", fig_2_matrix, width = 9, height = 6, dpi = 1000)
# ggsave("figures/quantile_plots/figure_2.eps", plot = fig_2_matrix, device = "eps", height = 6, width = 9)
