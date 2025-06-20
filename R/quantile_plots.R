library(tidyverse)
library(gridExtra)
library(wesanderson)
library(ggnewscale)
library(patchwork)


#Loading in aggregated compositions

subject_compositions = readRDS("data/processed/aggregated_compositions.rds")
demo = readRDS("data/processed/demo.rds")
# Adding weights
source("R/functions/get_new_weights.R")
new_weights = get_new_weights(demo)

compositions_weights <- merge(subject_compositions, new_weights, by = "SEQN")

compositions_male <- compositions_weights %>%
  filter(Gender == 1) 

compositions_female <- compositions_weights %>%
  filter(Gender == 2)

source("R/functions/quantile_plot_functions.R")

cent = 50
preds_sleep = get_quantile_curves(compositions_male, compositions_female, "avg_sleep_norm", "new_weights", cent, df = 5)

preds_wake = get_quantile_curves(compositions_male, compositions_female, "avg_wake_norm", "new_weights", cent, df = 5)

preds_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa", "new_weights", cent, df = 5)

preds_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_lipa", "new_weights", cent, df = 5)

preds_seb = get_quantile_curves(compositions_male, compositions_female, "avg_seb", "new_weights", cent, df = 5)

preds_rel_mvpa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_mvpa", "new_weights", cent, df = 5)

preds_rel_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_rel_lipa", "new_weights", cent, df = 5)

preds_rel_seb = get_quantile_curves(compositions_male, compositions_female, "avg_rel_seb", "new_weights", cent, df = 5)

preds_mvpa_lipa = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa_lipa", "new_weights", cent, df = 5)

preds_mvpa_seb = get_quantile_curves(compositions_male, compositions_female, "avg_mvpa_seb", "new_weights", cent, df = 5)

preds_lipa_seb = get_quantile_curves(compositions_male, compositions_female, "avg_lipa_seb", "new_weights", cent, df = 5)

preds_active_seb = get_quantile_curves(compositions_male, compositions_female, "avg_activity_seb", "new_weights", cent, df = 5)

##ggplots

## Plots of medians by gender

palette_med_plots = c("Female" = "#F98400", "Male" = "#00A08A")

med_plot_sleep = preds_sleep %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median sleep (min)", title = "Sleep") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median sleep (hours)", 
                                         breaks = c(7, 7.5, 8, 8.5, 9))) +  theme_bw(base_size = 17) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  theme(plot.title = element_text(size = 35, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"))

med_plot_sleep_with_points = ggplot(data = compositions_weights, aes(x = Age, y = avg_sleep_norm)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = preds_sleep, aes(y = quant_50, group = Gender, color = Gender), linewidth = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median sleep (hours)",
                                        breaks = seq(5, 12, 1))) +
  labs(y = "Median sleep (min)", title = "Sleep") +
  theme_bw(base_size = 17) +
  theme(plot.title = element_text(size = 27, hjust = 0.5, face = "bold")
       )

ggsave("figures/quantile_plots/med_plot_sleep.png", med_plot_sleep, width = 9, height = 8)
ggsave("figures/quantile_plots/med_plot_sleep_with_points.png", med_plot_sleep_with_points, width = 13, height = 8)

med_plot_wake = preds_wake %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median wake (min)", title = "Wake") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median wake (hours)", 
                                         breaks = c(15.5, 16, 16.5))) +
  theme_bw(base_size = 17) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  theme(plot.title = element_text(size = 35, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"))

ggsave("figures/quantile_plots/med_plot_wake.png", med_plot_wake, width = 13, height = 8)


med_plot_mvpa = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA (min)", title = "Absolute MVPA") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median MVPA (hours)"))+
  theme_bw(base_size = 17) +
  theme(plot.title = element_text(size = 27, hjust = 0.5, face = "bold"))


ggsave("figures/quantile_plots/med_plot_mvpa.png", med_plot_mvpa, width = 10)


med_plot_lipa = preds_lipa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA (min)", title = "Absolute LiPA") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median LiPA (hours)"))+
  theme_bw(base_size = 17) +
  theme(plot.title = element_text(size = 27, hjust = 0.5, face = "bold"))

ggsave("figures/quantile_plots/med_plot_lipa.png", med_plot_lipa, width = 10)


med_plot_seb = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)"))+
  theme_bw(base_size = 17) +
  theme(plot.title = element_text(size = 27, hjust = 0.5, face = "bold"))

ggsave("figures/quantile_plots/med_plot_seb.png", med_plot_seb, width = 10)


med_plot_mvpa_lipa = preds_mvpa_lipa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA/LiPA", title = "Median MVPA/LiPA by Gender") +
  theme_bw()

ggsave("figures/quantile_plots/med_plot_mvpa_lipa.png", med_plot_mvpa_lipa, width = 10)


med_plot_mvpa_seb = preds_mvpa_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA/SeB", title = "MVPA/SeB")  +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))

ggsave("figures/quantile_plots/med_plot_mvpa_seb.png", med_plot_mvpa_seb, width = 10)

med_plot_lipa_seb = preds_lipa_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA/SeB", title = "LiPA/SeB") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))

ggsave("figures/quantile_plots/med_plot_lipa_seb.png", med_plot_lipa_seb, width = 10)



med_plot_active_seb = preds_active_seb %>% 
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median (MVPA + LiPA)/SeB", title = "(MVPA + LiPA)/SeB") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))

ggsave("figures/quantile_plots/med_plot_active_seb.png", med_plot_active_seb, width = 10)

med_plot_wake_small = preds_wake %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median Wake (min)", title = "Wake") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median Wake (hours)", 
                                         breaks = c(15, 15.5, 16))) +
  theme_bw(base_size = 20) +
  theme(legend.position = "inside",
        legend_position_inside = c(0.65, 0.2),
        plot.title = element_text(hjust = 0.5, face = "bold"))

med_plot_seb_small = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)"))+
  theme_bw(base_size = 20) +
  guides(color = "none") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

med_plot_seb_and_wake = grid.arrange(med_plot_seb_small, med_plot_wake_small, 
                                    nrow = 1)

ggsave("figures/quantile_plots/med_plot_seb_and_wake.png", med_plot_seb_and_wake, width = 14, height = 6)



#Figure 1
fig_1 = grid.arrange(med_plot_sleep, med_plot_wake)

ggsave("figures/quantile_plots/figure_1.png", fig_1, height = 10, width = 12)


#Figure 2
med_plot_seb_small = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
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
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
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
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
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


test_plot = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 3) +
  scale_color_manual(values = palette_med_plots) +
  theme_bw() +
  theme(legend.text = element_text(size = 24),
        legend.title = element_blank(),
        legend.key.size = unit(7, "lines"))

legend = ggpubr::get_legend(test_plot)

legend_plot = ggpubr::as_ggplot(legend)

fig_2 = grid.arrange(med_plot_seb_small, med_plot_lipa_small, med_plot_mvpa_small, legend_plot, nrow = 2)

ggsave("figures/quantile_plots/figure_2.png", fig_2, width = 12, height = 11)

med_plot_seb_small_pres = preds_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  theme_bw(base_size = 28) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB (min)", title = "Absolute SeB") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median SeB (hours)")) +
  guides(color = "none") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"))

med_plot_lipa_small_pres = preds_lipa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  theme_bw(base_size = 28) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA (min)", title = "Absolute LiPA") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median LiPA (hours)"))+
  guides(color = "none") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"))

med_plot_mvpa_small_pres = preds_mvpa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1.5) +
  theme_bw(base_size = 28) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median MVPA (min)", title = "Absolute MVPA") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, name = "Median MVPA (hours)"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.6, 0.6),
        legend.key = element_rect(linewidth = 3))


fig_presentation = grid.arrange(med_plot_seb_small_pres, med_plot_lipa_small_pres, med_plot_mvpa_small_pres, nrow = 1)

ggsave("figures/quantile_plots/figure_2_presentation.png", fig_presentation, width = 24, height = 9)

# Figure 3
median_mvpa_wake = preds_rel_mvpa %>%
  select(Age, quant_50, Gender) %>%
  distinct() %>%
  mutate(State = "MVPA")  

median_lipa_wake = preds_rel_lipa%>%
  select(Age, quant_50, Gender) %>%
  mutate(State = "LiPA")

median_seb_wake = preds_rel_seb%>%
  select(Age, quant_50, Gender) %>%
  mutate(State = "SeB")

median_comps = rbind(median_mvpa_wake, median_lipa_wake, median_seb_wake) %>%
  group_by(Gender, Age) %>%
  mutate(quant_50_norm = quant_50 / sum(quant_50))

comp_plots = median_comps %>%
  mutate(State = factor(State, levels = c("MVPA", "LiPA", "SeB"))) %>%
  ggplot(aes(x = Age, y = quant_50_norm, fill = State, color = State)) +
  geom_col() +
  facet_wrap(vars(Gender)) +
  labs(y = "Median composition of Wake") +
  theme_bw(base_size = 20) +
  scale_fill_manual(values = c("MVPA" = "#E54E21", "LiPA" = "#0A9F9D", "SeB" = "#CEB175")) +
  scale_color_manual(values = c("MVPA" = "#E54E21", "LiPA" = "#0A9F9D", "SeB" = "#CEB175"))+
  theme(strip.text = element_text(face = "bold", size = 25),
        axis.text = element_text(face = "bold", size = 25),
        axis.title = element_text(face = "bold", size = 25),
        legend.key.size = unit(1.5, "cm"),
        panel.ontop = TRUE,
        panel.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 23)) +
  coord_cartesian(ylim = c(0.33, 1))

ggsave("figures/quantile_plots/figure_3.png", comp_plots, width = 13, height = 8.5, units = "in")

#Figure A1
fig_a1 = grid.arrange(med_plot_lipa_seb, med_plot_mvpa_seb, med_plot_active_seb, nrow = 2)
  
ggsave("figures/quantile_plots/figure_A1.png", fig_a1)



#Figure A2
fig_a2 = med_plot_mvpa_lipa +
  labs(title = "MVPA/LiPA") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))

ggsave("figures/quantile_plots/figure_A2.png", fig_a2)


med_plot_mvpa_wake = preds_rel_mvpa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Median MVPA/Wake", title = "MVPA/Wake") +
  scale_color_manual(values = palette_med_plots) +
  theme_bw(base_size = 17) +
  theme(legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"))

ggsave("figures/quantile_plots/med_plot_mvpa_wake.png", med_plot_mvpa_wake, width = 10)

med_plot_lipa_wake = preds_rel_lipa %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median LiPA/Wake", title = "LiPA/Wake") +
  theme_bw(base_size = 17) +
  theme(legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"))


ggsave("figures/quantile_plots/med_plot_lipa_wake.png", med_plot_lipa_wake, width = 10)

med_plot_seb_wake = preds_rel_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = palette_med_plots) +
  labs(y = "Median SeB/Wake", title = "SeB/Wake") +
  theme_bw(base_size = 17) +
  theme(legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"))

ggsave("figures/quantile_plots/med_plot_seb_wake.png", med_plot_seb_wake, width = 10)
ggsave("figures/quantile_plots/med_plot_seb_wake_small.png", med_plot_seb_wake, width = 7)


#Figure 2 3x2 matrix

med_plot_rel_seb_small = preds_rel_seb %>%
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
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
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
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
  ggplot(aes(Age, quant_50, group = Gender, color = Gender)) +
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

ggsave("figures/quantile_plots/figure_2_matrix.png", fig_2_matrix, width = 9, height = 6)


#Figure 3 top panel

###combining absolute trends

absolute_trends_combined = data.frame(quant_50 = c(preds_seb$quant_50, preds_lipa$quant_50, preds_mvpa$quant_50, preds_wake$quant_50), State = rep(c("SeB", "LiPA", "MVPA", "Wake"), each = 156), Gender = rep(c("Female", "Male"), times = 156/2*4), Age = rep(seq(3, 80), each = 2, times = 4))

### Renormalize to total wake time

absolute_trends_norm = absolute_trends_combined %>%
  group_by(Age, Gender) %>%
  summarize(PA_behaviors = sum(quant_50[State != "Wake"]),
            SeB = quant_50[State == "SeB"],
            LiPA = quant_50[State == "LiPA"],
            MVPA = quant_50[State == "MVPA"],
            SeB_prop = SeB / PA_behaviors,
            LiPA_prop = LiPA / PA_behaviors,
            MVPA_prop = MVPA / PA_behaviors,
            wake = quant_50[State == "Wake"]) %>%
  mutate(SeB_norm = wake * SeB_prop,
         LiPA_norm = wake * LiPA_prop,
         MVPA_norm = wake * MVPA_prop) %>%
  select(Age, Gender, SeB_norm, LiPA_norm, MVPA_norm) %>%
  rename(SeB = SeB_norm, LiPA = LiPA_norm, MVPA = MVPA_norm) %>%
  pivot_longer(cols = c(SeB, LiPA, MVPA), names_to = "State", values_to = "quant_50") %>%
  mutate(State = factor(State, levels = c("MVPA", "LiPA", "SeB", "Wake")))



fig_3_top_panel = 
  ggplot(data = absolute_trends_norm, aes(x = Age, y = quant_50, fill = State, color = State)) + 
  geom_col() + 
  labs(y = "Median Wake (min)") +
  facet_wrap(vars(Gender)) +
  theme_bw(base_size = 20) +
  scale_fill_manual(values = c("MVPA" = "#E54E21", "LiPA" = "#0A9F9D", "SeB" = "#CEB175")) +
  scale_color_manual(values = c("MVPA" = "#E54E21", "LiPA" = "#0A9F9D", "SeB" = "#CEB175")) +
  guides(fill = "none", color = "none") +
  theme(strip.text = element_text(face = "bold", size = 25),
                axis.text = element_text(face = "bold", size = 25),
                axis.title = element_text(face = "bold", size = 25),
                legend.key.size = unit(1.5, "cm"),
                panel.ontop = TRUE,
                panel.background = element_blank(),
                plot.title = element_text(face = "bold", hjust = 0.5),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 23))+
  coord_cartesian(ylim = c(5*60, 16*60))
  
ggsave("figures/quantile_plots/figure_3_top_panel.png", fig_3_top_panel, width = 13, height = 8.5, units = "in")


line_plot = preds_wake %>%
  ggplot(aes(Age, quant_50, color = Gender)) +
  facet_wrap(vars(Gender)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_color_manual(values = palette_med_plots) +
  theme_void(base_size = 20) +
  theme(strip.text = element_text(face = "bold", size = 25),
        axis.text = element_text(face = "bold", size = 25),
        axis.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 25, angle = 90),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 23))