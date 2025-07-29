library(tidyverse)
data = readRDS("data/processed/aggregated_compositions.rds")

bar_plot_paper = data %>%
  filter(SEQN == "62161") %>%
  mutate(
    hours_seb = floor(avg_seb/60),
    hours_lipa = floor(avg_lipa/60),
    hours_mvpa = floor(avg_mvpa/60),
    minutes_seb = round(avg_seb - 60*hours_seb) + 1,
    minutes_lipa = round(avg_lipa - 60*hours_lipa),
    minutes_mvpa = round(avg_mvpa - 60*hours_mvpa),
    wake_hours = floor(avg_wake/60),
    wake_minutes = round(avg_wake - 60*wake_hours)) %>%
  select(SEQN, avg_rel_mvpa, avg_rel_lipa, avg_rel_seb,
         hours_seb, hours_lipa, hours_mvpa,
         minutes_seb, minutes_lipa, minutes_mvpa,
         wake_hours, wake_minutes) %>%
  distinct() %>%
  mutate(avg_seb = avg_rel_seb/(avg_rel_seb + avg_rel_lipa + avg_rel_mvpa),
         avg_lipa = avg_rel_lipa/(avg_rel_seb + avg_rel_lipa + avg_rel_mvpa) - 0.01,
         avg_mvpa = avg_rel_mvpa/(avg_rel_seb + avg_rel_lipa + avg_rel_mvpa)) %>%
  select(-c(avg_rel_seb, avg_rel_lipa, avg_rel_mvpa)) %>%
  pivot_longer(
    cols = starts_with("hours") | starts_with("minutes") | starts_with("avg"), 
    names_to = c(".value", "state"), 
    names_pattern = "(hours|minutes|avg)_(.*)"
  ) %>%
  rename(proportion = avg) %>%
  mutate(state = factor(case_when(state == "mvpa" ~ "MVPA",
                                  state == "lipa" ~ "LiPA",
                                  .default = "SeB"), levels = c("MVPA", "LiPA", "SeB")),
         mims_cutoff = c("<14.66 MIMS", "14.66 - 36.00 MIMS", ">36.00 MIMS"))

paper_plot = ggplot(data = bar_plot_paper, aes(x = 0.65, y = proportion, fill = state)) + 
  scale_x_continuous(limits = c(0.60, 0.75)) +
  geom_col(width = 0.055) +
  geom_label(aes(label = state, x = 0.65, y = c(0.2, 0.38, 0.29)), 
             position = position_stack(vjust = 1.5), 
             size = 12) +
  geom_text(aes(label = ifelse(hours == 0, paste(minutes, "min", sep = ""), paste(hours, "h ", minutes, "min", sep = ""))), 
            x = .595,
            y = c(.24, 0.715, 0.975), 
            hjust = 0, 
            size = 8) +
  geom_text(aes(label = paste("(", mims_cutoff, ")", sep = "")), 
            x = 0.65, 
            y = c(.24, 0.715, 0.96),
            hjust = 0.5,
            size = 5.2) +
  geom_text(aes(label = paste(round(proportion * 100), "%", sep = "")), 
            x = .682, 
            hjust = 0,
            y = c(0.24, 0.715, 0.975), 
            size = 9.5, 
            color = "black") +
  scale_fill_manual(values = c("MVPA" = "#E54E21", "LiPA" = "#0A9F9D", "SeB" = "#CEB175")) +
  guides(fill = "none") +
  labs(title = "22 y/o Male",
       subtitle = paste("Wake = ", unique(bar_plot_paper$wake_hours), "h ", unique(bar_plot_paper$wake_minutes), "min", sep = "")) +
  theme_void(base_size = 14) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x = unit(0, "cm"),
        plot.title = element_text(hjust = 0.3, vjust = 1.3, size = 32),
        plot.subtitle = element_text(hjust = 0.3, vjust = 1.8, size = 20))

plot(paper_plot)

ggsave("figures/bar_plots/Figure_A1b.png", paper_plot, width = 8, height = 10)
