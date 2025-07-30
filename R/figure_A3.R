library(tidyverse)

demo <- readRDS("data/processed/demo.rds")
mims <- readRDS("data/processed/mims.rds")
## Use 95th percentile of all MIMS as MVPA cutoff
## Use 70th percentile of all MIMS as LiPA cutoff

lipa_cp <- unname(quantile(mims$MIMS, 0.70)) #15.16
mvpa_cp <- unname(quantile(mims$MIMS, 0.95)) #35.98

mims <- mims %>% mutate(PA  = case_when(Flag == "Wake" & MIMS <= lipa_cp ~ "SeB", 
                                        Flag == "Wake" & between(MIMS, lipa_cp, mvpa_cp) ~ "LiPA", 
                                        Flag == "Wake" & MIMS > mvpa_cp ~ "MVPA", 
                                        .default = Flag))



#Remove non-wear and unknown values
mims <- mims %>%
  filter(PA %in% c("MVPA", "LiPA", "SeB", "Sleep"))
# Histogram of log-transformed MIMS

mims_dist = mims %>%
  mutate(PA = factor(PA, levels = c("MVPA", "LiPA", "SeB", "Sleep"))) %>%
  ggplot(aes(x = log(MIMS + 1), fill = PA, weight = WTMEC2YR/2)) + 
  geom_histogram(bins = 50, color = "black") +
  scale_fill_manual(name = "Flags", values = c("Sleep" = "#798E87", "SeB" = "#CEB175", "LiPA" = "#0A9F9D", "MVPA" = "#E54E21")) +
  geom_vline(aes(xintercept = log(lipa_cp + 1)), linewidth = 1, color = "#0A9F9D") +
  geom_vline(aes(xintercept = log(mvpa_cp + 1)), linewidth = 1, color = "#E54E21") +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.5)
  )

ggsave("figures/histograms/figure_A3.png", mims_dist, width = 12, height = 7)
ggsave("figures/histograms/figure_A3.pdf", mims_dist, width = 12, height = 7)
