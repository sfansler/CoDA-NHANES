library(tidyverse)

mims = readRDS("data/processed/mims.rds")

one_subject = mims[mims$SEQN == "62161",]


lipa_cp <- unname(quantile(mims$MIMS, 0.70)) #14.659
mvpa_cp <- unname(quantile(mims$MIMS, 0.95)) #35.996

one_subject <- one_subject %>% mutate(PA  = case_when(Flag == "Wake" & MIMS <= lipa_cp ~ "SeB", 
                                        Flag == "Wake" & between(MIMS, lipa_cp, mvpa_cp) ~ "LiPA", 
                                        Flag == "Wake" & MIMS > mvpa_cp ~ "MVPA", 
                                        .default = Flag))



source("R/functions/create_heatmap_plot.R")


example_heatmap = create_heatmap_plot_2(one_subject, "62161")

ggsave("figures/heatmaps/figure_A1a.png", example_heatmap, width = 10, height = 3.5)
