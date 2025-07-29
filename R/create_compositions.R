library(tidyverse)
library(haven)

demo <- readRDS("data/processed/demo.rds")
mims <- readRDS("data/processed/mims.rds")
## Use 95th percentile of all MIMS as MVPA cutoff
## Use 70th percentile of all MIMS as LiPA cutoff

lipa_cp <- unname(quantile(mims$MIMS, 0.70)) #14.659
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
         avg_mvpa_lipa = mean(mvpa_lipa),
         avg_mvpa_seb = mean(mvpa_seb),
         avg_lipa_seb = mean(lipa_seb),
         avg_activity_seb = mean(activity_seb)) %>%
  select(SEQN, Age, Sex, avg_sleep, avg_sleep_norm, avg_wake, avg_wake_norm, avg_mvpa, avg_lipa, avg_seb, avg_rel_mvpa, avg_rel_lipa, 
         avg_rel_seb, avg_mvpa_lipa, avg_mvpa_seb, avg_lipa_seb, avg_activity_seb) %>%
  distinct() %>%
  ungroup()

saveRDS(day_compositions, "data/processed/daily_compositions.rds")
saveRDS(subject_compositions, "data/processed/aggregated_compositions.rds")
