library(tidyverse)
library(haven)
mims <- readRDS("data/intermediate/data_cleaned.rds")
demo_11 <- read_xpt("data/raw/DEMO_G.XPT") %>%
  select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, WTMEC2YR, RIDRETH1)

demo_13 <- read_xpt("data/raw/DEMO_H.XPT") %>%
  select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, WTMEC2YR, RIDRETH1)

demo = rbind(demo_11, demo_13)

#Only keep days with 90% of wear and at least 12 hours of Wake and 4 hours of Sleep
mims <- mims %>% 
filter(Non_wear < 144) %>%
filter(Wake >=12*60 & Sleep >= 4*60)

#Only keep subjects with 3 or more valid days
mims_seqn_valid <- mims %>% 
  group_by(SEQN, DayofWeek) %>% 
  select(SEQN, DayofWeek) %>% 
  distinct() %>% 
  group_by(SEQN) %>% 
  mutate(n_days = n()) %>% 
  filter(n_days >= 3) %>% 
  ungroup()


mims <- mims %>% filter(SEQN %in% mims_seqn_valid$SEQN)
demo_valid <- demo %>% filter(SEQN %in% mims_seqn_valid$SEQN)
#Merge valid mims with demo, save data
mims <- merge(mims, demo_valid, by = "SEQN") %>%
  rename(Age = RIDAGEYR,
         Gender = RIAGENDR)
saveRDS(mims, "data/processed/mims.rds")
saveRDS(demo_valid, "data/processed/demo.rds")
  