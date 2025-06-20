library(haven)
library(tidyverse)

pa_11 = read_xpt("https://ftp.cdc.gov/pub/NHANES/LargeDataFiles/PAXMIN_G.xpt") %>%
  select(SEQN, PAXDAYM, PAXDAYWM, PAXSSNMP, PAXPREDM, PAXMXM, PAXMYM, PAXMZM, PAXMTSM)

pa_13 = read_xpt("https://ftp.cdc.gov/pub/NHANES/LargeDataFiles/PAXMIN_H.xpt") %>%
  select(SEQN, PAXDAYM, PAXDAYWM, PAXSSNMP, PAXPREDM, PAXMXM, PAXMYM, PAXMZM, PAXMTSM)

pa <- rbind(pa_11, pa_13) %>%
  group_by(SEQN, PAXDAYM) %>%
  mutate(minutes = n()) %>%
  filter(minutes == 1440) %>%
  arrange(PAXDAYM, PAXSSNMP, .by_group = T) %>%
  mutate(time = PAXSSNMP / 4800,
         Time = 1:1440) %>%
  ungroup() %>%
  mutate(DayofWeek = case_when(PAXDAYWM == "1" ~ "Sunday",
                               PAXDAYWM == "2" ~ "Monday",
                               PAXDAYWM == "3" ~ "Tuesday",
                               PAXDAYWM == "4" ~ "Wednesday",
                               PAXDAYWM == "5" ~ "Thursday",
                               PAXDAYWM == "6" ~ "Friday",
                               PAXDAYWM == "7" ~ "Saturday"),
         Flag = case_when(PAXPREDM == "1" ~ "Wake",
                          PAXPREDM == "2" ~ "Sleep",
                          PAXPREDM == "3" ~ "Non-wear",
                          PAXPREDM == "4" ~ "Unknown")) %>%
  rename(Day = PAXDAYM,
         MIMSx = PAXMXM,
         MIMSy = PAXMYM,
         MIMSz = PAXMZM,
         MIMS = PAXMTSM) %>%
  select(SEQN, Day, DayofWeek, Time, MIMS, Flag) %>%
  #Creating summaries
  group_by(SEQN, Day) %>%
  mutate(Wake = sum(Flag == "Wake"),
         Sleep = sum(Flag == "Sleep"),
         Non_wear = sum(Flag == "Non-wear"),
         Unknown = sum(Flag == "Unknown")) %>%
  arrange(SEQN, Day) %>%
  ungroup()


saveRDS(pa, "data/intermediate/data_cleaned.rds")
