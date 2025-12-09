library(haven)
library(tidyverse)
library(gtsummary)
library(survey)
library(table1)

demo_valid = readRDS("data/processed/demo.rds") %>%
  mutate(included = 1)
demo_invalid = readRDS("data/processed/demo_excluded.rds") %>%
  mutate(included = 0)

bmi_11 = read_xpt("data/raw/BMX_G.XPT") %>%
  select(SEQN, BMXBMI)
bmi_13 = read_xpt("data/raw/BMX_H.XPT") %>%
  select(SEQN, BMXBMI)


demo = rbind(demo_valid, demo_invalid)

bmi = rbind(bmi_11, bmi_13)


data_combined = left_join(demo, bmi) %>%
  select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, BMXBMI, RIDRETH1, WTMEC2YR, included) %>%
  distinct()

source("R/functions/get_new_weights.R")
data_weighted = get_new_weights(data_combined)


table_data = data_weighted %>%
  rename(Age = RIDAGEYR,
         Sex = RIAGENDR,
         BMI = BMXBMI,
         Race = RIDRETH1,
         Included = included) %>%
  mutate(
  #mutate(age_cat = factor(case_when(between(Age, 3, 19) ~ "Age 3-19",
                             #between(Age, 20, 49) ~ "Age 20-49",
                             #between(Age, 50, 80) ~ "Age 50+"), levels = c("Age 3-19", "Age 20-49", "Age 50+")),
         Sex = case_when(Sex == 1 ~ "Male",
                            Sex == 2 ~ "Female"),
         Race = factor(case_when(Race == 1 ~ "Mexican American",
                          Race == 2 ~ "Other Hispanic",
                          Race == 3 ~ "Non-Hispanic White",
                          Race == 4 ~ "Non-Hispanic Black",
                          .default = "Other"), levels = c("Non-Hispanic White", "Non-Hispanic Black", "Mexican American", "Other Hispanic", "Other")),
         Included = factor(ifelse(Included == 1, "Included", "Excluded"), levels = c("Included", "Excluded")))  %>%
  select(-SEQN)
         #,-agecat)



tbl_1 = svydesign(id = ~1, weights = ~new_weights, data = table_data) %>%
  tbl_svysummary(by = Included, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                all_categorical() ~ "{p}%"), include = c(Age, Sex, BMI, Race), digits = list(Race = 0), missing = "no") %>%
  add_overall() %>%
  modify_table_styling(columns = "label",
                       rows = row_type == "label",
                       text_format = "bold",
                       footnote = NULL) %>%
  modify_header(all_stat_cols() ~ "**{level}** (n = {n_unweighted})") %>%
  modify_footnote(
    all_stat_cols() ~ NA
) %>%
  as_flex_table() %>%
  flextable::theme_vanilla()


flextable::save_as_docx(tbl_1, path = "tables/table_1.docx")
