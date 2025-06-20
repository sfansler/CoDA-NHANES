# Epidemiology of compositions of 24-hour behaviors in US adolescents and adults

This Github repo contains the code needed to reproduce the figures and results in the paper. Below find a detailed workflow to clean, process, and analyze the data.

## Preprocessing

### 1. R/data_cleaning.R

This file reads the raw NHANES physical activity data, cleans it for analysis, and creates summaries of daily wake, sleep, and non-wear according to the predicted NHANES flags. It then saves the clean data file.

### 2. R/exculsion.R

This file filters the clean dataset to exclude participants with fewer than 3 "valid" days as defined in the Study Population subsection of the paper's Methods section. It then saves the final physical activity and demographic datasets used in the analysis.

## Table 1

### 3. R/table_1.R

This file creates the table of study population characteristics.