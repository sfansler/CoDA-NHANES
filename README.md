# Lifespan trends in 24-hour movement behavior compositions in US youth and adults

This Github repo contains the code needed to reproduce the figures and results in the paper. Below find a detailed workflow to clean, process, and analyze the data.

## Preprocessing

### 1. R/data_cleaning.R

This file reads the raw NHANES physical activity data, cleans it for analysis, and creates summaries of daily wake, sleep, and non-wear according to the predicted NHANES flags. It then saves the clean data file.

### 2. R/exclusion.R

This file filters the clean dataset to exclude participants with fewer than 3 "valid" days as defined in the Study Population subsection of the paper's Methods section. It then saves the final physical activity and demographic datasets used in the analysis.

## Table 1

### 3. R/table_1.R

This file creates the table of study population characteristics.

## Compositions

### 4. R/create_compositions.R

Creates daily-level and subject-level compositions of wake time

### 5. R/median_plots.R

Creates all median plots across the lifespan and generates all figures in main analysis as well as figure A2

### 6. R/figure_A1a.R

Creates example heatmap of subject

### 7. R/figure_A1b.R

Creates compositional barplot of subject

### 8. R/figure_A3.R

Creates histogram of MIMS color-coded by wake-time behavior
