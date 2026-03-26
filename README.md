# Diversity
### Survey Analysis Toolkit

This repository provides two R functions for analyzing likert and demographic data:

1. `create_demo_gender_tab`: Create cross-tabulations and perform Fisher's exact test.  
2. `single_likert`: Plot single or multiple Likert survey items with optional demographic faceting.

## 1. create_demo_gender_tab

`create_demo_gender_tab` creates a **cross-tabulation table** for two categorical variables (e.g., minority status and gender) and performs **Fisher’s exact test**.

---

## Features

- Generates a **tabyl table** with:
  - Counts of each combination of categories
  - Row-wise percentages
  - Counts displayed in front of percentages (e.g., `5 (50%)`)
  - Row and column totals

- Creates a **contingency table** suitable for statistical tests.

- Performs **Fisher's exact test** on the two categorical variables.

---
### Example Usage

```r
# Run cross-tabulation
result <- create_demo_gender_tab(demo_vars, minority_group, gender_category)

# View tabyl table
result$tabyl_table

# View Fisher's test p-value
result$fisher_test$p.value

## 2. single_likert

`single_likert` is an R function to **plot single or multiple Likert survey items**. It can optionally facet by a demographic variable and display responses as **number stacked bars**.

### Features

- Works with **single or multiple Likert columns**.  
- Optional **demographic grouping** for divergent plots.  
- Custom **Likert levels** to control response order.  
- Custom **item order** for multiple items.  
- Flexible **y-axis limits** (`Limit`) and **axis label** (`Lab`).  
- Flipped coordinates for better readability.  

### Parameters

| Parameter       | Description |
|-----------------|-------------|
| `data`          | Data frame containing the Likert responses. |
| `likert_cols`   | Vector of column names (strings) containing Likert items. |
| `demo`          | demographic column name (string) for faceting. |
| `likert_levels` | vector of Likert levels (e.g., `"Strongly Disagree"` to `"Strongly Agree"`). |
| `list_order`    | order of demo items . |
| `Limit`         | Y-axis limits as a percentage range (default `c(0,100)`). |
| `Lab`           | Optional label for the y-axis. |

## Installation

Make sure you have the required packages installed:

```r
install.packages(c("dplyr", "janitor"))