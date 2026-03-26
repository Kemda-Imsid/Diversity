# Diversity
# create_demo_gender_tab

`create_demo_gender_tab` is an R function that creates a **cross-tabulation table** for two categorical variables (e.g., minority status and gender) and performs **Fisher’s exact test** to check for association. It leverages the `janitor` and `dplyr` packages.

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

## Installation

Make sure you have the required packages installed:

```r
install.packages(c("dplyr", "janitor"))