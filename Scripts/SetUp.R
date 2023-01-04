#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "readr", "readxl", "dplyr", "tidyr", "tidyverse", "lubridate", "janitor", # for wrangling
  "ggplot2", "tsibble", "feasts", "kableExtra" # for visualization
)

purrr::walk(packages, library, character.only = TRUE)

