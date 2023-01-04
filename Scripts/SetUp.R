#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "readr", "dplyr", "tidyr", "tidyverse", # for wrangling
  "forecast"
)
purrr::walk(packages, library, character.only = TRUE)
