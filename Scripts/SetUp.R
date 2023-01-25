#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "readxl", "naniar","stringr", "janitor", # for wrangling
  "fpp3", "tsibbledata", # for time series
  "kableExtra", "skimr", "manipulate", "ggridges", "ggthemes", # for visualization
  "Rcpp", "sf", # for mapping
  "broom", "forecast" ,"caret"# modeling
)

purrr::walk(packages, library, character.only = TRUE) # load the packages

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Rtools43/usr/bin", sep=";")) # setup Rtools43