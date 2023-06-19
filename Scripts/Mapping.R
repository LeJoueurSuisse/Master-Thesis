# ==============================================================================
# Data mapping
# ==============================================================================

# load the data for Switzerland
source(file = here::here("Scripts/SetUp.R"))
source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

gadmCHE0 <- st_read("Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_0.shp")
gadmCHE1 <- st_read("Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_1.shp")
gadmCHE2 <- st_read("Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_2.shp")
gadmCHE3 <- st_read("Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_3.shp")


gadmCHE1 <- st_read("Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_1.shp") %>%
  mutate(test = c(1:10, 30:35, 30:35, 50:53))


ggplot(gadmCHE1) +
  geom_sf(aes(fill = test)) +
  geom_sf_text(aes(label = NAME_1), size = 3.3, color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank())


######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE,
  scipen = 999
)

# ggplot options
theme_set(theme_light())

# knitr options
opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE,
  warning = FALSE
)

# creating function for a nice kable
kable_maker <- function(a_tibble, ...) {
  a_tibble %>%
    kable(longtable = TRUE,align='l',...) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    `if`(nrow(a_tibble) > 5, (.) %>% scroll_box(height = "260px"), .) %>% 
    `if`(ncol(a_tibble) > 20, (.) %>% scroll_box(height = "120px"), .)
}

# using the kable_maker to display the first 5 rows for exercise 3
kable_head <- function(a_tibble,...){
  a_tibble %>% 
    head(5) %>% kable_maker(...)
}