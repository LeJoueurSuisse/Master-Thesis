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


gadmCHE1 <- st_read("~/GitHub/Master-Thesis/Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_1.shp") %>%
  separate(HASC_1, c('Pays', 'Canton')) %>%
  mutate(test = c(1:10, 30:35, 30:35, 50:53))


ggplot(gadmCHE1) +
  geom_sf(aes(fill = test)) +
  geom_sf_text(aes(label = Canton), size = 3.3, color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank())
