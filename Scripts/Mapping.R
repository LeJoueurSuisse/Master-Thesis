# ==============================================================================
# Data mapping
# ==============================================================================

source(file = here::here("Scripts/SetUp.R"))
source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

ggplot(Swiss_data) +
  geom_sf(aes(fill = Consumption)) +
  geom_sf_text(aes(label = Canton), size = 3.3, color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank())

ggplot(Swiss_data) +
  geom_sf(aes(fill = Population)) +
  geom_sf_text(aes(label = Canton), size = 3.3, color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank())

ggplot(Swiss_data) +
  geom_sf(aes(fill = Production)) +
  geom_sf_text(aes(label = Canton), size = 3.3, color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank())
