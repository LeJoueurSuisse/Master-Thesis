
# ==============================================================================
# Exploration and Visualization for canton statistics
# ==============================================================================

source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

## Cantons Statistics exploration

canton_df_long %>%
  head(10) %>% 
  kable() %>% 
  kable_styling()

canton_df_long %>%
  group_by(Cantons) %>% 
  summarize(n = n()) 

# ==============================================================================

canton_df_long %>% group_by(date, Cantons) %>%
  summarize(total = sum(production)) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index(. ~ "2019-12-31") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

# ==============================================================================


