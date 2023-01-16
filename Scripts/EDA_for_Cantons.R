
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

# Best consumer
canton_df_long %>% 
  filter(year > 2017) %>%
  group_by(Cantons) %>% 
  summarize(Total_cons = sum(consumption)/1000000) %>%
  ggplot(aes(x = Total_cons, y = Cantons, fill = Cantons)) + 
  geom_col(color = "black") + 
  guides(fill = FALSE)

top5_consumer <- canton_df_long %>% group_by(Cantons) %>% 
  summarize(Total_cons = sum(consumption)) %>%
  arrange(desc(Total_cons)) %>%
  slice(1:5)

top5_consumer <- top5_consumer$Cantons

# Best producer
canton_df_long %>% 
  filter(year > 2017) %>%
  group_by(Cantons) %>% 
  summarize(Total_prod = sum(production)/1000000) %>%
  ggplot(aes(x = Total_prod, y = Cantons, fill = Cantons)) + 
  geom_col(color = "black") +
  guides(fill = FALSE)

top5_producer <- canton_df_long %>% group_by(Cantons) %>% 
  summarize(Total_prod = sum(production)) %>%
  arrange(desc(Total_prod)) %>%
  slice(1:5)

top5_producer <- top5_producer$Cantons

# ==============================================================================

## Quick visualization

# Daily consumption
canton_df_long %>% group_by(date, Cantons) %>%
  summarize(total = sum(consumption)) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>% 
  autoplot() + 
  ggtitle("Daily consumption per canton in 2019") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Daily production
canton_df_long %>% group_by(date, Cantons) %>%
  summarize(total = sum(production)) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>% 
  autoplot() + 
  ggtitle("Daily production per canton in 2019") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Monthly consumption
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(consumption)) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  ggtitle("Monthly consumption per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Monthly production
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  ggtitle("Monthly production per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# ==============================================================================

## Facet wrap by Cantons

# Consumption
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(consumption)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons) +
  ggtitle("Monthly consumption per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Production
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons) +
  ggtitle("Monthly production per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Monthly consumption with free scale
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(consumption)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons, scales = "free") +
  ggtitle("Monthly consumption per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Monthly production with free scale
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons, scales = "free") +
  ggtitle("Monthly production per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# ==============================================================================

## STL decomposition

# Daily consumption for 2019
canton_df_long %>% group_by(date, Cantons) %>% 
  summarize(Total_cons = sum(consumption)) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(STL(Total_cons)) %>%
  components() %>% autoplot()

# Daily production for 2019
canton_df_long %>% group_by(date, Cantons) %>% 
  summarize(Total_prod = sum(production)) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(STL(Total_prod)) %>%
  components() %>% autoplot()

# Monthly consumption
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_cons = sum(consumption)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  model(STL(total_cons))%>%
  components() %>% autoplot()

# Monthly production
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_prod = sum(production)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  model(STL(total_prod))%>%
  components() %>% autoplot() +
  facet_wrap(~Cantons)

# ==============================================================================

## Features analysis

canton_features <- canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_cons = sum(consumption)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>% 
  features(total_cons, feat_stl)

canton_features %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Cantons)) +
  geom_point(size = 2)

# ==============================================================================



























