
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
  select(c(1, 9:11)) %>%
  kable_head()

canton_df_long %>%
  group_by(Cantons) %>% 
  summarize(n = n())

unique(canton_df_long$Cantons)

# Best consumer
canton_df_long %>% 
  filter(year > 2017) %>%
  group_by(Cantons) %>% 
  summarize(Total_cons = sum(consumption)/1000000) %>%
  ggplot(aes(x = Total_cons, y = Cantons, fill = Cantons)) + 
  geom_col(color = "black") + 
  guides(fill = FALSE) +
  ggtitle("Total consumption per canton since 2018, in millions")

# Best producer
canton_df_long %>% 
  filter(year > 2017) %>%
  group_by(Cantons) %>% 
  summarize(Total_prod = sum(production)/1000000) %>%
  ggplot(aes(x = Total_prod, y = Cantons, fill = Cantons)) + 
  geom_col(color = "black") +
  guides(fill = FALSE) +
  ggtitle("Total production per canton since 2018, in millions")

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

# Top 3 consumer
canton_df_long %>% 
  filter(Cantons %in% top3_consumer) %>%
  group_by(date, Cantons) %>%
  summarize(total = sum(consumption)/1000) %>%
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

# Top 3 producer
canton_df_long %>% 
  filter(Cantons %in% top3_producer) %>%
  group_by(date, Cantons) %>%
  summarize(total = sum(production)/1000) %>%
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

# Monthly consumption top 3
canton_df_long %>% 
  filter(Cantons %in% top3_producer) %>%
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(consumption)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  ggtitle("Monthly consumption per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date")

# Monthly production top 3
canton_df_long %>% 
  filter(Cantons %in% top3_producer) %>%
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)/1000000) %>%
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
  guides(colour="none") +
  ggtitle("Monthly consumption per canton since 2018") + 
  ylab("Ammount in million of kWh") + xlab("Date") +
  labs(title = "Monthly consumption per canton since 2018",
       y = "Ammount in million of kWh", x = "") +
  theme(axis.text.x = element_text(angle=50, hjust=1))

# Production
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons) +
  guides(colour="none") +
  labs(title = "Monthly production per canton since 2018",
       y = "Ammount in million of kWh", x = "") +
  theme(axis.text.x = element_text(angle=50, hjust=1))

# Monthly consumption with free scale
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(consumption)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons, scales = "free") +
  guides(colour="none") +
  labs(title = "Monthly consumption per canton since 2018",
       subtitle = " free scale",
       y = "Ammount in million of kWh", x = "") +
  theme(axis.text.x = element_text(angle=50, hjust=1))

# Monthly production with free scale
canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(production)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons, scales = "free") +
  guides(colour="none") +
  labs(title = "Monthly production per canton since 2018",
       subtitle = " free scale",
       y = "Ammount in million of kWh", x = "") +
  theme(axis.text.x = element_blank())

# ==============================================================================

## STL decomposition

# Daily consumption for 2019
canton_df_long %>% 
  filter(Cantons %in% top3_consumer) %>%
  group_by(date, Cantons) %>% 
  summarize(Total_cons = sum(consumption)/1000000) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(STL(Total_cons)) %>%
  components() %>% autoplot()

# Daily production for 2019
canton_df_long %>% 
  filter(Cantons %in% top3_producer) %>%
  group_by(date, Cantons) %>% 
  summarize(Total_prod = sum(production)/1000000) %>%
  as_tsibble(index = date, key = Cantons) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(STL(Total_prod)) %>%
  components() %>% autoplot()

# Monthly consumption
canton_df_long %>% 
  filter(Cantons %in% top3_consumer) %>%
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_cons = sum(consumption)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  model(STL(total_cons))%>%
  components() %>% autoplot()

# Monthly production
canton_df_long %>% 
  filter(Cantons %in% top3_producer) %>%
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_prod = sum(production)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  model(STL(total_prod))%>%
  components() %>% autoplot() + 
  guides(
    colour = guide_legend(title = "Cantons")
  ) +
  scale_color_manual(labels = c("argovie", "soleure", "valais"), 
                     values = c("#F8766D", "#00BA38", "#619CFF"))



# ==============================================================================

## Features analysis
canton_features_C %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Cantons)) +
  geom_point(size = 2)

canton_features_P %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Cantons)) +
  geom_point(size = 2)

# ==============================================================================



























