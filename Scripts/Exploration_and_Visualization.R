
# ==============================================================================
# Exploration and Visualization for general statistics
# ==============================================================================

source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

## General Statistics exploration

general_df %>%
  head(10) %>% 
  kable() %>% 
  kable_styling()

# ==============================================================================

## Hourly Data
# effect of the day

# every 15mim data
general_df %>%
  group_by(time) %>% summarize(Total_cons = sum(energy_cons)) %>%
  as_tsibble() %>%
  filter_index("2019-01-06" ~ "2019-01-30") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

#hourly data
general_df %>%
  group_by(hourly) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>% 
  filter_index("2019-01-06" ~ "2019-01-30") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers

general_df %>% 
  group_by(hour, wday) %>% 
  summarise(Total_cons = sum(energy_cons)/10000000) %>%
  ggplot(aes(wday, hour, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Hour of the day", fill = "Energy consumed [10m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

general_df %>%
  group_by(hourly) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>% 
  filter_index("2019-01-06" ~ "2019-01-30") %>% 
  autoplot() + 
  ggtitle("Total energy consumed by end users") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers

general_df %>% 
  group_by(hour, wday) %>% 
  summarise(Total_cons = sum(energy_prod)/10000000) %>%
  ggplot(aes(wday, hour, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Hour of the day", fill = "Energy consumed [10m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

# Plot the data, grouping by the day of the week
ggplot(general_df %>% 
         group_by(wday, date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
         as_tsibble(index = date) %>%
         filter_index("2019-01-01" ~ "2019-12-31") %>%
         mutate(month = month(ymd(date), label = TRUE)), aes(x = date, y = Total_cons, color = wday)) +
  geom_line()

# ==============================================================================

## Daily
# effect of the month

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-11-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in Switzerland") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers

general_df %>%
  group_by(date) %>% summarize(Total_prod = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-11-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy produced in Switzerland") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

 
general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2020-12-31") %>% 
  gg_season(y = Total_cons)

general_df %>%
  group_by(wday, date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble(index = date) %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  mutate(month = month(ymd(date), label = TRUE)) %>%
  ggplot(aes(month, wday, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Month of the year", fill = "Energy consumed [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

general_df %>% 
  group_by(wday, month, year) %>% 
  summarise(Total_cons = sum(energy_prod)/1000000) %>%
  ggplot(aes(month, wday, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Month of the year", fill = "Energy produced [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc() # produce less on weekend

## With Monthly data 

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(end_users_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed by end users") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy produced by CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

## Monthly seasonality

general_dfM %>%
  group_by(month) %>%
  summarise(Total = sum(end_users_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2016-01-01" ~ "2022-11-01") %>%
  gg_subseries(Total) +
  ylab("Total sales") + 
  xlab("Month") +
  ggtitle("Seasonal subseries plot: End users consumption")

general_dfM %>%
  group_by(month) %>%
  summarise(Total = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2016-01-01" ~ "2022-11-01") %>%
  gg_subseries(Total) +
  ylab("Total sales") + 
  xlab("Month") +
  ggtitle("Seasonal subseries plot: End users consumption")

general_dfM %>%
  group_by(month) %>%
  summarise(Total = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index("2016-01-01" ~ "2022-11-01") %>%
  gg_subseries(Total) +
  ylab("Total sales") + 
  xlab("Month") +
  ggtitle("Seasonal subseries plot: Energy production")

general_dfM %>% 
  mutate(month = month(date)) %>%
  group_by(month, year) %>%
  summarise(Total = sum(energy_prod)/10000000) %>%
  ggplot() + 
  geom_bar(aes(y = Total, x = month, fill = year), stat = "identity") +
  coord_flip()+
  facet_grid(.~year) + 
  xlab("Month") +
  scale_colour_discrete("Year") +
  ylab("Total Sales") +
  ggtitle("Total production of energy per month")

general_dfM %>% 
  mutate(month = month(date)) %>%
  group_by(month, year) %>%
  summarise(Total = sum(energy_cons)/10000000) %>%
  ggplot() + 
  geom_bar(aes(y = Total, x = month, fill = year), stat = "identity") +
  coord_flip()+
  facet_grid(.~year) + 
  xlab("Month") +
  scale_colour_discrete("Year") +
  ylab("Total Sales") +
  ggtitle("Total production of energy per month")

# correlation of Month and Year

general_df %>% 
  group_by(date, month, year) %>% 
  summarise(Total_cons = sum(energy_cons)/1000000) %>%
  ggplot(aes(month, year, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Month of the year", fill = "Energy consumed [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

general_df %>% 
  group_by(date, month, year) %>% 
  summarise(Total_cons = sum(energy_prod)/1000000) %>%
  ggplot(aes(month, year, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Month of the year", fill = "Energy produced [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

# ==============================================================================

## stl decomp

# Daily consumption
components(STL_dcmpC) %>% autoplot()
components(STL_dcmpC_1year) %>% autoplot()

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(classical_decomposition(Total_cons, type =
                                  "additive")) %>%
  components() %>%
  autoplot() + xlab("Year")


# Daily production
components(STL_dcmpP) %>% autoplot()
components(STL_dcmpP_1year) %>% autoplot()

# Monthly consumption & production
components(STL_dcmp_MC) %>% autoplot()
components(STL_dcmp_MP) %>% autoplot()

# ==============================================================================

## Trend component

# Consumption

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2022-11-01") %>% 
  autoplot(Total_cons, color = "gray") + 
  autolayer(components(STL_dcmpC), trend, color = 'red') +
  ggtitle("Total TX_1 store daily sales") + 
  ylab("sales") + xlab("Day")

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot(Total_cons, color = "gray") + 
  autolayer(components(STL_dcmp_MC), trend, color = 'red') +
  ggtitle("Total TX_1 store daily sales") + 
  ylab("sales") + xlab("Day")

# Production

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2022-11-01") %>% 
  autoplot(Total_cons, color = "gray") + 
  autolayer(components(STL_dcmpP), trend, color = 'red') +
  ggtitle("Total TX_1 store daily sales") + 
  ylab("sales") + xlab("Day")

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot(Total_cons, color = "gray") + 
  autolayer(components(STL_dcmp_MP), trend, color = 'red') +
  ggtitle("Total TX_1 store daily sales") + 
  ylab("sales") + xlab("Day")

# ==============================================================================

# Autocorrelation 

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2022-11-01") %>% 
  ACF(Total_cons) %>% 
  autoplot()

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_cons)) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  ACF(Total_cons) %>% 
  autoplot()

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_prod)) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2022-11-01") %>% 
  ACF(Total_cons) %>% 
  autoplot()

general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_prod)) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  ACF(Total_cons) %>% 
  autoplot()

# ==============================================================================
# ==============================================================================

## Secund/tertiary component

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(pos_secund)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Positive secundary control energy") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(neg_secund)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Negative secundary control energy") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(pos_tertiary)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Positive tertiary control energy") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(neg_tertiary)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Negative tertiary control energy") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 
