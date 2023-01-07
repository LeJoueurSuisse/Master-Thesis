
# ==============================================================================
# Exploration and Visualization
# ==============================================================================

source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

## General Statistics exploration

general_df %>%
  head(10) %>% 
  kable() %>% 
  kable_styling()

# ==============================================================================

## Hourly

general_df %>%
  group_by(time) %>% summarize(Total_cons = sum(energy_cons)) %>%
  as_tsibble() %>%
  filter_index("2020-10-24" ~ "2020-10-25") %>% 
  autoplot() + 
  ggtitle("Total energy consumed by end users") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

# correlation of Hour and Day

general_df %>% 
  group_by(hour2, wday, month2) %>% 
  summarise(Total_cons = sum(energy_cons)/1000000) %>%
  ggplot(aes(wday, hour2, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Hour of the day", fill = "Energy consumed [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

general_df %>% 
  group_by(hour2, wday, month2) %>% 
  summarise(Total_cons = sum(energy_prod)/1000000) %>%
  ggplot(aes(wday, hour2, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Hour of the day", fill = "Energy produced [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

general_df %>% 
  group_by(wday, month2, year2) %>% 
  summarise(Total_cons = sum(energy_cons)/1000000) %>%
  ggplot(aes(month2, wday, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Hour of the day", fill = "Energy consumed [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

general_df %>% 
  group_by(wday, month2, year2) %>% 
  summarise(Total_cons = sum(energy_prod)/1000000) %>%
  ggplot(aes(month2, wday, fill = Total_cons)) +
  geom_tile() +
  labs(x = "Day of the week", y = "Hour of the day", fill = "Energy produced [1m]") +
  scale_fill_distiller(palette = "Spectral") +
  theme_hc()

## Daily

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(end_users_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed by end users") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(end_users_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2020-11-01" ~ "2022-11-01") %>% 
  gg_season(y = Total_cons)

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
  filter_index("2018-11-01" ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in Switzerland") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

## Monthly 

general_dfM <- general_df %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  as_tibble()

general_df %>%
  group_by(month) %>% summarize(Total_cons = sum(end_users_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed by end users") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_df %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed in CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

general_df %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy produced by CH") + 
  ylab("Ammount in million of kWh") + xlab("Date") #Check outliers 

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

bla <- general_dfM %>% 
  mutate(month = month(date)) %>%
  group_by(month, year) %>%
  summarise(Total = sum(energy_prod)/1000000)

ggplot(bla) + 
  geom_bar(aes(y = Total, x = month, fill = year), stat = "identity") +
  coord_flip()+
  facet_grid(.~year) + 
  xlab("Month") +
  scale_colour_discrete("Year") +
  ylab("Total Sales") +
  ggtitle("Total sales of items per month")

## stl decomp

STL_dcmp <- general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-11-01" ~ "2022-11-01") %>%
  model(STL(Total_cons))

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-11-01" ~ "2022-11-01") %>% 
  autoplot(Total_cons, color = "gray") + 
  autolayer(components(STL_dcmp), trend, color = 'red') +
  ggtitle("Total TX_1 store daily sales") + 
  ylab("sales") + xlab("Day") #Check outliers

# ==============================================================================

data_all %>% rename(total = total_energy_consumed_by_end_users_in_the_swiss_controlblock_k_wh) %>%
  group_by(date) %>% summarize(Total_cons = sum(total)) %>% 
  as_tsibble() %>%
  autoplot()

data_all %>% rename(total = total_energy_consumed_by_end_users_in_the_swiss_controlblock_k_wh) %>%
  group_by(month) %>% summarize(Total_cons = sum(total)) %>% 
  as_tsibble() %>%
  autoplot()

################################################################################



