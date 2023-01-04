# ==============================================================================
# Data management
# ==============================================================================

## Data loading

source(file = here::here("Scripts/SetUp.R"))

data_files <- list.files(here::here("Data/SwissGrid"))

## 2009 to 2014 Data

for(i in 1:6){                            
  assign(paste0("OldData_", 2008 + i), 
         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
                    sheet = "Zeitreihen0h15"))
}

## 2015 to 2022 Data

for(i in 7:length(data_files)){                            
  assign(paste0("Data_", 2008 + i), 
         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
                    sheet = "Zeitreihen0h15"))
}

# ==============================================================================

## Data cleaning

Data_2015 <- Data_2015 %>% 
  mutate(Time = ymd_hms(Data_2015$Timestamp), .before = 1) %>% 
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2016 <- Data_2016 %>% 
  mutate(Time = ymd_hms(Data_2016$Timestamp), .before = 1) %>% 
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2017 <- Data_2017 %>% 
  mutate(Time = ymd_hms(Data_2017$Timestamp), .before = 1) %>% 
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2018 <- Data_2018 %>% 
  mutate(Time = ymd_hms(Data_2018$Timestamp), .before = 1) %>% 
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2019 <- Data_2019 %>% 
  mutate(Time = ymd_hms(Data_2019$Timestamp), .before = 1) %>% 
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2020 <- Data_2020 %>% 
  mutate(Time = ymd_hms(Data_2020$Timestamp), .before = 1) %>% 
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2021 <- Data_2021 %>% 
  mutate(Time = dmy_hm(Data_2021$Timestamp), .before = 1) %>% 
  mutate(Timestamp = Time) %>%
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

Data_2022 <- Data_2022 %>% 
  mutate(Time = dmy_hm(Data_2022$Timestamp), .before = 1) %>% 
  mutate(Timestamp = Time) %>%
  separate(Timestamp, c('Date', 'Hour'), sep = " ")

################################################################################

## Data grouping

data_all <- rbind(Data_2015, Data_2016, Data_2017, Data_2018, Data_2019,
                  Data_2020, Data_2021, Data_2022) %>% clean_names()

data_all$date <- as.Date(data_all$date, format = "%Y-%m-%d")

data_all$month <- as.Date(cut(data_all$date, breaks = "months"))

data_all <- data_all %>% relocate(month, .after = time)

## Data Set splitting

my_variables <- colnames(data_all) # all the variable
general_var <- my_variables[1:13] # general statistics (prod, cons, flow)
border_var <- my_variables[c(1:4, 14:24)] # exchange with borders statistics
price_var <- my_variables[c(1:4, 25:28)] # price of control
canton_var <- my_variables[c(1:4, 29:66)] # prod and cons per cantons
foreign_var <- my_variables[c(1:4, 67:68)] # out of Switzerland manage by SwissGrid

general_df <- data_all %>% select(all_of(general_var)) %>%
  rename(end_users_cons = total_energy_consumed_by_end_users_in_the_swiss_controlblock_k_wh,
         energy_prod = total_energy_production_swiss_controlblock_k_wh,
         energy_cons = total_energy_consumption_swiss_controlblock_k_wh,
         net_outflow = net_outflow_of_the_swiss_transmission_grid_k_wh,
         grid_feed_in = grid_feed_in_swiss_transmission_grid_k_wh,
         pos_secund = positive_secundary_control_energy_k_wh,
         neg_secund = negative_secundary_control_energy_k_wh,
         pos_tertiary = positive_tertiary_control_energy_k_wh,
         neg_tertiary = negative_tertiary_control_energy_k_wh)

border_df <- data_all %>% select(all_of(border_var))
price_df <- data_all %>% select(all_of(price_var))
canton_df <- data_all %>% select(all_of(canton_var))
foreign_df <- data_all %>% select(all_of(foreign_var))

# ==============================================================================

## General Statistics exploration

general_df %>%
  head(10) %>% 
  kable() %>% 
  kable_styling()

# ==============================================================================

## Daily 

general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(end_users_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>% 
  autoplot() + 
  ggtitle("Total energy consumed by end users") + 
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



