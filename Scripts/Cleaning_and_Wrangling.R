# ==============================================================================
# Data management
# ==============================================================================

## Data loading

source(file = here::here("Scripts/SetUp.R"))

data_files <- list.files(here::here("Data/SwissGrid"))

# 2009 to 2014 Data

for(i in 1:6){                            
  assign(paste0("OldData_", 2008 + i), 
         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
                    sheet = "Zeitreihen0h15"))
}

# 2015 to 2022 Data

for(i in 7:length(data_files)){                            
  assign(paste0("Data_", 2008 + i), 
         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
                    sheet = "Zeitreihen0h15"))
}

# ==============================================================================

## Dealing with Date

# Years 2015 to 2020

for(i in 7:(length(data_files)-2)){                            
  assign(paste0("Data_", 2008 + i), 
         mutate(get(paste0("Data_", 2008 + i)), Time = ymd_hms(Timestamp), .before = 1) %>% 
         separate(Timestamp, c('Date', 'Hour'), sep = " ") %>% 
         mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
         mutate(Month = as.Date(cut(Date, breaks = "months")),
                Year = as.Date(cut(Date, breaks = "years"))) %>%
         relocate(c(Year, Month), .after = Time))
}

# Years 2021 & 2022

for(i in 13:(length(data_files))){                            
  assign(paste0("Data_", 2008 + i), 
         mutate(get(paste0("Data_", 2008 + i)), Time = dmy_hm(Timestamp), .before = 1) %>% 
         mutate(Timestamp = Time) %>%
         separate(Timestamp, c('Date', 'Hour'), sep = " ") %>% 
         mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
         mutate(Month = as.Date(cut(Date, breaks = "months")),
                Year = as.Date(cut(Date, breaks = "years"))) %>%
         relocate(c(Year, Month), .after = Time))
}

# ==============================================================================

## Creating sub-DF with related variables

# Groups all the Data into a single DF

data_all <- rbind(Data_2015, Data_2016, Data_2017, Data_2018, Data_2019,
                  Data_2020, Data_2021, Data_2022) %>% clean_names()


# Extracts related variables

my_variables <- colnames(data_all) # all the variable
general_var <- my_variables[1:14] # general statistics (prod, cons, flow)
border_var <- my_variables[c(1:5, 15:25)] # exchange with borders statistics
price_var <- my_variables[c(1:5, 26:29)] # price of control
canton_var <- my_variables[c(1:5, 30:67)] # prod and cons per cantons
foreign_var <- my_variables[c(1:5, 68:69)] # out of Switzerland manage by SwissGrid

# Creates user-friendly tsibble

# General Statistics

general_df <- data_all %>% select(all_of(general_var)) %>%
  rename(end_users_cons = total_energy_consumed_by_end_users_in_the_swiss_controlblock_k_wh,
         energy_prod = total_energy_production_swiss_controlblock_k_wh,
         energy_cons = total_energy_consumption_swiss_controlblock_k_wh,
         net_outflow = net_outflow_of_the_swiss_transmission_grid_k_wh,
         grid_feed_in = grid_feed_in_swiss_transmission_grid_k_wh,
         pos_secund = positive_secundary_control_energy_k_wh,
         neg_secund = negative_secundary_control_energy_k_wh,
         pos_tertiary = positive_tertiary_control_energy_k_wh,
         neg_tertiary = negative_tertiary_control_energy_k_wh) %>%
  #mutate(test = ifelse("19:00:00" >= hour & hour > "07:00:00", 
  #                     "Day", "Night")) %>%
  mutate(test = ifelse(hour > "07:00:00" & hour <= "19:00:00", 
                       "Day", "Night")) %>%
  #mutate(month2 = month(ymd(month), label = TRUE)) %>%
  mutate(month = month(ymd(month), label = TRUE)) %>%
  #mutate(hour2 = hour(hms(hour))) %>%
  mutate(hour = hour(hms(hour))) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1)) %>%
  #mutate(wday = weekdays(date)) %>%
  #mutate(year2 = year(date)) %>%
  mutate(year = year(date)) %>%
  mutate(hourly = format(as.POSIXlt(time), "%Y-%m-%d %H:00:00")) %>%
  mutate(hourly = ymd_hms(hourly))%>%
  relocate(c(hourly, year, month, date, wday, hour, test), .after = time)

# A monthly version

general_dfM <- general_df %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  as_tibble()

# Exchange with borders Statistics

border_df <- data_all %>% select(all_of(border_var))

# Price of controls

price_df <- data_all %>% select(all_of(price_var))

# Statistics per canton

canton_df <- data_all %>% select(all_of(canton_var))

# Statistics for foreign area controlled by SwissGrid

foreign_df <- data_all %>% select(all_of(foreign_var))

# ==============================================================================

## STL decomp

## Daily data for consumption

STL_dcmpC <- general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2022-11-01") %>%
  model(STL(Total_cons))

# 1 year only
STL_dcmpC_1year <- general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(STL(Total_cons))

## Daily data for production

STL_dcmpP <- general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index("2018-01-01" ~ "2022-11-01") %>%
  model(STL(Total_cons))

#1 year only
STL_dcmpP_1year <- general_df %>%
  group_by(date) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2019-12-31") %>%
  model(STL(Total_cons))

# Monthly data for consumption & production

STL_dcmp_MC <- general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_cons)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>%
  model(STL(Total_cons))

STL_dcmp_MP <- general_dfM %>%
  group_by(month) %>% summarize(Total_cons = sum(energy_prod)/1000000) %>%
  as_tsibble() %>%
  filter_index(. ~ "2022-11-01") %>%
  model(STL(Total_cons))

# ==============================================================================

## Clearing the environment

# Data

Data_to_remove <- c()

for(i in 1:8){
      Data_to_remove <- append(Data_to_remove, paste0("Data_", 2014 + i))
      Data_to_remove <- append(Data_to_remove, paste0("OldData_", 2008 + i))
}

remove(list = c(Data_to_remove, "data_all"))

# Values

# List all the objects in the environment
remove(list = ls.str(mode = "character"))
remove(list = ls.str(mode = "numeric"))

# ==============================================================================
