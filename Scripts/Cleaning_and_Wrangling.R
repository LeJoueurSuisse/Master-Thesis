# ==============================================================================
# Data management
# ==============================================================================

## Data loading

source(file = here::here("Scripts/SetUp.R"))

data_files <- list.files(here::here("Data/SwissGrid"))

# 2009 to 2014 Data

#for(i in 1:6){                            
#  assign(paste0("OldData_", 2008 + i), 
#         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
#                    sheet = "Zeitreihen0h15"))
#}

# 2015 to 2022 Data

for(i in 7:length(data_files)){                            
  assign(paste0("Data_", 2008 + i), 
         read_excel(paste0("~/GitHub/Master-Thesis/Data/SwissGrid/", data_files[i]), 
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

# Groups all the Data into a single for general use

data_all <- rbind(Data_2015, Data_2016, Data_2017, Data_2018, Data_2019,
                  Data_2020, Data_2021, Data_2022) %>% clean_names() %>%
  mutate(day_time = ifelse(hour > "07:00:00" & hour <= "19:00:00", 
                       "Day", "Night")) %>%
  mutate(month = month(ymd(month), label = TRUE)) %>%
  mutate(hour = hour(hms(hour))) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1)) %>%
  mutate(year = year(date)) %>%
  mutate(hourly = format(as.POSIXlt(time), "%Y-%m-%d %H:00:00")) %>%
  mutate(hourly = ymd_hms(hourly))%>%
  relocate(c(hourly, year, month, date, wday, hour, day_time), .after = time)


# Grouping related variables

my_variables <- colnames(data_all) # all the variable
times_var <- c(my_variables[1:8]) # time variable only
general_var <- my_variables[c(1:11, 14:17)] # general statistics 
border_var <- my_variables[c(1:8, 18:28)] # exchange with borders statistics
price_var <- my_variables[c(1:8, 29:32)] # price of control
canton_var <- my_variables[c(1:8, 33:70)] # prod and cons per cantons
foreign_var <- my_variables[c(1:8, 71:72)] # out of Switzerland manage by SwissGrid

# ==============================================================================

## Creates user-friendly tsibble

# General Statistics

general_df <- data_all %>% select(all_of(general_var)) %>%
  rename(end_users_cons = total_energy_consumed_by_end_users_in_the_swiss_controlblock_k_wh,
         energy_prod = total_energy_production_swiss_controlblock_k_wh,
         energy_cons = total_energy_consumption_swiss_controlblock_k_wh,
         pos_second = positive_secundary_control_energy_k_wh,
         neg_second = negative_secundary_control_energy_k_wh,
         pos_tertiary = positive_tertiary_control_energy_k_wh,
         neg_tertiary = negative_tertiary_control_energy_k_wh)

# A monthly version

general_dfM <- general_df %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  as_tibble()

# ==============================================================================

### Statistics per canton

canton_df <- data_all %>% select(all_of(canton_var))

## Dividing the data between production and consumption

# production

canton_prod <- canton_df[, -45] %>% 
  select(all_of(times_var), starts_with("prod"))

colnames(canton_prod) <- c(times_var, "argovie", "fribourg", "glaris", "grisons",
                            "lucerne", "neuchatel", "soleure", "saint_gall", "tessin",
                            "thurgovie", "valais", "appenzell", "bale", "berne_jura",
                            "schwytz_zoug", "obw_nidw_uri", "geneve_vaud", 
                            "schaff_zurich")

canton_long_prod <- canton_prod %>%
  pivot_longer(-c(canton_var[1:8]), names_to = "Cantons", values_to = "production")

# consumption

canton_cons <- canton_df[, -46] %>%
  select(all_of(times_var), starts_with("cons"))

colnames(canton_cons) <- c(times_var, "argovie", "fribourg", "glaris", "grisons",
                            "lucerne", "neuchatel", "soleure", "saint_gall", "tessin",
                            "thurgovie", "valais", "appenzell", "bale", "berne_jura",
                            "schwytz_zoug", "obw_nidw_uri", "geneve_vaud", 
                            "schaff_zurich")

canton_long_cons <- canton_cons %>%
  pivot_longer(-c(canton_var[1:8]), names_to = "Cantons", values_to = "consumption")

# combine production and consumption in a single tsibble

canton_df_long <- canton_long_prod %>%
  mutate(consumption = canton_long_cons$consumption)

# Gets the top 3 cantons for both cons and pro

# Top consumer
top3_consumer <- canton_df_long %>% 
  group_by(Cantons) %>% 
  summarize(Total_cons = sum(consumption)) %>%
  arrange(desc(Total_cons)) %>%
  slice(1:3)

top3_consumer <- top3_consumer$Cantons

# Top producer
top3_producer <- canton_df_long %>% 
  group_by(Cantons) %>% 
  summarize(Total_prod = sum(production)) %>%
  arrange(desc(Total_prod)) %>%
  slice(1:3)

top3_producer <- top3_producer$Cantons

## Features analysis

canton_features_C <- canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_cons = sum(consumption)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>% 
  features(total_cons, feat_stl)


canton_features_P <- canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total_prod = sum(production)/1000000) %>% 
  as_tsibble(index = month, key = Cantons) %>% 
  features(total_prod, feat_stl)

# ==============================================================================

# Statistics for foreign area controlled by SwissGrid

foreign_df <- data_all %>% select(all_of(foreign_var))

# Exchange with borders Statistics

border_df <- data_all %>% select(all_of(border_var)) %>%
  rename(Ch_At = cross_border_exchange_ch_at_k_wh,
         At_Ch = cross_border_exchange_at_ch_k_wh,
         Ch_Ge = cross_border_exchange_ch_de_k_wh,
         Ge_Ch = cross_border_exchange_de_ch_k_wh,
         Ch_Fr = cross_border_exchange_ch_fr_k_wh,
         Fr_Ch = cross_border_exchange_fr_ch_k_wh,
         Ch_It = cross_border_exchange_ch_it_k_wh,
         It_Ch = cross_border_exchange_it_ch_k_wh) %>%
  select(c(1, 3, 9:19))


border_long <- border_df %>%
  select(c(1:10)) %>%
  pivot_longer(c(3:10), names_to = "Type", values_to = "Amount")

imp_exp_long <- border_df %>%
  select(c(1:2, 11:13)) %>%
  pivot_longer(c(3:5), names_to = "Type", values_to = "Amount")


# Price of controls

price_df <- data_all %>% select(all_of(price_var))

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

# 1 year only
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

# Secondary data for consumption & production

STL_dcmp_PS <- general_df %>%
  group_by(date) %>% summarize(Total_pos = sum(pos_second)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  model(STL(Total_pos))

STL_dcmp_NS <- general_df %>%
  group_by(date) %>% summarize(Total_neg = sum(neg_second)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  model(STL(Total_neg))

# Tertiary data for consumption & production
STL_dcmp_PT <- general_df %>%
  group_by(date) %>% summarize(Total_pos = sum(pos_tertiary)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  model(STL(Total_pos))

STL_dcmp_NT <- general_df %>%
  group_by(date) %>% summarize(Total_neg = sum(neg_tertiary)/1000) %>%
  as_tsibble() %>%
  filter_index("2019-01-01" ~ "2022-11-01") %>% 
  model(STL(Total_neg))

# ==============================================================================

## Data for mapping 

#gadmCHE0 <- st_read("~/GitHub/Master-Thesis/Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_0.shp")
#gadmCHE1 <- st_read("~/GitHub/Master-Thesis/Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_1.shp")
#gadmCHE2 <- st_read("~/GitHub/Master-Thesis/Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_2.shp")
#gadmCHE3 <- st_read("~/GitHub/Master-Thesis/Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_3.shp")

#Cantons_mapping <- gadmCHE1 %>%
#  separate(HASC_1, c('Pays', 'Canton'))

# map of swzitzerland
gadmCHE1 <- st_read("~/GitHub/Master-Thesis/Data/Switzerland/gadm36_CHE_shp/gadm36_CHE_1.shp") %>%
  separate(HASC_1, c('Pays', 'Canton')) %>%
  arrange(Canton)

# population and size of canton
Swiss <- read_excel("~/GitHub/Master-Thesis/Data/Switzerland/OFSP/Swiss_data.xlsx")

gadmCHE1_cons <- canton_df_long %>% 
  group_by(Cantons) %>% 
  summarize(Total_cons = sum(consumption)/1000000) %>%
  mutate(Cantons = ifelse(Cantons == "appenzell", "appenzExt_appenzInt", Cantons)) %>%
  mutate(Cantons = ifelse(Cantons == "saint_gall", "saint gall", Cantons)) %>%
  mutate(Cantons = ifelse(Cantons == "bale", "baleCamp_baleVille", Cantons)) %>%
  separate_rows(Cantons, sep = "_") %>%
  arrange(Cantons) %>%
  mutate(Short = Swiss$Short) %>%
  arrange(Short)
  

gadmCHE1_prod <- canton_df_long %>% 
  group_by(Cantons) %>% 
  summarize(Total_prod = sum(production)/1000000) %>%
  mutate(Cantons = ifelse(Cantons == "appenzell", "appenzExt_appenzInt", Cantons)) %>%
  mutate(Cantons = ifelse(Cantons == "saint_gall", "saint gall", Cantons)) %>%
  mutate(Cantons = ifelse(Cantons == "bale", "baleCamp_baleVille", Cantons)) %>%
  separate_rows(Cantons, sep = "_") %>%
  arrange(Cantons) %>%
  mutate(Short = Swiss$Short) %>%
  arrange(Short)

Swiss <- Swiss %>%
  arrange(Short)  

Swiss_data <- gadmCHE1 %>%
  mutate(Total_cons = gadmCHE1_cons$Total_cons,
         Total_prod = gadmCHE1_prod$Total_prod,
         Population = Swiss$Pop,
         Size = Swiss$Size)

# we have to ajust data for grouped area 
# consumption will be based on population and production on the size of the canton

cons_corr <- c(1, 0.23, 0.77, 0.93, 0.6, 0.4, 1, 0.38, 1, 1, 
               0.07, 1, 1, 0.37, 0.32, 1, 0.05, 1, 0.56, 1, 1,
               0.31, 0.62, 1, 0.44, 0.95)

prod_corr <- c(1, 0.42, 0.58, 0.88, 0.93, 0.07, 1, 0.08, 1, 1, 
               0.12, 1, 1, 0.15, 0.27, 1, 0.15, 1, 0.79, 1, 1, 
               0.58, 0.92, 1, 0.21, 0.85)


Swiss_data <- Swiss_data %>%
  mutate(correction_con = cons_corr,
         correction_pro = prod_corr) %>%
  mutate(Consumption = Total_cons*correction_con,
         Production = Total_prod*correction_pro)

# we have to ajust data for grouped area 
# consumption will be based on population and production on the size of the canton

# ==============================================================================

## Clearing the environment

# Data

Data_to_remove <- c("canton_df", "canton_long_cons", "canton_long_prod", 
                    "canton_cons", "canton_prod", "gadmCHE1", "Swiss", 
                    "gadmCHE1_cons", "gadmCHE1_prod")

for(i in 1:8){
  Data_to_remove <- append(Data_to_remove, paste0("Data_", 2014 + i))
  # Data_to_remove <- append(Data_to_remove, paste0("OldData_", 2008 + i))
}

remove(list = c(Data_to_remove, "data_all"))

# Values

# List all the objects in the environment
remove(list = setdiff(ls.str(mode = "character"), c("top3_producer", "top3_consumer")))
remove(list = ls.str(mode = "numeric"))

# ==============================================================================

