source(file = here::here("Scripts/SetUp.R"))

data_files <- list.files(here::here("Data/SwissGrid"))

for(i in 1:6){                            
  assign(paste0("OldData_", 2008 + i), 
         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
                    sheet = "Zeitreihen0h15"))
}

for(i in 7:length(data_files)){                            
  assign(paste0("Data_", 2008 + i), 
         read_excel(paste0("Data/SwissGrid/", data_files[i]), 
                    sheet = "Zeitreihen0h15"))
}

################################################################################

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


data_all <- rbind(Data_2015, Data_2016, Data_2017, Data_2018, Data_2019,
                  Data_2020, Data_2021, Data_2022)

data_all$Date <- as.Date(data_all$Date, format = "%Y-%m-%d")

################################################################################

library(ggplot2)
library(tsibble)
library(feasts)

data_all %>% rename(total = "Total energy consumed by end users in the Swiss controlblock (kWh)") %>%
  group_by(Date) %>% summarize(Total_cons = sum(total)) %>% 
  as_tsibble() %>%
  autoplot()
