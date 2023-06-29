
# ==============================================================================
# Exploration and Visualization for exchange with borders
# ==============================================================================

source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

## General Statistics exploration

border_df %>%
  select(c(1, 9:15)) %>%
  kable_head()

border_long %>%
  group_by(year, Type) %>%
  summarize(total = sum(Amount)/1000000) %>%
  as_tsibble(index = year, key = Type) %>%
  ggplot(aes(x = as.factor(year), y = total, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(Type), ncol = 3)+
  guides(colour="none") +
  labs(title = "Monthly consumption per canton since 2018",
       subtitle = "Ammount in millions of kWh with free scale",
       y = "", x = "") +
  theme(axis.text.x = element_text(angle=80, hjust=1))



imp_exp_long %>%
  group_by(year, Type) %>%
  summarize(total = sum(Amount)/1000000) %>%
  as_tsibble(index = year, key = Type) %>%
  ggplot(aes(x = as.factor(year), y = total, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(Type), ncol = 3) +
  guides(colour="none") +
  guides(colour="none") +
  labs(title = "Monthly consumption per canton since 2018",
       subtitle = "Ammount in millions of kWh with free scale",
       y = "", x = "")

%>%
  ggplot(aes(x = total)) +
  geom_bar() +
  facet_wrap(vars(Year), ncol = 4) +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")


canton_df_long %>% 
  mutate(month = yearmonth(date), year = year(date)) %>%
  group_by(month, Cantons) %>%
  summarize(total = sum(consumption)/1000000) %>%
  as_tsibble(index = month, key = Cantons) %>%
  mutate(xlabel = "") %>%
  filter_index("2018-01-01" ~ "2022-11-30") %>% 
  autoplot() + 
  facet_wrap(~Cantons, scales = "free") +
  guides(colour="none") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Monthly consumption per canton since 2018",
       subtitle = "Ammount in millions of kWh with free scale",
       y = "", x = "")