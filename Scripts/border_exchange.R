
# ==============================================================================
# Exploration and Visualization for exchange with borders
# ==============================================================================

source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

## General Statistics exploration

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