
# ==============================================================================
# Data Forecast
# ==============================================================================

# Loading the full environment
source(file = here::here("Scripts/Cleaning_and_Wrangling.R"))

################### 
### ARIMA 
###################

cons.m.tot %>%
  autoplot()

# Residuals test
cons.m.tot %>% 
  model(arima = ARIMA(Consumption)) %>% 
  gg_tsresiduals()



# Default R ARIMA
cons.m.fit <- cons.m.tot %>%
  filter_index(. ~ "2021-12") %>%
  model(auto = ARIMA(Consumption, stepwise = FALSE, approximation = FALSE))

cons.m.fit %>%
  forecast(h = 17) %>%
  autoplot(cons.m.tot %>%
             filter_index("2018-01-01" ~ "2021-12"))

# =================================================

cons.m.fit %>%
  accuracy()

cons.d.fit %>%
  accuracy()

#AR1
cons.m.tot %>%
  model(AR1 = ARIMA(Consumption ~ pdq(1,1,0))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot %>%
             filter_index("2020-01-01" ~ .))


#MA1
cons.m.tot %>%
  model(AR1 = ARIMA(Consumption ~ pdq(0,1,1))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot %>%
             filter_index("2020-01-01" ~ .))

#AR111
cons.m.tot %>%
  model(AR1 = ARIMA(Consumption ~ pdq(1,1,1))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot %>%
             filter_index("2020-01-01" ~ .))

#AR211
cons.m.tot %>%
  model(AR1 = ARIMA(Consumption ~ pdq(2,1,1))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot %>%
             filter_index("2020-01-01" ~ .))

#AR112
cons.m.tot %>%
  model(AR1 = ARIMA(Consumption ~ pdq(1,1,2))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot %>%
             filter_index("2020-01-01" ~ .))

#AR212
cons.m.tot %>%
  model(AR1 = ARIMA(Consumption ~ pdq(2,1,2))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot %>%
             filter_index("2020-01-01" ~ .))

# lets check for the best ARIMA
arima.fit <- cons.m.tot %>% 
  model(arima211 = ARIMA(Consumption ~ pdq(2,1,1)),
        arima012 = ARIMA(Consumption ~ pdq(0,1,2)),
        arima210 = ARIMA(Consumption ~ pdq(2,1,0)),
        stepwise = ARIMA(Consumption),
        search = ARIMA(Consumption, stepwise = FALSE))

arima.fit %>%
  glance() %>% arrange(AICc) %>% select(1:7)

arima.fit %>% accuracy()

arima.d.fit <- cons.d.tot %>% 
  model(arima211 = ARIMA(Consumption ~ pdq(2,1,1)),
        arima012 = ARIMA(Consumption ~ pdq(0,1,2)),
        arima210 = ARIMA(Consumption ~ pdq(2,1,0)),
        stepwise = ARIMA(Consumption),
        search = ARIMA(Consumption, stepwise = FALSE))

arima.d.fit %>%
  glance() %>% arrange(AICc) %>% select(1:7)

arima.d.fit %>% accuracy()


################
### ETS
###############

cons.m.tot %>%
  model(ETS(Consumption)) %>%
  gg_tsresiduals()


# Default ETS model analysis
cons.m.tot %>% 
  model(ETS(Consumption)) %>% 
  report()

# SES with different Alpha levels
cons.m.tot %>%
  model(a1 = ETS(Consumption ~ error("A") + trend("N", alpha = 0.1) + season("N")),
        a2 = ETS(Consumption ~ error("A") + trend("N", alpha = 0.4) + season("N")),
        a3 = ETS(Consumption ~ error("A") + trend("N", alpha = 0.9) + season("N"))) %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot)

# Looking for optimal parameters
cons.m.tot %>%
  model(ETS(Consumption ~ error("A") + trend("N") + season("N"),
            opt_crit = "mse")) %>%
  coefficients()
# alpha at 0.96 and L at 6038

# Fitting a model with a high alpha
ets.consm.fit<- cons.m.tot %>%
  model(ETS(Consumption ~ error("A") + trend("N", alpha = 0.9) + season("N"))) 

ets.consm.fit %>% 
  forecast(h = 5) %>% 
  autoplot(cons.m.tot) +
  geom_line(aes(y = .fitted, colour = "Fitted"), data=augment(ets.consm.fit)) +
  ylab("Consumption") + xlab("Year")

# Adding trend and seasonality
ets2.consm.fit <- cons.m.tot %>% model(
  aaa = ETS(Consumption ~ error("A") + trend("A") + season("A")),
  mam = ETS(Consumption ~ error("M") + trend("A") + season("M")),
  aam = ETS(Consumption ~ error("A") + trend("A") + season("M")))

ets2.consm.fit %>% forecast(h = "2 years") %>%
  autoplot(cons.m.tot, level = NULL, size = c(1.2)) + xlab("Year")
# seems like additivity is way better than multiplicativity

ets2.consm.fit %>%
  glance() %>% arrange(AICc) %>% select(1:7)

ets.consm.fit %>% forecast(h = "17 months") %>%
  autoplot(cons.m.tot, level = NULL, size = c(1.2)) + xlab("Year")
##########
##TSLM##
#########

# Residuals test
cons.m.tot %>% 
  model(tslm = TSLM(Consumption)) %>% 
  gg_tsresiduals()

# fitting a TSLM          
tslm.consm.fit <- cons.m.tot %>%
  model(tslm = TSLM(Consumption ~ trend() + season()))

tslm.consm.fit %>%
  forecast(h=17) %>%
  autoplot(cons.m.tot)

cons.m.tot %>%
  autoplot(Consumption, col = "gray") +
  geom_line(data = augment(tslm.consm.fit), aes(y = .fitted), col = "blue")


tslmpw.consm.fit <- cons.m.tot %>%
  model(piecewise = TSLM(Consumption ~ trend(knots = c(2019, 2021))))

augment(tslmpw.consm.fit) %>%
  autoplot(.fitted, color = "red") +
  geom_line(aes(y = Consumption), colour = "black")

augment(tslm.consm.fit) %>%
  autoplot(.fitted, color = "red") +
  geom_line(aes(y = Consumption), colour = "black")


## forecasting with the 2 tslm models
tslm.consm.fit %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot)

tslmpw.consm.fit %>%
  forecast(h=5) %>%
  autoplot(cons.m.tot)

# Checking Best model and accuracy
tslm.consm.fit %>%
  glance() %>% arrange(AICc)

tslmpw.consm.fit %>%
  glance() %>% arrange(AICc) 

# clear bias towards normal TSLM 

tslm.consm.fit %>% accuracy()
tslmpw.consm.fit %>% accuracy()


# Predicting Using the standard TSLM model
tslm.fcst <- cons.m.tot %>%
  model(TSLM(Consumption ~ trend()+season() )) %>%
  forecast(h=5)

tslm.fcst %>% autoplot(cons.m.tot)



# Daily Consumption

## ETS 
ets.d.model <- decomposition_model(
  STL(Consumption),
  ETS(season_adjust ~ season("N"))
)

ets.d.fit <- cons.d.tot %>%
  model(stl_ets = ets.d.model)

ets.d.fit %>%
  forecast(h = 365) %>%
  autoplot(cons.d.tot %>%
             filter_index("2020-01-01" ~ .))

ets.d.model %>%
  accuracy()

## Arima
arima.d.model <- decomposition_model(
  STL(Consumption),
  ARIMA(season_adjust ~ season(12))
)

arima.d.fit <- cons.d.tot %>%
  model(stl_arima = arima.d.model) 

arima.d.fit %>%
  forecast(h = 365) %>%
  autoplot(cons.d.tot %>%
             filter_index("2020-01-01" ~ .))

arima.d.fit %>%
  accuracy()

## TSLM
tslm.d.model <- decomposition_model(
  STL(Consumption),
  TSLM(season_adjust ~ season(12))
)

tslm.d.fit <- cons.d.tot %>%
  model(stl_tslm = tslm.d.model) 

tslm.d.fit %>%
  forecast(h = 365) %>%
  autoplot(cons.d.tot %>%
             filter_index("2020-01-01" ~ .))

tslm.d.model %>%
  accuracy()


