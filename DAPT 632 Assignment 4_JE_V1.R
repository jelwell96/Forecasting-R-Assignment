#Call on required packages 
library("fpp3")
library("GGally")

#View the data
vic_elec
#Already a tsibble

#####
#Chapter 7, Question 1 - Half-hourly electricity demand for Victoria, Australia is contained in vic_elec. Extract the January 2014 electricity demand, and aggregate this data to daily with daily total demands and maximum temperatures.

#From the book - 
jan14_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

# Question 1.a - Plot the data and find the regression model for Demand with temperature as a predictor variable. Why is there a positive relationship?

#Plot the data in a scatterplot with a simple linear regression line
jan14_vic_elec %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  labs(x = "Temperature (C)", y ="Demand") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

#Fit a simple regression model for Demand by Temperature
fit_demand_temp <- jan14_vic_elec %>%
  model(
    tslm = TSLM(Demand ~ Temperature)
  )

#View results of the model
report(fit_demand_temp)

augment(fit_demand_temp)

#The positive relationship between Temperature and Demand for electricity makes sense. When it gets hotter there is more strain on energy consumption as people try to cool off. In other words, demand will increase because AC units are pulling more electricity to maintain comfortable temperatures.


#####
# Question 1.b - Produce a residual plot. Is the model adequate? Are there any outliers or influential observations?
# Hint b.  assess model adequacy based upon what you view in the residuals.. are they white noise?

fit_demand_temp %>% 
  gg_tsresiduals()

df <- left_join(jan14_vic_elec, residuals(fit_demand_temp), by = "Date")

ggplot(df, aes(x=Demand, y=.resid)) +
  geom_point() + ylab("Residuals")

#The innovation residuals appear to have a slightly upward trend. The histogram looks like it is close to a normal distribution on the right half but it is clearly not a normally distributed bell curve on the left half. ACF looks great and remains between the blue dotted lines revealing no correlation. January 1st, 5th, 26th and maybe the 27th would be good to look at as possible outliers.


#####
# Question 1.c - Use the model to forecast the electricity demand that you would expect for the next day if the maximum temperature was  15C and compare it with the forecast if the with maximum temperature was 35C. Do you believe these forecasts? The following R code will get you started:
# Hint - c.   After producing the forecasts, provide a graph of the forecasts and discuss if they seem reasonable

future_scenarios <- scenarios(
  Low_Temp = new_data(jan14_vic_elec, 1) %>%
    mutate(Temperature = 15),
  High_Temp = new_data(jan14_vic_elec, 1) %>%
    mutate(Temperature = 35),
  names_to = "Scenario")

fc_demand_temp <- forecast(fit_demand_temp, new_data = future_scenarios) 

#view forecasts and prediction intervals
fc_demand_temp_ints <- fc_demand_temp %>%
  hilo() %>%
  unpack_hilo()

View(fc_demand_temp_ints)

#plot the forecasted high and low temperature scenarios
jan14_vic_elec %>%
  autoplot(Demand) +
  autolayer(fc_demand_temp) +
  labs(title = "AUS Demand", y = "Demand")

# The demand forecasts seem reasonable when compared to actual values. January actual data does not have a record with the temperature at 15C so it makes sense that our predicted range for 15C would be slightly below the lowest day (January 6th - 19.6C). When looking at our high temperature forecast it also falls comfortably near days with similar temperatures. For example, it is below the highest temperature days when the temperature was in the low 40's.


#####
# Question 1.d - Give prediction intervals for your forecasts.

View(fc_demand_temp_ints)


#####
# Question 1.e - Plot Demand vs Temperature for all of the available data in vic_elec aggregated to daily total demand and maximum temperature. What does this say about your model?
#Hint - e.  Summarize all of vic_elec similar to part a, and then graph Demand vs Temp

#From the book - 
all_vic_elec <- vic_elec |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

all_vic_elec %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  labs(x = "Temperature (C)", y ="Demand") +
  geom_point() 

#This plot tells me my model is wrong when looking at the full data set because we are using a linear fit for nonlinear data. My model does not account for the parabolic nature and says that as temperature decreases so will demand. This concept only works from the upper extreme temperatures to about 20C. 20C is the "floor" of the data and all temperatures lower than 20C begin the reverse trend where as you decrease in temperature demand increases. Given the full data set, a polynomial regression model would likely be a better fit.

#####
#Chapter 7, Question 4 - The data set souvenirs concerns the monthly sales figures of a shop which opened in January 1987 and sells gifts, souvenirs, and novelties. The shop is situated on the wharf at a beach resort town in Queensland, Australia. The sales volume varies with the seasonal population of tourists. There is a large influx of visitors to the town at Christmas and for the local surfing festival, held every March since 1988. Over time, the shop has expanded its premises, range of products, and staff.

# Question 4.a - Produce a time plot of the data and describe the patterns in the graph. Identify any unusual or unexpected fluctuations in the time series.

#View the data
souvenirs
#Already a tsibble

autoplot(souvenirs)

# The plot shows a general positive trend with multiplicative monthly seasonality. It looks like the summer months in 1992 and 1993 seem to be on a significantly steeper positive trend than the previous years. In order identify any significant deviations I will need to do an STL decomposition. 
dcmp <- souvenirs %>%
  model(STL(log(Sales)))

components(dcmp) %>% 
  autoplot() + 
  labs(x = "Year")

#After doing an STL decomposition of the log transformed sales, I do not see any significant deviations in the remainder. There appears to be some unusual activity at 1991 with the trend line where it plateaus and even appears that sales dip for that year.


#####
# Question 4.b - Explain why it is necessary to take logarithms of these data before fitting a model.
# Hint b.  Provide a graph of the log-transformed data

# Taking the log of sales allows us to fit a linear model (or straightens it out). This works for data with an exponential trend or multiplicative seasonality by stabilizing the variance. Without doing so, the extreme values towards the right limit of the plot would skew most models. In taking the log of sales, it also makes the model more robust to outliers.

#Take the log of sales similar to above and use autoplot to graph it
autoplot(souvenirs, log(Sales))


#####
# Question 4.c - Fit a regression model to the logarithms of these sales data with a linear trend, seasonal dummies and a “surfing festival” dummy variable.
#Hint c. The “surfing festival” is described in the problem description. Take note of when the festival began.
#Hint c. Also provide a plot of the data along with the fitted values


#Regression model with linear trend and seasonal dummies
fit_sales <- souvenirs %>%
  model(TSLM(log(Sales) ~ trend() + season()))

#view the regression results
report(fit_sales)

augment(fit_sales)

augment(fit_sales) %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Sales, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(x = "Year", y = "Sales", title = "Souvenir Sales") +
  guides(colour=guide_legend(title=NULL))

#Adding in Surfing festival dummy variable, which begins 1988 and only occurs in March
fit_sales_festival <- souvenirs %>%
  mutate(Surfing_Festival = month(Month) == 3 & year(Month) >= 1988) %>%
  model(TSLM(log(Sales) ~ trend() + season() + Surfing_Festival))

#view the modified regression results which now include the festival
report(fit_sales_festival)

augment(fit_sales_festival)

augment(fit_sales_festival) %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Sales, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(x = "Year", y = "Sales", title = "Souvenir Sales") +
  guides(colour=guide_legend(title=NULL))


#####
# Question 4.d - Plot the residuals against time and against the fitted values. Do these plots reveal any problems with the model?

#Residuals over time
fit_sales_festival %>% 
  gg_tsresiduals()

#Residuals vs fitted values
augment(fit_sales_festival) %>%
  ggplot(aes(x= log(.fitted), y=.innov)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

#Residuals vs. fitted shows homoskedasticity so no worries there. The residuals over time and the histogram of the residuals look good. The main concern comes from the ACF which shows significant correlation at Lags (1M) 1, 2 and 3. This reveals that each data point is influenced by the previous three months, with the strongest influence coming from the most recent month and tapering off as you work backwards in time. 

#####
# Question 4.f - What do the values of the coefficients tell you about each variable?
# Hint f. Remember you transformed!

#the coefficients are in log scale and are difficult to interpret. 
fit_sales_festival %>%
  report()

#To properly interpret the coefficients we need to transform the coefficients.

fit_sales_festival_transform <- tidy(fit_sales_festival) %>%
  mutate(beta_effect = (exp(estimate) - 1) * 100)

view(fit_sales_festival_transform)

#Now I can see the coefficients in terms of percentages within the beta effect column. I can interpret the trend beta effect as each month passes I can expect a 2.2% increase in sales. Each of the SeasonYear#'s represent a month with January being the baseline. If the month in question is December I would look at the Beta_Effect SeasonYear12 and interpret the coefficient as a 611.25% increase when compared to January. This makes sense because of the drastic peak depicted at the end of the year on our plots above. Lastly, the intercept is the effect on sales holding all other variables constant.

#####
# Question 4.g - What does the Ljung-Box test tell you about your model?
# Hint g. Pay attention to the type of residual you use and the degrees of freedom. Assign lag = 24 due to seasonality
augment(fit_sales_festival) %>% 
  features(.innov, ljung_box, lag = 24, dof = 14)

#The Ljung-Box test tells me if there is autocorrelation in the residuals. With a p-value that close to 0, the evidence suggests we reject the null (no autocorrelation among the residuals). In other words, it is very likely there are autocorrelations up to lag 24.

#####
# Question 4.h - Regardless of your answers to the above questions, use your regression model to predict the monthly sales for 1994, 1995, and 1996. Produce prediction intervals for each of your forecasts.
# Hint h.  Create a scenario that adds 36 months of data and use the below code to add the necessary surfing festival variable. Prediction Intervals can be provided in the form of a graph. 

#create a variable for the next three years forecast
Souvenir_Three_Year <- new_data(souvenirs, 36) %>% 
    mutate(Surfing_Festival = month(Month) == 3)

fc_souvenirs <- forecast(fit_sales_festival, new_data = Souvenir_Three_Year)

#view forecasts and prediction intervals
fc_souvenir_ints <- fc_souvenirs %>%
  hilo() %>%
  unpack_hilo()

View(fc_souvenir_ints)

#plot the forecasted next three years of log sales
souvenirs %>%
  autoplot(Sales) +
  autolayer(fc_souvenirs) +
  labs(title = "Three Year Souvenir Forecast", y = "Log Sales")

#####
# Question 4.i - How could you improve these predictions by modifying the model?

#I could improve the model by creating indicator variables where the model significantly deviates from the data. After looking at fitted v. data graph again, I see  significant deviation for March of 1991.

#Add in the new indicator variable
fit_sales_festival_modified <- souvenirs %>%
  mutate(Surfing_Festival = month(Month) == 3 & year(Month) >= 1988,
         March91_Ind = month(Month) == 3 & year(Month) == 1991) %>%
  model(TSLM(log(Sales) ~ trend() + season() + Surfing_Festival + March91_Ind))

augment(fit_sales_festival_modified)

#view the results
augment(fit_sales_festival_modified) %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Sales, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(x = "Year", y = NULL, title = "Souvenir Sales") +
  guides(colour=guide_legend(title=NULL))

fit_sales_festival_modified %>% 
  report()

#Check to see how the modified model performed
glance(fit_sales_festival) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

glance(fit_sales_festival_modified) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
#My modified model shows lower AIC, AICc and BIC while improving the adjusted r squared revealing I have improved my model albeit slightly.

