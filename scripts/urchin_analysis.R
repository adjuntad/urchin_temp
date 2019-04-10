## load library and data ----
library(readxl)
urchin_data_r <- read_excel("data/urchin_data_r.xlsx")
View(urchin_data_r)

## modeling ----
#after looking over the data, one outlier was found, so we removed this data point 
urchin_dat <- subset(urchin_data_r, subset = righting_time < 900)

#change treatment variable to a factor (was character)
trtmt <- as.factor(urchin_dat$treatment)

#regression analysis looking at how righting time was influenced by day, time point, and treatment. 
#we use timepoint here to understand the affect of the point in the tidal cycle. 
#The model was plotted using partial residuals to understand how the response variable is affected by all the independent variables. 
tp.model <- lm(righting_time ~ day + time_point + trtmt, data = urchin_dat)
termplot(tp.model, partial.resid = T, se = T)
summary(tp.model)

#the same type of model is made using temperature rather than time point.
#temperature is used to understand how temperature affects righting time.
temp.model <- lm(righting_time ~ day + temperature + trtmt, data = urchin_dat)
termplot(temp.model, partial.resid = T, se = T)
summary(temp.model)

