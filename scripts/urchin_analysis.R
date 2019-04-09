## load library and data ----
library(readxl)
urchin_data_r <- read_excel("data/urchin_data_r.xlsx")
View(urchin_data_r)

## modeling ----
urchin_dat <- subset(urchin_data_r, subset = righting_time < 900)

trtmt <- as.factor(urchin_dat$treatment)

tp.model <- lm(righting_time ~ day + time_point + trtmt, data = urchin_dat)
termplot(tp.model, partial.resid = T, se = T)
summary(tp.model)

temp.model <- lm(righting_time ~ day + temperature + trtmt, data = urchin_dat)
termplot(temp.model, partial.resid = T, se = T)
summary(temp.model)

