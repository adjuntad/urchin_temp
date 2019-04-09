## load libraries and data ---------
library(readxl)
urchin_data_r <- read_excel("Desktop/urchin-final paper stuff/urchin_temp/data/urchin_data_r.xlsx")
View(urchin_data_r)
library(ggplot2)
library(gridExtra)
library(scatterplot3d)
library(MASS)
library(dplyr)

## explore data --------
ggplot(data = urchin_data_r)
  geom_boxplot(mapping = aes(x = treatment, y = righting_time)) +
  labs(x = "Treatment", y = "Righting Time (sec)")
day1 <- as.character(urchin_data_r$day)
ggplot(data = urchin_data_r) +
  geom_boxplot(mapping = aes(x = day1, y = righting_time)) +
  labs(x = "Day", y = "Righting Time (sec)")
tp <- as.character(urchin_data_r$time_point)
ggplot(data = urchin_data_r) +
  geom_boxplot(mapping = aes(x = tp, y = righting_time)) +
  labs(x = "Time Point", y = "Righting Time (sec)")

## plotting subsetted data ----------------
urchintemp_data <- subset(urchin_data_r, select = c(day , treatment , time_point , righting_time))

day1datahw <- filter(urchintemp_data, day == "1", treatment == "heat wave")
plot(day1datahw$time_point , day1datahw$righting_time, main = "day1datahw", xlab = "Time Point", ylab = "Righting Time (sec)")

day1dataa <- filter(urchintemp_data, day == "1", treatment == "ambient")
plot(day1dataa$time_point, day1dataa$righting_time, main = "day1dataa", xlab = "Time Point", ylab = "Righting Time (sec)") 

day2datahw <- filter(urchintemp_data, day == "2", treatment == "heat wave")
plot(day2datahw$time_point, day2datahw$righting_time, main = "day2datahw", xlab = "Time Point", ylab = "Righting Time (sec)")

day2dataa <- filter(urchintemp_data, day == "2", treatment == "ambient")
plot(day2dataa$time_point, day2dataa$righting_time, main = "day2dataa", xlab = "Time Point", ylab = "Righting Time (sec)")

day3datahw <- filter(urchintemp_data, day == "3", treatment == "heat wave")
plot(day3datahw$time_point, day3datahw$righting_time, main = "day3datahw", xlab = "Time Point", ylab = "Righting Time (sec)")

day3dataa <- filter(urchintemp_data, day == "3", treatment == "ambient")
plot(day3dataa$time_point, day3dataa$righting_time, main = "day3dataa", xlab = "Time Point", ylab = "Righting Time (sec)")

day1 <- filter(urchintemp_data, day == "1")
interaction.plot(day1$time_point, day1$treatment, day1$righting_time)

day2 <- filter(urchintemp_data, day == "2")
interaction.plot(day2$time_point, day2$treatment, day2$righting_time)

day3 <- filter(urchintemp_data, day == "3")
interaction.plot(day3$time_point, day3$treatment, day3$righting_time)


## modeling -----------
m <- aov(I(log(righting_time)) ~ day * time_point * treatment, data = urchintemp_data)
m1 <- aov(righting_time ~ time_point * treatment, data = day1)
m2 <- aov(righting_time ~ time_point * treatment, data = day2)
m3 <- aov(righting_time ~ time_point * treatment, data = day3)

urchin_dat <- subset(urchin_data_r, subset = righting_time < 900)

tpm <- lm(I((righting_time)) ~ day + time_point + as.factor(treatment), data = urchin_data_r)

termplot(tpm, partial.resid = T, se=T)
summary(tpm)
boxplot(righting_time ~ as.factor(treatment), data = urchin_dat)

trtmt <- as.factor(urchin_data_r$treatment)
tp.model <- lm(righting_time ~ day + time_point + trtmt, data = urchin_dat)
termplot(urchin.model, partial.resid = T, se = T)
summary(urchin.model)

temp.model <- lm(righting_time ~ day + temperature + trtmt, data = urchin_dat)
termplot(temp.model, partial.resid = T, se = T)
summary(temp.model)

urchin_dat <- subset(urchin_data_r, subset = righting_time < 900)
