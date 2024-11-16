data <- read.csv("C:/Users/Downloads/melanoma.csv")

head(data)

data <- data %>%
  mutate(status = factor(status,levels = c(1, 2, 3), labels = c("Died melanoma", "Alive", "Died unrelated")),
         sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male" )),
         ulcer = factor(ulcer, level = c (0, 1), labels = c("Absent", "Present")))

colnames(data)<-c("id","time","status","sex","age","year", "thickness","ulcer")

glimpse(data)

summary(data)

#Time: The max is way higher than the 3rd quarter,
#and the mean also higher than the median.

#more patient survival compared to those who died from malignant melanoma.
# 90 individuals had ulceration, while 115 did not.
#It would be intriguing to explore whether the thickness of their tumors,
#coupled with the presence of ulceration, had a significant impact on their survival.

#Additionally, there were more women represented in this dataset in comparison to men.

#Despite this, the mean and median indicate the data is fairly distributed


#skewness in its distribution.
#It would be compelling to observe the insights provided by a boxplot in this context.


ggplot(data, aes(x = time, fill = status)) +
  geom_histogram() +
  labs(title= "Time distribution and status",
       x ="time_in_days",
       y = "patient count")

# The distribution is skewed to the right, indicating that most patients have shorter follow-up times."""

ggplot(data, aes(x = age)) +
  geom_histogram () +
  labs(title= "age distribution of patient",
       x ="age",
       y = "patient count")

ggplot(data, aes(x = age, fill = status)) +
  geom_histogram() +
  labs(title= "age distribution and  status of survival",
       x ="age",
       y = "patient count")

# slightly left skewed but regression analysis will provide more information"""

ggplot(data, aes(x = age, fill = status)) +
  geom_histogram() +
  facet_grid(~sex)
labs(title= "age distribution and survival of different sex",
     x ="age",
     y = "patient count")

# normal distribution with the majority of the patients falling within the 40 - 70 age range"""

ggplot(data, aes(x = thickness)) +
  geom_histogram () +
  labs(title= "thickness of tumor distribution in patient",
       x ="thickness",
       y = "patient count")

# positive skewness, the tumor thickness exhibits a right skewed distribution"""

ggplot(data, aes(x = ulcer, fill = sex)) +
  geom_bar() +
  facet_grid(~status)
labs(title= " ulcer, sex and chances of survival",
     x ="ulcer",
     y = "patient count")

data1 <- lm(data$thickness ~ data$time)
data1

ggplot(data, aes(x = thickness, y = time, color = status)) +
  theme_bw() +
  geom_point() +
  geom_abline() +
  labs(title = "Thickness Distribution By Survival Time",
       y = "time_in_days",
       x = "Thickness")

# The correlation coefficient of -0.225 suggests a weak negative correlation between time and thickness"""

summary(data1)

data2 <- lm(data$age ~ data$time)
data2

summary(data2)

ggplot(data, aes(x = age, y = time, color = status)) +
  theme_bw() +
  geom_point() +
  geom_abline() +
  labs(title = "age Distribution By Survival Time",
       y = "time_in_days",
       x = "age")

# The correlation coefficient of -0.30 indicates weak negative correlation between time and age.

data3 <- lm(data$age ~ data$thickness)
data3

summary(data3)

ggplot(data, aes(x = age, y = thickness, color = status)) +
  theme_bw() +
  geom_point() +
  geom_abline() +
  labs(title = " Relationship between age and thickness of tumor",
       y = "thicknesss",
       x = "age")

# The correlation coefficient of 0.21 suggests a weak positive correlation between thickness and age."""

agesex1 <- t.test(age~sex, data = data)
agesex1

# The p-value of our test is 0.3408, which is notably higher than the alpha value of 0.05. Therefore, I cannot reject the null hypothesis of the test.

thicknesssex2 <- t.test(thickness~sex, data = data)
thicknesssex2

# The p-value of our test is 0.01009 , which is lower than the alpha value of 0.05. Therefore, I reject the null hypothesis of the test.

timesex1 <- t.test(time~sex, data = data)
timesex1

# The p-value of our test is 0.03868 , which is less than the alpha value of 0.05. Therefore, I reject the null hypothesis of the test


#QQplot for time
datatime <- ggplot(data = data, aes(sample = time))
datatime
datatime + stat_qq() + stat_qq_line()

#QQplot for the time in days of tumor on both sex(male and female)
datatime + stat_qq() + stat_qq_line() + facet_grid( ~ sex)

#QQplot for age
dataage <- ggplot(data = data, aes(sample = age))
dataage
dataage + stat_qq() + stat_qq_line()

#QQplot for the age in days of tumor on both sex(male and female)
dataage + stat_qq() + stat_qq_line() + facet_grid( ~ sex)

#QQplot for thickness
datathickness <- ggplot(data = data, aes(sample = thickness))
datathickness

#QQplot for the age in days of tumor on both sex(male and female)
datathickness + stat_qq() + stat_qq_line() + facet_grid( ~ sex)