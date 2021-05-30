################### Loading and Analysing Data #################
# Load dataset into covid dataframe
initial_covid <- read.csv('covid.csv', na="")
head(initial_covid, 15)
# schema of the dataset
str(initial_covid)

# check the names of the columns in dataset
# and modified if any column name is not adhere to standards
names(initial_covid)
# as we do not have any space or special character in the names
# and are all used underscore instead of space
# we do not require to change the names 

# Subset only columns which we want to include in our model
attach(initial_covid)
covid <- subset.data.frame(initial_covid,continent== 'Africa', select = c(continent, total_cases, new_cases,iso_code,
                                                     total_deaths, new_deaths, total_cases_per_million, 
                                                     total_deaths_per_million, new_cases_per_million, reproduction_rate, 
                                                     icu_patients, icu_patients_per_million, 
                                                     new_tests, new_tests_per_thousand, total_tests, total_tests_per_thousand,
                                                     positive_rate, total_vaccinations, total_vaccinations_per_hundred, people_vaccinated, people_fully_vaccinated,
                                                     stringency_index, population, population_density, median_age, aged_65_older, aged_70_older,
                                                     gdp_per_capita, handwashing_facilities, hospital_beds_per_thousand,
                                                     female_smokers, male_smokers, diabetes_prevalence, extreme_poverty, cardiovasc_death_rate))


detach(initial_covid)
# check the missing values using vim library
library(VIM)
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(missing_values)
# there are so many missing values. We need to work on them one by one to see the significance of each variable

# Lets check some basics stats of our covid dataset
summary(covid)
# we have total of 80445 values and we cannot work on variables which has missing values more than 40000

################# Data Processing ###############

# variables like total cases, new cases, deaths and many more have NAs because intitial data in some countries were not recorded or there were no cases to report for that time period of time.
# we can put 0 to replace them
attach(covid)
covid$total_cases[is.na(total_cases)] <- 0
covid$new_cases[is.na(new_cases)] <- 0
covid$total_deaths[is.na(total_deaths)] <- 0
covid$new_deaths[is.na(new_deaths)] <- 0
covid$total_cases_per_million[is.na(total_cases_per_million)] <- 0
covid$total_deaths_per_million[is.na(total_deaths_per_million)] <- 0
covid$new_cases_per_million[is.na(new_cases_per_million)] <- 0
covid$total_tests_per_thousand[is.na(total_tests_per_thousand)] <- 0
covid$total_tests[is.na(total_tests)] <- 0
covid$total_vaccinations[is.na(total_vaccinations)] <- 0
covid$people_vaccinated[is.na(people_vaccinated)] <- 0
covid$people_fully_vaccinated[is.na(people_fully_vaccinated)] <- 0

# lets have a look at missing values again
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
# we can observe there are variables which has so many Na values that we cannot include them in our model.
# It would be a better decision to drop those variables

# find variables which has NAs higher than 50000
summary(covid)
# by summary, we can see icu_patients, new_tests, new_tests_per_thousand, positive_rate, total_vaccinations_per_hundred, handwashing_facilities,
# has the highest number of Nas
covid <- subset.data.frame(covid,!is.na(population_density), select = -c(icu_patients, new_tests, new_tests_per_thousand, positive_rate, 
                                                                        total_vaccinations_per_hundred, handwashing_facilities,icu_patients_per_million,
                                                                        extreme_poverty, reproduction_rate, male_smokers, female_smokers, new_tests, total_vaccinations, new_deaths,
                                                                        total_deaths_per_million, icu_patients_per_million, total_cases_per_million, new_cases_per_million, hospital_beds_per_thousand))


# we will have a look at missing values again
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(missing_values)
# remove rows which has more than 4 NAs in it
#rowSums(is.na(covid))
#abc <- covid[rowSums(is.na(covid))]


covid <- na.omit(covid)
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(covid)
detach(covid)
# after removing missing values we left wth 61283 records.
# now we need to convert content categorical record into numerical value.
# For that we need to find out the different values of continent exists in the dataset
unique(continent)
# "Asia"          "Europe"        "Africa"        "North America" "South America" "Oceania"  by the output
covid$people_vaccinated <- as.numeric(covid$people_vaccinated)
attach(covid)
# covid$asia <- ifelse(continent == "Asia", 1,0)
# covid$europe <- ifelse(continent == "Europe", 1,0)
# covid$africa <- ifelse(continent == "Africa", 1,0)
# covid$north_america <- ifelse(continent == "North America", 1,0)
# covid$south_america <- ifelse(continent == "South America", 1,0)
# covid$oceania <- ifelse(continent == "Oceania", 1,0)

# removing continent and location as we do not need them anymore
covid <- subset.data.frame(covid,!is.na(population_density), select = -c(continent, iso_code))

################################## END OF DATA PROCESSING ##################################

################################## VARIABLES PRIORTISATION #################################


library(psych)
par(mar=rep(2, 4))
pdf('pairs.pdf')
  pairs(covid)
dev.off()
# pdf('myplot.pdf')
# pairs.panels(covid, 
#             smooth = TRUE, # If TRUE, draws loess smooths  
#             scale = FALSE, # If TRUE, scales the correlation text font  
#             density = TRUE, # If TRUE, adds density plots and histograms  
#            ellipses = TRUE, # If TRUE, draws ellipses   
#     method = "spearman",# Correlation method (also "pearson" or "kendall") 
#             pch = 21, # pch symbol   
#     lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
#             cor = TRUE, # If TRUE, reports correlations
#             jiggle = FALSE, # If TRUE, data points are jittered  
#             factor = 2, # Jittering factor  
#             hist.col = 4, # Histograms color   
#             stars = TRUE,
#             se = FALSE,
#             ci = TRUE) # If TRUE, adds confidence intervals 
# dev.off()

pdf('scatter_plot.pdf')
scatter.smooth(x = total_deaths, 
               y = total_cases, 
               main = "Total Deaths ~ Total Cases", 
               xlab = "Total Deaths (estimate)", ylab = "Total Cases (estimate)")
paste("correltion for Total Deaths and Total Cases: ", cor(total_deaths, total_cases))
# Corelation value is 0.93

scatter.smooth(x =total_deaths , 
               y = new_cases, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "New Cases (estimate)")
paste("correltion for Total Deaths and New Cases: ", cor(total_deaths, new_cases))
# Corelation value is 0.61

scatter.smooth(x = total_deaths , 
               y = total_tests, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "Total Tests (estimate)")
paste("correltion for Total Deaths and Total Tests: ", cor(total_deaths, total_tests))
# Corelation value is 0.65

scatter.smooth(x = total_deaths , 
               y = total_tests_per_thousand, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Tests (per 1000)", ylab = "Total Tests (estimate)")
paste("correltion for Total Deaths and Total Tests per 1000: ", cor(total_deaths, total_tests_per_thousand))
# Correlation value is 0.06

scatter.smooth(x = total_deaths , 
               y = people_vaccinated, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "People Vaccinated (estimate)")
paste("correltion for Total Deaths and People Vaccinated: ", cor(total_deaths, people_vaccinated))
# Correlation value is 0.64

scatter.smooth(x = total_deaths , 
               y = people_fully_vaccinated, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "People Fully Vaccinated (estimate)")
paste("correltion for Total Deaths and People Fully Vaccinated: ", cor(total_deaths, people_fully_vaccinated))
# Correlation value is 0.62

scatter.smooth(x = total_deaths , 
               y = stringency_index, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "Stringency Index (estimate)")
paste("correltion for Total Deaths and Total Tests: ", cor(total_deaths, stringency_index))
# Correlation value is 0.25

scatter.smooth(x = total_deaths , 
               y = population, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "Population (estimate)")
paste("correltion for Total Deaths and Population: ", cor(total_deaths, population))
# Correlation value is 0.58

scatter.smooth(x = total_deaths , 
               y = population_density, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "Population Density(estimate)")
paste("correltion for Total Deaths and Population Density: ", cor(total_deaths, population_density))
# Correlation value is  0.0139796256826579

scatter.smooth(x = total_deaths , 
               y = median_age, 
               main = "Total Deaths ~ New Cases", 
               xlab = "Total Deaths (estimate)", ylab = "Median Age  (estimate)")
paste("correltion for Total Deaths and Median Age: ", cor(total_deaths, median_age)) #0.182160553218309
# Correlation value is 0.65

paste("correltion for Total Deaths and Age 65 Older : ", cor(total_deaths, aged_65_older )) #0.190610756286401
paste("correltion for Total Deaths and Age 70 Older: ", cor(total_deaths, aged_70_older)) # 0.228247829270662
paste("correltion for Total Deaths and GDP Per Capita : ", cor(total_deaths, gdp_per_capita )) #-0.00360297141311468
paste("correltion for Total Deaths and Diabetes ", cor(total_deaths, diabetes_prevalence )) #-0.113682105472952
paste("correltion for Total Deaths and Cardio Death Rate : ", cor(total_deaths, cardiovasc_death_rate )) # -0.144475573869866


dev.off()

# It is clear that variables with Cardio Death Rate, Diabetes, GDP, Population density has low correlation value

covid <- subset(covid, select = -c(cardiovasc_death_rate, diabetes_prevalence, gdp_per_capita, population_density))


# now we will check the outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # charts shown in 4 rows x 2 cols


pdf('box-plot.pdf')

boxplot(stringency_index, 
        main = "Stringency Index", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(stringency_index)$out))
boxplot.stats(stringency_index)$out

boxplot(population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(population)$out))
boxplot.stats(population)$out

boxplot(population_density, 
        main = "Population Density", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(population_density)$out))
boxplot.stats(population_density)$out

boxplot(median_age, 
        main = "Median Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(median_age)$out))

boxplot.stats(median_age)$out
boxplot(aged_65_older, 
        main = "Aged 65 older ", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(aged_65_older)$out))

boxplot.stats(aged_65_older)$out


boxplot(aged_70_older, 
        main = "Aged 70 older ", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(aged_70_older)$out))
boxplot.stats(aged_70_older)$out

boxplot(people_fully_vaccinated, 
        main = "Aged 70 older ", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(people_fully_vaccinated)$out))
vaccination_outliers <- boxplot.stats(people_fully_vaccinated)$out
vaccination_outliers
# remove outliers laters
dev.off()

par(opar)

pdf('skewness.pdf')
# Check for normality
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
plot(density(total_cases), 
     main = "Density plot : Total Cases", 
     ylab = "Frequency", xlab = "Total Cases",
     sub = paste("Skewness : ", round(e1071::skewness(total_cases), 2)))
paste((round(e1071::skewness(total_cases), 2)))
# 3.46 density
polygon(density(total_cases), col = "red")



plot(density(new_cases), 
     main = "Density plot : New Cases", 
     ylab = "Frequency", xlab = "New Cases",
     sub = paste("Skewness : ", round(e1071::skewness(new_cases), 2)))
paste((round(e1071::skewness(new_cases), 2)))
# 4.77 density
polygon(density(new_cases), col = "red")


plot(density(total_tests), 
     main = "Density plot : Total Tests", 
     ylab = "Frequency", xlab = "Total Tests",
     sub = paste("Skewness : ", round(e1071::skewness(total_tests), 2)))
paste((round(e1071::skewness(total_tests), 2)))
# 5.98 density
polygon(density(total_tests), col = "red")


plot(density(total_tests_per_thousand), 
     main = "Density plot : Total Tests per thousand", 
     ylab = "Frequency", xlab = "Total Tests per thousand",
     sub = paste("Skewness : ", round(e1071::skewness(total_tests_per_thousand), 2)))
paste((round(e1071::skewness(total_tests_per_thousand), 2)))
# 4.26 density
polygon(density(total_tests_per_thousand), col = "red")


plot(density(people_vaccinated), 
     main = "Density plot : people_vaccinated", 
     ylab = "Frequency", xlab = "people_vaccinated",
     sub = paste("Skewness : ", round(e1071::skewness(people_vaccinated), 2)))
paste((round(e1071::skewness(people_vaccinated), 2)))
# 10.36 density
polygon(density(people_vaccinated), col = "red")


plot(density(stringency_index), 
     main = "Density plot : stringency_index", 
     ylab = "Frequency", xlab = "stringency_index",
     sub = paste("Skewness : ", round(e1071::skewness(stringency_index), 2)))
paste((round(e1071::skewness(stringency_index), 2)))
# -0.55 density
polygon(density(stringency_index), col = "red")

plot(density(population), 
     main = "Density plot : population", 
     ylab = "Frequency", xlab = "population",
     sub = paste("Skewness : ", round(e1071::skewness(population), 2)))
paste((round(e1071::skewness(population), 2)))
# 2.37 density
polygon(density(population), col = "red")


plot(density(median_age), 
     main = "Density plot : median_age", 
     ylab = "Frequency", xlab = "median_age",
     sub = paste("Skewness : ", round(e1071::skewness(median_age), 2)))
paste((round(e1071::skewness(median_age), 2)))
# -0.14 density
polygon(density(median_age), col = "red")

plot(density(aged_65_older), 
     main = "Density plot : aged_65_older", 
     ylab = "Frequency", xlab = "aged_65_older",
     sub = paste("Skewness : ", round(e1071::skewness(aged_65_older), 2)))
paste((round(e1071::skewness(aged_65_older), 2)))
# -0.63 density
polygon(density(aged_65_older), col = "red")

plot(density(aged_70_older), 
     main = "Density plot : aged_70_older", 
     ylab = "Frequency", xlab = "aged_65_older",
     sub = paste("Skewness : ", round(e1071::skewness(aged_70_older), 2)))
paste((round(e1071::skewness(aged_70_older), 2)))
# -0.22 density
polygon(density(aged_70_older), col = "red")

dev.off()

par <- opar


# Normality plots to validate the assumption
pdf("normality_plots.pdf")
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
hist(total_cases, main = "Normality proportion of Total deaths", xlab = "Total Deaths")
qqnorm(total_deaths)
qqline(total_deaths)
par <- opar

hist(new_cases, main = "Normality proportion of Total deaths", xlab = "Total Deaths")
qqnorm(total_deaths)
qqline(total_deaths)
par <- opar

dev.off()
mlr_model <- lm(total_deaths ~ new_cases + total_cases + aged_70_older + aged_65_older + population + 
                median_age +  stringency_index + people_vaccinated + total_tests_per_thousand + total_tests,  data = covid)
summary(mlr_model)


set.seed(1)
no_rows_data <- nrow(covid)
data_sample <- sample(1: no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
data_sample
training_data <- covid[data_sample, ]
testing_data <- covid[-data_sample, ]

mlr_model <- lm(total_deaths ~ new_cases + total_cases + aged_70_older + aged_65_older + population + 
                median_age +  stringency_index + people_vaccinated + total_tests_per_thousand + total_tests, data= training_data)

summary(mlr_model)


# summary
confint(mlr_model)



# car
library(car)
qqPlot(mlr_model, labels=row.names(covid), id.method="identify", simulate=TRUE,main="Q-Q Plot")
par <- opar

student_fit <- rstudent(mlr_model)
hist(student_fit,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")
rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(mlr_model)
nrow(covid)

# Split data into training and testing
set.seed(1)
no_rows_data <- nrow(covid)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace =FALSE)
training_data <- covid[sample, ]
testing_data <- covid[-sample, ]


fit <- lm(total_deaths ~ new_cases + total_cases + aged_70_older + aged_65_older + population + 
                        median_age +  stringency_index + people_vaccinated + total_tests_per_thousand + total_tests, data= training_data)

outlierTest(fit)
# check linearity

crPlots(mlr_model)


# Influential observations
library(car)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")


# Homoscedasticity 

ncvTest(fit)

spreadLevelPlot(fit)


# Multicollinearity
vif(mlr_model)


attributes(alias(fit)$Complete)$dimnames[[1]]

#covid <- subset(covid, select = -c(aged_70_older, aged_65_older, population, median_age))

set.seed(1)
no_rows_data <- nrow(covid)
data_sample <- sample(1: no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
data_sample
training_data <- covid[data_sample, ]
testing_data <- covid[-data_sample, ]

mlr_model <- lm(total_deaths ~ new_cases + total_cases + 
                         stringency_index + people_vaccinated + total_tests_per_thousand + total_tests, data= training_data)

summary(mlr_model)

# Homoscedasticity 

ncvTest(fit)

spreadLevelPlot(mlr_model)


# Multicollinearity
vif(mlr_model)

# We can check whether any of the variables indicate a multicollinearity problem
# if the value > 2


# predicting data


fit_model <- lm(total_deaths ~ new_cases + total_cases + 
                        stringency_index + people_vaccinated + total_tests_per_thousand + total_tests, data= training_data)
fit_model_sqrt <- lm(total_deaths ~ new_cases + total_cases + 
                             stringency_index + people_vaccinated + total_tests_per_thousand + total_tests, data= training_data)
predicted_deaths <- predict(fit_model, testing_data)
predicted_deaths_sqrt <- predict(fit_model_sqrt, testing_data)
converted_deaths_sqrt <- predicted_deaths_sqrt ^2

actuals_predictions <- data.frame(cbind(actuals = testing_data$total_deaths, predicted = predicted_deaths))
head(actuals_predictions, 100)


# make actuals_predicted dataframe for sqrt(total_deaths)
actuals_predictions_sqrt <- data.frame(cbind(actuals = testing_data$total_deaths, predicted = converted_deaths_sqrt))
head(actuals_predictions_sqrt, 100)



correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy


# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy


# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy


# Residual Standard Error (RSE), or sigma
sigma(fit_model)/ mean(testing_data$total_deaths)

# accuracy for data that we have in hand
sigma(fit_model_sqrt)/ mean(testing_data$total_deaths)

