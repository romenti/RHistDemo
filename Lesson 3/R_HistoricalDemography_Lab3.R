#### Lab 3: June 30, 2023 ####


# clean the global environment
rm(list=ls())

#### Dates in R ####

# date as a character string

mydate = "2023-06-30"

# convert to Date object

mydate = as.Date(mydate)


# object with current date and time

current_day_time = Sys.time()

# check the class

class(current_day_time)

# POSIXct by default 


# internal numerical representation
unclass(current_day_time)



# create POSIXlt object
current_day_time_new = as.POSIXlt(current_day_time)

# access the names of its components

names(unclass(current_day_time_new))

# extract components
current_day_time_new$yday


# create POSIXlt object
end_of_the_world = "December 21, 2012 00:00"

# control its format

strptime(end_of_the_world,"%B %d, %Y %H:%M")



# Handling different time zones 

# date
today_time = Sys.time()

today_time = make_datetime(year = 2023, month = 6, day = 30, 
                           hour = 10, min = 30, sec = 45, tz = "Europe/Rome")


# Change the printing

force_tz(today_time,"America/Chicago")

# Change time

with_tz(today_time,"America/Chicago")


# format your output

format(today_time, "%T %Z")




#### lubridate package ####

install.packages("lubridate")

# upload it

library("lubridate")


# alternatively

#if(!require(lubridate)) {
#  install.packages("lubridate"); require(lubridate)}


# numeric input
numeric = 20230630

ymd(numeric)


# character input
character = "2023/06/30"

ymd(character)



# create date

today = make_date(year = 2023, month = 6, day = 30)

# extract the week day from the date

# with label
wday(today,label = T)

# without label
wday(today, label = F)



# date
today_time = Sys.time()

# create a date with a precise time
# you can also specify the time zone


today_time = make_datetime(year = 2023, month = 6, day = 30, 
                           hour = 10, min = 30, sec = 45, tz = "Europe/Rome")


# Change the printing of the time zone

with_tz(today_time,"America/Chicago")

# Change the actual time zone

force_tz(today_time,"America/Chicago")

# format your output

format(today_time, "%T %Z")


# Number of days/weeks/years between two dates

today = today()

date_911 = make_date(year = 2001, month = 9, day = 11)

# standard R (only weeks and days)

time_diff_days = difftime(today,date_911,units = "days")

time_diff_weeks = difftime(today,date_911 ,units = "weeks")






# lubridate package

# time difference in seconds
time_diff = as.duration(today-date_911)

# number of days
time_diff_days = time_diff/ddays(1)

# number of months 
time_diff_weeks = time_diff/dmonths(1)

# number of years
time_diff_years = time_diff/dyears(1)


#### Dates: Data Example ####

install.packages("HistData")

library("HistData")


# install and upload the library
#if(!require(HistData)) {
#  install.packages("HistData"); require(HistData)}

# upload the data set

data("Nightingale")


# time series of cause-specific death rates

plot(Nightingale$Date,Nightingale$Disease.rate,xaxt="n",xlab = "",
     ylab = "Rates  per 1000",type="l",pch = 18, col = "red") # main plot
axis.Date(1, at = seq(Nightingale$Date[1], Nightingale$Date[nrow(Nightingale)], by = "month"),
          format = '%Y-%b', las = 2) # control labels for dates
lines(Nightingale$Date,Nightingale$Wounds.rate, pch = 18, col = "blue", type = "l") # add line
lines(Nightingale$Date,Nightingale$Other.rate, pch = 18, col = "green", type = "l") # add line
legend("topright", legend=c("Disease", "Wound", "Other"),
       col=c("red", "blue","green"), lty = 1, cex=0.8, title = "Cause of death") # add legend
title("Time series of cause-specific death rates") # add title

# calculate the number of days since the day of first recording
Nightingale$nr_days = as.duration(Nightingale$Date-Nightingale$Date[1])/ddays(1)

# plot the time series of cause-specific death rates using nr. of days as time scale
plot(Nightingale$nr_days,Nightingale$Disease.rate,xlab = "Nr. of days since ",
     ylab = "Rates per 1000",type="l",pch = 18, col = "red") # main plot
lines(Nightingale$nr_days,Nightingale$Wounds.rate, pch = 18, col = "blue", type = "l") # add line
lines(Nightingale$nr_days,Nightingale$Other.rate, pch = 18, col = "green", type = "l") # add line
legend("topright", legend=c("Disease", "Wound", "Other"), 
       col=c("red", "blue","green"), lty = 1, cex=0.8, title = "Cause of death") # add legend
title("Time series of cause-specific death rates") # add title

# calculate the number of weeks  since the day of first recording
Nightingale$nr_weeks = as.duration(Nightingale$Date-Nightingale$Date[1])/dweeks(1)

# plot the time series of cause-specific death rates using nr. of days as time scale
plot(Nightingale$nr_weeks,Nightingale$Disease.rate,xlab = "Nr. of weeks",
     ylab = "Rates per 1000",type="l",pch = 18, col = "red") # main plot
lines(Nightingale$nr_weeks,Nightingale$Wounds.rate, pch = 18, col = "blue", type = "l") # add line
lines(Nightingale$nr_weeks,Nightingale$Other.rate, pch = 18, col = "green", type = "l") # add line
legend("topright", legend=c("Disease", "Wound", "Other"),
       col=c("red", "blue","green"), lty = 1, cex=0.8, title = "Cause of death") # add legend
title("Time series of cause-specific death rates") # add title


#### Crude Birth Rates ####

# load all the data sets
load("Italy_Data.RData")

# data set for the analysis of birth dynamics

birth_analysis = merge(births[,c("Year","total_births")],
                       exposure[,c("Year","total_exp")],by="Year")


# Calculation of Crude Birth Rates

birth_analysis$birth_rates = birth_analysis$total_births/birth_analysis$total_exp*1000


# plot for CBR during the period 1872-2019

plot(birth_analysis$Year,birth_analysis$birth_rates,type = "l",xaxt="n",
     xlab = "Year",ylab = "Crude Birth Rate") # main plot
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
title("Evolution of Crude Birth Rates in Italy (1872-2019)") # add title


#relative variations in CBRs

var_italy_birth_rates= diff(birth_analysis$birth_rates)/birth_analysis$birth_rates[-nrow(birth_analysis)]*100


# plot relative variations in CBRs

plot(birth_analysis$Year[-1],var_italy_birth_rates,xlab="Year",
     ylab="Variation in Crude Birth Rates",type = "l",xaxt="n") # main plot
axis(1,at=seq(1873,2019,5),srt = 60,las=3) # adjust x-axis
abline(h=0,col="blue") # add horizontal line
title("Relative variations in CBRs in Italy (1873-2019)")  # add title


#### Death Rates ####

# data set for the analysis of deaths
death_analysis = merge(deaths[,c("Year","total_deaths")],
                       exposure[,c("Year","total_exp")],by="Year")
# CDR calculation
death_analysis$death_rates = death_analysis$total_deaths/death_analysis$total_exp*1000

# plot for CDR during the period 1872-2019
plot(death_analysis$Year,death_analysis$death_rates,type = "l",xaxt="n",
     xlab = "Year",ylab = "Crude death Rate") # main plot
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
title("Evolution of Death Rates in Italy (1872-2019)") # add title



# relative variations in CDRs
var_italy_death_rates = diff(death_analysis$death_rates)/death_analysis$death_rates[-nrow(death_analysis)]*100

# plot for relative variations in CDRs during the period 1872-2019
plot(death_analysis$Year[-1],var_italy_death_rates,type = "l",xaxt="n",
     xlab = "Year",ylab = "Variation in Crude Death Rates") # main plot
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
title("Relative variations in CDRs in Italy (1873-2019)") # add title

# both CDRs and CBRs on the same plot
plot(birth_analysis$Year,birth_analysis$birth_rates,xaxt="n",type = "l",
     xlab = "Calendar Year",ylab = "Crude Rate",col="red") # main plot
lines(death_analysis$Year,death_analysis$death_rates,
      xaxt="n",col="blue") # add line
legend("topright", 
       legend=c("Crude Birth Rate", "Crude Death Rate"),
       col=c("red", "blue"), lty = 1, cex=0.8, title = "Type") # add legend
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
axis(2,at=seq(0,60,10)) # adjust y-axis
text(x=1919,y=35,cex=0.65, pos=3,label="Spanish Flu") # add text to the plot
text(x=1965,y=20,cex=0.65, pos=3,label="Baby Boom")
text(x=1942,y=25,cex=0.65, pos=3,label="WW2")
text(x=1980,y=15,cex=0.65, pos=3,label="Baby Bust")
title("Evolution of CBRs and CDRs in Italy (1872-2019)") # add title

# relative variations in CDRs and in CBRs on the same plot
plot(birth_analysis$Year[-1],var_italy_birth_rates,xlab="Year",
     ylab="Relative Variation",type = "l",xaxt="n",col="red",
     ylim=c(-50,50)) # main plot
lines(death_analysis$Year[-1],var_italy_death_rates,xaxt="n",col="blue") # add line
abline(h=0,col="red")
legend("topright", legend=c("Crude Birth Rate", "Crude Death Rate"),
       col=c("red", "blue"), lty = 1, cex=0.8, title = "Type") # add legend
axis(1,at=seq(1873,2019,5),srt = 60,las=3) # adjust x-axis
title("Relative variations in CBRs and CDRs in Italy (1873-2019)") # add title

#### Rate of Natural Increase ####

# calculation
natural_increase_rates = (birth_analysis$birth_rates-death_analysis$death_rates)/1000

# plot of Rates of Natural Increase for the period 1872-2019
plot(birth_analysis$Year,natural_increase_rates,xaxt="n",type = "b",
     xlab = "Calendar Year",ylab="Naural Population Growth") # main plot
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
abline(h=0,col="purple") # add horizontal line
abline(v=1992,col="red") # add vertical line
text(2005,0,cex=0.80,pos=3,"Decline") # add text
title("Time series of Rates of Natural Growth in Italy (1872-2019)") # add title


#### Exponential Growth Model ####

# Set initial Population Size (= Pop. size in 1872)
P_init = pop_size$Total_pop[1]

# Fix Rate of Natural Increase (= Rate of Natural Increase in 1872)
R = natural_increase_rates[1]

# Time horizon for the simulation (1873-2019)
time_horizon = 1873:2019

# Projected Population Sizes (saved in a vector) 
P_exponential = P_init*exp(R*(0:length(time_horizon)))

# Plot projected and real population sizes
plot(1872:2019,P_exponential/1e06,xlab="Year",ylab="Population Size per 1 million",
     xaxt="n",type = "l",col="green") # main plot
lines(pop_size$Year,pop_size$Total/1e06,col="blue") # add line
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
legend("topleft", legend=c("Exponential","Real"),
       col=c("green","blue"), lty = 1, cex=0.8, title = "Type") # add legend
title("Exponential vs. Real Population Size in Italy (1873-2019)") # add title


# Model-based doubling time
T_double_model = log(2)/R
print(T_double_model)

# True doubling time
T_double_true = min(pop_size$Year[pop_size$Total_pop>2*P_init])-1872
print(T_double_true)

# Difference in the two
T_double_true-T_double_model


# Fixed Rate of Natural Increase (=Rate of Natural Increase in 2019)
R_new = natural_increase_rates[length(natural_increase_rates)]

# Model-based Halving time
T_halve = (-1)*log(2)/R_new

# Projection horizion
projection_horizon = 2020:2100 

# Initial Population Size (= pop. size in 2019)
P_init = pop_size$Total_pop[pop_size$Year==2019]

# Projected Population Sizes
P_projected = P_init*exp(R_new*0:length(projection_horizon))

#Plot projected population sizes
plot(2019:2100,P_projected/1e06,xlab="Year",ylab="Project Population Size per 1 million",
     type="l",xaxt="n",col="red") # add plot 
axis(1,at=seq(2019,2100,5),srt = 60,las=3) # adjust x-axis
title("Simulated Population Size in Italy (2020-2100)") # add title


####  Calculation of structural ratios  ####

# dependency ratio = young dep. ratio + old dep. ratio

# Make sure Age is numeric
pop_age$Age = ifelse(pop_age$Age == "110+", 110, as.numeric(pop_age$Age))

# Make sure Year is numeric
pop_age$Year = as.numeric(pop_age$Year)

# Select only Year, Age and Total
Italy_dep = pop_age[,c("Year","Age","Total")]

# Create a categorical variable for the three age groups
Italy_dep$Age_group = ifelse(Italy_dep$Age<15,"young",ifelse(Italy_dep$Age>64,"old","working"))

# Calculate the total population counts by year and age group
Italy_dep = aggregate(Italy_dep$Total,by=list(Italy_dep$Year,Italy_dep$Age_group),FUN=sum)ù

# rename the columns
colnames(Italy_dep) = c("Year","Age_group","Pop")


# write the data set in wide format
Italy_dep  = reshape(Italy_dep, idvar = "Year",timevar = "Age_group",direction = "wide")

# Dependency Ratio
Italy_dep$Dep_Index = (Italy_dep$Pop.old+Italy_dep$Pop.young)/Italy_dep$Pop.working*100
# Young Dependency Ratio
Italy_dep$Young_Index = (Italy_dep$Pop.young)/Italy_dep$Pop.working*100
# Aged Dependency Ratio
Italy_dep$Old_Index = (Italy_dep$Pop.old)/Italy_dep$Pop.working*100

# Generate a plot with the time series for three ratios 
plot(Italy_dep$Year,Italy_dep$Dep_Index,xlab="Year",ylab="Index",
     xaxt="n",type = "l",col="green",ylim=c(0,100)) # main plot
lines(Italy_dep$Year,Italy_dep$Young_Index,col="red") # add line
lines(Italy_dep$Year,Italy_dep$Old_Index,col="blue") # add line
axis(1,at=seq(1872,2019,5),srt = 60,las=3) # adjust x-axis
legend("topright", legend=c("Depedency Ratio","Child Dependency Ratio","Aged Dependency Ratio"),
       col=c("green","red","blue"), lty = 1, cex=0.6, title = "Type") # add legend
text(1890,70,cex=0.80,pos=3,"Old Demographic \n Regime") # add text 
text(1965,60,cex=0.80,pos=3,"Windows of \n Opportunity") # \n to change line in the text string
text(2017,10,cex=0.80,pos=3,"Aging")

# Aging Ratio
Italy_dep$Aging_Ratio = Italy_dep$Pop.old/Italy_dep$Pop.young*100

# Plot for Aging Ratio
plot(Italy_dep$Year,Italy_dep$Aging_Ratio,xlab="Year",ylab="Aging Index",
     xaxt="n",type = "l",col="purple",ylim=c(0,200))
axis(1,at=seq(1872,2019,5),srt = 60,las=3)

#### Linear Model ####

install.packages("gapminder")
library("gapminder")

# install gapminder
#if(!require(gapminder)) {
#  install.packages("gapminder"); require(gapmindera)}

# call the data set

data("gapminder")


# Exploratory Analysis
# calculate the average life exp. at birth 
# by continent and year
average_life_Exp = aggregate(gapminder$lifeExp,by=list(gapminder$continent,gapminder$year),FUN=mean)
# rename the variables
colnames(average_life_Exp) = c("Continent","Year","Average_life_exp")

# plot the average life exp by continent.
# one line for each continent
plot(average_life_Exp$Year[average_life_Exp$Continent=="Americas"],
     average_life_Exp$Average_life_exp[average_life_Exp$Continent=="Americas"],
     type="l",col="green",xlab="Year",ylab="Average Life Expectancy at Birth",
     ylim=c(30,80),xaxt="n") # main plot
axis(1,at=unique(average_life_Exp$Year),srt = 60,las=3) # adjus x-axis 
lines(average_life_Exp$Year[average_life_Exp$Continent=="Europe"],
      average_life_Exp$Average_life_exp[average_life_Exp$Continent=="Europe"],col="yellow") # add line
lines(average_life_Exp$Year[average_life_Exp$Continent=="Asia"],
      average_life_Exp$Average_life_exp[average_life_Exp$Continent=="Asia"],col="red")
lines(average_life_Exp$Year[average_life_Exp$Continent=="Africa"],
      average_life_Exp$Average_life_exp[average_life_Exp$Continent=="Africa"],col="blue")
lines(average_life_Exp$Year[average_life_Exp$Continent=="Oceania"],
      average_life_Exp$Average_life_exp[average_life_Exp$Continent=="Oceania"],col="purple")
legend(
  x ="bottomright",
  legend = levels(average_life_Exp$Continent), # for readability of legend
  col = c("blue","green","red","yellow","purple"),
  pch = 19, # same as pch=20, just smaller
  cex = .7 # scale the legend to look attractively sized
)


# calculate population size by continent across the years
pop_size = aggregate(gapminder$pop,by=list(gapminder$continent,gapminder$year),FUN=sum)
# rename columns
colnames(pop_size) = c("Continent","Year","Pop")

# plot: time series of population size in log-scale by continent
plot(pop_size$Year[pop_size$Continent=="Americas"],
     log(pop_size$Pop[pop_size$Continent=="Americas"]),
     type="l",col="green",xlab="Year",ylab="Log(Population Size)",
     ylim=c(15,23),xaxt="n")
axis(1,at=unique(average_life_Exp$Year),srt = 60,las=3)
lines(pop_size$Year[pop_size$Continent=="Europe"],
      log(pop_size$Pop[pop_size$Continent=="Europe"]),col="yellow")
lines(pop_size$Year[pop_size$Continent=="Asia"],
      log(pop_size$Pop[pop_size$Continent=="Asia"]),col="red")
lines(pop_size$Year[pop_size$Continent=="Africa"],
      log(pop_size$Pop[pop_size$Continent=="Africa"]),col="blue")
lines(pop_size$Year[pop_size$Continent=="Oceania"],
      log(pop_size$Pop[pop_size$Continent=="Oceania"]),col="purple")
legend(
  x ="bottomright",
  legend = levels(pop_size$Continent), # for readability of legend
  col = c("blue","green","red","yellow","purple"),
  pch = 19, # same as pch=20, just smaller
  cex = .7 # scale the legend to look attractively sized
) # add legend

# consider only the year 1952
gapminder_subset = gapminder[gapminder$year==1952,]


# plot histograms for GDP per capita and. Population Size in 1952
par(mfrow = c(2,1))
hist(gapminder_subset$gdpPercap,
     xlab = "GDP per capita", 
     main = "Distribution of GDP per capita in 1952")

hist(gapminder_subset$pop,
     xlab = "Population Size",
     main = "Distribution of Population Size in 1952")

# plot histograms for log of GDP per capita and log of Population Size in 1952
par(mfrow = c(2,1))
hist(log(gapminder_subset$gdpPercap),
     xlab = "GDP per capita",
     main = "Distribution of the log of GDP per capita in 1952")

hist(log(gapminder_subset$pop),
     xlab = "Population Size",
     main = "Distribution of the log of Population Size in 1952")

# plot log of GDP vs. life expectancy at birth in 1952
plot(log(gapminder_subset$gdpPercap),
     gapminder_subset$lifeExp,
     xlab="log of GDP",
     ylab="Life Expectancy at Birth",
     col=c("blue","green","red","yellow","purple"),pch = 20 ) # main plot
legend(
  x ="topleft",
  legend = levels(gapminder_subset$continent), # for readability of legend
  col = c("blue","green","red","yellow","purple"),
  pch = 19, # same as pch=20, just smaller
  cex = .7 # scale the legend to look attractively sized
) # add legend
abline(lm(lifeExp~I(log(gdpPercap)),gapminder_subset)) # add fitted regression line
title("log(GDP) vs. Life Expectnancy at Birth in 1952") # add title

# plot: log of. population size vs. life expectancy
plot(log(gapminder_subset$pop),
     gapminder_subset$lifeExp,
     xlab="log of Population Size",
     ylab="Life Expectancy at Birth",
     col=c("blue","green","red","yellow","purple"),pch = 20 ) # main plot
legend( 
  x ="topleft",
  legend = levels(gapminder_subset$continent), # for readability of legend
  col = c("blue","green","red","yellow","purple"),
  pch = 19, # same as pch=20, just smaller
  cex = .7 # scale the legend to look attractively sized
) # add legend
abline(lm(lifeExp~I(log(pop)),gapminder_subset)) # add fitted regression line
title("log(Pop size) vs. Life Expectnancy at Birth in 1952") # add title



#### Simple Linear Model in R####

# run simple linear model
simple_regression = lm(lifeExp~I(log(gdpPercap)),gapminder_subset)
# show the results
summary(simple_regression)


# look at the coefficients
coef(summary(simple_regression))

# generate plot for diagnostics
par(mfrow = c(2, 2))
plot(simple_regression)

#### Multiple Linear Regression Model in R ####

# run a multiple linear regression model 
multiple_regression = lm(lifeExp~I(log(gdpPercap))+continent,gapminder_subset)
# show the results
summary(multiple_regression)
# diagnotistics
par(mfrow = c(2, 2))
plot(multiple_regression)


#### Factor Variables in R ####

# assign a variable to type factor

as.factor(gapminder_subset$continent)

# factor
levels(gapminder_subset$continent)



# run a multiple linear regression model 
multiple_regression_newref = lm(lifeExp~I(log(gdpPercap))+relevel(continent,ref = 3),gapminder_subset)
# show the results
summary(multiple_regression_newref)


continents = factor(gapminder$continent,
                    levels=c("Asia","Africa","Oceania","Americas","Europe"))
levels(continents)


#### Model Comparison ####

# model comparison for nested models
anova(simple_regression,multiple_regression)


AIC_models = data.frame(type=c("simple","multiple"),
                        AIC = c(AIC(simple_regression),AIC(multiple_regression)))


BIC_models = data.frame(type=c("simple","multiple"),
                        BIC = c(BIC(simple_regression),BIC(multiple_regression)))


print(AIC_models)
print(BIC_models)


#### Logistic Regression in R ####

# create a new binary variable for the response 
gapminder_subset$lifeExp_cat = ifelse(gapminder_subset$lifeExp>=60,1,0)

# boxplots of log of GDP per capita
boxplot(log(gapminder_subset$gdpPercap) ~ 
          gapminder_subset$lifeExp_cat,
        col=c("orange","green"),
        names=c("<60",">=60"),
        xlab="Life Expectancy at Birth Category",
        ylab="log of the GDP")


# run a simple logistic regression model
simple_logistic_reg = glm(lifeExp_cat~I(log(gdpPercap)),gapminder_subset,family="binomial")
# show the results
summary(simple_logistic_reg)

# coefficients in odds scale
exp(coef(simple_logistic_reg))


# run a multiple linear regression model
multiple_logistic_reg = glm(lifeExp_cat~I(log(gdpPercap))+continent,gapminder_subset,family="binomial")
# show the results
summary(multiple_logistic_reg)


# perform model comparison
anova(simple_logistic_reg,multiple_logistic_reg,test="Chi")
