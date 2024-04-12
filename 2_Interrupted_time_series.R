##########Load packages################################
library(ggplot2)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(INLA)
library(lubridate)
library(boot)

set.seed(50)

#Set working directory
setwd("C:/filepath")

### read HDSS data into R###
hdss <- read_csv("data5.csv")
head(hdss)

#subset data to include only children born from 1st Jan 2015######################################################################## 
hdss$dob <- as.Date(hdss$dob, format = "%d/%m/%Y")# Convert the "dob" variable to a Date format
hdss_2015 <- subset(hdss, dob >= as.Date("2015-01-01"))
hdss_2015$mmyyyy2 <- format(hdss$dob, "%m-%Y")



##subsetdata
hdss_basse <- subset(hdss_2015, site=="Basse") ##(Basse)##
hdss_farafenni <- subset(hdss_2015, site=="Farafenni") ##(Farafenni)##


### Aggregate by month
hep0_no_crude_vax    <- aggregate(hep0_crude == 1 ~ mmyyyy2, hdss_2015, sum) 
hep0_no_delay_vax    <- aggregate(hep0_delayed == 1 ~ mmyyyy2, hdss_2015, sum) 
penta1_no_crude_vax    <- aggregate(penta1_crude == 1 ~ mmyyyy2, hdss_2015, sum) 
penta1_no_delay_vax    <- aggregate(penta1_delayed == 1 ~ mmyyyy2, hdss_2015, sum)
penta1_no_early_vax    <- aggregate(penta1_early == 1 ~ mmyyyy2, hdss_2015, sum)
penta1_no_timely_vax    <- aggregate(penta1_timely == 1 ~ mmyyyy2, hdss_2015, sum)
hep0_no_births <- aggregate(hep0_crude == 1 | hep0_crude == 0 ~ mmyyyy2, hdss_2015, FUN=length)

head(hep0_no_crude_vax)
head(hep0_no_delay_vax)
head(penta1_no_crude_vax)
head(penta1_no_delay_vax)
head(penta1_no_early_vax)
head(penta1_no_timely_vax)
head(hep0_no_births)

##merge all the outcomes which have been aggregated by month
dat <- merge(hep0_no_crude_vax, hep0_no_delay_vax, by = "mmyyyy2")
head(dat)

dat <- merge(dat, penta1_no_crude_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_delay_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_early_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_timely_vax, by = "mmyyyy2")
dat <- merge(dat, hep0_no_births, by = "mmyyyy2")
head(dat)

##change the names of the variables to reflect the actual names
names(dat) <- c("month_year", "hep0_crude_count", "hep0_delayed_count", "penta1_crude_count", "penta1_delayed_count", 
                "penta1_early_count", "penta1_timely_count", "no_births")
head(dat)

#Create new variable month_year2 to sort the data
dat$month_year2 <- my(dat$month_year) #From lubridate
dat <- dat[order(as.Date(dat$month_year2, format="%Y/%m/%d")),]
dat$tt <- 1:nrow(dat)   #Time variable for error term
dat$tt_1 <- dat$tt      #Time variable for trend

dat$tt_x63 <- rep(0,nrow(dat))
dat$tt_x63[dat$tt>63 & dat$tt <=68] <- 1    #Change between times 63 and 68, i.e. Dec 2019 and Aug 2020
dat$tt_x63s <- (dat$tt-63)*dat$tt_x63       #Equal to zero at time of interruption

dat$tt_x68 <- rep(0,nrow(dat))
dat$tt_x68[dat$tt>68 & dat$tt <=75] <- 1    #Change between times 68 and 75, i.e. Aug 2020 and Mar 2021
dat$tt_x68s <- (dat$tt-68)*dat$tt_x68       #Equal to zero at time of interruption.....

dat$tt_x75 <- rep(0,nrow(dat))
dat$tt_x75[dat$tt>75 & dat$tt <=80] <- 1    #Changes between times 75 and 80, i.e. Mar 2021 and Aug 2021
dat$tt_x75s <- (dat$tt-75)*dat$tt_x75

dat$tt_x80 <- rep(0,nrow(dat))
dat$tt_x80[dat$tt>80] <- 1                  #Changes between times 80 and 84, i.e. Aug 2021 and Dec 2021
dat$tt_x80s <- (dat$tt-80)*dat$tt_x80

##########################################################################################################################################
###################CHANGE hep0_delayed_count (numerator) and hep0_crude_count (denominator) to model other outcomes###################
######################################################################################################################################
#Model with complete data


## 1. A. Time-series for delayed HepB0 

formula <- hep0_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = hep0_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$hep0_delayed_count/dat$hep0_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_hep_dd <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)


# Create a vector of quarter-year labels based on the x values
month_year_labels <- c("Mar-15", "Jun-15", "Sep-15", "Dec-15", 
                       "Mar-16", "Jun-16", "Sep-16", "Dec-16", "Mar-17", "Jun-17", 
                       "Sep-17", "Dec-17", "Mar-18", "Jun-18", "Sep-18", "Dec-18",
                       "Mar-19", "Jun-19", "Sep-19", "Dec-19", "Mar-20", "Jun-20", 
                       "Sep-20", "Dec-20", "Mar-21", "Jun-21", "Sep-21", "Dec-21")

# Create the ggplot2 plot for time-series
ggplot(data_hep_dd, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60 with dashed style and size 1.5
  xlab("Month and Year") +  # Set the x-axis label
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall") # Add the title "OVERALL"


# 1. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("hep0_delayed_count", "hep0_crude_count")] <- NA  
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- hep0_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = hep0_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$hep0_delayed_count[tt] <- rbinom(1,dat2$hep0_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_hep_dc <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                          one_step = fitted.mean.1step,
                          one_step_lower = fitted.1step$`0.025quant`,
                          one_step_upper = fitted.1step$`0.975quant`,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_hep_dc, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=63
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 70, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 70, label = "pandemic epoch") 
###save 800 x 500


#1. C. Testing for level and slope changes
formula <- hep0_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = hep0_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out



obs <- dat$hep0_delayed_count/dat$hep0_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- hep0_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_hep_dx <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                          trend.pred = trend.pred, group=group,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_dc, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_hep_dx, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 95, label = "1st", color = "black") +
  annotate("text", x = 77, y = 95, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 95, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 70, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 70, label = "pandemic epoch") 
###save 800 x 600

#########################################################################################################################################

## 2. A. Time-series for crude HepB0 coverage 

formula <- hep0_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$hep0_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_hep_crude <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_crude, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall") # Add the title "OVERALL"


# 2. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c("hep0_crude_count","no_births")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- hep0_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = no_births,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$hep0_crude_count[tt] <- rbinom(1,dat2$no_births[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_hep_crude_c <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                               one_step = fitted.mean.1step,
                               one_step_lower = fitted.1step$`0.025quant`,
                               one_step_upper = fitted.1step$`0.975quant`,
                               fitted_lower = fitted.all$`0.025quant`,
                               fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_hep_crude_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 50, label = "pandemic epoch") 

###save 800 x 500

#2. C. Testing for level and slope changes
formula <- hep0_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$hep0_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- hep0_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_hep_crude_x <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                          trend.pred = trend.pred, group=group,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_crude_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_hep_crude_x, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "1st", color = "black") +
  annotate("text", x = 77, y = 90, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 90, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 50, label = "pandemic epoch") 
###save 800 x 600

#####################################################################################################################################

## 3. A. Time-series for delayed PENTA1 
formula <- penta1_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_delayed_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_dd <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_penta1_dd, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall") # Add the title "OVERALL"


# 3. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c("penta1_delayed_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_delayed_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}


#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_dc <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                             one_step = fitted.mean.1step,
                             one_step_lower = fitted.1step$`0.025quant`,
                             one_step_upper = fitted.1step$`0.975quant`,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_dc, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall")+
  geom_vline(xintercept = 63, col = "grey", linetype = "dashed", size = 0.7) +
  annotate("text", x = 50, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 40, label = "pandemic epoch") 

###save 800 x 500


#3. C. Testing for level and slope changes
formula <- penta1_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_delayed_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_dx <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                          trend.pred = trend.pred, group=group,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_dc, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_dx, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 70, label = "1st", color = "black") +
  annotate("text", x = 77, y = 70, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 70, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 5, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 5, label = "pandemic epoch") 
###save 900 x 600


######################################################################################################################################


## 4. A. Time-series for early PENTA1 
formula <- penta1_early_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_early_count/dat$penta1_crude_count
dat$prop_early <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_early <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_penta1_early, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with too early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall") # Add the title "OVERALL"


# 4. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c( "penta1_early_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_early_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_early_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}


#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_early_c <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                one_step = fitted.mean.1step,
                                one_step_lower = fitted.1step$`0.025quant`,
                                one_step_upper = fitted.1step$`0.975quant`,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_early_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 45, label = "pandemic epoch") 

###save 800 x 500



#4. C. Testing for level and slope changes
formula <- penta1_early_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_early_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_early_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_early_x <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                             trend.pred = trend.pred, group=group,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_early_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_early_x, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 40, label = "1st", color = "black") +
  annotate("text", x = 77, y = 40, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 40, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 2, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 2, label = "pandemic epoch") 
###save 800 x 600

######################################################################################################################################

## 5. A. Time-series for crude PENTA1 coverage 
formula <- penta1_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_crude <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_crude, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall") # Add the title "OVERALL"


# 5. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("penta1_crude_count", "no_births")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = no_births,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_crude_count[tt] <- rbinom(1,dat2$no_births[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_crude_c <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  one_step = fitted.mean.1step,
                                  one_step_lower = fitted.1step$`0.025quant`,
                                  one_step_upper = fitted.1step$`0.975quant`,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_crude_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 30, label = "pandemic epoch")

###save 800 x 500


#5. C. Testing for level and slope changes
formula <- penta1_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$penta1_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_crude_x <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                               trend.pred = trend.pred, group=group,
                               fitted_lower = fitted.all$`0.025quant`,
                               fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_crude_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_crude_x, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "1st", color = "black") +
  annotate("text", x = 77, y = 90, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 90, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 30, label = "pandemic epoch") 
###save 800 x 600

#########################################################################################################################################

## 6. A. Time-series for timely PENTA1 coverage 
formula <- penta1_timely_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_timely_count/dat$penta1_crude_count
dat$prop_timely <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_timely <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                 fitted_lower = fitted.all$`0.025quant`,
                                 fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_timely, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall") # Add the title "OVERALL"


# 6. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("penta1_timely_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_timely_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_timely_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_timely_c <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  one_step = fitted.mean.1step,
                                  one_step_lower = fitted.1step$`0.025quant`,
                                  one_step_upper = fitted.1step$`0.975quant`,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_timely_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 45, label = "pandemic epoch")

###save 800 x 500



#6 C. Testing for level and slope changes
formula <- penta1_timely_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_timely_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_timely_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_timely_x <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  trend.pred = trend.pred, group=group,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_timely_c, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_timely_x, aes(y = trend.pred * 100, group=group), color = "#ffec95", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "First", color = "black", size = 3) +
  annotate("text", x = 77, y = 90, label = "Second", color = "black", size = 3) +
  annotate("text", x = 81.5, y = 90, label = "Third", color = "black", size = 3) +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Overall")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 45, label = "pandemic epoch") 
###save 800 x 600
#######################################################################################################################################

#########################################################################################################################################
#################################Generate outcome for Basse and Farafenni###########################################################
#####################################################################################################################################
 
## BASSE

### Aggregate by month
hep0_no_crude_vax    <- aggregate(hep0_crude == 1 ~ mmyyyy2, hdss_basse, sum) 
hep0_no_delay_vax    <- aggregate(hep0_delayed == 1 ~ mmyyyy2, hdss_basse, sum) 
penta1_no_crude_vax    <- aggregate(penta1_crude == 1 ~ mmyyyy2, hdss_basse, sum) 
penta1_no_delay_vax    <- aggregate(penta1_delayed == 1 ~ mmyyyy2, hdss_basse, sum)
penta1_no_early_vax    <- aggregate(penta1_early == 1 ~ mmyyyy2, hdss_basse, sum)
penta1_no_timely_vax    <- aggregate(penta1_timely == 1 ~ mmyyyy2, hdss_basse, sum)
hep0_no_births <- aggregate(hep0_crude == 1 | hep0_crude == 0 ~ mmyyyy2, hdss_basse, FUN=length)

head(hep0_no_crude_vax)
head(hep0_no_delay_vax)
head(penta1_no_crude_vax)
head(penta1_no_delay_vax)
head(penta1_no_early_vax)
head(penta1_no_timely_vax)
head(hep0_no_births)

##merge all the outcomes which have been aggregated by month
dat <- merge(hep0_no_crude_vax, hep0_no_delay_vax, by = "mmyyyy2")
head(dat)

dat <- merge(dat, penta1_no_crude_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_delay_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_early_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_timely_vax, by = "mmyyyy2")
dat <- merge(dat, hep0_no_births, by = "mmyyyy2")
head(dat)

##change the names of the variables to reflect the actual names
names(dat) <- c("month_year", "hep0_crude_count", "hep0_delayed_count", "penta1_crude_count", "penta1_delayed_count", 
                "penta1_early_count", "penta1_timely_count", "no_births")
head(dat)

#Create new variable month_year2 to sort the data
dat$month_year2 <- my(dat$month_year) #From lubridate
dat <- dat[order(as.Date(dat$month_year2, format="%Y/%m/%d")),]
#dat <- dat[order(as.Date(dat$month_year, format="%m/%Y")),] #This did not work
dat$tt <- 1:nrow(dat)   #Time variable for error term
dat$tt_1 <- dat$tt      #Time variable for trend

dat$tt_x63 <- rep(0,nrow(dat))
dat$tt_x63[dat$tt>63 & dat$tt <=68] <- 1    #Change between times 63 and 68, i.e. Dec 2019 and Aug 2020
dat$tt_x63s <- (dat$tt-63)*dat$tt_x63       #Equal to zero at time of interruption

dat$tt_x68 <- rep(0,nrow(dat))
dat$tt_x68[dat$tt>68 & dat$tt <=75] <- 1    #Change between times 68 and 75, i.e. Aug 2020 and Mar 2021
dat$tt_x68s <- (dat$tt-68)*dat$tt_x68       #Equal to zero at time of interruption.....

dat$tt_x75 <- rep(0,nrow(dat))
dat$tt_x75[dat$tt>75 & dat$tt <=80] <- 1    #Changes between times 75 and 80, i.e. Mar 2021 and Aug 2021
dat$tt_x75s <- (dat$tt-75)*dat$tt_x75

dat$tt_x80 <- rep(0,nrow(dat))
dat$tt_x80[dat$tt>80] <- 1                  #Changes between times 80 and 84, i.e. Aug 2021 and Dec 2021
dat$tt_x80s <- (dat$tt-80)*dat$tt_x80

##########################################################################################################################################


########################CHANGE hep0_delayed_count (numerator) and hep0_crude_count (denominator) to model other outcomes
#Model with complete data
#NB: AR models outperformed RW models; AR1 had better (in- and out-sample) predictive performance than AR2

## 1. A. Time-series for delayed HepB0 

formula <- hep0_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = hep0_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$hep0_delayed_count/dat$hep0_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_hep_dd_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)


# Create a vector of quarter-year labels based on the x values
month_year_labels <- c("Mar-15", "Jun-15", "Sep-15", "Dec-15", 
                       "Mar-16", "Jun-16", "Sep-16", "Dec-16", "Mar-17", "Jun-17", 
                       "Sep-17", "Dec-17", "Mar-18", "Jun-18", "Sep-18", "Dec-18",
                       "Mar-19", "Jun-19", "Sep-19", "Dec-19", "Mar-20", "Jun-20", 
                       "Sep-20", "Dec-20", "Mar-21", "Jun-21", "Sep-21", "Dec-21")

# Create the ggplot2 plot for time-series
ggplot(data_hep_dd_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60 with dashed style and size 1.5
  xlab("Month and Year") +  # Set the x-axis label
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse") # Add the title "BASSE"


# 1. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("hep0_delayed_count", "hep0_crude_count")] <- NA  
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- hep0_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = hep0_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$hep0_delayed_count[tt] <- rbinom(1,dat2$hep0_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_hep_dc_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                          one_step = fitted.mean.1step,
                          one_step_lower = fitted.1step$`0.025quant`,
                          one_step_upper = fitted.1step$`0.975quant`,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_hep_dc_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 70, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 70, label = "pandemic epoch") 
###save 800 x 500


#1. C. Testing for level and slope changes
formula <- hep0_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = hep0_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$hep0_delayed_count/dat$hep0_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- hep0_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_hep_dx_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                          trend.pred = trend.pred, group=group,
                          fitted_lower = fitted.all$`0.025quant`,
                          fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_dc_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_hep_dx_basse, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 95, label = "1st", color = "black") +
  annotate("text", x = 77, y = 95, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 95, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Basse")+ #add Basse
  annotate("text", x = 50, y = 70, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 70, label = "pandemic epoch") 
###save 800 x 600

#########################################################################################################################################

## 2. A. Time-series for crude HepB0 coverage 

formula <- hep0_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$hep0_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_hep_crude_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_crude_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse") # Add the title "Basse"


# 2. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c("hep0_crude_count","no_births")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- hep0_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = no_births,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$hep0_crude_count[tt] <- rbinom(1,dat2$no_births[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_hep_crude_c_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                               one_step = fitted.mean.1step,
                               one_step_lower = fitted.1step$`0.025quant`,
                               one_step_upper = fitted.1step$`0.975quant`,
                               fitted_lower = fitted.all$`0.025quant`,
                               fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_hep_crude_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 50, label = "pandemic epoch") 

###save 800 x 500

#2. C. Testing for level and slope changes
formula <- hep0_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$hep0_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- hep0_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_hep_crude_x_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                               trend.pred = trend.pred, group=group,
                               fitted_lower = fitted.all$`0.025quant`,
                               fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_hep_crude_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_hep_crude_x_basse, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "1st", color = "black") +
  annotate("text", x = 77, y = 90, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 90, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 50, label = "pandemic epoch") 
###save 800 x 600

#####################################################################################################################################

## 3. A. Time-series for delayed PENTA1 
formula <- penta1_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_delayed_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_dd_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_penta1_dd_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse") # Add the title "Basse"


# 3. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c("penta1_delayed_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_delayed_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}


#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_dc_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                             one_step = fitted.mean.1step,
                             one_step_lower = fitted.1step$`0.025quant`,
                             one_step_upper = fitted.1step$`0.975quant`,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_dc_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 40, label = "pandemic epoch") 

###save 800 x 500


#3. C. Testing for level and slope changes
formula <- penta1_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_delayed_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_dx_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                             trend.pred = trend.pred, group=group,
                             fitted_lower = fitted.all$`0.025quant`,
                             fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_dc_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_dx_basse, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 70, label = "1st", color = "black") +
  annotate("text", x = 77, y = 70, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 70, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 5, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 5, label = "pandemic epoch") 
###save 800 x 500


######################################################################################################################################

## 4. A. Time-series for early PENTA1 
formula <- penta1_early_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_early_count/dat$penta1_crude_count
dat$prop_early <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_early_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_penta1_early_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with too early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse") # Add the title "Basse"


# 4. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c( "penta1_early_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_early_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_early_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}


#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_early_c_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  one_step = fitted.mean.1step,
                                  one_step_lower = fitted.1step$`0.025quant`,
                                  one_step_upper = fitted.1step$`0.975quant`,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_early_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 45, label = "pandemic epoch") 

###save 800 x 500


#4. C. Testing for level and slope changes
formula <- penta1_early_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$penta1_early_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_early_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_early_x_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  trend.pred = trend.pred, group=group,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_early_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_early_x_basse, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 40, label = "1st", color = "black") +
  annotate("text", x = 77, y = 40, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 40, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 2, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 2, label = "pandemic epoch") 
###save 800 x 600

######################################################################################################################################

## 5. A. Time-series for crude PENTA1 coverage 
formula <- penta1_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_crude_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_crude_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse") # Add the title "Basse"


# 5. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("penta1_crude_count", "no_births")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = no_births,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_crude_count[tt] <- rbinom(1,dat2$no_births[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_crude_c_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  one_step = fitted.mean.1step,
                                  one_step_lower = fitted.1step$`0.025quant`,
                                  one_step_upper = fitted.1step$`0.975quant`,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_crude_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 30, label = "pandemic epoch")

###save 800 x 500


#5. C. Testing for level and slope changes
formula <- penta1_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$penta1_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_crude_x_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                  trend.pred = trend.pred, group=group,
                                  fitted_lower = fitted.all$`0.025quant`,
                                  fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_crude_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_crude_x_basse, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "1st", color = "black") +
  annotate("text", x = 77, y = 90, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 90, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 30, label = "pandemic epoch") 
###save 800 x 600

#########################################################################################################################################

## 6. A. Time-series for timely PENTA1 coverage 
formula <- penta1_timely_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_timely_count/dat$penta1_crude_count
dat$prop_timely <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_timely_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                 fitted_lower = fitted.all$`0.025quant`,
                                 fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_timely_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse") # Add the title "Basse"


# 6. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("penta1_timely_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_timely_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_timely_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_timely_c_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                   one_step = fitted.mean.1step,
                                   one_step_lower = fitted.1step$`0.025quant`,
                                   one_step_upper = fitted.1step$`0.975quant`,
                                   fitted_lower = fitted.all$`0.025quant`,
                                   fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_timely_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 45, label = "pandemic epoch")

###save 800 x 500



#6 C. Testing for level and slope changes
formula <- penta1_timely_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_timely_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_timely_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_timely_x_basse <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                   trend.pred = trend.pred, group=group,
                                   fitted_lower = fitted.all$`0.025quant`,
                                   fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_timely_c_basse, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_timely_x_basse, aes(y = trend.pred * 100, group=group), color = "#ffec95", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "First", color = "black", size = 3) +
  annotate("text", x = 77, y = 90, label = "Second", color = "black", size = 3) +
  annotate("text", x = 81.5, y = 90, label = "Third", color = "black", size = 3) +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Basse")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 45, label = "pandemic epoch") 
###save 800 x 600

######################################################################################################################################
##################################################### Farafenni ###################################################################
#####################################################################################################################################

## FARAFENNI

### Aggregate by month
hep0_no_crude_vax    <- aggregate(hep0_crude == 1 ~ mmyyyy2, hdss_farafenni, sum) 
hep0_no_delay_vax    <- aggregate(hep0_delayed == 1 ~ mmyyyy2, hdss_farafenni, sum) 
penta1_no_crude_vax    <- aggregate(penta1_crude == 1 ~ mmyyyy2, hdss_farafenni, sum) 
penta1_no_delay_vax    <- aggregate(penta1_delayed == 1 ~ mmyyyy2, hdss_farafenni, sum)
penta1_no_early_vax    <- aggregate(penta1_early == 1 ~ mmyyyy2, hdss_farafenni, sum)
penta1_no_timely_vax    <- aggregate(penta1_timely == 1 ~ mmyyyy2, hdss_farafenni, sum)
hep0_no_births <- aggregate(hep0_crude == 1 | hep0_crude == 0 ~ mmyyyy2, hdss_farafenni, FUN=length)

head(hep0_no_crude_vax)
head(hep0_no_delay_vax)
head(penta1_no_crude_vax)
head(penta1_no_delay_vax)
head(penta1_no_early_vax)
head(penta1_no_timely_vax)
head(hep0_no_births)

##merge all the outcomes which have been aggregated by month
dat <- merge(hep0_no_crude_vax, hep0_no_delay_vax, by = "mmyyyy2")
head(dat)

dat <- merge(dat, penta1_no_crude_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_delay_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_early_vax, by = "mmyyyy2")
dat <- merge(dat, penta1_no_timely_vax, by = "mmyyyy2")
dat <- merge(dat, hep0_no_births, by = "mmyyyy2")
head(dat)

##change the names of the variables to reflect the actual names
names(dat) <- c("month_year", "hep0_crude_count", "hep0_delayed_count", "penta1_crude_count", "penta1_delayed_count", 
                "penta1_early_count", "penta1_timely_count", "no_births")
head(dat)

#Create new variable month_year2 to sort the data
dat$month_year2 <- my(dat$month_year) #From lubridate
dat <- dat[order(as.Date(dat$month_year2, format="%Y/%m/%d")),]
#dat <- dat[order(as.Date(dat$month_year, format="%m/%Y")),] #This did not work
dat$tt <- 1:nrow(dat)   #Time variable for error term
dat$tt_1 <- dat$tt      #Time variable for trend

dat$tt_x63 <- rep(0,nrow(dat))
dat$tt_x63[dat$tt>63 & dat$tt <=68] <- 1    #Change between times 63 and 68, i.e. Dec 2019 and Aug 2020
dat$tt_x63s <- (dat$tt-63)*dat$tt_x63       #Equal to zero at time of interruption

dat$tt_x68 <- rep(0,nrow(dat))
dat$tt_x68[dat$tt>68 & dat$tt <=75] <- 1    #Change between times 68 and 75, i.e. Aug 2020 and Mar 2021
dat$tt_x68s <- (dat$tt-68)*dat$tt_x68       #Equal to zero at time of interruption.....

dat$tt_x75 <- rep(0,nrow(dat))
dat$tt_x75[dat$tt>75 & dat$tt <=80] <- 1    #Changes between times 75 and 80, i.e. Mar 2021 and Aug 2021
dat$tt_x75s <- (dat$tt-75)*dat$tt_x75

dat$tt_x80 <- rep(0,nrow(dat))
dat$tt_x80[dat$tt>80] <- 1                  #Changes between times 80 and 84, i.e. Aug 2021 and Dec 2021
dat$tt_x80s <- (dat$tt-80)*dat$tt_x80

##########################################################################################################################################


########################CHANGE hep0_delayed_count (numerator) and hep0_crude_count (denominator) to model other outcomes
#Model with complete data

## 1. A. Time-series for delayed HepB0 

formula <- hep0_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = hep0_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$hep0_delayed_count/dat$hep0_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_hep_dd_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)


# Create a vector of quarter-year labels based on the x values
month_year_labels <- c("Mar-15", "Jun-15", "Sep-15", "Dec-15", 
                       "Mar-16", "Jun-16", "Sep-16", "Dec-16", "Mar-17", "Jun-17", 
                       "Sep-17", "Dec-17", "Mar-18", "Jun-18", "Sep-18", "Dec-18",
                       "Mar-19", "Jun-19", "Sep-19", "Dec-19", "Mar-20", "Jun-20", 
                       "Sep-20", "Dec-20", "Mar-21", "Jun-21", "Sep-21", "Dec-21")

# Create the ggplot2 plot for time-series
ggplot(data_hep_dd_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60 with dashed style and size 1.5
  xlab("Month and Year") +  # Set the x-axis label
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni") # Add the title "Farafenni"


# 1. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("hep0_delayed_count", "hep0_crude_count")] <- NA  
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- hep0_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = hep0_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$hep0_delayed_count[tt] <- rbinom(1,dat2$hep0_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_hep_dc_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                one_step = fitted.mean.1step,
                                one_step_lower = fitted.1step$`0.025quant`,
                                one_step_upper = fitted.1step$`0.975quant`,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_hep_dc_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 70, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 70, label = "pandemic epoch") 
###save 800 x 500


#1. C. Testing for level and slope changes
formula <- hep0_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = hep0_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$hep0_delayed_count/dat$hep0_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- hep0_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_hep_dx_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                trend.pred = trend.pred, group=group,
                                fitted_lower = fitted.all$`0.025quant`,
                                fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_dc_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_hep_dx_farafenni, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 95, label = "1st", color = "black") +
  annotate("text", x = 77, y = 95, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 95, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with delayed HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Farafenni")+ #add Basse
  annotate("text", x = 50, y = 70, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 70, label = "pandemic epoch") 
###save 800 x 600

#########################################################################################################################################

## 2. A. Time-series for crude HepB0 coverage 

formula <- hep0_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$hep0_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_hep_crude_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                   fitted_lower = fitted.all$`0.025quant`,
                                   fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_hep_crude_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni") # Add the title "Farafenni"


# 2. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c("hep0_crude_count","no_births")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- hep0_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = no_births,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$hep0_crude_count[tt] <- rbinom(1,dat2$no_births[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_hep_crude_c_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                     one_step = fitted.mean.1step,
                                     one_step_lower = fitted.1step$`0.025quant`,
                                     one_step_upper = fitted.1step$`0.975quant`,
                                     fitted_lower = fitted.all$`0.025quant`,
                                     fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_hep_crude_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 50, label = "pandemic epoch") 

###save 800 x 500

#2. C. Testing for level and slope changes
formula <- hep0_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$hep0_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- hep0_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_hep_crude_x_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                     trend.pred = trend.pred, group=group,
                                     fitted_lower = fitted.all$`0.025quant`,
                                     fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_hep_crude_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_hep_crude_x_farafenni, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "1st", color = "black") +
  annotate("text", x = 77, y = 90, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 90, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for HepB0 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 50, label = "pandemic epoch") 
###save 800 x 600

#####################################################################################################################################

## 3. A. Time-series for delayed PENTA1 
formula <- penta1_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_delayed_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_dd_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                   fitted_lower = fitted.all$`0.025quant`,
                                   fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_penta1_dd_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni") # Add the title "Farafenni"


# 3. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c("penta1_delayed_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_delayed_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_delayed_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}


#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_dc_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                   one_step = fitted.mean.1step,
                                   one_step_lower = fitted.1step$`0.025quant`,
                                   one_step_upper = fitted.1step$`0.975quant`,
                                   fitted_lower = fitted.all$`0.025quant`,
                                   fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_dc_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 40, label = "pandemic epoch") 

###save 800 x 500


#3. C. Testing for level and slope changes
formula <- penta1_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_delayed_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_delayed_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_dx_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                   trend.pred = trend.pred, group=group,
                                   fitted_lower = fitted.all$`0.025quant`,
                                   fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_dc_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_dx_farafenni, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 70, label = "1st", color = "black") +
  annotate("text", x = 77, y = 70, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 70, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with delayed PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 5, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 5, label = "pandemic epoch") 
###save 800 x 600


######################################################################################################################################

## 4. A. Time-series for early PENTA1 
formula <- penta1_early_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_early_count/dat$penta1_crude_count
dat$prop_early <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_early_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                      fitted_lower = fitted.all$`0.025quant`,
                                      fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot for time-series
ggplot(data_penta1_early_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with too early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni") # Add the title "Farafenni"


# 4. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"),c( "penta1_early_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_early_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_early_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}


#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_early_c_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                        one_step = fitted.mean.1step,
                                        one_step_lower = fitted.1step$`0.025quant`,
                                        one_step_upper = fitted.1step$`0.975quant`,
                                        fitted_lower = fitted.all$`0.025quant`,
                                        fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_early_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion with early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 45, label = "pandemic epoch") 

###save 800 x 500


#4. C. Testing for level and slope changes
formula <- penta1_early_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_early_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_early_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_early_x_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                        trend.pred = trend.pred, group=group,
                                        fitted_lower = fitted.all$`0.025quant`,
                                        fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_early_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_early_x_farafenni, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 40, label = "1st", color = "black") +
  annotate("text", x = 77, y = 40, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 40, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion with early PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 2, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 2, label = "pandemic epoch") 
###save 800 x 600

######################################################################################################################################

## 5. A. Time-series for crude PENTA1 coverage 
formula <- penta1_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_crude_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                      fitted_lower = fitted.all$`0.025quant`,
                                      fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_crude_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni") # Add the title "Farafenni"


# 5. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("penta1_crude_count", "no_births")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_crude_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS 
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = no_births,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_crude_count[tt] <- rbinom(1,dat2$no_births[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_crude_c_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                        one_step = fitted.mean.1step,
                                        one_step_lower = fitted.1step$`0.025quant`,
                                        one_step_upper = fitted.1step$`0.975quant`,
                                        fitted_lower = fitted.all$`0.025quant`,
                                        fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_crude_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 30, label = "pandemic epoch")

###save 800 x 500


#5. C. Testing for level and slope changes
formula <- penta1_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = no_births,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out


obs <- dat$penta1_crude_count/dat$no_births
dat$prop_crude <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_crude_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_crude_x_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                        trend.pred = trend.pred, group=group,
                                        fitted_lower = fitted.all$`0.025quant`,
                                        fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_crude_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_crude_x_farafenni, aes(y = trend.pred * 100, group=group), color = "yellow", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "1st", color = "black") +
  annotate("text", x = 77, y = 90, label = "2nd", color = "black") +
  annotate("text", x = 81.5, y = 90, label = "3rd", color = "black") +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children vaccinated for PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 14))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 30, label = "pandemic epoch") 
###save 800 x 600

#########################################################################################################################################

## 6. A. Time-series for timely PENTA1 coverage 
formula <- penta1_timely_count ~ 1 + tt_1 + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

obs <- dat$penta1_timely_count/dat$penta1_crude_count
dat$prop_timely <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

# Create a data frame with the fitted values and observations
data_penta1_timely_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs,
                                       fitted_lower = fitted.all$`0.025quant`,
                                       fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_timely_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "lightgrey", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_line(aes(y = fitted_mean * 100), color = "black") +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100)) +  # Plot the observations as points
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  geom_vline(xintercept = 63, col = "blue") +  # Add the blue vertical line at x=60
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni") # Add the title "Farafenni"


# 6. B. Counterfactual scenario: One-step ahead prediction
dat1             <- dat
dat1[which(dat1$month_year2>="2019-12-01"), c("penta1_timely_count", "penta1_crude_count")] <- NA
head(dat1)
out.1step <- data.frame()
for (tt in 63:84){
  print(tt)
  if (tt==63) dat2 <- dat1[1:tt,]
  formula <- penta1_timely_count ~ 1 + tt_1 + f(tt, model = "ar1") #Note   #START - CHECK THIS
  mod2 <- inla(formula,
               data = dat2,
               family = "binomial", Ntrials = penta1_crude_count,
               control.predictor = list(compute = TRUE, link=1),
               control.compute = list(dic = TRUE))
  
  fitted.all2 = round(mod2$summary.fitted.values[,1:5], 4)
  fitted.mean2 = as.vector(data.matrix(as.vector(fitted.all2[,"mean"])))
  out.1step <- rbind(out.1step,fitted.all2[tt,])
  if (tt<84){
    dat2[tt,] <- dat[tt,]
    dat2$penta1_timely_count[tt] <- rbinom(1,dat2$penta1_crude_count[tt],fitted.mean2[tt]) #round(dat2$hep0_no_births[tt]*fitted.mean2[tt],0)     
    dat2[tt+1,] <- dat1[tt+1,]
  }
}

#For plotting
fitted.1step <- rbind(fitted.all[1:62,], out.1step)
fitted.1step[dat1$tt<63,1:5] <- NA
fitted.mean.1step = as.vector(data.matrix(as.vector(fitted.1step[,"mean"])))


# Create a data frame with the fitted values and observations
data_penta1_timely_c_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                         one_step = fitted.mean.1step,
                                         one_step_lower = fitted.1step$`0.025quant`,
                                         one_step_upper = fitted.1step$`0.975quant`,
                                         fitted_lower = fitted.all$`0.025quant`,
                                         fitted_upper = fitted.all$`0.975quant`)

# Create the ggplot2 plot
ggplot(data_penta1_timely_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.5) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as black solid line
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Month and Year") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 68, y = 45, label = "pandemic epoch")

###save 800 x 500



#6 C. Testing for level and slope changes
formula <- penta1_timely_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s + f(tt, model = "ar1") #
mod <- inla(formula,
            data = dat,
            family = "binomial", Ntrials = penta1_crude_count,
            control.predictor = list(compute = TRUE, link=1),
            control.compute = list(dic = TRUE, waic = TRUE))

summary(mod)

out <- exp(mod$summary.fixed[, c(1,3,4,5)])
out

obs <- dat$penta1_timely_count/dat$penta1_crude_count
dat$prop_delayed <- obs

fitted.all = round(mod$summary.fitted.values[,1:5], 4)
fitted.mean = as.vector(data.matrix(as.vector(fitted.all[,"mean"])))

#####################NEW
form <- penta1_timely_count ~ 1 + tt_1 + tt_x63 + tt_x63s + tt_x68 + tt_x68s + tt_x75 + tt_x75s + 
  tt_x80 + tt_x80s
mat <- model.matrix(form, data=dat)
mod.fixed <- as.matrix(mod$summary.fixed["mean"]$mean)
trend.pred <- inv.logit(mat %*% mod.fixed)
group <- c(rep(1,63), rep(2,68-63), rep(3, 75-68), rep(4,80-75), rep(5,84-80))
#####################

# Create a data frame with the fitted values and observations
data_penta1_timely_x_farafenni <- data.frame(x = 1:84, fitted_mean = fitted.mean, obs = obs, 
                                         trend.pred = trend.pred, group=group,
                                         fitted_lower = fitted.all$`0.025quant`,
                                         fitted_upper = fitted.all$`0.975quant`)


# Create the ggplot2 plot for time-series
ggplot(data_penta1_timely_c_farafenni, aes(x = x)) +
  geom_ribbon(aes(ymin = fitted_lower * 100, ymax = fitted_upper * 100), fill = "#9eacd3", alpha = 0.5) +  # Fill the area between confidence intervals for fitted mean with light grey
  geom_ribbon(aes(ymin = one_step_lower * 100, ymax = one_step_upper * 100), fill = "#fab99d", alpha = 0.3) +  # Fill the area between confidence intervals for one-step-ahead with light red
  geom_line(aes(y = fitted_mean * 100), color = "#03539d", size = 1) +  # Plot the fitted mean as blue solid line
  geom_line(data = data_penta1_timely_x_farafenni, aes(y = trend.pred * 100, group=group), color = "#ffec95", size = 1) + # Make color lighter shade of yellow
  geom_point(aes(y = obs * 100), alpha = 0.2) +  # Plot the observations as points
  geom_line(aes(y = one_step * 100), linetype = "solid", color = "#f04d3f", size = 1) +  # Plot the one-step-ahead values as red solid line
  geom_vline(xintercept = 63, col = "red", linetype = "dashed", size = 1) +  # Add the red vertical line at x=60
  geom_vline(xintercept = 68, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 75, col = "red", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 80, col = "red", linetype = "dashed", size = 0.7) +
  annotate("text", x = 69.5, y = 90, label = "First", color = "black", size = 3) +
  annotate("text", x = 77, y = 90, label = "Second", color = "black", size = 3) +
  annotate("text", x = 81.5, y = 90, label = "Third", color = "black", size = 3) +
  scale_x_continuous(limits = c(3, 84), breaks = seq(3, 84, by = 3), labels = month_year_labels) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set the y-axis limits and intervals
  xlab("Birth Cohort (month and year)") +  # Set the x-axis label
  ylab("Proportion of children with timely PENTA1 (%)") +  # Set the y-axis label
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Farafenni")+
  annotate("text", x = 50, y = 45, label = "pre-pandemic epoch") +
  annotate("text", x = 70, y = 45, label = "pandemic epoch") 
###save 800 x 600


#########################################################################################################################################
##############################Compute mean number delayed outside window (2018, 2019, 2020, 2021)#########################################
#########################################################################################################################################

##1. (OVERALL)

#subset data to include only children born from 1st Jan 2018
hdss_2018 <- subset(hdss, dob >= as.Date("2018-01-01"))
hdss_2018$mmyyyy2 <- format(hdss_2018$dob, "%m-%Y")

# Convert 'mmyyyy2' to a Date format
hdss_2018$mmyyyy2 <- as.Date(paste0("01-", hdss_2018$mmyyyy2), format = "%d-%m-%Y")

# Group by 'mmyyyy2' and calculate statistics for 'hep0_days_delayed'
result <- hdss_2018 %>%
  group_by(mmyyyy2) %>%
  summarize(mean_delay = mean(hep0_days_delayed, na.rm = TRUE),
            median_delay = median(hep0_days_delayed, na.rm = TRUE),
            sd_delay = sd(hep0_days_delayed, na.rm = TRUE),
            q1_delay = quantile(hep0_days_delayed, probs = 0.25, na.rm = TRUE),
            q3_delay = quantile(hep0_days_delayed, probs = 0.75, na.rm = TRUE))

##create new variable by numbering 1:nrow
result$tt <- 1:nrow(result) #create new variable "tt" numbered from 1:48

# Create a vector of x-axis labels
x_labels <- c("Jan-18", "Mar-18", "May-18", "Jul-18", "Sep-18", "Nov-18",
              "Jan-19", "Mar-19", "May-19", "Jul-19", "Sep-19", "Nov-19",
              "Jan-20", "Mar-20", "May-20", "Jul-20", "Sep-20", "Nov-20",
              "Jan-21", "Mar-21", "May-21", "Jul-21", "Sep-21", "Nov-21")


ggplot(result, aes(x = tt)) +
  geom_rect(aes(xmin = 31, xmax = 33, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_rect(aes(xmin = 37, xmax = 40, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_rect(aes(xmin = 43, xmax = 45, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_vline(xintercept = 27, col = "red", linetype = "dashed", linewidth = 0.75) + 
  geom_hline(yintercept = 21, col = "black", linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = 32, y = 28, label = "1st Wave", color = "black", angle = 90) +
  annotate("text", x = 38.5, y = 28, label = "2nd Wave", color = "black", angle = 90) +
  annotate("text", x = 44, y = 28, label = "3rd Wave", color = "black", angle = 90) +
  annotate("text", x = 13, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 37, y = 10, label = "pandemic epoch")+
  geom_point(aes(y = mean_delay), shape = 15, size = 2, color = "red") +
  geom_line(aes(y = mean_delay), linewidth = 0.3, color = "black") +
  scale_x_continuous(limits = c(1, 48), breaks = seq(1, 48, by = 2),
                     labels = x_labels) +
  scale_y_continuous(limits = c(1, 30), breaks = seq(1, 30, by = 1)) +
  xlab("Cohort (Month and Year)") +
  ylab("Number of days delayed (mean)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Delayed HepB0 (overall)")
#SAVE 600 X 600

######################################################################################################################################

##2. (Basse)

#subset data to include only children born from 1st Jan 2018
hdss_2018_basse <- subset(hdss_basse, dob >= as.Date("2018-01-01"))
hdss_2018_basse$mmyyyy2 <- format(hdss_2018_basse$dob, "%m-%Y")

# Convert 'mmyyyy2' to a Date format
hdss_2018_basse$mmyyyy2 <- as.Date(paste0("01-", hdss_2018_basse$mmyyyy2), format = "%d-%m-%Y")

# Group by 'mmyyyy2' and calculate statistics for 'hep0_days_delayed'
result_basse <- hdss_2018_basse %>%
  group_by(mmyyyy2) %>%
  summarize(mean_delay = mean(hep0_days_delayed, na.rm = TRUE),
            median_delay = median(hep0_days_delayed, na.rm = TRUE),
            sd_delay = sd(hep0_days_delayed, na.rm = TRUE),
            q1_delay = quantile(hep0_days_delayed, probs = 0.25, na.rm = TRUE),
            q3_delay = quantile(hep0_days_delayed, probs = 0.75, na.rm = TRUE))

##create new variable by numbering 1:nrow
result_basse$tt <- 1:nrow(result_basse) #create new variable "tt" numbered from 1:48


ggplot(result_basse, aes(x = tt)) +
  geom_rect(aes(xmin = 31, xmax = 33, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_rect(aes(xmin = 37, xmax = 40, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_rect(aes(xmin = 43, xmax = 45, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_vline(xintercept = 27, col = "red", linetype = "dashed", linewidth = 0.75) + 
  geom_hline(yintercept = 21, col = "black", linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = 32, y = 28, label = "1st Wave", color = "black", angle = 90) +
  annotate("text", x = 38.5, y = 28, label = "2nd Wave", color = "black", angle = 90) +
  annotate("text", x = 44, y = 28, label = "3rd Wave", color = "black", angle = 90) +
  annotate("text", x = 13, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 37, y = 10, label = "pandemic epoch")+
  geom_point(aes(y = mean_delay), shape = 15, size = 2, color = "red") +
  geom_line(aes(y = mean_delay), linewidth = 0.3, color = "black") +
  scale_x_continuous(limits = c(1, 48), breaks = seq(1, 48, by = 2),
                     labels = x_labels) +
  scale_y_continuous(limits = c(1, 30), breaks = seq(1, 30, by = 1)) +
  xlab("Cohort (Month and Year)") +
  ylab("Number of days delayed (mean)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Delayed HepB0 (Basse)")
##save 600 x 600

######################################################################################################################################
##3. (Farafenni)

#subset data to include only children born from 1st Jan 2018
hdss_2018_farafenni <- subset(hdss_farafenni, dob >= as.Date("2018-01-01"))
hdss_2018_farafenni$mmyyyy2 <- format(hdss_2018_farafenni$dob, "%m-%Y")

# Convert 'mmyyyy2' to a Date format
hdss_2018_farafenni$mmyyyy2 <- as.Date(paste0("01-", hdss_2018_farafenni$mmyyyy2), format = "%d-%m-%Y")

# Group by 'mmyyyy2' and calculate statistics for 'hep0_days_delayed'
result_farafenni <- hdss_2018_farafenni %>%
  group_by(mmyyyy2) %>%
  summarize(mean_delay = mean(hep0_days_delayed, na.rm = TRUE),
            median_delay = median(hep0_days_delayed, na.rm = TRUE),
            sd_delay = sd(hep0_days_delayed, na.rm = TRUE),
            q1_delay = quantile(hep0_days_delayed, probs = 0.25, na.rm = TRUE),
            q3_delay = quantile(hep0_days_delayed, probs = 0.75, na.rm = TRUE))

##create new variable by numbering 1:nrow
result_farafenni$tt <- 1:nrow(result_farafenni) #create new variable "tt" numbered from 1:48


ggplot(result_farafenni, aes(x = tt)) +
  geom_rect(aes(xmin = 31, xmax = 33, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_rect(aes(xmin = 37, xmax = 40, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_rect(aes(xmin = 43, xmax = 45, ymin = -Inf, ymax = Inf), fill = "#f9eee8") +
  geom_vline(xintercept = 27, col = "red", linetype = "dashed", linewidth = 0.75) + 
  geom_hline(yintercept = 21, col = "black", linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = 32, y = 28, label = "1st Wave", color = "black", angle = 90) +
  annotate("text", x = 38.5, y = 28, label = "2nd Wave", color = "black", angle = 90) +
  annotate("text", x = 44, y = 28, label = "3rd Wave", color = "black", angle = 90) +
  annotate("text", x = 13, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 37, y = 10, label = "pandemic epoch")+
  geom_point(aes(y = mean_delay), shape = 15, size = 2, color = "red") +
  geom_line(aes(y = mean_delay), linewidth = 0.3, color = "black") +
  scale_x_continuous(limits = c(1, 48), breaks = seq(1, 48, by = 2),
                     labels = x_labels) +
  scale_y_continuous(limits = c(1, 30), breaks = seq(1, 30, by = 1)) +
  xlab("Cohort (Month and Year)") +
  ylab("Number of days delayed (mean)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Delayed HepB0 (Farafenni)")
##save 600 x 600

#########################################################################################################################################
##############################Compute difference between paired months (2020-2019, 2021-2020)############################################
#########################################################################################################################################

#1. Delayed HepB0 (overall)

#clean data
diff_hep_dd <- subset(data_hep_dd, x >= 49) #subset data to include Jan-2019 and above
diff_hep_dd$tt <- 1:nrow(diff_hep_dd) #create new variable "tt" numbered from 1:36
diff_hep_dd$cohort <- sprintf('%02d-%04d', (diff_hep_dd$tt - 1) %% 12 + 1, 2019 + (diff_hep_dd$tt - 1) %/% 12) #convert "tt" to mm-yy
diff_hep_dd$cohort_date <- as.Date(paste0("01-", diff_hep_dd$cohort), format = "%d-%m-%Y") #convert "cohort" to actual date field


# Create a new dataframe for the calculations
new_diff_hep_dd <- data.frame(cohort = character(0), percent_diff = numeric(0), percent_diff2 = numeric(0))

# Calculate percent differences
for (i in 1:nrow(diff_hep_dd)) {
  if (year(diff_hep_dd$cohort_date[i]) >= 2020) {
    prev_month <- format(diff_hep_dd$cohort_date[i] - years(1), "%m")
    prev_year <- "2019"
    prev_cohort <- paste0(prev_month, "-", prev_year)
    prev_row <- diff_hep_dd[diff_hep_dd$cohort == prev_cohort, ]
    
    prev_month_2020 <- format(diff_hep_dd$cohort_date[i] - years(1), "%m")
    prev_year_2020 <- "2020"
    prev_cohort_2020 <- paste0(prev_month_2020, "-", prev_year_2020)
    prev_row_2020 <- diff_hep_dd[diff_hep_dd$cohort == prev_cohort_2020, ]
    
    percent_diff <- diff_hep_dd$fitted_mean[i] - prev_row$fitted_mean
    
    if (!is.na(prev_row_2020$fitted_mean)) {
      percent_diff2 <- diff_hep_dd$fitted_mean[i] - prev_row_2020$fitted_mean
    } else {
      percent_diff2 <- NA
    }
    
    new_row <- data.frame(cohort = diff_hep_dd$cohort[i], percent_diff = percent_diff, percent_diff2 = percent_diff2)
    new_diff_hep_dd <- rbind(new_diff_hep_dd, new_row)
  }
}

# Replace 0.0000 with NA in percent_diff2 column for 2020
new_diff_hep_dd$percent_diff2[new_diff_hep_dd$cohort %in% c("01-2020", "02-2020", "03-2020", "04-2020", "05-2020", "06-2020", "07-2020", "08-2020", "09-2020", "10-2020", "11-2020", "12-2020")] <- NA

new_diff_hep_dd$tt <- 1:nrow(new_diff_hep_dd) #create new variable "tt" numbered from 1:36

# Print the new dataframe
print(new_diff_hep_dd)


# Create a vector of x-axis labels
x_axis_labels <- c("Jan-2020", "Feb-2020", "Mar-2020", "Apr-2020", "May-2020", "Jun-2020", 
                   "Jul-2020", "Aug-2020", "Sep-2020", "Oct-2020", "Nov-2020", "Dec-2020", 
                   "Jan-2021", "Feb-2021", "Mar-2021", "Apr-2021", "May-2021", "Jun-2021", 
                   "Jul-2021", "Aug-2021", "Sep-2021", "Oct-2021", "Nov-2021", "Dec-2021")

ggplot(new_diff_hep_dd, aes(x = tt)) +
  geom_point(aes(y = percent_diff2 * 100, color = "2021 - 2020"), shape = 15, size = 2) +
  geom_rect(aes(xmin = 7, xmax = 9, ymin = -Inf, ymax = Inf), fill = "#dee1e6", color = NA, alpha = 0.02) +
  geom_rect(aes(xmin = 13, xmax = 16, ymin = -Inf, ymax = Inf), fill = "#dee1e6", color = NA, alpha = 0.02) +
  geom_rect(aes(xmin = 19, xmax = 21, ymin = -Inf, ymax = Inf), fill = "#dee1e6", color = NA, alpha = 0.02) +
  geom_line(aes(y = percent_diff * 100, color = "2020,2021 - 2019"), size = 0.7) +
  geom_point(aes(y = percent_diff * 100, color = "2020,2021 - 2019"), shape = 15, size = 2) +
  geom_line(aes(y = percent_diff2 * 100, color = "2021 - 2020"), size = 0.7) +
  geom_hline(yintercept = 0.0, col = "black", size = 0.7) +
  geom_vline(xintercept = 3, col = "red", linetype = "dashed", size = 0.7) + 
  geom_vline(xintercept = 13, col = "grey", linetype = "dashed", size = 0.7) + 
  annotate("text", x = 8, y = 10, label = "1st wave", color = "black", size = 3) +
  annotate("text", x = 14.5, y = 10, label = "2nd wave", color = "black", size = 3) +
  annotate("text", x = 20, y = 10, label = "3rd wave", color = "black", size = 3) +
  annotate("text", x = 2.5, y = 1, label = "Delay increased", color = "black", size = 4) +
  annotate("text", x = 2.5, y = -1, label = "Delay decreased", color = "black", size = 4) +
  scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, by = 1),
                     labels = x_axis_labels) +
  scale_y_continuous(limits = c(-20, 10), breaks = seq(-20, 10, by = 5)) +
  xlab("Cohort (Month and Year)") +
  ylab("Relative difference (percent change)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Delayed HepB0 (overall)")

###save 900 x 550
######################################################################################################################################
################################################ Create supplementary material########################################################
#####################################################################################################################################
#1. OVERALL
# Your provided dataset
cohort <- data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  `2015` = c(829, 670, 659, 669, 635, 773, 649, 596, 775, 782, 744, 676),
  `2016` = c(753, 641, 594, 488, 498, 596, 457, 513, 769, 800, 803, 744),
  `2017` = c(803, 667, 679, 607, 531, 579, 502, 552, 747, 877, 795, 685),
  `2018` = c(714, 637, 682, 637, 598, 511, 435, 615, 884, 1021, 966, 839),
  `2019` = c(805, 755, 790, 572, 650, 541, 485, 634, 869, 932, 856, 757),
  `2020` = c(873, 776, 678, 637, 649, 552, 544, 733, 911, 965, 851, 766),
  `2021` = c(723, 573, 556, 482, 528, 503, 425, 461, 695, 758, 660, 665))

# Reshape the data for plotting
cohort_long <- reshape2::melt(cohort, id.vars = "Month", variable.name = "Year", value.name = "Value")

# Convert Month to factor with correct order
cohort_long$Month <- factor(cohort_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Create the line plot
ggplot(data = cohort_long, aes(x = Month, y = Value, color = Year, group = Year)) +
  geom_point(aes(y = Value, color = Year), shape = 15, size = 2) +
  geom_line(size = 1.3) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 100)) +
  scale_color_brewer(palette = "RdYlGn", labels = c("2015", "2016", "2017", "2018", "2019", "2020", "2021")) +  # Set color palette and legend labels
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Overall Cohort Trends (2015-2021)",
       x = "Monthly birth cohort",
       y = "Number of children",
       color = "Year")
##save 900 by 600 "cohort_trend_overall####
#################################################################################
##2. BASSE
# Your provided dataset
cohort_basse <- data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  `2015` = c(653, 504, 475, 511, 518, 550, 471, 458, 611, 610, 576, 500),
  `2016` = c(557, 453, 431, 364, 388, 401, 329, 407, 595, 623, 612, 587),
  `2017` = c(594, 476, 496, 456, 391, 416, 382, 419, 568, 678, 616, 495),
  `2018` = c(528, 450, 502, 499, 468, 386, 334, 465, 682, 774, 740, 644),
  `2019` = c(597, 552, 579, 444, 502, 427, 390, 480, 660, 742, 672, 595),
  `2020` = c(647, 575, 507, 495, 468, 432, 441, 548, 712, 740, 658, 579),
  `2021` = c(546, 420, 402, 358, 394, 389, 308, 353, 527, 593, 509, 544)
)

# Reshape the data for plotting
cohort_basse_long <- reshape2::melt(cohort_basse, id.vars = "Month", variable.name = "Year", value.name = "Value")

# Convert Month to a factor with correct order
cohort_basse_long$Month <- factor(cohort_basse_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Create the line plot
ggplot(data = cohort_basse_long, aes(x = Month, y = Value, color = Year, group = Year)) +
  geom_point(aes(y = Value, color = Year), shape = 15, size = 2) +
  geom_line(size = 1.3) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 100)) +
  scale_color_brewer(palette = "RdYlGn", labels = c("2015", "2016", "2017", "2018", "2019", "2020", "2021")) +  # Set color palette and legend labels
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Basse HDSS Cohort Trends (2015-2021)",
       x = "Monthly birth cohort",
       y = "Number of children",
       color = "Year")
##save 900 by 600 "cohort_trend_basse####
#################################################################################
##3. FARAFENNI
# Your provided dataset
cohort_farafenni <- data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  `2015` = c(176, 166, 184, 158, 117, 223, 178, 138, 164, 172, 168, 176),
  `2016` = c(196, 188, 163, 124, 110, 195, 128, 106, 174, 177, 191, 157),
  `2017` = c(209, 191, 183, 151, 140, 163, 120, 133, 179, 199, 179, 190),
  `2018` = c(186, 187, 180, 138, 130, 125, 101, 150, 202, 247, 226, 195),
  `2019` = c(208, 203, 211, 128, 148, 114, 95, 154, 209, 190, 184, 162),
  `2020` = c(226, 201, 171, 142, 181, 120, 103, 185, 199, 225, 193, 187),
  `2021` = c(177, 153, 154, 124, 134, 114, 117, 108, 168, 165, 151, 121)
)

# Reshape the data for plotting
cohort_farafenni_long <- reshape2::melt(cohort_farafenni, id.vars = "Month", variable.name = "Year", value.name = "Value")

# Convert Month to a factor with correct order
cohort_farafenni_long$Month <- factor(cohort_farafenni_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Create the line plot
ggplot(data = cohort_farafenni_long, aes(x = Month, y = Value, color = Year, group = Year)) +
  geom_point(aes(y = Value, color = Year), shape = 15, size = 2) +
  geom_line(size = 1.3) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50)) +
  scale_color_brewer(palette = "RdYlGn", labels = c("2015", "2016", "2017", "2018", "2019", "2020", "2021")) +  # Set color palette and legend labels
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Farafenni HDSS Cohort Trends (2015-2021)",
       x = "Monthly birth cohort",
       y = "Number of children",
       color = "Year")
##save 900 by 600 "cohort_trend_farafenni####
#################################################################################