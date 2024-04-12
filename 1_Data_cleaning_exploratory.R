##########Load packages################################
library(ggplot2)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(dplyr)


#Set working directory
setwd("C:/filepath")


### read HDSS data into R###
hdss <- read_csv("data.csv")

#subset data to include only children born from 1st January 2015######################################################################## 

hdss$dob <- as.Date(hdss$dob, format = "%d/%m/%Y")# Convert the "dob" variable to a Date format
hdss_2015 <- subset(hdss, dob >= as.Date("2015-01-01"))


#create new variable called Epoch ############################################################################
hdss_2015$epoch <- ifelse(hdss_2015$dob >= as.Date("2020-01-01"), "pandemic epoch", "prepandemic epoch")


##################################################################################

#hdss2015
hdss_2015$latitude <- str_replace_all(hdss_2015$latitude, "[^[:alnum:][:space:].-]", "")

# remove non-ASCII characters from the "longitude" column
hdss_2015$longitude <- str_replace_all(hdss_2015$longitude, "[^[:alnum:][:space:].-]", "")

########################################################################################################################################
#CALCULATE AGE OF VACCINATION

hdss_2015$hep0_age_vac <- as.numeric(as.Date(hdss_2015$date_hep0, format = "%d/%m/%Y") - as.Date(hdss_2015$dob, format = "%d/%m/%Y"))
hdss_2015$penta1_age_vac <- as.numeric(as.Date(hdss_2015$date_penta1, format = "%d/%m/%Y") - as.Date(hdss_2015$dob, format = "%d/%m/%Y"))

##############################################################################################################################################
#EXPLORE THE DATA#

# create a new dataframe with subset columns to see side by side dob, date_hep0, and hep0_age_vac (there are negative and very large values)
hdss_2015_subset <- hdss_2015[, c("dob", "date_hep0", "hep0_age_vac", "penta1_age_vac", "epoch")]

##subsetdata
hdss_2015_basse <- subset(hdss_2015, site=="Basse") ##(Basse)##
hdss__2015_farafenni <- subset(hdss_2015, site=="Farafenni") ##(Farafenni)##

# Calculate the proportion of negative values in the hep0_age_vac column
nrow(hdss_2015)#give no. of rows in the data
nrow(hdss_2015_basse)#give no. of rows in the data_basse
nrow(hdss__2015_farafenni)#give no. of rows in the data_farafenni

sum(hdss_2015$hep0_age_vac < 0, na.rm = TRUE) #counts number of rows with negative values in the hep0_age_vac
sum(hdss_2015_basse$hep0_age_vac < 0, na.rm = TRUE) #counts number of rows with negative values in the hep0_age_vac_basse
sum(hdss__2015_farafenni$hep0_age_vac < 0, na.rm = TRUE) #counts number of rows with negative values in the hep0_age_vac_farafenni


sum(hdss_2015$penta1_age_vac < 0, na.rm = TRUE) #counts number of rows with negative values in the penta1
sum(hdss_2015_basse$penta1_age_vac < 0, na.rm = TRUE)#counts number of rows with negative values in the penta1_basse
sum(hdss__2015_farafenni$penta1_age_vac < 0, na.rm = TRUE)#counts number of rows with negative values in the penta1_farafenni


sum(hdss_2015$hep0_age_vac > 150, na.rm = TRUE) #counts number of rows with values >150 in the hep0_age_vac
sum(hdss_2015_basse$hep0_age_vac > 150, na.rm = TRUE) #counts number of rows with values >150 in the hep0_age_vac_basse
sum(hdss__2015_farafenni$hep0_age_vac > 150, na.rm = TRUE) #counts number of rows with values >150 in the hep0_age_vac_farafenni


neg_values <- hdss_2015[hdss_2015$hep0_age_vac < 0 & !is.na(hdss_2015$hep0_age_vac), ]#Subset the data to include only rows with -ve values (160)
large_values <- hdss_2015[hdss_2015$hep0_age_vac > 150 & !is.na(hdss_2015$hep0_age_vac), ]#subset data to include large values >150 (93)


# create a histogram with normal distribution curve
#hepB0
ggplot(data = hdss_2015 %>% 
         filter(!is.na(hep0_age_vac)) %>% 
         filter(hep0_age_vac > 0) %>% 
         filter(hep0_age_vac <= 120), aes(x = hep0_age_vac)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "cornflowerblue", color = "white") +
  ggtitle("Distribution of hepB0_age_vac") +
  xlab("hepB0_age_vac") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 75, by = 5), limits = c(0, 75)) +
  theme_bw()

#penta1
ggplot(data = hdss_2015 %>% 
         filter(!is.na(penta1_age_vac)) %>% 
         filter(penta1_age_vac > 0) %>% 
         filter(penta1_age_vac <= 220), aes(x = penta1_age_vac)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "cornflowerblue", color = "white") +
  ggtitle("Distribution of penta1_age_vac") +
  xlab("penta1_age_vac") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 200, by = 10), limits = c(0, 200)) +
  theme_bw()


#facet distribution by "epoch"
#hepB0
ggplot(data = hdss_2015 %>% 
         filter(!is.na(hep0_age_vac)) %>% 
         filter(hep0_age_vac > 0) %>% 
         filter(hep0_age_vac <= 120), aes(x = hep0_age_vac, fill = epoch)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", position = "dodge") +
  ggtitle("Distribution of hepB0_age_vac") +
  xlab("hepB0_age_vac") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 75, 5),limits = c(0, 75)) +
  facet_wrap(~epoch, nrow = 1) +
  theme_bw()+
  theme(legend.position = "bottom")

#penta1
ggplot(data = hdss_2015 %>% 
         filter(!is.na(penta1_age_vac)) %>% 
         filter(penta1_age_vac > 0) %>% 
         filter(penta1_age_vac <= 220), aes(x = penta1_age_vac, fill = epoch)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", position = "dodge") +
  ggtitle("Distribution of penta1_age_vac") +
  xlab("penta1_age_vac") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 200, by = 10), limits = c(0, 200)) +
  facet_wrap(~epoch, nrow = 1) +
  theme_bw()+
  theme(legend.position = "bottom")


#facet distribution by "epoch" and "site"
#hepB0
ggplot(data = hdss_2015 %>% 
         filter(!is.na(hep0_age_vac)) %>% 
         filter(hep0_age_vac > 0) %>% 
         filter(hep0_age_vac <= 120), 
       aes(x = hep0_age_vac, fill = epoch)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", position = "dodge") +
  ggtitle("Distribution of hepB0_age_vac") +
  xlab("hepB0_age_vac") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 75, 10),limits = c(0, 75)) +
  facet_grid(. ~ site + epoch) +
  theme_bw()+
  theme(legend.position = "bottom")

#penta1
ggplot(data = hdss_2015 %>% 
         filter(!is.na(penta1_age_vac)) %>% 
         filter(penta1_age_vac > 0) %>% 
         filter(penta1_age_vac <= 220), 
       aes(x = penta1_age_vac, fill = epoch)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", position = "dodge") +
  ggtitle("Distribution of penta1_age_vac") +
  xlab("penta1_age_vac") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 200, 20),limits = c(0, 200)) +
  facet_grid(. ~ site + epoch) +
  theme_bw()+
  theme(legend.position = "bottom")


########################################################################################################################################
#CATEGORIZE TIMELINESS AND NUMBER OF DAYS DELAYED

###categorise dimensions of timeliness based on benchmark definitions for hep0 (>1 day is delayed, convert -ve values to NAs)###
#hepB0
hdss_2015 <- hdss_2015 %>%
  mutate(hep0_age_vac_cat = cut(ifelse(hep0_age_vac < 0, NA, as.numeric(hep0_age_vac)),
                                breaks = c(-Inf, 1, Inf), labels = c("timely", "delayed")))  
#penta1
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_age_vac_cat = case_when(
    site == "Farafenni" & penta1_age_vac >= 61 & penta1_age_vac <= 90 ~ "timely",
    site == "Farafenni" & penta1_age_vac < 61 ~ "early",
    site == "Farafenni" & penta1_age_vac > 90 ~ "delayed",
    site == "Basse" & dob >= as.Date("2020-01-01") & penta1_age_vac >= 42 & penta1_age_vac <= 70 ~ "timely",
    site == "Basse" & dob >= as.Date("2020-01-01") & penta1_age_vac < 42 ~ "early",
    site == "Basse" & dob >= as.Date("2020-01-01") & penta1_age_vac > 70 ~ "delayed",
    site == "Basse" & dob < as.Date("2020-01-01") & penta1_age_vac >= 61 & penta1_age_vac <= 90 ~ "timely",
    site == "Basse" & dob < as.Date("2020-01-01") & penta1_age_vac < 61 ~ "early",
    site == "Basse" & dob < as.Date("2020-01-01") & penta1_age_vac > 90 ~ "delayed",
    TRUE ~ NA_character_))


##calculate difference in timeliness as continuous variable based on benchmark 
#hepB0 (note: if -ve value OR >150 assigned NAs)##
hdss_2015 <- hdss_2015 %>%
  mutate(hep0_days_delayed = ifelse(hep0_age_vac < 0 | hep0_age_vac > 150, NA,
                                    ifelse(hep0_age_vac == 0 | hep0_age_vac == 1, 0,
                                           as.numeric(hep0_age_vac) - 1)))

#penta1_delayed
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_days_delayed = ifelse(penta1_age_vac < 0 | is.na(penta1_age_vac) | penta1_age_vac <= 90, 
                                      NA, penta1_age_vac - 90))

#penta1_early
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_days_early = ifelse(penta1_age_vac < 0 | is.na(penta1_age_vac) | penta1_age_vac >= 61, 
                                    NA, 61 - penta1_age_vac))


########################################################################################################################################
#CREATE NEW VARIABLES with 0's and 1's 
#1. for hep0 crude coverage (note: assign 0 to cells with NAs and 1 to cells with -ve/+ve number in hep0_age_vac)
hdss_2015 <- hdss_2015 %>%
  mutate(hep0_crude = ifelse(is.na(hep0_age_vac), 0, 1))


#2. for hep0_timely (note: 1 to cells with timely, 0 to cells with "delayed" in hep0_age_vac_cat. NAs remain as NAs)
hdss_2015 <- hdss_2015 %>%
  mutate(hep0_timely = ifelse(hep0_age_vac_cat == "timely", 1, ifelse(hep0_age_vac_cat == "delayed", 0, NA)))


#3. for hep0_delayed (note: 1 to cells with delayed, 0 to cells with "timely" in hep0_age_vac_cat. NAs remain as NAs)
hdss_2015 <- hdss_2015 %>%
  mutate(hep0_delayed = ifelse(hep0_age_vac_cat == "delayed", 1, ifelse(hep0_age_vac_cat == "timely", 0, NA)))

#4. for penta1 crude coverage (note: assign 0 to cells with NAs and 1 to cells with -ve/+ve number in penta1_age_vac)
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_crude = ifelse(is.na(penta1_age_vac), 0, 1))

#5. for penta1_timely (note: 1 to cells with timely, 0 to cells with "delayed" AND "early" in penta1_age_vac_cat. NAs remain as NAs)
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_timely = ifelse(penta1_age_vac_cat == "timely", 1, ifelse(penta1_age_vac_cat %in% c("delayed", "early"), 0, NA)))


#6. for penta1_delayed (note: 1 to cells with delayed, 0 to cells with "timely" AND "early" in penta1_age_vac_cat. NAs remain as NAs)
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_delayed = ifelse(penta1_age_vac_cat == "delayed", 1, ifelse(penta1_age_vac_cat %in% c("timely", "early"), 0, NA)))


#7. for penta1_delayed (note: 1 to cells with delayed, 0 to cells with "timely" AND "delayed" in penta1_age_vac_cat. NAs remain as NAs)
hdss_2015 <- hdss_2015 %>%
  mutate(penta1_early = ifelse(penta1_age_vac_cat == "early", 1, ifelse(penta1_age_vac_cat %in% c("timely", "delayed"), 0, NA)))



#####################################################################################################################################

###FURTHER EXPLORATORY ANALYSIS

#1. make cummulative curve of hep0 age vaccination for 2018,2019,2020, 2021
hdss_filtered <- hdss_2015 %>%
  filter(!is.na(hep0_age_vac),
         hep0_age_vac >= 0,
         hep0_age_vac < 150) %>%
  group_by(year_of_birth, hep0_age_vac) %>%
  summarise(n_children = n()) %>%
  mutate(cumulative_children = cumsum(n_children),
         total_children = sum(n_children),
         percent_vaccinated = cumulative_children / total_children * 100) %>%
  ungroup()

ggplot(hdss_filtered %>% filter(hep0_age_vac >= 0 & hep0_age_vac < 150), 
       aes(x = hep0_age_vac, y = percent_vaccinated, color = factor(year_of_birth))) +
  geom_step() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  ggtitle("Cumulative HepB0 Vaccination Coverage by Age") +
  xlab("Age at HepB0 Vaccination (days)") +
  ylab("Children Vaccinated (%)") +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(0, 90)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_color_discrete(name = "Year of Birth") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2021 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2021")) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2020 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2020")) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2019 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2019")) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2018 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2018")) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2017 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2017")) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2016 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2016")) +
  geom_hline(data = filter(hdss_filtered, year_of_birth == 2015 & hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(year_of_birth), linetype = "2015")) +
  scale_linetype_manual(name = "Year of Birth", values = c("2021" = "dashed", "2020" = "dashed",
                                                           "2019" = "dashed", "2018" = "dashed",
                                                           "2017" = "dashed", "2016" = "dashed",
                                                           "2015" = "dashed")) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "solid",
                                                               "solid", "solid", "solid")))) +
  annotate("text", x = 2, y = 100, label = "Age 1 day", vjust = 1)

###############

#2. make cummulative curve of penta1 age vaccination for 2015,2016,2017,2018,2019,2020,2021
hdss_filtered_penta1 <- hdss_2015 %>%
  filter(!is.na(penta1_age_vac),
         penta1_age_vac > 0,
         penta1_age_vac <= 220) %>%
  group_by(year_of_birth, penta1_age_vac) %>%
  summarise(n_children = n()) %>%
  mutate(cumulative_children = cumsum(n_children),
         total_children = sum(n_children),
         percent_vaccinated = cumulative_children / total_children * 100) %>%
  ungroup()

ggplot(hdss_filtered_penta1 %>% filter(penta1_age_vac >= 0 & penta1_age_vac < 220), 
       aes(x = penta1_age_vac, y = percent_vaccinated, color = factor(year_of_birth))) +
  geom_step() +
  geom_vline(xintercept = 61, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 90, linetype = "dashed", color = "red") +
  ggtitle("Cumulative PENTA1 Vaccination Coverage by Age") +
  xlab("Age at PENTA1 Vaccination (days)") +
  ylab("Children Vaccinated (%)") +
  scale_x_continuous(breaks = seq(0, 220, 10), limits = c(0, 220)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_color_discrete(name = "Year of Birth") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "solid",
                                                               "solid", "solid", "solid")))) +
  annotate("text", x = 62, y = 100, label = "Age 61 day", vjust = 1)


#############



#3. make cummulative curve of hep0 age vaccination for prepandemic vs pandemic
hdss_filtered_epoch <- hdss_2015 %>%
  filter(!is.na(hep0_age_vac),
         hep0_age_vac >= 0,
         hep0_age_vac < 150) %>%
  group_by(epoch, hep0_age_vac) %>%
  summarise(n_children = n()) %>%
  mutate(cumulative_children = cumsum(n_children),
         total_children = sum(n_children),
         percent_vaccinated = cumulative_children / total_children * 100) %>%
  ungroup()

ggplot(hdss_filtered_epoch %>% filter(hep0_age_vac >= 0 & hep0_age_vac < 150), 
       aes(x = hep0_age_vac, y = percent_vaccinated, color = factor(epoch))) +
  geom_step() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  ggtitle("Cumulative HepB0 Vaccination Coverage by Age") +
  xlab("Age at HepB0 Vaccination (days)") +
  ylab("Children Vaccinated (%)") +
  scale_x_continuous(breaks = seq(0, 90, 5), limits = c(0, 90)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_color_discrete(name = "epoch") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_hline(data = hdss_filtered_epoch %>% filter(hep0_age_vac == 1),
             aes(yintercept = percent_vaccinated, color = factor(epoch)),
             linetype = "dashed") +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid")))) +
  annotate("text", x = 2, y = 100, label = "Age 1 day", vjust = 1)


########

#4. make cummulative curve of penta1 age vaccination for prepandemic vs pandemic
hdss_filtered_epoch_penta1 <- hdss_2015 %>%
  filter(!is.na(penta1_age_vac),
         penta1_age_vac > 0,
         penta1_age_vac <= 220) %>%
  group_by(epoch, penta1_age_vac) %>%
  summarise(n_children = n()) %>%
  mutate(cumulative_children = cumsum(n_children),
         total_children = sum(n_children),
         percent_vaccinated = cumulative_children / total_children * 100) %>%
  ungroup()

ggplot(hdss_filtered_epoch_penta1 %>% filter(penta1_age_vac > 0 & penta1_age_vac <= 220), 
       aes(x = penta1_age_vac, y = percent_vaccinated, color = factor(epoch))) +
  geom_step() +
  geom_vline(xintercept = 61, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 90, linetype = "dashed", color = "red") +
  ggtitle("Cumulative PENTA1 Vaccination Coverage by Age") +
  xlab("Age at PENTA1 Vaccination (days)") +
  ylab("Children Vaccinated (%)") +
  scale_x_continuous(breaks = seq(0, 220, 10), limits = c(0, 220)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_color_discrete(name = "epoch") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_hline(data = hdss_filtered_epoch_penta1 %>% filter(penta1_age_vac == 61),
             aes(yintercept = percent_vaccinated, color = factor(epoch)),
             linetype = "dashed") +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid")))) +
  annotate("text", x = 62, y = 100, label = "Age 61 day", vjust = 1)

############

#5. make combines violin and box plots of hep0 days delayed for all years (2018, 2019, 2020, 2021)
hdss_2015 %>%
  mutate(hep0_days_delayed = ifelse(hep0_days_delayed == 0, NA, hep0_days_delayed)) %>%
  ggplot(aes(x = factor(year_of_birth), y = hep0_days_delayed, fill = factor(year_of_birth))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 3, color = "black", 
               aes(label = paste("Mean = ", round(..y.., 1)) )) +
  xlab("Year of Birth") +
  ylab("Days Delayed") +
  ggtitle("HepB0 Vaccination Delay by Year of Birth") +
  guides(fill = FALSE) +  # Remove the legend for fill
  scale_fill_discrete(name = "Year of Birth") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 10))

#####
#6. make combines violin and box plots of hep0 days delayed for all years (2018, 2019, 2020, 2021)
hdss_2015 %>%
  mutate(penta1_days_delayed = ifelse(penta1_days_delayed == 0, NA, penta1_days_delayed)) %>%
  ggplot(aes(x = factor(year_of_birth), y = penta1_days_delayed, fill = factor(year_of_birth))) +
  geom_violin(fill = "lightgrey",trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  stat_summary(fun.y = mean, geom = "text", vjust = -1, size = 3, color = "black", 
               aes(label = paste("Mean = ", round(..y.., 1)))) +
  xlab("Year of Birth") +
  ylab("Days Delayed") +
  ggtitle("PENTA1 Vaccination Delay by Year of Birth") +
  guides(fill = FALSE) +  # Remove the legend for fill
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 10))

######
#7. make horizontal bar charts for the proportion with crude hep0 vaccination for 2018, 2019, 2020, 2021
#a. overall
hdss_2015 %>%
  filter(year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth) %>%
  summarise(vaccinated_prop = mean(hep0_crude)) %>%
  ggplot(aes(x = factor(year_of_birth), y = vaccinated_prop * 100)) +
  geom_col(fill = "lightgrey", width = 0.4, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion Vaccinated") +
  ggtitle("Proportion of Children Vaccinated for HepB0 by Year of Birth") +
  theme_bw()+
  guides(fill = FALSE)  # Remove the legend for fill

#########
#8. make horizontal bar charts for the proportion with crude penta1 vaccination for 2018, 2019, 2020, 2021
#a. overall
hdss_2015 %>%
  filter(year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth) %>%
  summarise(vaccinated_prop = mean(penta1_crude)) %>%
  ggplot(aes(x = factor(year_of_birth), y = vaccinated_prop * 100)) +
  geom_col(fill = "lightgrey", width = 0.4, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion Vaccinated") +
  ggtitle("Proportion of Children Vaccinated for PENTA1 by Year of Birth") +
  theme_bw()+
  guides(fill = FALSE)  # Remove the legend for fill

######

#7b. by site (Farafenni and Basse)
hdss_2015 %>%
  filter(year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth, site) %>%
  summarise(vaccinated_prop = mean(hep0_crude)) %>%
  ggplot(aes(x = factor(year_of_birth), y = vaccinated_prop * 100, fill = factor(year_of_birth))) +
  geom_col(fill = "lightgrey",width = 0.5, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion Vaccinated") +
  ggtitle("Proportion of Children Vaccinated for HepB0 by Year of Birth") +
  theme_bw() +
  facet_wrap(~ site)+
  guides(fill = FALSE)  # Remove the legend for fill

######

#8b. by site (Farafenni and Basse)
hdss_2015 %>%
  filter(year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth, site) %>%
  summarise(vaccinated_prop = mean(penta1_crude)) %>%
  ggplot(aes(x = factor(year_of_birth), y = vaccinated_prop * 100, fill = factor(year_of_birth))) +
  geom_col(fill = "lightgrey", width = 0.5, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion Vaccinated") +
  ggtitle("Proportion of Children Vaccinated for PENTA1 by Year of Birth") +
  theme_bw() +
  facet_wrap(~ site)+
  guides(fill = FALSE)  # Remove the legend for fill

#######

#9. make horizontal bar chart for the proportion of children with delayed hep0 vaccination for 2018, 2019, 2020, 2021
hdss_2015 %>%
  filter(!is.na(hep0_delayed), year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth, site) %>%
  summarise(delayed_prop = mean(hep0_delayed)) %>%
  ggplot(aes(x = factor(year_of_birth), y = delayed_prop * 100)) +
  geom_col(fill = "lightgrey", width = 0.5, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion Delayed") +
  ggtitle("Proportion of Children Delayed for HepB0 by Year of Birth") +
  theme_bw() +
  facet_wrap(~ site)+
  guides(fill = FALSE)  # Remove the legend for fill

####

#10. make horizontal bar chart for the proportion of children with delayed PENTA1 vaccination for 2018, 2019, 2020, 2021
hdss_2015 %>%
  filter(!is.na(penta1_delayed), year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth, site) %>%
  summarise(delayed_prop = mean(penta1_delayed)) %>%
  ggplot(aes(x = factor(year_of_birth), y = delayed_prop * 100)) +
  geom_col(fill = "lightgrey", width = 0.5, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion Delayed") +
  ggtitle("Proportion of Children Delayed for PENTA1 by Year of Birth") +
  theme_bw() +
  facet_wrap(~ site)+
  guides(fill = FALSE)  # Remove the legend for fill

#####
#11. make horizontal bar chart for the proportion of children with EARLY PENTA1 vaccination for 2018, 2019, 2020, 2021
hdss_2015 %>%
  filter(!is.na(penta1_early), year_of_birth %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) %>%
  group_by(year_of_birth, site) %>%
  summarise(early_prop = mean(penta1_early)) %>%
  ggplot(aes(x = factor(year_of_birth), y = early_prop * 100)) +
  geom_col(fill = "lightgrey", width = 0.5, color = "black") +
  scale_fill_discrete(name = "Year of Birth") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 100, by = 10), "%")) +
  xlab("Year of Birth") +
  ylab("Proportion too early") +
  ggtitle("Proportion of Children too early for PENTA1 by Year of Birth") +
  theme_bw() +
  facet_wrap(~ site)+
  guides(fill = FALSE)  # Remove the legend for fill


#########################################################################################################################################
#Make TIME SERIES ANALYSIS to to compare crude coverage, proportion delayed, and proportion early every quarter

# Create a new variable "mmyyyy" with the month and year of birth by extracting "mm" and "yyyy" from "dob"
hdss_2015$mmyyyy <- paste0(month(hdss_2015$dob, label = TRUE), " ", year(hdss_2015$dob))


# Create new variable "quarter" which is a sequence of quarters from Mar-2018 to Feb-2022
hdss_2015 <- hdss_2015 %>% 
  mutate(quarter = case_when(
    mmyyyy %in% c("Mar 2015", "Apr 2015", "May 2015") ~ "Q1",
    mmyyyy %in% c("Jun 2015", "Jul 2015", "Aug 2015") ~ "Q2",
    mmyyyy %in% c("Sep 2015", "Oct 2015", "Nov 2015") ~ "Q3",
    mmyyyy %in% c("Dec 2015", "Jan 2016", "Feb 2016") ~ "Q4",
    mmyyyy %in% c("Mar 2016", "Apr 2016", "May 2016") ~ "Q5",
    mmyyyy %in% c("Jun 2016", "Jul 2016", "Aug 2016") ~ "Q6",
    mmyyyy %in% c("Sep 2016", "Oct 2016", "Nov 2016") ~ "Q7",
    mmyyyy %in% c("Dec 2016", "Jan 2017", "Feb 2017") ~ "Q8",
    mmyyyy %in% c("Mar 2017", "Apr 2017", "May 2017") ~ "Q9",
    mmyyyy %in% c("Jun 2017", "Jul 2017", "Aug 2017") ~ "Q10",
    mmyyyy %in% c("Sep 2017", "Oct 2017", "Nov 2017") ~ "Q11",
    mmyyyy %in% c("Dec 2017", "Jan 2018", "Feb 2018") ~ "Q12",
    mmyyyy %in% c("Mar 2018", "Apr 2018", "May 2018") ~ "Q13",
    mmyyyy %in% c("Jun 2018", "Jul 2018", "Aug 2018") ~ "Q14",
    mmyyyy %in% c("Sep 2018", "Oct 2018", "Nov 2018") ~ "Q15",
    mmyyyy %in% c("Dec 2018", "Jan 2019", "Feb 2019") ~ "Q16",
    mmyyyy %in% c("Mar 2019", "Apr 2019", "May 2019") ~ "Q17",
    mmyyyy %in% c("Jun 2019", "Jul 2019", "Aug 2019") ~ "Q18",
    mmyyyy %in% c("Sep 2019", "Oct 2019", "Nov 2019") ~ "Q19",
    mmyyyy %in% c("Dec 2019", "Jan 2020", "Feb 2020") ~ "Q20",
    mmyyyy %in% c("Mar 2020", "Apr 2020", "May 2020") ~ "Q21",
    mmyyyy %in% c("Jun 2020", "Jul 2020", "Aug 2020") ~ "Q22",
    mmyyyy %in% c("Sep 2020", "Oct 2020", "Nov 2020") ~ "Q23",
    mmyyyy %in% c("Dec 2020", "Jan 2021", "Feb 2021") ~ "Q24",
    mmyyyy %in% c("Mar 2021", "Apr 2021", "May 2021") ~ "Q25",
    mmyyyy %in% c("Jun 2021", "Jul 2021", "Aug 2021") ~ "Q26",
    mmyyyy %in% c("Sep 2021", "Oct 2021", "Nov 2021") ~ "Q27",
    mmyyyy %in% c("Dec 2021") ~ "Q28",
    TRUE ~ NA_character_
  ))


#subset data to exclude children born in Feb 2018
#data <- data[data$quarter != "Feb 2018", ]


# subset data to include dob and mmyyyy
hdss_2015_subset <- hdss_2015[, c("dob", "date_hep0", "hep0_age_vac", "date_penta1", "penta1_age_vac", "epoch", "mmyyyy")]
#########################################################################################################################################

### time series of hep0 coverage quarterly from march 2015 to Feb 2022

##subsetdata
data.1 <- subset(hdss_2015, site=="Basse") ##(Basse)##
data.2 <- subset(hdss_2015, site=="Farafenni") ##(Farafenni)##

### Calculate the proportion of children vaccinated for hepB0 in each quarter
hep0_prop <- aggregate(hep0_crude == 1 ~ quarter, hdss_2015, mean) ##overall
hep0_prop$quarter <- factor(hep0_prop$quarter, levels = paste0("Q", 1:28)) ##overall
hep0_prop_month <- aggregate(hep0_crude == 1 ~ mmyyyy, hdss_2015, mean) ##overall_month
write.csv(hep0_prop_month, "hep0_prop_month.csv") #write dataframe to file and order before reading into R


hep0_prop_basse <- aggregate(hep0_crude == 1 ~ quarter, data.1, mean) ##Basse
hep0_prop_basse$quarter <- factor(hep0_prop_basse$quarter, levels = paste0("Q", 1:28)) ##Basse

hep0_prop_farafenni <- aggregate(hep0_crude == 1 ~ quarter, data.2, mean) ##Farafenni
hep0_prop_farafenni$quarter <- factor(hep0_prop_farafenni$quarter, levels = paste0("Q", 1:28)) ##Farafenni

#1a Create a time series plot for hepB0

#overall by quarter
ggplot(hep0_prop, aes(x = quarter, y = `hep0_crude == 1` * 100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion Vaccinated for Hep0") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 69.4, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 90, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 90, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


#overall by month
### read HDSS data into R###
hep0_prop_month_edit <- read_csv("hep0_prop_month_edit.csv")

# Convert mmyyyy to factor with the desired order
hep0_prop_month_edit$mmyyyy <- factor(hep0_prop_month_edit$mmyyyy, levels = unique(hep0_prop_month_edit$mmyyyy))


ggplot(hep0_prop_month_edit, aes(x = mmyyyy, y = `hep0_crude == 1` * 100)) +
  geom_line(group = 1) +
  labs(x = "Month and Year", y = "Proportion Vaccinated for Hep0") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 58, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 67.37, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 61, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 70.35, linetype = "dashed", color = "red") +
  annotate("text", x = 55, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 67, y = 40, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
##save 948 by 773 (01_hepB0_vaccinated_month)


##Basse
ggplot(hep0_prop_basse, aes(x = quarter, y = `hep0_crude == 1` * 100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion Vaccinated for Hep0 in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 71, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 90, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 90, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##Farafenni
ggplot(hep0_prop_farafenni, aes(x = quarter, y = `hep0_crude == 1` * 100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion Vaccinated for Hep0 in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 66, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 90, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 90, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


##############
### Calculate the proportion of children vaccinated for penta1 in each quarter
penta1_prop <- aggregate(penta1_crude == 1 ~ quarter, hdss_2015, mean) ##overall
penta1_prop$quarter <- factor(penta1_prop$quarter, levels = paste0("Q", 1:28)) ##overall
penta1_prop_month <- aggregate(penta1_crude == 1 ~ mmyyyy, hdss_2015, mean) ##overall_month
write.csv(penta1_prop_month, "penta1_prop_month.csv") #write dataframe to file and order before reading in

penta1_prop_basse <- aggregate(penta1_crude == 1 ~ quarter, data.1, mean) ##Basse
penta1_prop_basse$quarter <- factor(penta1_prop_basse$quarter, levels = paste0("Q", 1:28)) ##Basse
penta1_prop_farafenni <- aggregate(penta1_crude == 1 ~ quarter, data.2, mean) ##Farafenni
penta1_prop_farafenni$quarter <- factor(penta1_prop_farafenni$quarter, levels = paste0("Q", 1:28)) ##Farafenni

#1b Create a time series plot for penta1

#overall_quarter
ggplot(penta1_prop, aes(x = quarter, y = `penta1_crude == 1` * 100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion Vaccinated for penta1") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 46, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 90, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 90, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#overall by month
### read HDSS data into R###
penta1_prop_month_edit <- read_csv("penta1_prop_month_edit.csv")

# Convert mmyyyy to factor with the desired order
penta1_prop_month_edit$mmyyyy <- factor(penta1_prop_month_edit$mmyyyy, levels = unique(penta1_prop_month_edit$mmyyyy))

ggplot(penta1_prop_month_edit, aes(x = mmyyyy, y = `penta1_crude == 1` * 100)) +
  geom_line(group = 1) +
  labs(x = "Month and Year", y = "Proportion Vaccinated for PENTA1") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 58, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 42.93, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 61, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 48.96, linetype = "dashed", color = "red") +
  annotate("text", x = 55, y = 60, label = "pre-pandemic epoch") +
  annotate("text", x = 67, y = 60, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
##save 948 by 773 (01_penta1_vaccinated_month)

##Basse
ggplot(penta1_prop_basse, aes(x = quarter, y = `penta1_crude == 1` * 100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion Vaccinated for penta1 in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 42, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 90, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 90, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##Farafenni
ggplot(penta1_prop_farafenni, aes(x = quarter, y = `penta1_crude == 1` * 100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion Vaccinated for penta1 in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 58, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 90, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 90, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#########################

### time series of proportion of children with delayed hepB0 quarterly from march 2015 to Feb 202

## Calculate the proportion of children with delayed vaccination in each quarter
hep0_prop_delayed <- aggregate(hep0_delayed == 1 ~ quarter, subset(hdss_2015, !is.na(hep0_delayed)), mean) ##overall
hep0_prop_delayed$quarter <- factor(hep0_prop_delayed$quarter, levels = paste0("Q", 1:28)) ##overall
hep0_prop_delayed_month <- aggregate(hep0_delayed == 1 ~ mmyyyy, subset(hdss_2015, !is.na(hep0_delayed)), mean) ##overall_month
write.csv(hep0_prop_delayed_month, "hep0_prop_delayed_month.csv") #write dataframe to file and order before reading in

hep0_prop_delayed_basse <- aggregate(hep0_delayed == 1 ~ quarter, subset(data.1, !is.na(hep0_delayed)), mean) ##Basse
hep0_prop_delayed_basse$quarter <- factor(hep0_prop_delayed_basse$quarter, levels = paste0("Q", 1:28)) ##Basse
hep0_prop_delayed_farafenni <- aggregate(hep0_delayed == 1 ~ quarter, subset(data.2, !is.na(hep0_delayed)), mean) ##Farafenni
hep0_prop_delayed_farafenni$quarter <- factor(hep0_prop_delayed_farafenni$quarter, levels = paste0("Q", 1:28)) ##Farafeni

##2a Create a time series plot
##overall Hepb0_quarter
ggplot(hep0_prop_delayed, aes(x = quarter, y = `hep0_delayed == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with delayed HepB0") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 80, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 80, label = "pandemic epoch")+
  geom_hline(yintercept = 89.8, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#overall by month
### read HDSS data into R###
hep0_prop_delayed_month_edit <- read_csv("hep0_prop_delayed_month_edit.csv")

# Convert mmyyyy to factor with the desired order
hep0_prop_delayed_month_edit$mmyyyy <- factor(hep0_prop_delayed_month_edit$mmyyyy, levels = unique(hep0_prop_delayed_month_edit$mmyyyy))

ggplot(hep0_prop_delayed_month_edit, aes(x = mmyyyy, y = `hep0_delayed == 1` * 100)) +
  geom_line(group = 1) +
  labs(x = "Month and Year", y = "Proportion delayed for Hep0") +
  theme_bw() +
  scale_y_continuous(limits = c(40, 100), breaks = seq(40, 100, by = 5)) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 93.86, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 63, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 86.73, linetype = "dashed", color = "red") +
  annotate("text", x = 52, y = 50, label = "pre-pandemic epoch") +
  annotate("text", x = 69, y = 50, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
##save 948 by 773 (01_hepB0_delayed_month)

##Basse HepB0
ggplot(hep0_prop_delayed_basse, aes(x = quarter, y = `hep0_delayed == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with delayed HepB0 in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 80, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 80, label = "pandemic epoch")+
  geom_hline(yintercept = 89.0, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##Farafenni HepB0
ggplot(hep0_prop_delayed_farafenni, aes(x = quarter, y = `hep0_delayed == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with delayed HepB0 in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 80, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 80, label = "pandemic epoch")+
  geom_hline(yintercept = 92.3, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##############################

## Calculate the proportion of children with delayed vaccination in each quarter for penta1
penta1_prop_delayed <- aggregate(penta1_delayed == 1 ~ quarter, subset(hdss_2015, !is.na(penta1_delayed)), mean) ##overall
penta1_prop_delayed$quarter <- factor(penta1_prop_delayed$quarter, levels = paste0("Q", 1:28)) ##overall
penta1_prop_delayed_month <- aggregate(penta1_delayed == 1 ~ mmyyyy, subset(hdss_2015, !is.na(penta1_delayed)), mean) ##overall_month
write.csv(penta1_prop_delayed_month, "penta1_prop_delayed_month.csv") #write dataframe to file and order before reading in

penta1_prop_delayed_basse <- aggregate(penta1_delayed == 1 ~ quarter, subset(data.1, !is.na(penta1_delayed)), mean) ##Basse
penta1_prop_delayed_basse$quarter <- factor(penta1_prop_delayed_basse$quarter, levels = paste0("Q", 1:28)) ##Basse
penta1_prop_delayed_farafenni <- aggregate(penta1_delayed == 1 ~ quarter, subset(data.2, !is.na(penta1_delayed)), mean) ##Farafenni
penta1_prop_delayed_farafenni$quarter <- factor(penta1_prop_delayed_farafenni$quarter, levels = paste0("Q", 1:28)) ##Farafeni

##2a Create a time series plot
##overall penta1_quarter
ggplot(penta1_prop_delayed, aes(x = quarter, y = `penta1_delayed == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with delayed penta1") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 30, label = "pandemic epoch")+
  geom_hline(yintercept = 18.4, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#overall by month
### read HDSS data into R###
penta1_prop_delayed_month_edit <- read_csv("penta1_prop_delayed_month_edit.csv")

# Convert mmyyyy to factor with the desired order
penta1_prop_delayed_month_edit$mmyyyy <- factor(penta1_prop_delayed_month_edit$mmyyyy, levels = unique(penta1_prop_delayed_month_edit$mmyyyy))

ggplot(penta1_prop_delayed_month_edit, aes(x = mmyyyy, y = `penta1_delayed == 1` * 100)) +
  geom_line(group = 1) +
  labs(x = "Month and Year", y = "Proportion with delayed PENTA1") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2)) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 12.65, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 63, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 27.49, linetype = "dashed", color = "red") +
  annotate("text", x = 52, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 69, y = 30, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
##save 948 by 773 (01_penta1_delayed_month)

##Basse penta1
ggplot(penta1_prop_delayed_basse, aes(x = quarter, y = `penta1_delayed == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with delayed penta1 in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 30, label = "pandemic epoch")+
  geom_hline(yintercept = 18.8, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##Farafenni penta1
ggplot(penta1_prop_delayed_farafenni, aes(x = quarter, y = `penta1_delayed == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with delayed penta1 in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 30, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 30, label = "pandemic epoch")+
  geom_hline(yintercept = 17.6, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##################

## Calculate the proportion of children with EARLY vaccination in each quarter for penta1
penta1_prop_early <- aggregate(penta1_early == 1 ~ quarter, subset(hdss_2015, !is.na(penta1_early)), mean) ##overall
penta1_prop_early$quarter <- factor(penta1_prop_early$quarter, levels = paste0("Q", 1:28)) ##overall
penta1_prop_early_month <- aggregate(penta1_early == 1 ~ mmyyyy, subset(hdss_2015, !is.na(penta1_early)), mean) ##overall_month
write.csv(penta1_prop_early_month, "penta1_prop_early_month.csv") #write dataframe to file and order before reading in

penta1_prop_early_basse <- aggregate(penta1_early == 1 ~ quarter, subset(data.1, !is.na(penta1_early)), mean) ##Basse
penta1_prop_early_basse$quarter <- factor(penta1_prop_early_basse$quarter, levels = paste0("Q", 1:28)) ##Basse
penta1_prop_early_farafenni <- aggregate(penta1_early == 1 ~ quarter, subset(data.2, !is.na(penta1_early)), mean) ##Farafenni
penta1_prop_early_farafenni$quarter <- factor(penta1_prop_early_farafenni$quarter, levels = paste0("Q", 1:28)) ##Farafeni

##2a Create a time series plot
##overall penta1_quarter
ggplot(penta1_prop_early, aes(x = quarter, y = `penta1_early == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with early penta1") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 40, label = "pandemic epoch")+
  geom_hline(yintercept = 24.7, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#overall by month
### read HDSS data into R###
penta1_prop_early_month_edit <- read_csv("penta1_prop_early_month_edit.csv")

# Convert mmyyyy to factor with the desired order
penta1_prop_early_month_edit$mmyyyy <- factor(penta1_prop_early_month_edit$mmyyyy, levels = unique(penta1_prop_early_month_edit$mmyyyy))

ggplot(penta1_prop_early_month_edit, aes(x = mmyyyy, y = `penta1_early == 1` * 100)) +
  geom_line(group = 1) +
  labs(x = "Month and Year", y = "Proportion with too early PENTA1") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 2)) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 17.23, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 63, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 10.27, linetype = "dashed", color = "red") +
  annotate("text", x = 52, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 69, y = 40, label = "pandemic epoch") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
##save 948 by 773 (01_penta1_early_month)


##Basse penta1
ggplot(penta1_prop_early_basse, aes(x = quarter, y = `penta1_early == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with early penta1 in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 40, label = "pandemic epoch")+
  geom_hline(yintercept = 32.4, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

##Farafenni penta1
ggplot(penta1_prop_early_farafenni, aes(x = quarter, y = `penta1_early == 1`*100)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Proportion with early penta1 in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 40, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 40, label = "pandemic epoch")+
  geom_hline(yintercept = 7.9, linetype = "dashed", color = "blue") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))




####################
###3. Combined time series of proportion of children vaccinated and those with delayed hepB0 quarterly from march 2018 to Feb 202
ggplot() +
  geom_line(data = hep0_prop_delayed, aes(x = quarter, y = `hep0_delayed == 1`*100, color = "Proportion with delayed HepB0", group = 1), linewidth = 1) +
  geom_line(data = hep0_prop, aes(x = quarter, y = `hep0_crude == 1`*100, color = "Proportion Vaccinated for Hep0", group = 1), linewidth = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter from March 2018 to Feb 2022", y = "Percentage of children") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red", linewidth = 0.7) +
  annotate("text", x = 5, y = 20, label = "pre-pandemic epoch") +
  annotate("text", x = 11, y = 20, label = "pandemic epoch") +
  geom_hline(yintercept = 89.8, linetype = "dashed", color = "#e1a84a") +
  geom_hline(yintercept = 69.4, linetype = "dashed", color = "#4aaead") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("Proportion with delayed HepB0" = "#e1a84a", "Proportion Vaccinated for Hep0" = "#4aaead"))+
  ggtitle("Quarterly crude coverage of HepB0 and proportion of delayed hepB0") 

###############################



# Calculate the median number of days children were delayed for hepB0 in each quarter
hep0_days_delayed <- aggregate(hep0_days_delayed ~ quarter, subset(hdss_2015, !is.na(hep0_days_delayed) & hep0_days_delayed > 0), median)#overall
hep0_days_delayed$quarter <- factor(hep0_days_delayed$quarter, levels = paste0("Q", 1:28)) #overall
hep0_days_delayed_basse <- aggregate(hep0_days_delayed ~ quarter, subset(data.1, !is.na(hep0_days_delayed) & hep0_days_delayed > 0), median) #Basse
hep0_days_delayed_basse$quarter <- factor(hep0_days_delayed_basse$quarter, levels = paste0("Q", 1:28)) #Basse
hep0_days_delayed_farafenni <- aggregate(hep0_days_delayed ~ quarter, subset(data.2, !is.na(hep0_days_delayed) & hep0_days_delayed > 0), median) #Farafenni
hep0_days_delayed_farafenni$quarter <- factor(hep0_days_delayed_farafenni$quarter, levels = paste0("Q", 1:28)) #Farafenni

#4a Create a time series plot hepB0
#overall
ggplot(hep0_days_delayed, aes(x = quarter, y = `hep0_days_delayed`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days delayed (HepB0)") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 10, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#Basse
ggplot(hep0_days_delayed_basse, aes(x = quarter, y = `hep0_days_delayed`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days delayed (HepB0) in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 19, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 10, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#Farafenni
ggplot(hep0_days_delayed_farafenni, aes(x = quarter, y = `hep0_days_delayed`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days delayed (HepB0) in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 23, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 10, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

############################
#DELAYED
# Calculate the median number of days children were delayed for penta1 in each quarter
penta1_days_delayed <- aggregate(penta1_days_delayed ~ quarter, subset(hdss_2015, !is.na(penta1_days_delayed) & penta1_days_delayed > 0), median)#overall
penta1_days_delayed$quarter <- factor(penta1_days_delayed$quarter, levels = paste0("Q", 1:28)) #overall
penta1_days_delayed_basse <- aggregate(penta1_days_delayed ~ quarter, subset(data.1, !is.na(penta1_days_delayed) & penta1_days_delayed > 0), median) #Basse
penta1_days_delayed_basse$quarter <- factor(penta1_days_delayed_basse$quarter, levels = paste0("Q", 1:28)) #Basse
penta1_days_delayed_farafenni <- aggregate(penta1_days_delayed ~ quarter, subset(data.2, !is.na(penta1_days_delayed) & penta1_days_delayed > 0), median) #Farafenni
penta1_days_delayed_farafenni$quarter <- factor(penta1_days_delayed_farafenni$quarter, levels = paste0("Q", 1:28)) #Farafenni

#4a Create a time series plot penta1
#overall
ggplot(penta1_days_delayed, aes(x = quarter, y = `penta1_days_delayed`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days delayed (penta1)") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 10, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#Basse
ggplot(penta1_days_delayed_basse, aes(x = quarter, y = `penta1_days_delayed`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days delayed (penta1) in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 21, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 10, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 10, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#Farafenni
ggplot(penta1_days_delayed_farafenni, aes(x = quarter, y = `penta1_days_delayed`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days delayed (penta1) in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 17, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 5, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 5, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

###############################
#EARLY
# Calculate the median number of days children were too early for penta1 in each quarter
penta1_days_early <- aggregate(penta1_days_early ~ quarter, subset(hdss_2015, !is.na(penta1_days_early) & penta1_days_early > 0), median)#overall
penta1_days_early$quarter <- factor(penta1_days_early$quarter, levels = paste0("Q", 1:28)) #overall
penta1_days_early_basse <- aggregate(penta1_days_early ~ quarter, subset(data.1, !is.na(penta1_days_early) & penta1_days_early > 0), median) #Basse
penta1_days_early_basse$quarter <- factor(penta1_days_early_basse$quarter, levels = paste0("Q", 1:28)) #Basse
penta1_days_early_farafenni <- aggregate(penta1_days_early ~ quarter, subset(data.2, !is.na(penta1_days_early) & penta1_days_early > 0), median) #Farafenni
penta1_days_early_farafenni$quarter <- factor(penta1_days_early_farafenni$quarter, levels = paste0("Q", 1:28)) #Farafenni

#4a Create a time series plot penta1
#overall
ggplot(penta1_days_early, aes(x = quarter, y = `penta1_days_early`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days too early (penta1)") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 11, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 15, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 15, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#Basse
ggplot(penta1_days_early_basse, aes(x = quarter, y = `penta1_days_early`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days too early (penta1) in Basse") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 11, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 15, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 15, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#Farafenni
ggplot(penta1_days_early_farafenni, aes(x = quarter, y = `penta1_days_early`)) +
  geom_line(group = 1) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                              "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")) +
  labs(x = "Quarter", y = "Median days too early (penta1) in Farafenni") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 4, linetype = "dashed", color = "blue") +
  annotate("text", x = 15, y = 15, label = "pre-pandemic epoch") +
  annotate("text", x = 23, y = 15, label = "pandemic epoch")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
##save 750 x 550
#############################################


#write hdss_2015 data data file
write.csv(hdss_2015, "hdss_2015.csv")


#########################################################################################################################################
#plot the cluster locations (villages) where the hdss data were collected from

### read HDSS_2015 data into R###
hdss_2015 <- read_csv("hdss_2015.csv")

# subset data to include dob and mmyyyy
hdss_2015_village <- hdss_2015[, c("village_name", "village_code", "latitude", "longitude", "site")]

# Aggregate the dataset by village_code
hdss_2015_aggregated <- hdss_2015_village %>%
  group_by(village_code) %>%
  summarise(village_name = first(village_name),
            latitude = first(latitude),
            longitude = first(longitude),
            site = first(site))


####Read in all shape files##########################
adm1 <- st_read(paste0("gmb_admbnda_adm1_2022.shp"))
adm2 <- st_read(paste0("gmb_admbnda_adm2_2022.shp"))
adm3 <- st_read(paste0("gmb_admbnda_adm3_2022.shp"))

############################
# Convert both shapefiles to sf objects with the same geometry column name
gmb_shp_1_sf <- st_as_sf(adm1, "newname")
gmb_shp_sf <- st_as_sf(adm3, "newname")

# Combine shapefiles
gmb_combined <- bind_rows(gmb_shp_1_sf, gmb_shp_sf)

# Create a variable to distinguish between admin 1 and admin 2 boundaries
gmb_combined$type <- ifelse(is.na(gmb_combined$ADM2_EN), "ADM1_EN", "ADM3_EN")


# Plot clinic points villages in the hdss area
ggplot() +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM3_EN", ], color = "white", size = 0.8) +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM1_EN", ], color = "black", size = 1, alpha = 0.1, show.legend = FALSE) +
  geom_point(data = hdss_2015_aggregated, aes(x = longitude, y = latitude, color = site)) +
  scale_color_discrete(name = "site") +
  scale_size(range = c(2, 4), guide = "none") +
  theme_bw() +
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Villages in Farafenni and Basse HDSS")

#####save 1300 x 450 (villages_Hdss)
#############################################################################################################################################



