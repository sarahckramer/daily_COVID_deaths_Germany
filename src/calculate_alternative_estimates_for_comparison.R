#######################################################################################################################
### For comparison, calculate the change in total reported deaths by day
# Author: Sarah Kramer
# Date: 19/11/2020
# Note: These should produce estimates equal to official reports
#######################################################################################################################

# Load necessary libraries:
library(reshape2)

#######################################################################################################################

### Read in and format data ###
# Read in data:
source('src/read_data_to_list.R')

# Format important columns:
source('src/standardize_relevant_column_names.R', encoding = 'UTF-8')

#######################################################################################################################

### Calculate CHANGE from previous day ###
# Sum where NeuerTodesfall == 1 or -1:
delta_deaths <- lapply(list_covid_deaths, function(ix) {
  aggregate(AnzahlTodesfall ~ Bundesland + IdBundesland + Datenstand, data = ix[ix$NeuerTodesfall %in% c(-1, 1), ],
            FUN = sum)
})

# Combine into dataframe:
delta_deaths <- do.call('rbind', delta_deaths)

# Change Bundesland level names:
delta_deaths$Bundesland <- as.character(delta_deaths$Bundesland)
delta_deaths$Bundesland[delta_deaths$IdBundesland == 8] <- 'Baden-Wuerttemberg'
delta_deaths$Bundesland[delta_deaths$IdBundesland == 16] <- 'Thueringen'
delta_deaths$Bundesland <- factor(delta_deaths$Bundesland)

# And add English names:
delta_deaths$Bundesland_ENG <- delta_deaths$Bundesland
levels(delta_deaths$Bundesland_ENG) <- c('Baden-Wuerttemberg', 'Bavaria', 'Berlin', 'Brandenburg', 'Bremen', 'Hamburg',
                                       'Hesse', 'Mecklenburg-Vorpommern', 'Lower Saxony', 'North Rhine-Westphalia',
                                       'Rhineland-Palatinate', 'Saarland', 'Saxony', 'Saxony-Anhalt',
                                       'Schleswig-Holstein', 'Thuringia')

# Reorder columns:
delta_deaths <- delta_deaths[, c(1, 5, 2:4)]

# Remove 0s and negatives:
delta_deaths <- delta_deaths[delta_deaths$AnzahlTodesfall > 0, ]

#######################################################################################################################

### Add in estimates from March 9-26; April 5 ###
# Read in data:
delta_deaths_march <- read.csv('data_formatted/new_deaths_missing_March.csv')
delta_deaths_april <- read.csv('data_formatted/new_deaths_missing_April5.csv')

# Convert Datenstand column to date:
delta_deaths_march$Datenstand <- as.Date(delta_deaths_march$Datenstand, format = '%Y-%m-%d')
delta_deaths_april$Datenstand <- as.Date(delta_deaths_april$Datenstand, format = '%Y-%m-%d')

# Combine all into one data frame:
delta_deaths <- rbind(delta_deaths_march, delta_deaths, delta_deaths_april)
rm(delta_deaths_march, delta_deaths_april)

# Sort by date of reporting:
delta_deaths <- delta_deaths[order(delta_deaths$Datenstand), ]

#######################################################################################################################

### Replace NAs with 0s ###
# Start by converting to wide format:
delta_deaths_WIDE <- dcast(Bundesland + Bundesland_ENG + IdBundesland ~ Datenstand, value.var = 'AnzahlTodesfall',
                           data = delta_deaths)

# Now replace any NAs with 0s:
delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]][is.na(delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]])] <- 0

#######################################################################################################################

### Calculate total deaths at the country level ###
delta_deaths_WIDE <- rbind(delta_deaths_WIDE,
                           c('Total', 'Total', '99',
                             colSums(delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]])))

# And convert all back to numeric:
delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]] <- apply(delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]],
                                                          2, 'as.numeric')

#######################################################################################################################

### Ensure that counts on these days match official reports ###
# Start by calculating cumulative deaths each day:
delta_deaths_total <- delta_deaths_WIDE
for (i in dim(delta_deaths_total)[2]:5) {
  delta_deaths_total[, i] <- rowSums(delta_deaths_total[, 4:i])
}; rm(i)

# Compare to counts on: April 4-6; April 30; May 31; June 30







#######################################################################################################################

### Output data ###
# First melt data frame with 0s and Total counts included:
delta_deaths <- melt(delta_deaths_WIDE, id.vars = colnames(delta_deaths_WIDE)[1:3])

# Correct column names:
colnames(delta_deaths)[4:5] <- c('Datenstand', 'AnzahlTodesfall')

# Convert 'Datenstand' to date:
delta_deaths$Datenstand <- as.Date(delta_deaths$Datenstand, format = '%Y-%m-%d')

# Write to file:
write.csv(delta_deaths, file = 'data_formatted/new_deaths_LONG.csv', row.names = FALSE)
write.csv(delta_deaths_WIDE, file = 'data_formatted/new_deaths_WIDE.csv', row.names = FALSE)

# Clean up:
rm(list = ls())

#######################################################################################################################

### Preliminary plot of data over time ###
# Goal is just to see if there is a super clear weekend effect
library(ggplot2)
p1 <- ggplot(data = delta_deaths, aes(x = Datenstand, y = AnzahlTodesfall)) + geom_line() +
  theme_classic() + facet_wrap(~ Bundesland_ENG)
print(p1)

# NOTE: Might still need to update this if some Bundeslaender are missing from some dates!







