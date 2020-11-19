#######################################################################################################################
### For comparison, calculate the change in total reported deaths by day
# Author: Sarah Kramer
# Date: 19/11/2020
# Note: These should produce estimates equal to official reports
#######################################################################################################################

# Load necessary libraries:
# library(stringr) # b/c read in in read_data_to_list

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

### Calculate CUMULATIVE reported deaths each day ###
# Sum where NeuerTodesfall == 1 or 0:
total_deaths <- lapply(list_covid_deaths, function(ix) {
  aggregate(AnzahlTodesfall ~ Bundesland + IdBundesland + Datenstand, data = ix[ix$NeuerTodesfall %in% c(0, 1), ],
            FUN = sum)
})

# Combine into dataframe:
total_deaths <- do.call('rbind', total_deaths)

# Change Bundesland level names:
total_deaths$Bundesland <- as.character(total_deaths$Bundesland)
total_deaths$Bundesland[total_deaths$IdBundesland == 8] <- 'Baden-Wuerttemberg'
total_deaths$Bundesland[total_deaths$IdBundesland == 16] <- 'Thueringen'
total_deaths$Bundesland <- factor(total_deaths$Bundesland)

# And add English names:
total_deaths$Bundesland_ENG <- total_deaths$Bundesland
levels(total_deaths$Bundesland_ENG) <- c('Baden-Wuerttemberg', 'Bavaria', 'Berlin', 'Brandenburg', 'Bremen', 'Hamburg',
                                         'Hesse', 'Mecklenburg-Vorpommern', 'Lower Saxony', 'North Rhine-Westphalia',
                                         'Rhineland-Palatinate', 'Saarland', 'Saxony', 'Saxony-Anhalt',
                                         'Schleswig-Holstein', 'Thuringia')

# Reorder columns:
total_deaths <- total_deaths[, c(1, 5, 2:4)]

#######################################################################################################################

### Add in estimates from March 9-26; April 5 ###



# Note: ensure that counts on these days match official reports

#######################################################################################################################

### QUICK - Check that differences between cumulative = directly calculated change from c(-1, 1) vals ###

# Use only March 28 - April 4:
delta_deaths <- delta_deaths[delta_deaths$Datenstand <= '2020-04-04' & delta_deaths$Datenstand > '2020-03-27', ]
total_deaths <- total_deaths[total_deaths$Datenstand <= '2020-04-04', ]

library(reshape2)
total_deaths <- dcast(Bundesland + Bundesland_ENG + IdBundesland ~ Datenstand, data = total_deaths, value.var = 'AnzahlTodesfall')
total_deaths[, 4:12][is.na(total_deaths[, 4:12])] <- 0
total_deaths[, 5:12] <- total_deaths[, 5:12] - total_deaths[, 4:11]
total_deaths <- total_deaths[, c(1:3, 5:12)]
total_deaths <- melt(total_deaths, id.vars = c('Bundesland', 'Bundesland_ENG', 'IdBundesland'))
names(total_deaths)[4:5] <- c('Datenstand', 'num_new_COMP')
total_deaths <- total_deaths[total_deaths$num_new_COMP > 0, ]

delta_deaths <- merge(delta_deaths, total_deaths, by = c('Bundesland', 'Bundesland_ENG', 'IdBundesland', 'Datenstand'),
                      all = TRUE)
all.equal(delta_deaths$AnzahlTodesfall, delta_deaths$num_new_COMP) # TRUE
# Note: Yes, this produces same result as other method; no need to do both
# Note: April 16 seems to be missing Bundeslaender 10-16?

#######################################################################################################################

#######################################################################################################################







