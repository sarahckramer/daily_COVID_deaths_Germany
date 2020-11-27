#######################################################################################################################
### Get number of new deaths by day, by Bundesland, from archived RKI data
# Author: Sarah Kramer
# Date: 19/11/2020
# Note: These should produce estimates equal to official reports
# Note: Here we are interested in the first "wave" only (March - June)
#######################################################################################################################

# Load necessary libraries:
library(stringr)
library(reshape2)

#######################################################################################################################
### Check for any missing files ###
# First 2 deaths reported in Nordrhein-Westfalen on 09/03; data here start on 27/03
# We may need to find some way (combo of pdfs on RKI's site and news articles) to fill in the early deaths

# Get list of files for each month:
file_list_March <- list.files('data_RKI/Maerz/', pattern = '.csv') # 27-31/03
file_list_April <- list.files('data_RKI/April/', pattern = '.csv') # missing 1
file_list_May <- list.files('data_RKI/Mai/', pattern = '.csv') # complete
file_list_June <- list.files('data_RKI/Juni/', pattern = '.csv') # complete
# but could look at later too, if only because they could help rule out mistakenly-reported deaths

# Check for missing:
which(!unlist(lapply(str_pad(string = 1:30, width = 2, pad = 0), function(ix) {
  file.exists(paste0('data_RKI/April/RKI_COVID19_2020-04-', ix, '.csv'))
})))
which(!unlist(lapply(str_pad(string = 1:31, width = 2, pad = 0), function(ix) {
  file.exists(paste0('data_RKI/Mai/RKI_COVID19_2020-05-', ix, '.csv'))
})))
which(!unlist(lapply(str_pad(string = 1:30, width = 2, pad = 0), function(ix) {
  file.exists(paste0('data_RKI/Juni/RKI_COVID19_2020-06-', ix, '.csv'))
})))
# only April 5 is missing - can get data from pdf

# # Clean up:
rm(file_list_March, file_list_April, file_list_May, file_list_June)

#######################################################################################################################

### Read in and format data ###
# Read in data:
source('src/read_data_to_list.R')

# # Check that size of files grows over time:
# print(lapply(list_covid_deaths, function(ix) {dim(ix)}))
# # Why last file in April smaller than two before?
# # Seems to be b/c rows are consolidated; i.e., one row with 2 cases, instead of 2 separate rows

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

# # Remove 0s and negatives:
# delta_deaths <- delta_deaths[delta_deaths$AnzahlTodesfall > 0, ]

#######################################################################################################################

### Add in estimates from March 9-26; April 5; April 16 ###
# Read in data:
delta_deaths_march <- read.csv('data_formatted/new_deaths_missing_March.csv')
delta_deaths_april5 <- read.csv('data_formatted/new_deaths_missing_April5.csv')
delta_deaths_april16 <- read.csv('data_formatted/new_deaths_missing_April16.csv')

# Convert Datenstand column to date:
delta_deaths_march$Datenstand <- as.Date(delta_deaths_march$Datenstand, format = '%Y-%m-%d')
delta_deaths_april5$Datenstand <- as.Date(delta_deaths_april5$Datenstand, format = '%Y-%m-%d')
delta_deaths_april16$Datenstand <- as.Date(delta_deaths_april16$Datenstand, format = '%Y-%m-%d')

# Remove April 16 from delta_deaths before adding in corrected:
delta_deaths <- delta_deaths[delta_deaths$Datenstand != '2020-04-16', ]

# Combine all into one data frame:
delta_deaths <- rbind(delta_deaths, delta_deaths_march, delta_deaths_april5, delta_deaths_april16)
rm(delta_deaths_march, delta_deaths_april5, delta_deaths_april16)

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
delta_deaths_WIDE$Bundesland <- as.character(delta_deaths_WIDE$Bundesland)
delta_deaths_WIDE$Bundesland_ENG <- as.character(delta_deaths_WIDE$Bundesland_ENG)

delta_deaths_WIDE <- rbind(delta_deaths_WIDE,
                           c('Total', 'Total', '99',
                             colSums(delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]])))

delta_deaths_WIDE$Bundesland <- factor(delta_deaths_WIDE$Bundesland)
delta_deaths_WIDE$Bundesland_ENG <- factor(delta_deaths_WIDE$Bundesland_ENG)

# And convert all back to numeric:
delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]] <- apply(delta_deaths_WIDE[, 4:dim(delta_deaths_WIDE)[2]],
                                                          2, 'as.numeric')

#######################################################################################################################

# ### Ensure that counts on these days match official reports ###
# # Start by calculating cumulative deaths each day:
# delta_deaths_total <- delta_deaths_WIDE
# for (i in dim(delta_deaths_total)[2]:5) {
#   delta_deaths_total[, i] <- rowSums(delta_deaths_total[, 4:i])
# }; rm(i)
# 
# # Compare to counts on: April 4-6; April 30; May 31; June 30
# 
# delta_deaths_total$`2020-03-26` # same
# delta_deaths_total$`2020-04-04` # same
# delta_deaths_total$`2020-04-05` # same
# delta_deaths_total$`2020-04-06` # same
# delta_deaths_total$`2020-06-30` # same
# 
# rm(delta_deaths_total)

#######################################################################################################################

### Output data ###
# First melt data frame with 0s and total counts included:
delta_deaths <- melt(delta_deaths_WIDE, id.vars = colnames(delta_deaths_WIDE)[1:3])

# Correct column names:
colnames(delta_deaths)[4:5] <- c('Datenstand', 'AnzahlTodesfall')

# Convert 'Datenstand' to date:
delta_deaths$Datenstand <- as.Date(delta_deaths$Datenstand, format = '%Y-%m-%d')

#######################################################################################################################

# # Explore: Set negatives to zero, but subtract the same amount from the previous day's total:
# delta_deaths <- delta_deaths[delta_deaths$Bundesland != 'Total', ]
# delta_deaths$Bundesland <- factor(delta_deaths$Bundesland)
# 
# delta_deaths_ORIG <- delta_deaths
# 
# index_neg <- which(delta_deaths$AnzahlTodesfall < 0)
# while (length(index_neg) > 0) {
#   for (i in index_neg) {
#     delta_deaths$AnzahlTodesfall[delta_deaths$Bundesland == delta_deaths$Bundesland[i] &
#                                    delta_deaths$Datenstand == delta_deaths$Datenstand[i] - 1] <-
#       delta_deaths$AnzahlTodesfall[delta_deaths$Bundesland == delta_deaths$Bundesland[i] &
#                                      delta_deaths$Datenstand == delta_deaths$Datenstand[i] - 1] +
#       delta_deaths$AnzahlTodesfall[delta_deaths$Bundesland == delta_deaths$Bundesland[i] &
#                                      delta_deaths$Datenstand == delta_deaths$Datenstand[i]]
# 
#     delta_deaths$AnzahlTodesfall[delta_deaths$Bundesland == delta_deaths$Bundesland[i] &
#                                    delta_deaths$Datenstand == delta_deaths$Datenstand[i]] <- 0
#   }
# 
#   index_neg <- which(delta_deaths$AnzahlTodesfall < 0)
# }; rm(index_neg, i)
# 
# delta_deaths_WIDE <- dcast(Bundesland + Bundesland_ENG + IdBundesland ~ Datenstand, value.var = 'AnzahlTodesfall',
#                            data = delta_deaths)
# 
# delta_deaths_total <- delta_deaths_WIDE
# for (i in dim(delta_deaths_total)[2]:5) {
#   delta_deaths_total[, i] <- rowSums(delta_deaths_total[, 4:i])
# }; rm(i)
# 
# delta_deaths_total$`2020-03-26` # same
# delta_deaths_total$`2020-04-30` # same
# delta_deaths_total$`2020-05-31` # same; now Saxony UNDER by 2?
# delta_deaths_total$`2020-06-02`[13] # Saxony back to being equal
# delta_deaths_total$`2020-06-30` # same; Saxony back to normal
# # so the day-by-day cumulative values do not exactly match, but overall they come out the same by the end of the wave
# # there will be differences on the day(s) before the negative report, but those will be remedied on the day OF the negative report

#######################################################################################################################

# # Write to file:
# write.csv(delta_deaths, file = 'data_formatted/new_deaths_LONG.csv', row.names = FALSE)
# write.csv(delta_deaths_WIDE, file = 'data_formatted/new_deaths_WIDE.csv', row.names = FALSE)

# Clean up:
rm(list = ls())
