#######################################################################################################################
### Determine where data on some Bundeslaender are missing, and manually add
# Author: Sarah Kramer
# Date: 20/11/2020
#######################################################################################################################

# ### Read in and format data ###
# # Read in data:
# source('src/read_data_to_list.R')
# 
# # Format important columns:
# source('src/standardize_relevant_column_names.R', encoding = 'UTF-8')
# 
# #######################################################################################################################
# 
# ### Determine where some Bundeslaender are missing ###
# # Check how many are present for each date:
# number_bl_present <- unlist(lapply(list_covid_deaths, function(ix) {
#   length(unique(ix[, 'Bundesland']))
# }))
# 
# # Which dates don't have all 16?:
# which(number_bl_present != 16) # 20
# # Only April 16
# 
# # Determine which Bundeslaender missing:
# dat_missing <- list_covid_deaths[[20]]
# print(unique(dat_missing$Bundesland))
# # so missing: Berlin; Brandenburg; Mecklenburg-Vorpommern; Saarland; Sachsen; Sachsen-Anhalt; Thueringen
# 
# # Clean up:
# rm(list = ls())

#######################################################################################################################

### Manually compile these data ###
# Start with data frame of Bundeslaender names and IDs:
df_names <- as.data.frame(cbind(c('Schleswig-Holstein', 'Hamburg', 'Niedersachsen', 'Bremen', 'Nordrhein-Westfalen',
                                  'Hessen', 'Rheinland-Pfalz', 'Baden-Wuerttemberg', 'Bayern', 'Saarland',
                                  'Berlin', 'Brandenburg', 'Mecklenburg-Vorpommern', 'Sachsen', 'Sachsen-Anhalt',
                                  'Thueringen'),
                                c('Schleswig-Holstein', 'Hamburg', 'Lower Saxony', 'Bremen', 'North Rhine-Westphalia',
                                  'Hesse', 'Rhineland-Palatinate', 'Baden-Wuerttemberg', 'Bavaria', 'Saarland',
                                  'Berlin', 'Brandenburg', 'Mecklenburg-Vorpommern', 'Saxony', 'Saxony-Anhalt',
                                  'Thuringia'),
                                1:16),
                          stringsAsFactors = TRUE)
colnames(df_names) <- c('Bundesland', 'Bundesland_ENG', 'IdBundesland')

# Order names alphabetically to match RKI reports:
df_names <- df_names[order(df_names$Bundesland), ]

# Calculate the difference in cumulative deaths between the 16th and 15th for these Bundeslaender:
new_deaths_apr16 <- cbind(df_names,
                          as.Date('2020-04-16'),
                          c(802, 1049, 74, 54, 21, 80, 187, 13, 248, 663, 87, 71, 94, 26, 55, 45),
                          c(767, 954, 62, 42, 21, 67, 173, 11, 224, 607, 77, 58, 73, 26, 54, 38))
names(new_deaths_apr16)[4:6] <- c('Datenstand', 'new_t', 'new_tminus1')

# Subtract previous day's total from "current" day's total:
new_deaths_apr16$AnzahlTodesfall <- new_deaths_apr16$new_t - new_deaths_apr16$new_tminus1

# Remove unnecessary columns:
new_deaths_apr16 <- new_deaths_apr16[, c(1:4, 7)]

# # CHECK: Do data for Bundeslaender that are included match estimates calculated here?
# new_deaths <- read.csv('data_formatted/new_deaths_LONG.csv')
# new_deaths <- new_deaths[new_deaths$Datenstand == '2020-04-16', ]
# new_deaths$Datenstand <- as.Date(new_deaths$Datenstand, format = '%Y-%m-%d')
# new_deaths <- merge(new_deaths, new_deaths_apr16, by = c('Bundesland', 'Bundesland_ENG', 'IdBundesland', 'Datenstand'))
# # Bavaria is off by 37, all others the same
# # To correct error in Bavaria, let's just use these data instead of those previously used in "new_deaths"

# Remove where 0 deaths:
new_deaths_apr16 <- new_deaths_apr16[new_deaths_apr16$AnzahlTodesfall > 0, ]

# Write to file:
write.csv(new_deaths_apr16, file = 'data_formatted/new_deaths_missing_April16.csv', row.names = FALSE)

# Clean up:
rm(list = ls())
