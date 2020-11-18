#######################################################################################################################
### Manually input data on # of deaths on days with no csv files in the repository
# Author: Sarah Kramer
# Date: 18/11/2020
# Note: CSV files unavailable for April 5, March 9-26
#######################################################################################################################

# Load libraries:
library(reshape2)

#######################################################################################################################

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

#######################################################################################################################

### Get data for March 9-26 ###
# Copy cumulative deaths on each date into data frame:
new_deaths_march <- cbind(df_names,
                          # as.Date(unlist(lapply(as.list(as.Date('2020-03-09'):as.Date('2020-03-26')),
                          #                       function(ix) {rep(ix, 16)})),
                          #         origin = '1970-01-01'),
                          c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0), # 9
                          c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0), # 10
                          c(0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0), # 11
                          c(1, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0), # 12
                          c(1, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0), # 13
                          c(2, 1, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0), # 14
                          c(2, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0), # 15 
                          # 4 in Bavaria might be a 3? (or vice versa)
                          # two false reports on the 16th - total of 13 instead of 11
                          # but question is: were 2 false reports already present on the 15th? or the 14th? or only one?
                          # and was that one present on the 14th, or starting the 15th?
                          # use reported numbers (4 in Bayern on the 15th) unless larger than later reports
                          c(2, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0), # 16 # same, and 13 deaths reported; assume 1 is error and use data from 17th
                          c(2, 4, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0), # 17
                          c(2, 4, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0), # 18
                          c(6, 8, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0), # 19
                          c(10, 12, 0, 0, 0, 0, 1, 0, 0, 6, 1, 0, 0, 0, 1, 0), # 20
                          c(16, 19, 1, 0, 0, 0, 2, 0, 0, 6, 1, 0, 0, 0, 1, 0), # 21 # 1 reported in Sachsen-Anhalt but corrected the next day, so set here to 0
                          c(21, 21, 1, 0, 0, 0, 2, 0, 1, 6, 2, 0, 0, 0, 1, 0), # 22
                          c(21, 26, 1, 0, 0, 0, 3, 0, 4, 28, 2, 0, 0, 0, 1, 0), # 23
                          c(30, 30, 1, 0, 0, 0, 4, 0, 6, 33, 5, 1, 1, 0, 2, 1), # 24
                          c(37, 37, 2, 1, 0, 0, 4, 0, 7, 43, 5, 2, 6, 1, 3, 1), # 25
                          c(56, 47, 4, 1, 1, 0, 6, 0, 8, 53, 6, 2, 7, 1, 3, 3)) # 26
# each column is CUMULATIVE deaths reported in each region in report published on that day

# Set column names as date (defaults to number of days since 1970-01-01):
colnames(new_deaths_march)[4:dim(new_deaths_march)[2]] <- as.Date('2020-03-09'):as.Date('2020-03-26')

# Calculate NEW deaths each day:
new_deaths_march[, 5:21] <- new_deaths_march[, 5:21] - new_deaths_march[, 4:20]

# Melt data frame:
new_deaths_march <- melt(new_deaths_march, id.vars = c('Bundesland', 'Bundesland_ENG', 'IdBundesland'))

# Change column names:
colnames(new_deaths_march)[4:5] <- c('Datenstand', 'AnzahlTodesfall')

# Ensure none negative:
print(summary(new_deaths_march$AnzahlTodesfall))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.6875  0.0000 22.0000 

# Convert dates to correct format:
new_deaths_march$Datenstand <- as.Date(as.numeric(as.character(new_deaths_march$Datenstand)), origin = '1970-01-01')

# Remove where no new deaths:
new_deaths_march <- new_deaths_march[new_deaths_march$AnzahlTodesfall > 0, ]

#######################################################################################################################

### "Confirm" counts against cumulative case count ###
# Add data from March:
a <- rbind(new_deaths_march, new_deaths)



# Check cumulative totals at each date (though April 4) against reported values:





#######################################################################################################################

### Get data by Bundesland for April 5 ###
# Copy cumulative deaths for April 5 and April 4 (last two columns):
new_deaths_apr5 <- cbind(df_names,
                         as.Date('2020-04-05'),
                         c(367, 396, 24, 17, 6, 19, 56, 5, 89, 245, 32, 14, 32, 12, 18, 10),
                         c(316, 349, 22, 12, 6, 16, 42, 5, 85, 200, 29, 14, 24, 11, 17, 10))
names(new_deaths_apr5)[4:6] <- c('Datenstand', 'new_t', 'new_tminus1')

# Subtract previous day's total from "current" day's total:
new_deaths_apr5$AnzahlTodesfall <- new_deaths_apr5$new_t - new_deaths_apr5$new_tminus1




# Check agreement with cumulative deaths up through April 4, as well as new deaths on April 6, in our dataset?
# TO DO: Need to add March in first
a <- rbind(new_deaths_march, new_deaths)
aggregate(AnzahlTodesfall ~ Bundesland, data = a[a$Datenstand <= '2020-04-04', ], FUN = sum)
# under-estimates by: 2, 3, 0, 1, 0, 0, 0, 0, -1, 2, 0, 0, 0, 0, 0, 0
# under by 8, over by 1
# So mostly okay, but actually overestimates deaths by 1 in Niedersachsen
# I don't think it makes sense for our cumulative total to overestimate the reported amount - underestimate maybe,
# since we might remove retracted cases on their original reporting date and not later, but overestimates don't
# really seem to make sense
aggregate(AnzahlTodesfall ~ Bundesland, data = a[a$Datenstand <= '2020-04-04', ], FUN = sum)$AnzahlTodesfall + new_deaths_apr5$AnzahlTodesfall




# Remove where 0 deaths:
new_deaths_apr5 <- new_deaths_apr5[new_deaths_apr5$AnzahlTodesfall > 0, ]

# Remove unnecessary columns:
new_deaths_apr5 <- new_deaths_apr5[, c(1:4, 7)]

#######################################################################################################################

### Notes ###
# I don't think there is any way to account for whether or not any of these deaths were later removed from the data,
# though; and data here are differences between the day in question and the day before, not necessarily newly-
# reported deaths
