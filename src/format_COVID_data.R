#######################################################################################################################
### Get number of new deaths by day, by Bundesland, from archived RKI data
# Author: Sarah Kramer
# Date: 17/11/2020
# Note: Here we are interested in the first "wave" only (March - June)
#######################################################################################################################

# Load necessary libraries:
library(stringr)

# Load all necessary functions:
source('src/functions.R')

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
rm(list = ls())

#######################################################################################################################

### Read in and format data ###
# source('src/read_data_to_list.R')







list_march <- lapply(file_list_March, function(ix) {
  read.csv(file = paste0('data_RKI/Maerz/', ix))
})
list_april <- lapply(file_list_April, function(ix) {
  read.csv(file = paste0('data_RKI/April/', ix))
})
list_may <- lapply(file_list_May, function(ix) {
  read.csv(file = paste0('data_RKI/Mai/', ix))
})
# list_june <- lapply(file_list_June, function(ix) {
#   read.csv(file = paste0('data_RKI/Juni/', ix))
# })

# Set Datenstand columns to be year-month-day format:
list_march <- lapply(1:length(file_list_March), function(ix) {
  list_march[[ix]]$Datenstand <- as.Date(str_sub(file_list_March[[ix]], 13, 22), format = '%Y-%m-%d')
  list_march[[ix]]
})
list_april <- lapply(1:length(file_list_April), function(ix) {
  list_april[[ix]]$Datenstand <- as.Date(str_sub(file_list_April[[ix]], 13, 22), format = '%Y-%m-%d')
  list_april[[ix]]
})
list_may <- lapply(1:length(file_list_May), function(ix) {
  list_may[[ix]]$Datenstand <- as.Date(str_sub(file_list_May[[ix]], 13, 22), format = '%Y-%m-%d')
  list_may[[ix]]
})

# Clean up:
rm(file_list_March, file_list_April, file_list_May, file_list_June)

#######################################################################################################################

### Format all data ###
# Join all data into one list:
list_covid_deaths <- c(list_march, list_april, list_may)
rm(list_march, list_april, list_may)


print(lapply(list_covid_deaths, function(ix) {dim(ix)}))

# a <- list_april[[28]]; b <- list_april[[29]]
# # b still has higher number of cumulative deaths and cases...
# # cases grouped, so one row with count of 2 instead of 2 rows?
# a <- a[a$NeuerTodesfall != -9, ]; b <- b[b$NeuerTodesfall != -9, ]
# # now b is bigger than a


# Standardize column names:
source('src/standardize_relevant_column_names.R', encoding = 'UTF-8')




# Keep only data on new deaths:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  ix[ix[, 'NeuerTodesfall'] %in% c(-1, 1), ]
  # if ('NeuerTodesfall' %in% names(ix)) {
  #   ix[ix[, 'NeuerTodesfall'] %in% c(-1, 1), ]
  # } else {
  #   print('Error.')
  # }
})

# If NeuerTodesfall == 1: Count this as a new death for the current Datenstand
# If NeuerTodesfall == -1: Identify where this case was originally reported as a death, and subtract 1 from that date
# Do this in 2 steps - deal with NeuerTodesfall == 1 first

#######################################################################################################################

### STEP 1: Newly-reported deaths each day ###
# Sum new deaths over Landkreise, age groups, and genders to get total new deaths by Bundesland:
new_deaths <- lapply(list_covid_deaths, function(ix) {
  aggregate(AnzahlTodesfall ~ Bundesland + IdBundesland + Datenstand, data = ix[ix$NeuerTodesfall == 1, ], FUN = sum)
})

# Merge all data:
new_deaths <- do.call('rbind', new_deaths)

# Change Bundesland level names:
for (i in 1:16) {
  # print(i)
  print(as.vector(unique(new_deaths$Bundesland[new_deaths$IdBundesland == i])))
}; rm(i)
# 8 = Baden-Wuerttemberg; 16 = Thueringen
new_deaths$Bundesland <- as.character(new_deaths$Bundesland)
new_deaths$Bundesland[new_deaths$IdBundesland == 8] <- 'Baden-Wuerttemberg'
new_deaths$Bundesland[new_deaths$IdBundesland == 16] <- 'Thueringen'
new_deaths$Bundesland <- factor(new_deaths$Bundesland)

# And add English names:
new_deaths$Bundesland_ENG <- new_deaths$Bundesland
levels(new_deaths$Bundesland_ENG) <- c('Baden-Wuerttemberg', 'Bavaria', 'Berlin', 'Brandenburg', 'Bremen', 'Hamburg',
                                       'Hesse', 'Mecklenburg-Vorpommern', 'Lower Saxony', 'North Rhine-Westphalia',
                                       'Rhineland-Palatinate', 'Saarland', 'Saxony', 'Saxony-Anhalt',
                                       'Schleswig-Holstein', 'Thuringia')

# Reorder columns:
new_deaths <- new_deaths[, c(1, 5, 2:4)]

#######################################################################################################################

### STEP 2: Remove deaths later revised/removed from dataset ###
# Get list only of removed deaths:
list_removed <- lapply(list_covid_deaths, function(ix) {
  ix[ix$NeuerTodesfall == -1, ]
})

# Keep only important columns for identifying these people in data:
list_removed <- lapply(list_removed, function(ix) {
  ix <- ix[, which(names(ix) %in% c('IdBundesland', 'IdLandkreis', 'Altersgruppe', 'Geschlecht',
                                    'Meldedatum', 'Datenstand', 'AnzahlTodesfall'))]
  ix
})

# And do similar for data as a whole (use to search for when deaths originally reported):
list_covid_deaths_RED <- lapply(list_covid_deaths, function(ix) {
  ix <- ix[, which(names(ix) %in% c('IdBundesland', 'IdLandkreis', 'Altersgruppe', 'Geschlecht',
                                    'Meldedatum', 'Datenstand', 'AnzahlTodesfall', 'NeuerTodesfall'))]
  ix
})

# Combine into dataframe:
df_removed <- do.call('rbind', list_removed)
rm(list_removed)

# Again, same for full dataset:
df_covid_deaths_RED <- do.call('rbind', list_covid_deaths_RED)
rm(list_covid_deaths_RED)







# Unify format of Meldedatum:
standardize_date_format <- function(dat, colDate) {
  # Try several possible formats:
  dat$Meldedatum_a <- as.Date(dat[, colDate], format = '%Y-%m-%d')
  print(summary(dat$Meldedatum_a))
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%m/%d/%Y')
  print(summary(dat$Meldedatum_a))
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%d/%m/%Y')
  print(summary(dat$Meldedatum_a))
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%Y/%m/%d')
  print(summary(dat$Meldedatum_a))
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%d.%m.%Y')
  print(summary(dat$Meldedatum_a))
  
  # Replace old dates with newly-formatted dates:
  dat[, colDate] <- dat$Meldedatum_a
  dat <- dat[, 1:(dim(dat)[2] - 1)]
  
  # Correct obvious errors (date cannot be after end of June, or before first reported case (01-27)):
  dates_to_update <- dat[, colDate][dat[, colDate] >= as.Date('2020-07-01', format = '%Y-%m-%d') |
                                   dat[, colDate] < as.Date('2020-02-10', format = '%Y-%m-%d')]
  print(dates_to_update)
  dates_to_update <- as.Date(paste(str_sub(dates_to_update, 1, 4), str_sub(dates_to_update, 9, 10),
                                   str_sub(dates_to_update, 6, 7), sep = '-'), format = '%Y-%m-%d')
  print(dates_to_update)
  dat[, colDate][dat[, colDate] >= as.Date('2020-07-01', format = '%Y-%m-%d') |
                dat[, colDate] < as.Date('2020-02-10', format = '%Y-%m-%d')] <- dates_to_update
  
  # Return updated data frame:
  dat
}

df_removed <- standardize_date_format(df_removed, 'Meldedatum')
df_covid_deaths_RED <- standardize_date_format(df_covid_deaths_RED, 'Meldedatum')








# For each case in df_removed, find where first reported as a death:
# df_removed$Meldedatum <- as.character(df_removed$Meldedatum)
# df_covid_deaths_RED$Meldedatum <- as.character(df_covid_deaths_RED$Meldedatum)
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED) # possible this misses reports due to weird date formats, though

# for (i in 1:length(time_of_first_reporting)) {
#   print(df_removed[i, ])
#   print(time_of_first_reporting[[i]])
#   print('------------------------------------------------------------------------------------------------------------')
# }; rm(i)

# Often it's from the day before, but there are also cases that can't be found anywhere in the previous data
# Could first subtract out/remove rows for unambiguous matches, then match again and continue the process...

# Ideally, every death should show up first as a new death (NeuerTodesfall == 1), then a not-new death at all later timepoints -
# is this the case?
# I'm not convinced - Tracking df_removed[113, ], he appears as a death beginning on 4/19, but not at every timepoint between 4/19
# and 4/29; and he never appears as a NEW case, just starts being reported as a case at some point
# So where a -1 can't be matched to an earlier reported death, it seems safe to ignore it, right? Because it doesn't correspond
# to any of the deaths we've previously added to our total

# If case cannot be identified in the previous new deaths, remove from consideration:
# What if Meldedatum is before March 27 (and therefore not included in the dataset)?
# I think we can still ignore them - we still wouldn't know when the case was first reported as a death
# (if it ever was reported as a "new" death)
n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
  dim(ix)[1]
})) # identify how many matching records
# 43 with no matching records; 94 with no matching records
# after updating date formats, down to 20 (from 94) with no matches!

# Calculate how much we may expect to ultimately overestimate cumulative deaths:
sum(df_removed$AnzahlTodesfall[which(n_matching_records == 0)]) # -21 (through May)

df_removed <- df_removed[which(n_matching_records > 0), ]

# Re-search for matching records on smaller dataframe:
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)
n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
  dim(ix)[1]
}))









# Then, subtract from the relevant totals where a single, unambiguous record is found of a retracted deaths original
# reporting as a new death:




remove_later_retracted_deaths <- function(ix, dat_newDeaths, dat_search, dat_removed, matches, n_matches_COMP1) {
  
  df_match <- matches[[ix]]
  
  # If multiple matches, choose most recent:
  if (dim(df_match)[1] > 1) {
    df_match <- df_match[df_match$Datenstand == max(df_match$Datenstand), ]
  }
  
  # If multiple matches on same reporting date, choose first:
  if (dim(df_match)[1] > 1) {
    df_match <- df_match[1, ]
  }
  
  # First, identify Bundesland and date, and subtract relevant # of deaths from "new_deaths"
  dat_newDeaths$AnzahlTodesfall[dat_newDeaths$IdBundesland == df_match$IdBundesland & dat_newDeaths$Datenstand == df_match$Datenstand] <-
    dat_newDeaths$AnzahlTodesfall[dat_newDeaths$IdBundesland == df_match$IdBundesland & dat_newDeaths$Datenstand == df_match$Datenstand] +
    dat_removed[ix, 'AnzahlTodesfall'] # values in df_removed are negative, so add retracted deaths to # of deaths on relevant date/Bundesland
  
  # Also, remove the matched case from df_covid_deaths_RED, to prevent multiple cases matching to the same record:
  dat_search$AnzahlTodesfall[which(rownames(dat_search) == rownames(df_match))] <-
    dat_search$AnzahlTodesfall[which(rownames(dat_search) == rownames(df_match))] + dat_removed[ix, 'AnzahlTodesfall']
  dat_search <- dat_search[dat_search$AnzahlTodesfall != 0, ] # positive and negative okay
  # dat_search <- dat_search[-which(rownames(dat_search) == rownames(df_match)), ]
  
  # Now, re-search df_covid_deaths_RED to ensure that all with 1 match (except current) still have 1 match:
  matches_COMP <- match_to_first_report(dat_removed, dat_search)
  n_matches_COMP2 <- unlist(lapply(matches_COMP, function(jx) {
    dim(jx)[1]
  }))
  
  if (ix != length(n_matches_COMP1)) { # otherwise no vector left to compare!
    if (all.equal(n_matches_COMP1[(ix + 1):length(n_matches_COMP1)], n_matches_COMP2[(ix + 1):length(n_matches_COMP2)]) != TRUE) {
      print(paste0('Error: ', ix))
      matches <- matches_COMP
      n_matches_COMP1 <- n_matches_COMP2
    }
  } else {
    print(table(n_matches_COMP2))
  }
  print(table(n_matches_COMP1[(ix + 1):length(n_matches_COMP1)])) # ensure that all retractions not yet handled still have a match
  
  return(list(dat_newDeaths, dat_search, matches, n_matches_COMP1))
}

iterate_removals <- function(dat_newDeaths_iter, dat_search_iter, dat_removed_ORIG, matches_iter, n_matches_ORIG) {
  # Subtracts out the number of retracted deaths from the total new deaths on the day the retracted death was first reported;
  # if multiple matching records for a single retraction, choose the most recent
  
  n_matches_COMP1 <- n_matches_ORIG
  
  if (any(n_matches_ORIG == 1)) {
    
    for (i in which(n_matches_ORIG == 1)) {
      
      res <- remove_later_retracted_deaths(i, dat_newDeaths_iter, dat_search_iter, dat_removed_ORIG, matches_iter, n_matches_COMP1)
      dat_newDeaths_iter <- res[[1]]
      dat_search_iter <- res[[2]]
      matches_iter <- res[[3]]
      n_matches_COMP1 <- res[[4]]
    
    }
    
  } else { # only want to run this once all of the unique matches have been removed
    
    for (i in 1:length(n_matches_ORIG)) {
      
      res <- remove_later_retracted_deaths(i, dat_newDeaths_iter, dat_search_iter, dat_removed_ORIG, matches_iter, n_matches_COMP1)
      dat_newDeaths_iter <- res[[1]]
      dat_search_iter <- res[[2]]
      matches_iter <- res[[3]]
      n_matches_COMP1 <- res[[4]]
      
    }
    
  }
  
  return(list(dat_newDeaths_iter, dat_search_iter))
}






while (any(n_matching_records == 1)) {
  
  # Generate corrected new_deaths dataframe:
  res_removed <- iterate_removals(new_deaths, df_covid_deaths_RED, df_removed, time_of_first_reporting, n_matching_records)
  new_deaths <- res_removed[[1]]; df_covid_deaths_RED <- res_removed[[2]]
  rm(res_removed)
  
  # Remove records with only 1 unambiguous match from df_removed:
  df_removed <- df_removed[which(n_matching_records != 1), ]
  
  # Re-search for matching records on smaller dataframe:
  time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)
  n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
    dim(ix)[1]
  }))
  
}
print(table(n_matching_records))

# res_removed <- remove_later_retracted_deaths(new_deaths, df_covid_deaths_RED, df_removed, time_of_first_reporting, n_matching_records)
# # at end of run there are still 8 records with a single match, even though a corresponding record should have been removed for 3 of these
# # (for total of 154 with 0 matches)
# # are these all where there was a match record with AnzahlTodesfall > 1?
# # time_of_first_reporting[c(9, 70, 96, 131, 133, 134, 137, 162)]
# # first 3 are; other 5 are records that previously had multiple matches and now do not
# new_deaths <- res_removed[[1]]; df_covid_deaths_RED <- res_removed[[2]]
# rm(res_removed)
# 
# # Remove records with only 1 unambiguous match from df_removed, and search again:
# df_removed <- df_removed[which(n_matching_records != 1), ]
# # this will deal with issue above - removes all those that PREVIOUSLY had 1 match
# 
# # Re-search for matching records on smaller dataframe:
# time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)
# n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
#   dim(ix)[1]
# }))

# Okay! Now we can move on to where there are multiple matches for each record.
# Eventually turn this all into some sort of loop...
print(dim(unique(df_removed)))
# So 2 entries have a duplicate - let's assume these do refer to 2 different people

res_removed <- iterate_removals(new_deaths, df_covid_deaths_RED, df_removed, time_of_first_reporting, n_matching_records)
new_deaths <- res_removed[[1]]; df_covid_deaths_RED <- res_removed[[2]]
rm(res_removed)











# Clean up:
rm(time_of_first_reporting, n_matching_records, df_removed, df_covid_deaths_RED)

# Ensure number of new deaths never drops below 0:
print(summary(new_deaths$AnzahlTodesfall))

# For now, remove where no deaths:
new_deaths <- new_deaths[new_deaths$AnzahlTodesfall > 0, ]

#######################################################################################################################

### Output results ###
# Write new deaths by date of reporting:
write.csv(new_deaths, file = 'data_formatted/new_deaths_TEMP_20-11.csv', row.names = FALSE)

#######################################################################################################################

# NOTES:

# Why last file in April smaller than two before?
# Seems to be b/c rows are consolidated; i.e., one row with 2 cases, instead of 2 separate rows




# TO EXPLORE:

# Are all Bundeslaender contained in every data set??
# Note: April 16 seems to be missing Bundeslaender 10-16?

# Adding all in new_deaths, + April 5, + most of March, minus df_removed -> still missing 49 deaths - check where missing Bundeslaender?
# (In other words, make sure cumulative equals reported on last day of April/June)

# And obviously add in data from June!

### Adjust dates (Refdatum always at least one ahead of reporting date) ###
# Or this can be handled by observation model

# Maybe ultimately read in later data as well, to see if any of the reported deaths from first wave were later "retracted"?

# Will need to fill in with 0 in the long data if no deaths

# And unify date formats

# Comment all functions

# Check that all files have all necessary libraries loaded

#######################################################################################################################

#######################################################################################################################

#######################################################################################################################

#######################################################################################################################
