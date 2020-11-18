#######################################################################################################################
### Get number of new deaths by day, by Bundesland, from archived RKI data
# Author: Sarah Kramer
# Date: 17/11/2020
# Note: Here we are interested in the first "wave" only (March - June)
#######################################################################################################################

# Load necessary libraries:
library(stringr)

# Function to match retracted death cases to the time the case was first reported as a death:
match_to_first_report <- function(dat_removed, dat_search) {
  # dat_removed: data frame of deaths with -1 in column "NeuerTodesfall"
  # dat_search: data frame including all reports where "NeuerTodesfall" != 0
  
  list_out <- list() # output will be a list of matching records
  for (i in 1:dim(dat_removed)[1]) {
    search_case <- dat_removed[i, ]
    list_out[[i]] <- dat_search[dat_search$IdBundesland == search_case$IdBundesland &
                                  dat_search$Altersgruppe == search_case$Altersgruppe &
                                  dat_search$Geschlecht == search_case$Geschlecht &
                                  dat_search$Meldedatum == search_case$Meldedatum &
                                  dat_search$IdLandkreis == search_case$IdLandkreis &
                                  dat_search$NeuerTodesfall == 1 &
                                  search_case$Datenstand > dat_search$Datenstand, ]
    # match on: Bundesland, Landkreis, age group, sex; ensure case comes from report BEFORE the date of retraction
  }
  
  list_out
}

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
# rm(list = ls())

#######################################################################################################################

### Read in all data ###
list_march <- lapply(file_list_March, function(ix) {
  read.csv(file = paste0('data_RKI/Maerz/', ix))
})
list_april <- lapply(file_list_April, function(ix) {
  read.csv(file = paste0('data_RKI/April/', ix))
})
# list_may <- lapply(file_list_May, function(ix) {
#   read.csv(file = paste0('data_RKI/Mai/', ix))
# })
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






# Clean up:
rm(file_list_March, file_list_April, file_list_May, file_list_June)

#######################################################################################################################

### Format all data ###
# Join all data into one list:
list_covid_deaths <- c(list_march, list_april)
rm(list_march, list_april)

# Standardize name of new deaths column:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  if ('Neuer.Todesfall' %in% names(ix)) {
    names(ix)[names(ix) == 'Neuer.Todesfall'] <- 'NeuerTodesfall'
  }
  ix
})

# Standardize name of Bundesland ID column:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  if ('ï..IdBundesland' %in% names(ix)) {
    names(ix)[names(ix) == 'ï..IdBundesland'] <- 'IdBundesland'
  }
  ix
})

# Standardize name of Landkreis ID column:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  if ('Landkreis.ID' %in% names(ix)) {
    names(ix)[names(ix) == 'Landkreis.ID'] <- 'IdLandkreis'
  }
  ix
})

# Keep only data on new deaths:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  if ('NeuerTodesfall' %in% names(ix)) {
    ix[ix[, 'NeuerTodesfall'] %in% c(-1, 1), ]
  } else {
    print('Error.')
  }
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

# # Unify format of Meldedatum?:
# df_removed$Meldedatum_a <- as.Date(df_removed$Meldedatum, format = '%Y-%m-%d')
# df_removed$Meldedatum_a[is.na(df_removed$Meldedatum_a)] <- as.Date(df_removed$Meldedatum[is.na(df_removed$Meldedatum_a)], format = '%m/%d/%Y')
# df_removed$Meldedatum_a[is.na(df_removed$Meldedatum_a)] <- as.Date(df_removed$Meldedatum[is.na(df_removed$Meldedatum_a)], format = '%Y/%m/%d')
# df_removed$Meldedatum_a[is.na(df_removed$Meldedatum_a)] <- as.Date(df_removed$Meldedatum[is.na(df_removed$Meldedatum_a)], format = '%d.%m.%Y')
# 
# # obviously the August one is wrong - switch format:
# dates_to_update <- df_removed$Meldedatum_a[df_removed$Meldedatum_a > as.Date('2020-07-01', format = '%Y-%m-%d')]
# dates_to_update <- as.Date(paste(str_sub(dates_to_update, 1, 4), str_sub(dates_to_update, 9, 10),
#                                  str_sub(dates_to_update, 6, 7), sep = '-'), format = '%Y-%m-%d')
# df_removed$Meldedatum_a[df_removed$Meldedatum_a > as.Date('2020-07-01', format = '%Y-%m-%d')] <- dates_to_update
# 
# df_removed$Meldedatum <- df_removed$Meldedatum_a
# df_removed$Meldedatum_a <- NULL
# rm(dates_to_update)
# # Might all mess with ability to match on Meldedatum in next step, though

# For each case in df_removed, find where first reported as a death:
df_removed$Meldedatum <- as.character(df_removed$Meldedatum)
df_covid_deaths_RED$Meldedatum <- as.character(df_covid_deaths_RED$Meldedatum)
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)

# for (i in 1:length(time_of_first_reporting)) {
#   print(df_removed[i, ])
#   print(time_of_first_reporting[[i]])
#   print('------------------------------------------------------------------------------------------------------------')
# }; rm(i)
# Often it's from the day before, but there are also cases that can't be found anywhere in the previous data
# Could first subtract out/remove rows for unambiguous matches, then match again and continue the process...
# I guess if there are no matches (no previous timepoint at which the case shows up as a new death), there's
# actually no need to subtract anything

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

df_removed <- df_removed[which(n_matching_records > 0), ]

# Re-search for matching records on smaller dataframe:
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)
n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
  dim(ix)[1]
}))

# NOTE: Function does not actually match on whether the number of deaths being retracted matches the number originally
# enterred in the "matching" entry; so far this isn't a problem, but check before proceeding

# for (i in 1:length(n_matching_records)) {
#   print(df_removed[i, ])
#   print(time_of_first_reporting[[i]])
#   print('------------------------------------------------------------------------------------------------------------')
# }; rm(i)

# Then, subtract from the relevant totals where a single, unambiguous record is found of a retracted deaths original
# reporting as a new death:
for (i in which(n_matching_records == 1)) {
  df_match <- time_of_first_reporting[[i]]
  
  # First, identify Bundesland and date, and subtract relevant # of deaths from "new_deaths"
  new_deaths$AnzahlTodesfall[new_deaths$IdBundesland == df_match$IdBundesland & new_deaths$Datenstand == df_match$Datenstand] <-
    new_deaths$AnzahlTodesfall[new_deaths$IdBundesland == df_match$IdBundesland & new_deaths$Datenstand == df_match$Datenstand] +
    df_removed[i, 'AnzahlTodesfall'] # values in df_removed are negative, so add retracted deaths to # of deaths on relevant date/Bundesland
  
  # Also, remove the matched case from df_covid_deaths_RED, to prevent multiple cases matching to the same record:
  df_covid_deaths_RED <- df_covid_deaths_RED[-which(rownames(df_covid_deaths_RED) == rownames(df_match)), ]
  
  # Now, re-search df_covid_deaths_RED to ensure that all with 1 match (except current) still have 1 match:
  time_of_first_reporting_COMP <- match_to_first_report(df_removed, df_covid_deaths_RED)
  n_matching_records_COMP <- unlist(lapply(time_of_first_reporting_COMP, function(ix) {
    dim(ix)[1]
  }))
  
  if (i != length(n_matching_records)) { # otherwise no vector left to compare!
    if (!all.equal(n_matching_records[(i + 1):length(n_matching_records)], n_matching_records_COMP[(i + 1):length(n_matching_records_COMP)])) {
      print(paste0('Error: ', i))
    }
  }
}
# removing matched records doesn't seem to influence how many records are found for the others, meaning none of the cases where
# multiple matching records were found are repeats

# Clean up:
rm(i, df_match, time_of_first_reporting_COMP, n_matching_records_COMP)

# Remove records with only 1 unambiguous match from df_removed, and search again:
df_removed <- df_removed[which(n_matching_records != 1), ]

# Re-search for matching records on smaller dataframe:
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)
n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
  dim(ix)[1]
}))

print(dim(unique(df_removed)))
# So 2 entries have a duplicate - let's assume these do refer to 2 different people

# Loop through and assume correction is for most RECENT death (?):
for (i in 1:length(n_matching_records)) {
  df_match <- time_of_first_reporting[[i]]
  
  # Choose most recent:
  df_match <- df_match[df_match$Datenstand == max(df_match$Datenstand), ]
  
  # If multiple occurred on same day, choose first:
  if (dim(df_match)[1] > 1) {
    df_match <- df_match[1, ]
  }
  
  # First, identify Bundesland and date, and subtract relevant # of deaths from "new_deaths"
  new_deaths$AnzahlTodesfall[new_deaths$IdBundesland == df_match$IdBundesland & new_deaths$Datenstand == df_match$Datenstand] <-
    new_deaths$AnzahlTodesfall[new_deaths$IdBundesland == df_match$IdBundesland & new_deaths$Datenstand == df_match$Datenstand] +
    df_removed[i, 'AnzahlTodesfall'] # values in df_removed are negative, so add retracted deaths to # of deaths on relevant date/Bundesland
  
  # Also, remove the matched case from df_covid_deaths_RED, to prevent multiple cases matching to the same record:
  df_covid_deaths_RED <- df_covid_deaths_RED[-which(rownames(df_covid_deaths_RED) == rownames(df_match)), ]
  
  # Now, re-search df_covid_deaths_RED to ensure that all with 1 match (except current) still have 1 match:
  time_of_first_reporting_COMP <- match_to_first_report(df_removed, df_covid_deaths_RED)
  n_matching_records_COMP <- unlist(lapply(time_of_first_reporting_COMP, function(ix) {
    dim(ix)[1]
  }))
  
  if (i != length(n_matching_records)) { # otherwise no vector left to compare!
    if (all.equal(n_matching_records[(i + 1):length(n_matching_records)], n_matching_records_COMP[(i + 1):length(n_matching_records_COMP)]) != TRUE) {
      # print(paste0('Error: ', i))
      time_of_first_reporting <- time_of_first_reporting_COMP
    }
  }
}

# Clean up:
rm(i, df_match, time_of_first_reporting, time_of_first_reporting_COMP, n_matching_records, n_matching_records_COMP,
   df_removed, df_covid_deaths_RED)

# Ensure number of new deaths never drops below 0:
print(summary(new_deaths$AnzahlTodesfall))

# For now, remove where no deaths:
new_deaths <- new_deaths[new_deaths$AnzahlTodesfall > 0, ]

#######################################################################################################################

### Output results ###
# Write new deaths by date of reporting:
write.csv(new_deaths, file = 'data_formatted/new_deaths_TEMP.csv', row.names = FALSE)

#######################################################################################################################

# TO EXPLORE:
# Why last file in April smaller than two before??
# Are all Bundeslaender contained in every data set??
# Adding all in new_deaths, + April 5, + most of March, minus df_removed -> still missing 49 deaths - check where missing Bundeslaender?
# (In other words, make sure cumulative equals reported on last day of April/June)
# And obviously add in data from May and June!

### Adjust dates (Refdatum always at least one ahead of reporting date) ###

# Maybe ultimately read in later data as well, to see if any of the reported deaths from first wave were later "retracted"?

# Will need to fill in with 0 in the long data if no deaths

# And unify date formats

# For comparison can calculate difference from cumulative deaths on day before (difference between sum of
# AnzahlTodesfall where NeuerTodesfall 0 or 1 each day); also sum of AnzahlTodesfall where NeuerTodesfall
# is equal to 1 or -1

#######################################################################################################################

#######################################################################################################################

#######################################################################################################################

#######################################################################################################################
