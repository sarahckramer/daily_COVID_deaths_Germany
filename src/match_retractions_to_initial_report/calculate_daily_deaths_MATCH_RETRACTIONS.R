#######################################################################################################################
### Attempt to correct for retractions by identifying when death first reported
# Author: Sarah Kramer
# Date: 17/11/2020
#######################################################################################################################

# Load all necessary functions:
source('src/match_retractions_to_initial_report/functions_for_matching_retractions.R')

#######################################################################################################################

### Read in and format data ###
# Read all data into list:
source('src/read_data_to_list.R')

# Standardize column names:
source('src/standardize_relevant_column_names.R', encoding = 'UTF-8')

# Keep only data on new deaths:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  ix[ix[, 'NeuerTodesfall'] %in% c(-1, 1), ]
})

# If NeuerTodesfall == 1: Count this as a new death for the current Datenstand
# If NeuerTodesfall == -1: Identify where this case was originally reported as a death, and subtract 1 from that date
# Do this in 2 steps - deal with NeuerTodesfall == 1 first

#######################################################################################################################

### STEP 1: Newly-reported deaths each day ###
# Sum new deaths over Landkreise, age groups, and genders to get total new deaths by Bundesland:
new_deaths <- lapply(list_covid_deaths, function(ix) {
  if (any(ix[, 'NeuerTodesfall'] == 1)) {
    aggregate(AnzahlTodesfall ~ Bundesland + IdBundesland + Datenstand, data = ix[ix$NeuerTodesfall == 1, ], FUN = sum)
  } else {
    NA
  }
})

# Remove any empty elements of new_deaths:
new_deaths <- new_deaths[which(unlist(lapply(new_deaths, function(ix) {length(ix)})) > 1)]

# Merge all data:
new_deaths <- do.call('rbind', new_deaths)

# Change Bundesland level names:
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
df_removed <- standardize_date_format(df_removed, 'Meldedatum')
df_covid_deaths_RED <- standardize_date_format(df_covid_deaths_RED, 'Meldedatum')

# For each case in df_removed, find where first reported as a death:
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)

# Often it's from the day before, but there are also cases that can't be found anywhere in the previous data
# Could first subtract out/remove rows for unambiguous matches, then match again and continue the process...

# Ideally, every death should show up first as a new death (NeuerTodesfall == 1), then a not-new death at all later timepoints -
# is this the case?
# I'm not convinced - Tracking df_removed[113, ], he appears as a death beginning on 4/19, but not at every timepoint between 4/19
# and 4/29; and he never appears as a NEW case, just starts being reported as a case at some point
# So where a -1 can't be matched to an earlier reported death, it seems safe to ignore it, right? Because it doesn't correspond
# to any of the deaths we've previously added to our total

# Calculate how many matching records to each retracted death:
n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
  dim(ix)[1]
})) # identify how many matching records
# 21 with no matching records

# Calculate how much we may expect to ultimately overestimate cumulative deaths:
print(sum(df_removed$AnzahlTodesfall[which(n_matching_records == 0)])) # -22

# If case cannot be identified in the previous new deaths, remove from consideration:
df_removed <- df_removed[which(n_matching_records > 0), ]

# Re-search for matching records on smaller dataframe:
time_of_first_reporting <- match_to_first_report(df_removed, df_covid_deaths_RED)
n_matching_records <- unlist(lapply(time_of_first_reporting, function(ix) {
  dim(ix)[1]
}))

# Then, subtract from the relevant totals where a single, unambiguous record is found of a retracted deaths original
# reporting as a new death:
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

# Now move on to where there are multiple matches for each record:
print(dim(unique(df_removed)))
# Note: 2 entries have a duplicate - let's assume these do refer to 2 different people

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
write.csv(new_deaths, file = 'data_formatted/new_deaths_SEARCHED.csv', row.names = FALSE)

# Clean up:
rm(list = ls())

#######################################################################################################################

# Potential next steps:
# Maybe ultimately read in later data as well, to see if any of the reported deaths from first wave were later "retracted"?
# Add in data from April 16
# Check against cumulative counts in official reports at various timepoints
