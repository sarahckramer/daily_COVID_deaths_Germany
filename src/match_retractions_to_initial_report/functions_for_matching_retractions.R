#######################################################################################################################
### Collect all functions for data processing in one place
# Author: Sarah Kramer
# Date: 20/11/2020
#######################################################################################################################

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

#
standardize_date_format <- function(dat, colDate) {
  # Try several possible formats:
  dat$Meldedatum_a <- as.Date(dat[, colDate], format = '%Y-%m-%d')
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%m/%d/%Y')
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%d/%m/%Y')
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%Y/%m/%d')
  dat$Meldedatum_a[is.na(dat$Meldedatum_a)] <- as.Date(dat[, colDate][is.na(dat$Meldedatum_a)], format = '%d.%m.%Y')
  
  # Replace old dates with newly-formatted dates:
  dat[, colDate] <- dat$Meldedatum_a
  dat <- dat[, 1:(dim(dat)[2] - 1)]
  
  # Correct obvious errors (date cannot be after end of June, or before first reported case (01-27)):
  dates_to_update <- dat[, colDate][dat[, colDate] >= as.Date('2020-07-01', format = '%Y-%m-%d') |
                                      dat[, colDate] < as.Date('2020-02-10', format = '%Y-%m-%d')]
  dates_to_update <- as.Date(paste(str_sub(dates_to_update, 1, 4), str_sub(dates_to_update, 9, 10),
                                   str_sub(dates_to_update, 6, 7), sep = '-'), format = '%Y-%m-%d')
  dat[, colDate][dat[, colDate] >= as.Date('2020-07-01', format = '%Y-%m-%d') |
                   dat[, colDate] < as.Date('2020-02-10', format = '%Y-%m-%d')] <- dates_to_update
  
  # Return updated data frame:
  dat
}

# 
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

#
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
      # print(ix)
      matches <- matches_COMP
      n_matches_COMP1 <- n_matches_COMP2
    }
  }
  
  return(list(dat_newDeaths, dat_search, matches, n_matches_COMP1))
}
