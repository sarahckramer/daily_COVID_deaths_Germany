#######################################################################################################################
### Ensure that columns needed for analysis have uniform naming convention
# Author: Sarah Kramer
# Date: 19/11/2020
#######################################################################################################################

### First, check whether important column names uniform ###
for (column_name in c('IdBundesland', 'IdLandkreis', 'Altersgruppe', 'Geschlecht',
                      'Meldedatum', 'Datenstand', 'AnzahlTodesfall', 'NeuerTodesfall')) {
  
  if (any(!unlist(lapply(list_covid_deaths, function(ix) {
    column_name %in% colnames(ix)
  })))) {
    print(column_name)
  }
  
}
rm(column_name)
# not uniform: IdBundesland; IdLandkreis; Meldedatum; NeuerTodesfall

#######################################################################################################################

### Then, standardize those that are not ###

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

# Standardize name of initial case report date:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  if ('Meldedatum2' %in% names(ix)) {
    names(ix)[names(ix) == 'Meldedatum2'] <- 'Meldedatum'
  }
  ix
})

# Standardize name of new deaths column:
list_covid_deaths <- lapply(list_covid_deaths, function(ix) {
  if ('Neuer.Todesfall' %in% names(ix)) {
    names(ix)[names(ix) == 'Neuer.Todesfall'] <- 'NeuerTodesfall'
  }
  ix
})

#######################################################################################################################

### Finally, re-check that all are now standardized ###
for (column_name in c('IdBundesland', 'IdLandkreis', 'Altersgruppe', 'Geschlecht',
                      'Meldedatum', 'Datenstand', 'AnzahlTodesfall', 'NeuerTodesfall')) {
  
  if (any(!unlist(lapply(list_covid_deaths, function(ix) {
    column_name %in% colnames(ix)
  })))) {
    print(column_name)
  }
  
}
rm(column_name)
