#######################################################################################################################
### Read in all available data from first wave (March - June 2020)
# Author: Sarah Kramer
# Date: 19/11/2020
#######################################################################################################################

# Load libaries:
library(stringr)

# Get list of files for each month:
file_list_March <- list.files('data_RKI/Maerz/', pattern = '.csv') # 27-31/03
file_list_April <- list.files('data_RKI/April/', pattern = '.csv') # missing 1
file_list_May <- list.files('data_RKI/Mai/', pattern = '.csv') # complete
file_list_June <- list.files('data_RKI/Juni/', pattern = '.csv') # complete

# Read data:
list_march <- lapply(file_list_March, function(ix) {
  read.csv(file = paste0('data_RKI/Maerz/', ix))
})
list_april <- lapply(file_list_April, function(ix) {
  read.csv(file = paste0('data_RKI/April/', ix))
})
list_may <- lapply(file_list_May, function(ix) {
  read.csv(file = paste0('data_RKI/Mai/', ix))
})
list_june <- lapply(file_list_June, function(ix) {
  read.csv(file = paste0('data_RKI/Juni/', ix))
})

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
list_june <- lapply(1:length(file_list_June), function(ix) {
  list_june[[ix]]$Datenstand <- as.Date(str_sub(file_list_June[[ix]], 13, 22), format = '%Y-%m-%d')
  list_june[[ix]]
})

# Join all data into one list:
list_covid_deaths <- c(list_march, list_april, list_may, list_june)

# Clean up:
rm(file_list_March, file_list_April, file_list_May, file_list_June, list_march, list_april, list_may, list_june)
