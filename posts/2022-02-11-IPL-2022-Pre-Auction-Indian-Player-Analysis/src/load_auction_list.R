library(tibble)
library(stringr)
library(lubridate)

countries <- c(
  "Afghanistan", "Australia", "Bangladesh", "England", "Ireland", "India",
  "New Zealand", "South Africa", "Sri Lanka", "West Indies", "Zimbabwe",
  "Namibia", "Nepal", "Scotland", "USA"
)
specialisms <- c("BATSMAN", "ALL-ROUNDER", "BOWLER", "WICKETKEEPER")

load_auction_list <- function(auction_list_file) {
  auction_list <- readLines(auction_list_file)
  auction_list <- auction_list[2:length(auction_list)]
  tibble(
    name = get_name(auction_list),
    specialism = get_specialism(auction_list),
    country = get_country(auction_list),
    date_of_birth = get_date_of_birth(auction_list),
    auction_string = auction_list
  )
}


get_name <- function(auction_list) {
  sapply(str_split(sapply(str_split(sapply(str_split(auction_list, str_c(countries, collapse="|")), "[[", 1), " ", 4), "[[", 4), " "), remove_last_element_and_join_rest)
}

get_specialism <- function(auction_list) {
  str_match(auction_list, str_c(specialisms, collapse="|"))[,1]
}

get_country <- function(auction_list) {
  str_match(auction_list, str_c(countries, collapse="|"))[,1]
}

get_date_of_birth <- function(auction_list) {
  dmy(sapply(str_split(sapply(str_split(auction_list, str_c(specialisms, collapse="|")), "[[", 1), " "), get_nth_element_from_end, n=3))
}

get_nth_element_from_end <- function(vec, n) {
  vec[length(vec)-n+1]
}

remove_last_element_and_join_rest <- function(vec, sep = " ") {
  str_c(head(vec, -1), collapse = " ")
}
