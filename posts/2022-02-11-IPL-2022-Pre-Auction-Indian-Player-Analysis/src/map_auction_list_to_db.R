library(yaml)
library(RMariaDB)

config <- yaml::read_yaml("~/.config/config.yaml")
con <- dbConnect(MariaDB(), host =config$aws$host, port = config$aws$port, user = config$aws$user, password = config$aws$password, dbname = "cricket")
players <- dbGetQuery(con, "SELECT * FROM players")
dbDisconnect(con)

manual_mappings_df <- tibble(
  name_from_auction = c(
    "Wanindu Hasaranga", "Kuldeep Yadav", "Ankit Singh Rajpoot",
    "Syed Khaleel Ahmed", "Akila Dhananjaya", "Pathum Nissaanka",
    "Danushka Gunatilaka", "Mark Adnair", "Jon Russ Jaggesar", "Shahbaz Ahmed",
    "Lukman Hussain Meriwala", "Prithviraj Yarra", "Sanjay Yadav"
  ),
  name_in_db = c(
    "PWH de Silva", "Kuldeep Yadav", "AS Rajpoot", "KK Ahmed", "A Dananjaya",
    "P Nissanka", "MD Gunathilaka", "MR Adair", "JL Jaggesar", "Shahbaz Ahmed",
    "LI Meriwala", "Y Prithvi Raj", NA_character_
  )
)


find_player_name_in_db <- function(name_from_auction_list, dob_from_auction_list, players_df = players) {
  split_name_from_auction_list_by_spaces <- str_split(str_remove_all(name_from_auction_list, "\\."), " ")[[1]]
  forename <- split_name_from_auction_list_by_spaces[1]
  surname <- str_c(split_name_from_auction_list_by_spaces[2:length(split_name_from_auction_list_by_spaces)], collapse = " ")
  
  if (name_from_auction_list %in% manual_mappings_df$name_from_auction) {
    return(manual_mappings_df$name_in_db[which(name_from_auction_list == manual_mappings_df$name_from_auction)])
  }
  
  matches <- players_df %>%
    filter(
      str_detect(name, fixed(surname, ignore_case = TRUE)),
      date_of_birth == dob_from_auction_list
    )
  if (nrow(matches) == 1) {
    return(matches$name)
  }
  
  if (nchar(forename) > 1) {
    matches <- players_df %>%
      filter(
        str_detect(name, fixed(forename, ignore_case = TRUE)),
        date_of_birth == dob_from_auction_list
      )
    if (nrow(matches) == 1) {
      return(matches$name)
    } 
  }
  
  matches <- players_df %>%
    filter(
      (name == str_c(forename, surname, sep = " ")) | (str_detect(name, fixed(surname, ignore_case = TRUE)) & str_sub(forename, 1, 1) == str_sub(name, 1, 1)) 
    )
  if (nrow(matches) == 1) {
    return(matches$name)
  }
  
  NA_character_
}


