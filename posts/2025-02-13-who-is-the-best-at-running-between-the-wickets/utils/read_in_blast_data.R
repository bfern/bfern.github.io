library(readr)
library(dplyr)

t20_bbb_links <- list(
  "2015" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AIrqfgrtrWnzL2y91kArA6I/t20_2015.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2016" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ACehDaYpVx9tBvfBWmVK7Ds/t20_2016.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2017" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AJt4x4hRw6lIYUy0JC1Rw4I/t20_2017.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2018" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AIiMP2xD16cy_gPNDvFTvFA/t20_2018.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2019" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ANdcqTtYYWQOJ_pcuIYaTlQ/t20_2019.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2020" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ANc9rKEFNRpuJ73Xt0z59C4/t20_2020.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2021" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ADCtr7XVpSsVs9D-UYwHSZo/t20_2021.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2022" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AHnU-4jiZzSWUCndhXUIjag/t20_2022.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2023" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AH2g3ggZO77cGLWkd-BID88/t20_2023.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2024" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AGwhkn3pweMcSkwT-H88pkw/t20_2024.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1"
)

major_english_county_grounds <- c(
  "Edgbaston, Birmingham",
  "Trent Bridge, Nottingham",
  "Riverside Ground, Chester-le-Street",
  "Headingley, Leeds",
  "Kennington Oval, London",
  "County Ground, Hove",
  "County Ground, Northampton",
  "The Rose Bowl, Southampton",
  "County Ground, New Road, Worcester",
  "Grace Road, Leicester",
  "County Ground, Chelmsford",
  "St Lawrence Ground, Canterbury",
  "Sophia Gardens, Cardiff",
  "Old Trafford, Manchester",
  "County Ground, Bristol",
  "County Ground, Derby",
  "The Cooper Associates County Ground, Taunton",
  "Lord's, London"
)

read_in_blast_data <- function(season_links = t20_bbb_links, grounds = major_english_county_grounds) {
  t20_bbb_list <- list()
  for (i in 1:10) {
    year <- 2014 + i
    t20_bbb_list[[i]] <- tryCatch(
      {
        df <- read_csv(
          season_links[[as.character(year)]],
          show_col_types = FALSE,
          col_select = -1, # first column looks to have been caused by writing indexes from a pandas dataframe
          name_repair = "minimal" # needed because of column without name mentiond above
        )
        if (ncol(df) == 1 && colnames(df) == "<!DOCTYPE html>") {
          stop(paste("Error: The file for year", year, "appears to be an HTML document instead of a CSV file."))
        }
        df %>%
          mutate(ground = case_when(
            ground == "County Ground, Taunton" ~ "The Cooper Associates County Ground, Taunton",
            TRUE ~ ground
          )) %>%
          filter(
            competition == "T20 Blast", # t20 blast only
            ground %in% grounds # selected grounds only
          )
      },
      error = function(e) {
        message(paste("Error reading data for year", year, "from URL:", url))
      }
    )
  }
  bind_rows(t20_bbb_list)
}