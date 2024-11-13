library(readr)
library(dplyr)
library(stringr)

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

t20_bbb_list <- list()
for (i in 1:10) {
  year <- 2014 + i
  t20_bbb_list[[i]] <- tryCatch(
    {
      df <- read_csv(t20_bbb_links[[as.character(year)]])
      if (ncol(df) == 1 && colnames(df) == "<!DOCTYPE html>") {
        stop(paste("Error: The file for year", year, "appears to be an HTML document instead of a CSV file."))
      }
      df %>%
        filter(competition == "T20 Blast")
    },
    error = function(e) {
      message(paste("Error reading data for year", year, "from URL:", url))
    }
  )
}
t20_bbb <- bind_rows(t20_bbb_list)

