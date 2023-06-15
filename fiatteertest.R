require(ggplot2)
require(readxl)
require(dplyr)
require(lubridate)
library(tidyverse)

library(ggstream)

#works but not-ideal (case_match seems better?)
result_file <-
  read_excel("oefeningen\\fiatteerlijst_26_04_23.xlsx") %>%
  mutate(
    across(contains("result") , as.numeric),
    across(contains(c("datum", "date")), as.Date),
    across(contains(
      c(
        "hoednhd",
        "klant",
        "code",
        "ID",
        "labnummer",
        "status",
        "groep",
        "workday"
      )
    ), as.factor)
    
  )
summary(result_file)

# run through each column name in sequence (ideally at load time)
# determine column data type according to column name with some flexibility
# make sure results columns are numeric but do NOT remove their non-numeric results (new column for those?)
#
#
#
#
#

result_file <-
  read_excel("oefeningen\\fiatteerlijst_26_04_23.xlsx") %>%
  mutate(pick(
    case_match (
      contains("result") ~ as.numeric,
      contains(c("datum", "date")) ~ as.Date,
      contains(
        c(
          "hoednhd",
          "klant",
          "code",
          "ID",
          "labnummer",
          "status",
          "groep",
          "prio"
        )
      ) ~ as.factor,
      .default = as.factor
    )
  ))
summary(result_file)

case_fiatteer_types <- function(column = cur_column()) {
  case_match()
  
}