library(rvest)
library(tidyverse)
library(stringr)

url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)

# h is of class xml_document
class(h)

h

#Use html_nodes to extract all nodes of that type
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab

#Convert this to data frame
tab <- tab %>% html_table
class(tab)


#set column names
tab <- tab %>% setNames(c("state","population", "total", "murders","gun_murders", "gun_ownership","total_rate",
                          "murder_rate", "gun_murder_rate"))
head(tab)

#There is still more data wrangling to do, ie remove commas and characters where numbers should be.
murders_new <- tab %>% mutate_at(2:9, parse_number)
murders_new %>% head

str_subset(tab$murders, "\\[")



