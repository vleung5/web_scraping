#Extracting a table from a PDF
#install.packages("pdftools")

library(dslabs)
data("research_funding_rates")
research_funding_rates 

#download pdf file
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file, mode="wb")
txt <- pdf_text(temp_file)
file.remove(temp_file)

#We keep the page we want by using this code:
raw_data_research_funding_rates <- txt[2]

raw_data_research_funding_rates %>% head

#split each line with \n
tab <- str_split(raw_data_research_funding_rates, "\n")
tab %>% head

#Because we start off with just one element in the string, we end up with a list with just one entry:
tab <- tab[[1]]
tab %>% head

#we see that the information for the column names is the third and forth entires:
the_names_1 <- tab[3]
the_names_2 <- tab[4]

#We want to remove the leading space and everything following the comma. We can use regex for the latter. 
#Then we can obtain the elements by splitting using the space. We want to split only when there are 2 or 
#more spaces to avoid splitting success rate. So we use the regex \\s{2,} as follows:
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2

#Here we want to trim the leading space and then split by space as we did for the first line:
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

#Now we can join these to generate one name for each column:
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names



#Now let's get the actual data. Looking at tab, we see information is in lines 6 through 14.
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()


identical(research_funding_rates, new_research_funding_rates)
