# National Staff Abscence Rates ----------------------------------------------------------
library(htmltools)
library(rvest)
library(xml2)
library(dplyr)
library(readxl)
library(tidyr)
library(magrittr)
library(odbc)
library(dbplyr)
library(DBI)
library(stringr)
library(beepr)


#Specifying the url for desired website to be scraped
url <- paste("https://www.england.nhs.uk/statistics/statistical-work-areas/mixed-sex-accommodation/msa-data/")


#Reading the HTML code from the website
webpage2 <- read_html(url)


link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xls") %>% # find those that end in xlsx
  str_subset("^(?!.*Time)") %>% # find those that end in xlsx
  .[[1]] 

  


#Download File
destfile <- "MSA.xlsx"
curl::curl_download(link, destfile)


MSA_Date <- read_excel("MSA.xlsx", sheet = "Provider - All", 
                  range = "C5:C5", col_names = FALSE) |> 
  pull() %>%
  paste("01",.) |> 
  as.Date("%d %B %Y")




MSA <- read_excel("MSA.xlsx", sheet = "Provider - All", 
                          skip = 14) |> 
  drop_na(4) |> 
  select(1:5, `Finished Consultant Episodes` = 6, `Breach Rate` = 7) |> 
  mutate(Effective_Snapshot_Date = MSA_Date)

# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Mixed_Sex_Accomodation")) %>% 
 ## collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  collect() %>% 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Mixed_Sex_Accomodation")) %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(2) |> 
  collect() %>%   
  pull()

Load <- function() {
  
  if(Loaded_Date == MSA_Date) {
    print(paste("Not Loading MSA Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Mixed_Sex_Accomodation"), MSA, append = TRUE)
    print(paste("Loaded MSA Data for ", MSA_Date))
  }
}
                 
Load()
