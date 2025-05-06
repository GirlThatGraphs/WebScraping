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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/excess-under-75-mortality-rates-in-adults-with-serious-mental-illness")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/excess-under-75-mortality-rates-in-adults-with-serious-mental-illness") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  slice(2) |> 
  pull()


url2 <- paste0("https://digital.nhs.uk",web_data)

#Reading the HTML code from the website
webpage2 <- read_html(url2)

link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".csv") # find those that end in xlsx  



#Download File
destfile <- "SMIPremDeaths.csv"
curl::curl_download(link, destfile)

SMIPremDeaths <- read.csv("SMIPremDeaths.csv") |> 
  mutate_all(str_replace_all,"\\*","") |> 
  pivot_longer(8:20) |> 
  mutate(Period = PERIOD_OF_COVERAGE) |> 
  separate_wider_delim(Period, delim = ";", names = c("Deaths", "MH")) |> 
  mutate(Deaths_Period_Start = str_sub(Deaths, str_locate(Deaths, "20")[1],str_locate(Deaths, "20")[1]+9),
         Deaths_Period_End = str_sub(Deaths, str_locate(Deaths, "to 20")[1]+3,str_locate(Deaths, "to 20")[1]+12),
         In_Contact_With_2nd_MH_Services_Period_Start = str_sub(MH, str_locate(MH, "20")[1],str_locate(MH, "20")[1]+9),
         In_Contact_With_2nd_MH_Services_Period_End = str_sub(MH, str_locate(MH, "to 20")[1]+3,str_locate(MH, "to 20")[1]+12)) |> 
  mutate(Measure_Value_Str = as.character(value),
         Effective_Snapshot_Date = Deaths_Period_End,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Three Years",
         Unique_ID = "",
         AuditKey = "",
         condition = "") |> 
  select(1:2, 12:15, 3:4,22,8:9, 16, 17:21, 6:7)

Year <- SMIPremDeaths |> 
  select(YEAR) |> 
  head(1) |> 
  pull()


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Excess_U75_Mort_Rate_In_Adults_Serious_Mental_Illness1")) %>% 
  collect() %>% 
  select(YEAR) |> 
  group_by(YEAR) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(YEAR)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Excess_U75_Mort_Rate_In_Adults_Serious_Mental_Illness1")) %>% 
  collect() %>% 
  select(YEAR) |> 
  group_by(YEAR) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(YEAR)) |> 
  head(1) |> 
  select(2) |> 
  pull()

Load <- function() {
  
  if(Loaded_Date == Year) {
    print(paste("Not Loading SMI Premature Deaths Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Excess_U75_Mort_Rate_In_Adults_Serious_Mental_Illness1"), SMIPremDeaths, append = TRUE)
    print(paste("Loaded SMI Premature Deaths Data for ", Year))
  }
}
                 
Load()
