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
library(lubridate)
library(beepr)


#Specifying the url for desired website to be scraped
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data_html <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/nhs-workforce-statistics/") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  mutate(monthlong = str_remove(link,"/data-and-information/publications/statistical/nhs-workforce-statistics/")) |> 
  mutate(month = substr(monthlong,1,3)) |> 
##  filter(month %in% c("jun", "sep", "dec", "mar")) |> 
  head(1) |> 
  select(monthlong) |> 
  pull()

url2 <- paste0(url, "/", web_data_html)

#Reading the HTML code from the website
webpage2 <- read_html(url2)


thismonth <- as.Date(paste0("01-",web_data_html), "%d-%B-%Y") |> 
  format.Date("%Y%m")

lastmonth <- as.Date(paste0("01-",web_data_html), "%d-%B-%Y") %m-% months(1) |> 
  format.Date("%Y%m")

Period1 <- paste0(lastmonth, " to ", thismonth)

SnapshotDate <- as.Date(paste0("01-",web_data_html), "%d-%B-%Y")


link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xlsx") %>% # find those that end in xlsx
  str_subset("Turnover") %>% # find those that end in xlsx
  str_subset("bench") %>% # find those that end in xlsx
  .[[1]]     




#Download File
destfile <- "workforce_turnover.xlsx"
curl::curl_download(link, destfile)

Workforce_HC <- read_excel(destfile, sheet = "Monthly turnover, HC", skip = 7) |> 
  select(1:10,12) |> 
  pivot_longer(cols = 10:11) |> 
  mutate(Period = Period1) |> 
  select(12,10,6:9,Head_Count = 11) |> 
  filter(Head_Count > 0) |> 
  mutate(Effective_Snapshot_Date = SnapshotDate)

Workforce_FTE <- read_excel(destfile, sheet = "Monthly turnover, FTE", skip = 7) |> 
  select(1:10,12) |> 
  pivot_longer(cols = 10:11) |> 
  mutate(Period = Period1) |> 
  select(12,10,6:9,FTE = 11) |> 
  filter(FTE > 0) |> 
  mutate(Effective_Snapshot_Date = SnapshotDate)


Workforce <- Workforce_HC |> 
  full_join(Workforce_FTE) |> 
  mutate(DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Snapshot Monthly",
         Unique_ID = "",
         AuditKey = "") |> 
  rename( `Org Code` = 3)

# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Monthly_Turnover_From_Org_Benchmarking_Source1")) %>% 
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

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Monthly_Turnover_From_Org_Benchmarking_Source1")) %>% 
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
  
  if(Loaded_Date == SnapshotDate) {
    print(paste("Not Loading Workforce Turnover Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Monthly_Turnover_From_Org_Benchmarking_Source1"), Workforce, append = TRUE)
    print(paste("Loaded Workforce Turnover Data for ", SnapshotDate))
  }
}
                 
Load()
