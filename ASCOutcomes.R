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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  slice(1) |> 
  pull()

url2 <- paste0("https://digital.nhs.uk",web_data)

#Reading the HTML code from the website
webpage2 <- read_html(url2)

link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.csv") |>  # find those that end in xlsx
  head(1)



#Download File
destfile <- "ASCOutcomes.csv"
curl::curl_download(link, destfile)

year <- as.Date(paste0("20",str_sub(link, str_locate(link, "20")[1]+5,str_locate(link, "20")[1]+6),"-03-31"))


ASCOutcomes <- read.csv("ASCOutcomes.csv") |> 
  mutate(Measure = "",
         Measure_Value_Str = as.character(Measure_Value),
         Effective_Snapshot_Date = year,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Annual",
         Unique_ID = "",
         AuditKey = "",
         Measure_Collection = "") |> 
  select(1,4,3,5:6,11,9:10,7:8,12:18)


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Adult_Social_Care_Outcomes_FrameworkOpen_Data1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Adult_Social_Care_Outcomes_FrameworkOpen_Data1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(2) |> 
  pull()

Load <- function() {
  
  if(Loaded_Date == year) {
    print(paste("Not Loading ASC Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Adult_Social_Care_Outcomes_FrameworkOpen_Data1"), ASCOutcomes, append = TRUE)
    print(paste("Loaded ASC Data for ", year))
  }
}
                 
Load()
