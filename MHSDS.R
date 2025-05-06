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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics")


#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data_html2 <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/mental-health-services-monthly-statistics/") |> 
  head(1) |>
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  mutate(urlshort = str_remove(link,"/data-and-information/publications/statistical/mental-health-services-monthly-statistics")) |> 
  mutate(monthlong = str_remove(link,"/data-and-information/publications/statistical/mental-health-services-monthly-statistics/performance-")) |> 
  mutate(monthdate = as.Date(paste0("01-",monthlong), "%d-%B-%Y")) 

link <- web_data_html2 |> 
  select(urlshort) |> 
  pull()


effectdate <- web_data_html2 |> 
  select(monthdate) |> 
  pull()

url2 <- paste0(url, link)

#Reading the HTML code from the website
webpage2 <- read_html(url2)


link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") |>  # find those that end in xlsx
  head(1)


#Download File
download.file(link,"mhsds.zip")

# List the files in the ZIP archive
zip_files <- unzip("mhsds.zip", list = TRUE)


# Extract the name of the first CSV file from the ZIP
csv_file_name <- zip_files$Name |>    
  str_subset(  format(effectdate,"%b")) 

# Read the CSV file dynamically
data <- read.csv(unz("mhsds.zip", csv_file_name))

MHPerformance <- data |> 
  mutate(Effective_Snapshot_Date = effectdate ) |> 
  filter(MEASURE_ID == "MHS26")


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Mental_Health_Performance")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Mental_Health_Performance")) %>% 
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
  
  if(Loaded_Date == effectdate) {
    print(paste("Not Loading Mental Health Performance Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Mental_Health_Performance"), MHPerformance, append = TRUE)
    print(paste("Loaded Mental Health Performance Data for ", effectdate))
  }
}

Load()


