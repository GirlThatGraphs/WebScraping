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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/appointments-in-general-practice") |> 
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
  str_subset("\\.zip") |>  # find those that end in xlsx  
  str_subset("Practice_Level_Crosstab") # find those that end in xlsx  

month <- substr(link, nchar(link)-9, nchar(link)-4)
  
download.file(link,"GPAD.zip")

# List all files in the zip archive
zip_files <- unzip("GPAD.zip", list = TRUE)


# If there are multiple files, you can loop over them and read them into a list
achievement_files <- zip_files$Name |> 
  str_subset(month)

# Read all the matching files into a list
data_list <- lapply(achievement_files, function(file) {
  read.csv(unz("GPAD.zip", file))
})


combined_data <- do.call(rbind, data_list)



GPAD <- combined_data |> 
  mutate(Effective_Snapshot_Date = month,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Annual",
         Unique_ID = "",
         AuditKey = "",
         Measure_Collection = "") 

# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Appts_In_General_Practice_Prac_Level_Crosstab1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Appts_In_General_Practice_Prac_Level_Crosstab1")) %>% 
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
  
  if(Loaded_Date == month) {
    print(paste("Not Loading GPAD Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Appts_In_General_Practice_Prac_Level_Crosstab1"), GPAD, append = TRUE)
    print(paste("Loaded GPAD Data for ", month))
  }
}
                 
Load()
