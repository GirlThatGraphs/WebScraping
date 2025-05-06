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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/cervical-screening-programme/cervical-screening-programme-coverage-statistics-management-information")

#Reading the HTML code from the website
webpage2 <- read_html(url)

link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.zip") # find those that end in xlsx  

download.file(link,"cervical.zip")

# List all files in the zip archive
zip_files <- unzip("cervical.zip", list = TRUE)


# If there are multiple files, you can loop over them and read them into a list
achievement_files <- zip_files$Name |> 
  str_subset("gp")

# Read all the matching files into a list
data_list <- lapply(achievement_files, function(file) {
  read.csv(unz("cervical.zip", file))
})


combined_data <- do.call(rbind, data_list)


combined_data <- combined_data |> 
  filter(Month == max(combined_data$Month))


month <- combined_data |> 
  select (Month) |> 
  head(1) |> 
  pull()

combined_data[combined_data=="Suppressed"] <- NA

cervical <- combined_data |> 
  mutate(Value_str = as.character(15),
         Value_str2 = as.character(21),
         Effective_Snapshot_Date = month,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Quartely",
         Unique_ID = "",
         AuditKey = "",
         a="",
         b="",
         c="",
         d="",
         e="") |> 
  select(11,10,5,6,3,12:15,22,16,17,21,23:33)


# SQL Upload --------------------------------------------------------------


SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz__Cervical_ScreeningGP_Level_Data1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz__Cervical_ScreeningGP_Level_Data1")) %>% 
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
    print(paste("Not Loading Cervical Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz__Cervical_ScreeningGP_Level_Data1"), cervical, append = TRUE)
    print(paste("Loaded Cervical Data for ", month))
  }
}
                 
Load()
