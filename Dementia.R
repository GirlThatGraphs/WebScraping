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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/primary-care-dementia-data")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/primary-care-dementia-data") |> 
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
  str_subset("ass-plans") # find those that end in xlsx  

download.file(link,"dem.zip")

# List all files in the zip archive
zip_files <- unzip("dem.zip", list = TRUE)


# If there are multiple files, you can loop over them and read them into a list
achievement_files <- zip_files$Name

# Read all the matching files into a list
data_list <- lapply(achievement_files, function(file) {
  read.csv(unz("dem.zip", file))
})


combined_data <- do.call(rbind, data_list) |> 
  mutate(ACH_DATE = as.Date(ACH_DATE, "%d-%b-%y")) 
 
month <-  max(combined_data$ACH_DATE)

combined_data <- combined_data |> 
  filter(ACH_DATE == month)


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Rec_Dementia_DiagBy_Age_Care_Plans_Refs_Assessments1")) %>% 
  collect() %>% 
  select(ACH_DATE) |> 
  group_by(ACH_DATE) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(ACH_DATE)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Rec_Dementia_DiagBy_Age_Care_Plans_Refs_Assessments1")) %>% 
  collect() %>% 
  select(ACH_DATE) |> 
  group_by(ACH_DATE) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(ACH_DATE)) |> 
  head(1) |> 
  select(2) |> 
  pull()

Load <- function() {
  
  if(Loaded_Date == month) {
    print(paste("Not Loading Dementia Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Rec_Dementia_DiagBy_Age_Care_Plans_Refs_Assessments1"), combined_data, append = TRUE)
    print(paste("Loaded Dementia Data for ", month))
  }
}
                 
Load()
