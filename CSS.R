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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults")

#Reading the HTML code from the website
webpage <- read_html(url)


#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  slice(1) |> 
  pull()


url2 <- paste0("https://digital.nhs.uk",web_data,"/datasets")

#Reading the HTML code from the website
webpage2 <- read_html(url2)





link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.zip") %>% # find those that end in xlsx
  .[[1]] 



download.file(link,"CSS.zip")


# List all files in the zip archive
zip_files <- unzip("CSS.zip", list = TRUE)

# Find the file that ends with "Organisation"
csv_file <- zip_files$Name  


# Read the CSV file from the zip archive
data <- read.csv(unz("CSS.zip", csv_file)) 


date <- data |> 
  select(1) |> 
  head(1) |> 
  pull()




# SQL Upload --------------------------------------------------------------


SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Community_Services_Stats_Data1")) %>% 
 ## collect() %>% 
  select(REPORTING_PERIOD_START) |> 
  group_by(REPORTING_PERIOD_START) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(REPORTING_PERIOD_START)) |> 
  head(1) |> 
  select(1) |> 
  collect() %>% 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Community_Services_Stats_Data1")) %>% 
  select(REPORTING_PERIOD_START) |> 
  group_by(REPORTING_PERIOD_START) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(REPORTING_PERIOD_START)) |> 
  head(1) |> 
  select(2) |> 
  collect() %>%   
  pull()

Load <- function() {
  
  if(Loaded_Date == date) {
    print(paste("Not Loading CSS Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Community_Services_Stats_Data1"), data, append = TRUE)
    print(paste("Loaded CSS Data for ", date))
  }
}
                 
Load()
