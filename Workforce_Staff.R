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
  head(1) |> 
  select(monthlong) |> 
  pull()

url2 <- paste0(url, "/", web_data_html)

#Reading the HTML code from the website
webpage2 <- read_html(url2)


link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.zip") %>% # find those that end in xlsx
  str_subset("csv") %>% # find those that end in xlsx
  .[[1]]     


download.file(link,"Workforce_Staff.zip")

# List all files in the zip archive
zip_files <- unzip("Workforce_Staff.zip", list = TRUE)

# Find the file that ends with "Organisation"
csv_file <- zip_files$Name |> 
  str_subset("Organisation")  
  

# Read the CSV file from the zip archive
data <- read.csv(unz("Workforce_Staff.zip", csv_file)) 

  
Workforce_Staff <- data |> 
  filter (Date == max(data$Date)) |> 
  select(6,8:13,1) |> 
  mutate(DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Snapshot Monthly",
         Unique_ID = "",
         AuditKey = "") 



thismonth <- Workforce_Staff |> 
  select(Date) |> 
  head(1) |> 
  pull()


# SQL Upload --------------------------------------------------------------


SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Staff_Group_And_Organisation1")) %>% 
 ## collect() %>% 
  select(Date) |> 
  group_by(Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Date)) |> 
  head(1) |> 
  select(1) |> 
  collect() %>% 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Staff_Group_And_Organisation1")) %>% 
  select(Date) |> 
  group_by(Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Date)) |> 
  head(1) |> 
  select(2) |> 
  collect() %>%   
  pull()

Load <- function() {
  
  if(Loaded_Date == thismonth) {
    print(paste("Not Loading Workforce Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Staff_Group_And_Organisation1"), Workforce_Staff, append = TRUE)
    print(paste("Loaded Workforce Data for ", thismonth))
  }
}
                 
Load()
