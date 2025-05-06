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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/data-on-written-complaints-in-the-nhs")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data_html <- html_nodes(webpage,'.cta__button')

#Find the latest publication 
web_data <- xml_attrs(web_data_html[[1]]) %>% 
  data.frame() %>% 
  head(1) %>% 
  pull()

url2 <- paste0("https://digital.nhs.uk",web_data)

#Reading the HTML code from the website
webpage2 <- read_html(url2)

link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") # find those that end in xlsx



#Download File
download.file(link,"complaints.zip")

# List the files in the ZIP archive
zip_files <- unzip("complaints.zip", list = TRUE)

# Extract the name of the first CSV file from the ZIP
csv_file_name <- zip_files$Name |> str_subset("Site")  # Assuming you want the first file

# Read the CSV file dynamically
data <- read.csv(unz("complaints.zip", csv_file_name))



#Read file as normal, here I am getting the monthly nation absence rates for acutes 
Complaints <- data |> 
  pivot_longer(cols = 6:83) |> 
  mutate(Measure_Value_Str = as.character(value),
         DataSourceFileForThisSnapshot_Version = "",
         Effective_Snapshot_Date = as.Date(paste("20", substr(Year, 6,9),"-03-31",sep = "")),
         Report_Period_Length = "Anual",
         Unique_ID = "",
         AuditKey = "") |> 
  select(1:2,11:13,15,14,16:18,4,10 )

Year = Complaints |> 
  select(Effective_Snapshot_Date) |> 
  head(1) |> 
  pull()


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_NHS_Complaints_Written_Complaints_In_NHS1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_NHS_Complaints_Written_Complaints_In_NHS1")) %>% 
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
  
  if(Loaded_Date == Year) {
    print(paste("Not Loading Complaints Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_NHS_Complaints_Written_Complaints_In_NHS1"), Complaints, append = TRUE)
    print(paste("Loaded Complaints Data for ", Year))
  }
}

Load()


