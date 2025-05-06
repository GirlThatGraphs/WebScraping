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
library(readODS)

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)



#Specifying the url for desired website to be scraped
url <- paste("https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans")

#Reading the HTML code from the website
webpage <- read_html(url)

link <-
  webpage %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("content.explore-education-statistics") |> 
  head(1) 


download.file(link,"SEND.zip", mode = "wb")

zip_file <- "SEND.zip"  # Replace with your zip file path
unzip(zip_file)  # Unzip the file

csv_files <- list.files(file.path("data"), pattern = "*.csv", full.names = TRUE)

# Loop over each CSV file
for (csv_file in csv_files) {
  # Extract the base name (without the folder path and extension)
  file_name <- tools::file_path_sans_ext(basename(csv_file))
  
  # Read the CSV file
  data <- read.csv(csv_file)
  
  max_time <- max(data$time_period, na.rm = TRUE)
  data_filtered <- data[data$time_period == max_time, ]
  
  # Assign the filtered data frame to a variable
  assign(file_name, data_filtered)
  
  Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo",paste0("zzz_",file_name))) %>% 
    collect() %>% 
    arrange(desc(time_period)) |> 
    head(1) |> 
    select(1) |> 
    pull()
  
  if(Loaded_Date == max_time) {
    print(paste("Not Loading ",file_name," for ", Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = paste0("zzz_",file_name)), file_name, append = TRUE)
    print(paste("Loaded ",file_name," for ", date))
  }
  }