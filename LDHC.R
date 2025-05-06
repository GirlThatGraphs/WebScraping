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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/learning-disabilities-health-check-scheme")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/learning-disabilities-health-check-scheme") |> 
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
  str_subset("\\.csv") 



#Download File
destfile <- "ldhc.csv"
curl::curl_download(link, destfile)

LDHC <- read.csv("ldhc.csv") |> 
  mutate(ACH_DATE = as.Date(as.character(ACH_DATE), "%Y%m%d"),
         Data_Quality_Flag = "",
         Measure_Value_Str = as.character(VALUE),
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Annual",
         Unique_ID = "",
         AuditKey = "",
         Measure_Collection = "") |> 
  select(1:2,16,18:19,15,21:24,17,20)

month <- LDHC |> 
  select(ACH_DATE) |> 
  head(1) |> 
  pull()


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Learning_Disabilities_Health_Check_Scheme_Data_Monthly1")) %>% 
  collect() %>% 
  select(ACH_DATE) |> 
  group_by(ACH_DATE) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(ACH_DATE)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Learning_Disabilities_Health_Check_Scheme_Data_Monthly1")) %>% 
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
    print(paste("Not Loading LD Health Check Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Learning_Disabilities_Health_Check_Scheme_Data_Monthly1"), LDHC, append = TRUE)
    print(paste("Loaded LD Health Check Data for ", month))
  }
}
                 
Load()
