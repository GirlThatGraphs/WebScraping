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
web_data_html2 <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/nhs-workforce-statistics/") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  mutate(monthlong = str_remove(link,"/data-and-information/publications/statistical/nhs-workforce-statistics/")) |> 
  mutate(month = substr(monthlong,1,3)) |> 
  mutate(monthdate = as.Date(paste0("01-",monthlong), "%d-%B-%Y")) 


web_data_html <- web_data_html2 |> 
  filter(monthdate <= web_data_html2$monthdate[1]) |> 
  filter(month %in% c("jun", "sep", "dec", "mar")) |> 
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
  str_subset("\\.xlsx") %>% # find those that end in xlsx
  str_subset("Post") %>% # find those that end in xlsx
  .[[1]]     



#Download File
destfile <- "workforce.xlsx"
curl::curl_download(link, destfile)

Workforce <- read_excel(destfile, sheet = "Source - Org, SG, grade, AoW") |> 
  mutate(DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Snapshot Quarterly",
         Unique_ID = "",
         AuditKey = "") |> 
  select(2,6,8:12,1,13:16)

thismonth <- Workforce |> 
  select(Month) |> 
  head(1) |> 
  pull()


# SQL Upload --------------------------------------------------------------


SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_NHS_Workforce_HCHS_Staff_Trusts_And_Commissioners1")) %>% 
 ## collect() %>% 
  select(Month) |> 
  group_by(Month) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Month)) |> 
  head(1) |> 
  select(1) |> 
  collect() %>% 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_NHS_Workforce_HCHS_Staff_Trusts_And_Commissioners1")) %>% 
  select(Month) |> 
  group_by(Month) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Month)) |> 
  head(1) |> 
  select(2) |> 
  collect() %>%   
  pull()

Load <- function() {
  
  if(Loaded_Date == thismonth) {
    print(paste("Not Loading Workforce Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_NHS_Workforce_HCHS_Staff_Trusts_And_Commissioners1"), Workforce, append = TRUE)
    print(paste("Loaded Workforce Data for ", thismonth))
  }
}
                 
Load()
