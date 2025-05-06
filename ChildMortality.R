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
url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales")


#Reading the HTML code from the website
webpage2 <- read_html(url)


link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xlsx") %>% # find those that end in xlsx
  .[[1]] 

  
  
 link <- paste0("https://www.ons.gov.uk",link)


#Download File
destfile <- "childmortality.xlsx"
curl::curl_download(link, destfile)

year <- str_sub(link, str_locate(link, "20")[1],str_locate(link, "20")[1]+3) 
year <- as.Date(paste0(year,"-12-31"))


childmortality <- read_excel(destfile, sheet = "3", skip = 9) |> 
  select(-'Neonatal unreliable indicator',
         -'Infant unreliable indicator',
         -'Postneonatal unreliable indicator',
         -'Stillbirth unreliable indicator',
         -'Perinatal unreliable indicator') |> 
  mutate_all(str_replace_all,"[x]","") |> 
  pivot_longer(4:14) |> 
  mutate(Measure_Value_Str = value,
         Effective_Snapshot_Date = year,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Annual",
         Unique_ID = "",
         AuditKey = "") |>   
  select(1,2,4:11,3)

# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Live_Births_Still_Births_Infant_Deaths_Resid1")) %>% 
 ## collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  collect() %>% 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Live_Births_Still_Births_Infant_Deaths_Resid1")) %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(2) |> 
  collect() %>%   
  pull()

Load <- function() {
  
  if(Loaded_Date == year) {
    print(paste("Not Loading Child Mortality Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Live_Births_Still_Births_Infant_Deaths_Resid1"), childmortality, append = TRUE)
    print(paste("Loaded Child Mortality Data for ", year))
  }
}
                 
Load()
