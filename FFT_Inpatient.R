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
url <- paste("https://www.england.nhs.uk/fft/friends-and-family-test-data/")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a')|> 
  html_attr("href") |> 
  str_subset("publication/friends-and-family-test-data")%>% 
  data.frame() %>% 
  head(1) %>% 
  pull()

url2 <- (web_data)

#Reading the HTML code from the website
webpage2 <- read_html(url2)

thismonth <- sub("https://www.england.nhs.uk/publication/friends-and-family-test-data-","",url2) %>%
  sub("https://www.england.nhs.uk/friends-and-family-test-data-","",.) %>%
  gsub("/#heading-1","",.)  %>% 
  gsub("/#header-1","",.)  %>%
  paste("01-",., sep="") |> 
  as.Date("%d-%B-%Y")

link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xlsm") %>% # find those that end in xlsx
  str_subset("fft-i|inpatient|FFT-i") %>% # find those that end in xlsx
  .[[1]]     



#Download File
destfile <- "Inpatient.xlsm"
curl::curl_download(link, destfile)

FFTIn <- read_excel(destfile, sheet = "Trusts", skip = 9) |> 
  drop_na(3) |> 
  set_colnames(c("Area_Team_Code","Trust_Code","Trust_Name","Total Responses","Total Eligible","Percentage Positive","Percentage Negative",
                 "Very Good","Good","Neither Good nor Poor","Poor","Very Poor","Don't Know",
                 "Mode SMS","Mode Electronic Discharge","Mode Electronic Home","Mode Paper Discharge","Mode Paper Home","Mode Telephone","Mode Online","Mode Other")) |> 
  mutate(Trust_Code = case_when(Trust_Name == "England (including Independent Sector Providers)" ~ "ENG - including Independent Sector Providers",
                                    Trust_Name == "England (excluding Independent Sector Providers)" ~ "ENG - excluding Independent Sector Providers",
                                    Trust_Name == "Selection (excluding suppressed data)" ~ "ENG - excluding suppressed data",
                                    TRUE ~ Trust_Code)) |> 
  select(-Trust_Name,-Area_Team_Code) |> 
  pivot_longer(2:19,values_transform = list(value = as.numeric)) |>
  rename(Measure_Value = value,
         Measure = name) |> 
  mutate(Measure_Category = case_when(Measure == "Mode Telephone" ~ "Mode of Collection",
                                      Measure == "Mode Online" ~ "Mode of Collection",
                                      Measure == "Mode Other" ~ "Mode of Collection",
                                      Measure == "Mode SMS" ~ "Mode of Collection",
                                      Measure == "Mode Electronic Discharge" ~ "Mode of Collection",
                                      Measure == "Mode Paper Discharge" ~ "Mode of Collection",
                                      Measure == "Mode Electronic Home" ~ "Mode of Collection",
                                      Measure == "Mode Paper Home" ~ "Mode of Collection",
                                      Measure == "Very Good" ~ "Breakdown of Responses",
                                      Measure == "Good" ~ "Breakdown of Responses",
                                      Measure == "Neither Good nor Poor" ~ "Breakdown of Responses",
                                      Measure == "Poor" ~ "Breakdown of Responses",
                                      Measure == "Very Poor" ~ "Breakdown of Responses",
                                      Measure == "Don't Know" ~ "Breakdown of Responses",
                                      TRUE ~ Measure)) |> 
  mutate(Effective_Snapshot_Date =  thismonth) |> 
  mutate(Measure_Value_Str = as.character(Measure_Value),
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Monthly",
         Unique_ID = "",
         AuditKey = "") |> 
  select(1,4,2,3,6,5:10)




# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_FriendsAndFamilyTest_Inpatients_Trusts1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_FriendsAndFamilyTest_Inpatients_Trusts1")) %>% 
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
  
  if(Loaded_Date == thismonth) {
    print(paste("Not Loading FFT Inpatient Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
    } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_FriendsAndFamilyTest_Inpatients_Trusts1"), FFTIn, append = TRUE)
    print(paste("Loaded FFT Inpatient Data for ", thismonth))
    }
}
                 
Load()
