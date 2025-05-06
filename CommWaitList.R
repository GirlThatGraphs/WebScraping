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
url <- paste("https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/")

#Reading the HTML code from the website
webpage <- read_html(url)


link<-
  webpage %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".xlsx") |>  # find those that end in xlsx
  head(1)


#Download File
destfile <- "CommWaitList.xlsx"
curl::curl_download(link, destfile)

Date <- read_excel("CommWaitList.xlsx", 
                   sheet = "Table 4", range = "A1:A1", col_names = FALSE) |> 
  pull() |> 
  str_extract("(?<=, ).*") %>%
  paste("01", .) |> 
  as.Date(format = "%d %B %Y")

WL_Total <- read_excel("CommWaitList.xlsx", 
                           sheet = "Table 4", skip = 4) |> 
  mutate(Weeks_By_Service = "Total",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_1wk <- read_excel("CommWaitList.xlsx", 
                       sheet = "Table 4a", skip = 4) |> 
  mutate(Weeks_By_Service = "0-1 weeks (0-7 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_2wk <- read_excel("CommWaitList.xlsx", 
                       sheet = "Table 4b", skip = 4) |> 
  mutate(Weeks_By_Service = ">1-2 weeks (8-14 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_4wk <- read_excel("CommWaitList.xlsx", 
                     sheet = "Table 4c", skip = 4) |> 
  mutate(Weeks_By_Service = ">2-4 weeks (15-28 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_12wk <- read_excel("CommWaitList.xlsx", 
                     sheet = "Table 4d", skip = 4) |> 
  mutate(Weeks_By_Service = ">4-12 weeks (29-84 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_18wk <- read_excel("CommWaitList.xlsx", 
                     sheet = "Table 4e", skip = 4) |> 
  mutate(Weeks_By_Service = ">12-18 weeks (85-126 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_52wk <- read_excel("CommWaitList.xlsx", 
                      sheet = "Table 4f", skip = 4) |> 
  mutate(Weeks_By_Service = ">18-52 weeks (127-364 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")

WL_1yr <- read_excel("CommWaitList.xlsx", 
                      sheet = "Table 4g", skip = 4) |> 
  mutate(Weeks_By_Service = ">52 weeks (>365 days)",
         Effective_Snapshot_Date = Date) |> 
  pivot_longer(4:50, names_to = "Measure", values_to = "Measure_Value")


CommWL <- rbind(WL_Total,
             WL_1wk,
             WL_2wk,
             WL_4wk,
             WL_12wk,
             WL_18wk,
             WL_52wk,
             WL_1yr)
             
  



# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Community_Health_Services_Waiting_Lists1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Community_Health_Services_Waiting_Lists1")) %>% 
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
  
  if(Loaded_Date == Date) {
    print(paste("Not Loading Community Wait List Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Community_Health_Services_Waiting_Lists1"), CommWL, append = TRUE)
    print(paste("Loaded Community Wait List Data for ", Date))
  }
}

Load()


