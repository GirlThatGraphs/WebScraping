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


#Specifying the url for desired website to be scraped
url <- paste("https://www.gov.uk/government/collections/vaccine-uptake")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("cover-programme") |>
  str_subset("quarterly") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  slice(1) |> 
  pull()

url2 <- paste0("https://www.gov.uk",web_data)

#Reading the HTML code from the website
webpage2 <- read_html(url2)

link <-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.ods") |> 
  str_subset("cover-quarter|cover-data-tables") |> 
  head(1) 

  
#Download File
destfile <- "coverla.ods"
curl::curl_download(link, destfile)


quarter  <- as.numeric(str_sub(link, str_locate(link, "quarter")[1]+8,str_locate(link, "quarter")[1]+8) ) 
year <- as.numeric(str_sub(link, str_locate(link, "quarter")[1]+10,str_locate(link, "quarter")[1]+13))  
date <- case_when(quarter == 4 ~ as.Date(paste0((year+1), "-03-31" )),
                  quarter == 1 ~ as.Date(paste0((year), "-06-30" )),
                  quarter == 2 ~ as.Date(paste0((year), "-09-30" )),
                  quarter == 3 ~ as.Date(paste0((year), "-12-31" ))
                  )


coverla <- read_ods("coverla.ods", sheet = "Table10", skip = 5) |> 
  pivot_longer(10:42) |>   
  mutate(Quarter = quarter) |> 
  select(2,4,6,8, 15,9,13,14) |> 
  mutate(Effective_Snapshot_Date = date,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Quarterly",
         Unique_ID = "",
         AuditKey = "",
         value = as.numeric(value),
         Metric_Value_Str = value)
  


# SQL Upload --------------------------------------------------------------




SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server ="PSFADHSSTP02.ad.elc.nhs.uk\\SWL",
                            database ="Data_Lab_SWL_RawDataModel",
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_COVER_Local_Authority1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_COVER_Local_Authority1")) %>% 
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
  
  if(Loaded_Date == date) {
    print(paste("Not Loading COVER LA Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_COVER_Local_Authority1"), coverla, append = TRUE)
    print(paste("Loaded COVER La Data for ", date))
  }
}
                 
Load()
