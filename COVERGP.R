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
  str_subset("GP-data|GP_data") |> 
  head(1) 

Q <- str_sub(link, str_locate(link, "Q")[1]+1,str_locate(link, "Q")[1]+1)
Y <- as.numeric(str_sub(link, str_locate(link, "_20")[1]+1,str_locate(link, "_20")[1]+4))
X <- Y+1

quarter <- case_when(Q==1 ~ paste0("01-04-",Y),
                   Q==2 ~ paste0("01-07-",Y),
                   Q==3 ~ paste0("01-10-",Y),
                   Q==4 ~ paste0("01-01-",X))

## quarter  <- as.Date(paste0("01-",str_sub(link, str_locate(link, "to-")[1]+3,str_locate(link, "ods")[1]-2)),"%d-%B-%Y")

  
  
#Download File
destfile <- "covergp.ods"
curl::curl_download(link, destfile)

covergp <- read_ods("covergp.ods", sheet = "Table_1", skip=5) 

colnames(covergp) <- c("ICB code",
                       "ICB name",
                       "Local authority code",
                       "GP code",
                       "12m Denominator",
                       "12mDTaPIPVHibHepB%",
                       "12m MenB%",
                       "12m PCV1%",
                       "12m Rota%",
                       "24m Denominator",
                       "24m DTaP/IPV/Hib(Hep) %",
                       "24m MMR1%",
                       "24m Hib/MenC%",
                       "24m PCV Booster%",
                       "24m MenB Booster%",
                       "5y Denominator",
                       "5y DTaP/IPV/Hib%",
                       "5y Hib/MenC%",
                       "5y DTaPIPV%",
                       "5y MMR1%",
                       "5y MMR2%")

covergp <- covergp |> 
  filter(grepl("^[0-9.]+$", `12m Denominator`)) |> 
  filter(grepl("^[0-9.]+$", `24m Denominator`)) |> 
  filter(grepl("^[0-9.]+$", `5y Denominator`)) |> 
  filter(`12m Denominator` > 0) |> 
  filter(`24m Denominator` > 0) |> 
  filter(`5y Denominator` > 0) |> 
  mutate(`12m Denominator` = as.numeric(gsub("[^0-9.-]", "", `12m Denominator`)),
         `24m Denominator` = as.numeric(gsub("[^0-9.-]", "", `24m Denominator`)),
         `5y Denominator` = as.numeric(gsub("[^0-9.-]", "", `5y Denominator`)),
         `12m MenB%` = as.numeric(gsub("[^0-9.-]", "", `12m MenB%`)),
         `12m PCV1%` = as.numeric(gsub("[^0-9.-]", "", `12m PCV1%`)),
         `12m Rota%` = as.numeric(gsub("[^0-9.-]", "", `12m Rota%`)),
         `24m DTaP/IPV/Hib(Hep) %` = as.numeric(gsub("[^0-9.-]", "", `24m DTaP/IPV/Hib(Hep) %`)),
         `24m MMR1%` = as.numeric(gsub("[^0-9.-]", "", `24m MMR1%`)),
         `24m Hib/MenC%` = as.numeric(gsub("[^0-9.-]", "", `24m Hib/MenC%`)),
         `24m PCV Booster%` = as.numeric(gsub("[^0-9.-]", "", `24m PCV Booster%`)),
         `24m MenB Booster%` = as.numeric(gsub("[^0-9.-]", "", `24m MenB Booster%`)),
         `5y DTaP/IPV/Hib%` = as.numeric(gsub("[^0-9.-]", "", `5y DTaP/IPV/Hib%`)),
         `5y Hib/MenC%` = as.numeric(gsub("[^0-9.-]", "", `5y Hib/MenC%`)),
         `5y DTaPIPV%` = as.numeric(gsub("[^0-9.-]", "", `5y DTaPIPV%`)),
         `5y MMR1%` = as.numeric(gsub("[^0-9.-]", "", `5y MMR1%`)),
         `5y MMR2%` = as.numeric(gsub("[^0-9.-]", "", `5y MMR2%`)),
         `12mDTaPIPVHibHepB%` = as.numeric(gsub("[^0-9.-]", "", `12mDTaPIPVHibHepB%`))) |> 
  pivot_longer(5:21) |> 
  select(1,4:6,2) |> 
  mutate(Measure_Value_Str = as.character(value),
         Effective_Snapshot_Date = quarter,
         DataSourceFileForThisSnapshot_Version = "",
         Report_Period_Length = "Quarterly",
         Unique_ID = "",
         AuditKey = "") |> 
  select(1:4,6:11,5)
  


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)


Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_COVER_GP_Practice1")) %>% 
  collect() %>% 
  select(Effective_Snapshot_Date) |> 
  group_by(Effective_Snapshot_Date) |> 
  summarise(Count = n()) |> 
  ungroup() |> 
  arrange(desc(Effective_Snapshot_Date)) |> 
  head(1) |> 
  select(1) |> 
  pull()

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_COVER_GP_Practice1")) %>% 
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
  
  if(Loaded_Date == quarter) {
    print(paste("Not Loading COVER GP Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_COVER_GP_Practice1"), covergp, append = TRUE)
    print(paste("Loaded COVER GP Data for ", quarter))
  }
}
                 
Load()
