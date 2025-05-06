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
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/national-diabetes-audit")


#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the publications section
web_data <- html_nodes(webpage,'a') |> 
  html_attr("href") |> 
  str_subset("data-and-information/publications/statistical/national-diabetes-audit") |> 
  str_subset("quarterly") |>  
  str_subset("core-q") |> 
  data.frame() |> 
  `colnames<-`(c("link")) |> 
  slice(1) |> 
  pull()

url2 <- paste0("https://digital.nhs.uk",web_data)

webpage2 <- read_html(url2)

link<-
  webpage2 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xlsx") %>% # find those that end in xlsx
  .[[1]] 




#Download File
destfile <- "NDA.xlsx"
curl::curl_download(link, destfile)

date <- read_excel("NDA.xlsx", sheet = "Title sheet", 
                          range = "A4:A4", col_names = FALSE) |> 
  pull() |> 
  str_extract("(?<=to ).*") %>%
  paste("01", .) |> 
  as.Date("%d %B %Y")
  

# Read the rest of the data
data <- read_excel("NDA.xlsx", sheet = "Type 2 and other CP_TT", skip = 2)

header1 = c("", "", "", "", 
           rep("HbA1c",3),
           rep("Blood Pressure",3),
           rep("Cholesterol",3),
           rep("Serum Creatinine",3),
           rep("Urine Albumin",3),
           rep("Foot Surveillance",3),
           rep("BMI",3),
           rep("Smoking",3),
           rep("All Eight Care Processes",3),
           rep("HbA1c <= 48 mmol/mol (6.5%)",3),
           rep("HbA1c <= 53 mmol/mol (7.0%)",3),
           rep("HbA1c <= 58 mmol/mol (7.5%)",3),
           rep("HbA1c <= 75 mmol/mol (9.0%)",3),
           rep("HbA1c <= 86 mmol/mol (10.0%)",3),
           rep("Blood Pressure <= 140/80 old",3),
           rep("Blood Pressure <= 140/90",3),
           rep("Primary Prevention - On Statins without CVD History ",3),
           rep("Secondary Prevention - On Statins with CVD History",3),
           rep("Combined Prevention - On Statins",3),
           rep("All Three Treatment Targets old",3),
           rep("All Three Treatment Targets",3)
           )

header2 <- gsub("\\.\\.\\.[0-9]+$", "", colnames(data))


# Now combine them safely
combined_header <- paste0(trimws(header1), "_", trimws(header2))

colnames(data) <- combined_header



# First, identify ID columns (non-metrics)
id_cols <- grep("^(_Audit year|_ICB Code|_PCN Code|_Organisation code)", colnames(data), value = TRUE)

# Pivot to long format
long_data <- data %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = c("Measure", "Metric"),
    names_sep = "_",
    values_to = "Value"
  ) |> 
  mutate(Effective_Snapshot_Date = date)


# SQL Upload --------------------------------------------------------------

SQL_CONNECTION <- dbConnect(odbc(), 
                            driver = "SQL Server",
                            server = Sys.getenv("SQL_SERVER"),
                            database = Sys.getenv("SQL_DATABASE"),
                            trustedconnection = TRUE)

Loaded_Date <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Nat_Diabetes_Type_2_Other_CP_TT_England1")) %>% 
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

Loaded_No <- tbl(SQL_CONNECTION, in_schema("dbo","zzz_Nat_Diabetes_Type_2_Other_CP_TT_England1")) %>% 
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
  
  if(Loaded_Date == date) {
    print(paste("Not Loading NDA Data:",Loaded_No,"records for" , Loaded_Date, "already loaded"))
    beep(9)
  } else {
    dbWriteTable(SQL_CONNECTION, Id(schema = "dbo", table = "zzz_Nat_Diabetes_Type_2_Other_CP_TT_England1"), long_data, append = TRUE)
    print(paste("Loaded NDA ", date))
  }
}
                 
Load()
