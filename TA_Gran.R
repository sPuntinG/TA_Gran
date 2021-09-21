# R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"

# Check your current R version ----------------------------------------------------------------------------------------
R.version.string # if want to update from RStudio: "Help" > "Check for Updates" etc.


# FILL UP these and then you're READY TO GO -----------------------------------------------------------------------

# Folder with all the titration output files
datadir <- "C:/Users/Dell/Documents/Germany/Work/Projects/Galaxea/Thermal Performance Curves experiment/TA stuff/TPC_Titrations/20191216/CSV/"                
setwd(datadir)

# csv File that contains Sample_ID and Ws (weight of the sample, in g): you need to make it yourself
SIDWSdir <- "C:/Users/Dell/Documents/Germany/Work/Projects/Galaxea/Thermal Performance Curves experiment/TA stuff/TPC_Titrations/20191216/SampleID_Ws_Sal.csv"

# Separator in your csv file (you need to manually check this)
Sep <- ";"

# Where you want to save the results (csv) and how to name it
writedr <- "C:/Users/Dell/Documents/Germany/Work/Projects/Galaxea/Thermal Performance Curves experiment/TA stuff/TPC_Titrations/20191216\\TA_results_20191216.csv"

# Titrant (HCl) normality
NHCl <- 0.1

# Only if you want to subsample your SampleID_Ws dataset 
Titration_date <- "16.12.2019"  # In my case I wanted to use only values for this day of titrations
# still need to figure out how to deal with it if no need to filter ....!!!

# TROUBLESHOOTING --------------------------------------------------------------------------------------------

# If you're getting an error it's very likely that there were:

# TYPOS in your Sample_ID somewhere:
# refer to section "Create final tibble, ready for batch TA calculations - Part 1: check for typos"

# csv might not use this ";" as SEPARATOR. It might also be that only some csv files 
# use a different the separator (it happened to me) so check that

# PARSING of date time data: check the format of your data (e.g. dmy_hm vs dmy_hms) and adjust accordingly
# see paragraph "From datetime to filename format"


# Any suggested improvement is welcome :)



# Load packages needed -------------------------------------------------------------------------------

# Automatically install and load the packages required
if (!require("pacman")) install.packages("pacman")
pacman::p_load("oce", "tidyverse", "lubridate")   

# Import SampleID_Ws ----------------------------------------------------------------------------------------

# For my incubation I created a single csv with all the samples and Ws, so I want to reduce it to keep only 
# the samples that have a correspondnent in the CSV folder I'm using 
# e.g. 20191208 I want only the samples that were titrated on the 8th Dec 2019

SampleID_Ws_Sal_raw <- read_csv(SIDWSdir)

SampleIDWS <- SampleID_Ws_Sal_raw %>%
  # filter(Titr_date == "08.12.2019") %>%
  filter(Titr_date == Titration_date) %>%
  select(Sample_ID, Ws, Sal)
  
rm(SampleID_Ws_Sal_raw)   


# Import csv files (titrations' output files) ---------------------------------------------------------------------------------------------

listof = list.files(pattern = "*.csv")    # 62 files (CAl and summary + 60 titrations)
toberemoved = listof[(nchar(listof) < max(nchar(listof))) ] # find files that have shorter name (e.g. CAL and the summay one)
listoff = listof [! listof %in% toberemoved]                # now it has 60 files (also matched with nrows in SampleID_Ws)

# Import in a single tibble
read_flnm <- function(flnm) {
  read_delim(flnm, delim = Sep, locale = readr::locale(encoding = "latin1")) %>% 
    mutate(Filename = flnm)
}

Flnm_all <- listoff %>% map_df(~read_flnm(.)) 

# Format in a more convenient way
Flnm_all <- Flnm_all %>%
  mutate(Filename = str_replace(Filename, "-", "_")) %>%          # 'cause R sees "-" as an operator ...
  mutate(Filename = str_replace(Filename, "_\\d\\d.csv", "")) %>% # Remove "_XX.csv" from string (Filename)
  rename(Temp = `?C`)                                             # Get rid of this PITA character type ...



# Work on the SUMMARY file -------------------------------------------------------------------------------------------------

# Select (and then read) only summary file based on (its) name length 
keepsummonly <- max(nchar(listof)) - 18 # the titrator always names output files from titrations as
keepsummonly                            # "Method_name" + "dd_mm_yy-hh_mm_ss.csv" (X + 18)
# This way we will always select the summary file, regardless of the length of the method name 

summaryfile = listof[nchar(listof) == keepsummonly]
Summary_0 <- read_delim(summaryfile, delim = Sep, col_names = FALSE, skip = 1) # Delim should be ";" (but once was "," ... )
# (with this specif.s no need to change the locale for characters as it skipts the painful row)

rm(datadir, listof, summaryfile, toberemoved, keepsummonly)  

## Create tibble "Summary" with only the stuff that we need, in the right format

# Function to correct nchar of datetime parts (e.g. "1" becomes "01" -> keep same nr of char)
addzero <- function(vrbl) {
  ifelse((nchar(vrbl) == 1), paste("0", vrbl, sep = ""), vrbl)
}

# From datetime to filename format (to match with Filename in Flnm_all) ...
Summary <- Summary_0 %>%
  select(X2, X4, X6) %>%  #These I know are the ones that I need (I previously checked)
  rename(Methodname = X2,
         Sample_ID = X4,
         Timetitr_end = X6) %>%
  mutate(
    Timetitr_end = as_datetime(mdy_hms(Timetitr_end)),  # <- HERE CHECK if mdy_hms is the format of your data
    Day = day(Timetitr_end),
    Month = month(Timetitr_end),
    Year = year(Timetitr_end),
    Hour = hour(Timetitr_end),
    Mins = minute(Timetitr_end)) %>%
  mutate_at(c("Day", "Month", "Hour", "Mins"), addzero) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Year = str_replace(Year, "^[20]*", "")) 

# Arrange datetime components to match the Filename format in Flnm_all
Summary <- Summary %>%
  mutate(Filename = paste(Methodname, Day, Month, Year, Hour, Mins, sep = "_")) %>%
  select(Sample_ID, Filename) %>% 
  mutate(Filename = str_replace(Filename, "0.1N", "0_1N"))

rm(Summary_0)



# Create final tibble, ready for batch TA calculations - Part 1: check for typos ---------------------------------------------------------------------

# Check if Sample_ID in Summary and DampleID_Ws are the same (make sure youuse exaclty same format when manually inputting data in SampleID_Ws!)
Nr_matchingSIDs <- length(intersect(SampleIDWS$Sample_ID, Summary$Sample_ID)) 
Nr_Titrations <- length(listoff)

# warning() doesn't stop execution  
ifelse(Nr_matchingSIDs - Nr_Titrations == 0, "All good, can continue", stop("*** Something is wrong with Sample_ID format! ***\n -> use MISMATCH to check", call. = F ))

# show what's not matching
In_SampleIDWS <- setdiff(SampleIDWS$Sample_ID, Summary$Sample_ID)
In_Summary <- setdiff(Summary$Sample_ID, SampleIDWS$Sample_ID)

MISMATCH <- tibble(In_SampleIDWS, In_Summary)  
MISMATCH  

# Checked on notes: B_53_40_d7 was accidentally saved as d6 -> need to correct summary
 Summary <- Summary %>%
   mutate(Sample_ID = recode(Sample_ID, "B_53_40_d6" = "B_53_40_d7"))

# Check again
Nr_matchingSIDs <- length(intersect(SampleIDWS$Sample_ID, Summary$Sample_ID)) 
Nr_Titrations <- length(listoff)

ifelse(Nr_matchingSIDs - Nr_Titrations == 0, "All good, can continue", stop("*** Something is wrong with Sample_ID format! ***\n -> use MISMATCH to check", call. = F ))

# REPEAT this section UNTIL you get the OK to continue

rm(Nr_matchingSIDs, Nr_Titrations, listoff, In_SampleIDWS, In_Summary, MISMATCH)


# Create final tibble, ready for batch TA calculations - Part 2: assemble ---------------------------------------------------------------------

# Associate Sample_ID to Filename in new tibble (Titr_Flnm_SID) 
Titr_Flnm_SID <- left_join(Flnm_all, Summary, by = ("Filename"))

# Now also add Ws (from SampleID_Ws) in a new tibble (Titr_Flnm_SID_Ws)
Titr_Flnm_SIDWS <- left_join(Titr_Flnm_SID, SampleIDWS, by = ("Sample_ID")) # CHECK that Sample_ID are in the same format!

rm(Titr_Flnm_SID)

# Split by group (Filename)
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

Titr_Flnm_SIDWS_List <- Titr_Flnm_SIDWS %>% 
  named_group_split(Filename)

rm(Flnm_all, Summary, SampleIDWS, Titr_Flnm_SIDWS)


# TA_Gran function ------------------------------------------------------------------------------------------

TA_Gran <- function(., N = NHCl, Ws = 50, pH, Vt, S = 35, Temp = 25) { 
  
  options(digits = 4)
  swd <- swRho(salinity = S, temperature = Temp, pressure = 10.13253)/1000
  Vs <- Ws * swd
  p <- data.frame(pH = pH, Vt = Vt)
  z <- p
  iii <- which((3.5 <= p$pH) & (p$pH <= 4.5))
  z <- p[iii,]
  Vtt <- z$Vt
  z <- z$pH   
  F1 <- (Vs + Vtt)*10^-z
  f <- lm(Vtt ~ F1)
  Ve <- coef(f)[1]
  TA <- Ve * 1000 * N / Vs
  return(TA)
  attributes(TA) <- NULL
  attr(TA, "unit") <- "mmol/L"
  attr(TA, "name") <- "Total Alkalinity from Gran approximation"
  
}



# Calculate TA and write results into csv ---------------------------------------------------------------------- 

# (This part is a bit verbose, but it works)

# TA
TA_mmol <- vector("double", length(Titr_Flnm_SIDWS_List))
for(i in seq_along(Titr_Flnm_SIDWS_List)) {
  TA_mmol[[i]] <- TA_Gran(Titr_Flnm_SIDWS_List[[i]], 
                          pH = Titr_Flnm_SIDWS_List[[i]]$pH, 
                          Vt = Titr_Flnm_SIDWS_List[[i]]$ml,
                          Ws = Titr_Flnm_SIDWS_List[[i]]$Ws[1],
                          Temp = mean(Titr_Flnm_SIDWS_List[[i]]$Temp),
                          S = ifelse(is.na(Titr_Flnm_SIDWS_List[[i]]$Sal[1]), "35", Titr_Flnm_SIDWS_List[[i]]$Sal[1]))
}
TA_mmol                    # NOTE: default Salinity is always 35 (either if you don't provide it or if it's NA)

# Sample_ID
Sample_ID <- vector("character", length(Titr_Flnm_SIDWS_List))
for(i in seq_along(Titr_Flnm_SIDWS_List)) {
  Sample_ID[[i]] <- first(Titr_Flnm_SIDWS_List[[i]]$Sample_ID)
}
Sample_ID

# Filename
# This is not super necessary, but just in case to double check
Filename <- vector("character", length(Titr_Flnm_SIDWS_List))
for(i in seq_along(Titr_Flnm_SIDWS_List)) {
  Filename[[i]] <- first(Titr_Flnm_SIDWS_List[[i]]$Filename)
}
Filename 

# Salinity
S <- vector("double", length(Titr_Flnm_SIDWS_List))
for(i in seq_along(Titr_Flnm_SIDWS_List)) {
  S[[i]] <- first(Titr_Flnm_SIDWS_List[[i]]$Sal)
}
S

# Create summary tibble
TA_results <- tibble(Sample_ID, TA_mmol, Filename, N = NHCl, S)
View(TA_results)

# Write csv
write.csv(TA_results, writedr, row.names = TRUE)
