## Should the below list of packages need to be installed, remove '#'(s) and run:
#libraries<-c("tidyverse", "cowplot", "magick", "readODS", "here", "plotly", "highcharter", "htmlwidgets")
#install.packages(libraries, repos="http://cran.rstudio.com")

## NECESSARY PACKAGES:

library(here)         # to create user-specific filepaths
library(readODS)      # to retrieve data from ODS files
library(tidyverse)    # for various visualisation packages
library(cowplot)      # for various visualisation options
library(magick)       # to import images onto visualisations
library(plotly)       # to create interactive scatterplots
library(highcharter)  # to create interactive barplots
library(htmlwidgets)  # to save interactive visualisations

## DATA PREPARATION:
# Extraction of raw data from ODS file

country_names<-c("England","Wales","Scotland","Northern Ireland")
pathway<-paste0(here("raw_secondary_data", "area-timeseries-20jun24.ods"))
countries_list<-list()
for (i in seq_along(country_names)){
  countries<-country_names[i]
  countries_list[[countries]]<-read_ods(pathway, sheet = i+3)
} # this loop extracts all of the raw data from the country-specific sheets and imports it into R
raw_extracted_data<-data.frame(countries_list) # which is then put into a dataframe

rm(list=setdiff(ls(), c("raw_extracted_data", "country_names"))) # removes now unnecessary variables to clean global environment

# Glimpse of the raw data

head(raw_extracted_data)

# Processing and cleaning of data

processed_extracted_data<-raw_extracted_data[-c(1:3),] # removes unnecessary text
names(processed_extracted_data)<-as.matrix(processed_extracted_data[1,]) # labels each column by their original titles
processed_extracted_data<-processed_extracted_data[-1,] # removes text names from the data
names(processed_extracted_data)<-make.unique(names(processed_extracted_data)) # makes each column unique
processed_extracted_data<-processed_extracted_data %>% select(-starts_with("Note")) # removes erroneous "note" columns
str(processed_extracted_data) # checks structure of dataframe for character variables
processed_extracted_data<-processed_extracted_data%>% mutate_if(is.character, as.numeric) # converts all variables into numeric form
rownames(processed_extracted_data) <- NULL # corrects for the erroneous row numbers extracted when subsetting from the raw data

# Glimpse of the processed data
processed_extracted_data

# Mutual code for future visualisations
fig_path<-here("figs") # creates the necessary path for saving the figures
logo_file<-paste0(here("logo","Picture1.jpg")) # creates the path for applying the logo
year_ending_March_31st<-processed_extracted_data$`Year ending 31 March` # timeframe for visualisations
