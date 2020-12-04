library(tidyverse)
library(readr)

file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("./data")){dir.create("./data")}
download.file(file_url, destfile = "./data/reproducible_research_project_2.bz2", method = "curl")

# 2. Read data into global environment
storm <- read.table("data/reproducible_research_project_2.bz2", header = TRUE, sep = ",", dec = ".")


## Example report: https://www.rpubs.com/rdpeng/13396