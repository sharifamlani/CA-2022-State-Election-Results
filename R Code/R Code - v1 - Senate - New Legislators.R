#Sharif Amlani
#R 4.1.1
#Fall 2022

######################## Code Summary ##################

######################## Resources  ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Library #####################
library(rvest)
library(dplyr)

######################### Functions ###################

######################## Scrape Data ##################

# Read in the HTML of the page
url <- "https://en.wikipedia.org/wiki/2022_California_State_Senate_election"
html <- read_html(url)

#################### Get and Clean Data ##################
#Start Loop Here:
xpath_counter <- 3
total <- length(html %>% html_nodes(".wikitable"))
df.3 <- NULL
i <- 1
for(i in seq(2,total, by = 1)){
  # Use CSS selectors to select the table containing the vote share data

  table <- html %>% html_nodes(".wikitable") %>% .[[i]]
  
  # Extract the rows of the table as a data frame
  df <- table %>% html_table()
  
  #Set Column Names
  colnames(df) <- df[1,]
  colnames(df)[1] <- "ID"
  
  #Extact only the general election dataset
  df$Row_Numbers <- seq(1, nrow(df), by = 1)
  
  GE_RN <- subset(df, ID == "General election")$Row_Numbers
  TV_RN <- max(subset(df, ID == "Total votes")$Row_Numbers)
  
  df.2 <- df[(GE_RN +1):(TV_RN-1),1:5]
  
  #Assign District Number
  xpath_counter <- xpath_counter + 1
  xpath <- paste("/html/body/div[3]/div[3]/div[5]/div[1]/h2[", xpath_counter, "]/span[1]", sep = "")
  
  html_string <- html %>% html_nodes(xpath = xpath)
  df.2$District  <- html_string %>% html_text()
  
  #Data Management
  df.2$Row_Numbers <- NULL
  df.2$ID <- NULL
  names(df.2)[names(df.2) == '%'] <- "Percent"
  df.2$Votes <- as.numeric(gsub(",", "", df.2$Votes))
  df.2$Percent <- as.numeric(df.2$Percent)
  
  df.3 <- rbind(df.3, df.2)
}

head(df.3)

#################### Data Management ##################

#****************** Code Incumbency ***********************
df.3$Incumbency <- NA
df.3$Incumbency <- ifelse(df.3$Candidate %in% grep("incumbent", df.3$Candidate, value = T), "Yes", "No")

#****************** Code Winner ***********************
df.4 <- NULL
i <- "District 1"
for(i in unique(df.3$District)){
  df.loop.1 <- subset(df.3, District == i)  
  
  df.loop.1$Winner <- ifelse(df.loop.1$Percent == max(df.loop.1$Percent), "Winner", "Loser")
  
  df.4 <- rbind(df.4, df.loop.1)
}

#****************** Make District First Column ***********************
cols_to_keep <- setdiff(names(df.4), "District")  # Select all columns except "b"
df.5 <- df.4 %>% select(District , cols_to_keep) # Combine "b" with the other columns in the desired order

#****************** Chamber ***********************
df.5$Chamber <- "Senate"
df.5$Election <- "General"
df.5$Year <- 2022

#################### Save Data ##################
#setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fifth Year/Fall Quarter/GSR/Assignments/Data Work/New Legislators/Senate/Data")

write.csv(df.5, file = "2022 CA Senate - General Election Results.csv", row.names = F)