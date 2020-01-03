#Prepare data
library("jsonlite")
library(tidyverse)

#read in tweets
tweets <- stream_in(file("tweets_data1.json"))
#select language and place
place_lang <- tweets[,c("lang", "place")]
place <- place_lang$place
place <- place[,c(3,5,7)] #select country, place_type, and place name
lang <- place_lang$lang #select language 

place_lang <- cbind(place,lang)

#filter tweets in U.S, and place type is city
place_lang <- place_lang %>%
  filter(country == "United States", place_type == "city")%>%
  select(full_name, lang)

#split full name into cities and states
place_lang <- place_lang %>%
  separate(full_name, into = c("city", "state_id"), sep = ", ")

#read county information
uscities <- read_csv("uscities.csv")
uscities <- uscities[,c(1,3,5,6)] #select cities, couties, states and county code

place_lang_use <- left_join(place_lang, uscities, by = c("city", "state_id"))

write.csv(place_lang_use, "place_lang_use1.csv", row.names = F)
