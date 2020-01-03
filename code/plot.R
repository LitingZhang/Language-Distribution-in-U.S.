#
#Plot language distribution heatmaps
#Author: Liting Zhang
#Date: 2/1/2019
#


library(tidyverse)
library(rgdal)
library(leaflet)

#read in language and location data, total 1, 216, 936 record
place_lang_use <- read_csv("place_lang_use1.csv")

#read geo spatial data for counties in U.S.
county <- readOGR("/home/liting/Downloads/cb_2018_us_county_500k/")

#find two 10 languages
lang_rank <- table(place_lang_use$lang) %>% sort(decreasing = TRUE)
lang_rank
lang_select <- lang_rank[1:10]
lang_select <- names(lang_select)

#get top 10 language count by county
lang10 <- as.data.frame(place_lang_use$county_fips)
names(lang10) <- "county_fips"
for(i in lang_select)
{
  print(i) 
  lang <- place_lang_use %>%
    filter(lang == i) %>%
    group_by(county_fips) %>%
    summarise(COUNT = n())
  names(lang) <- c("county_fips", i)
  lang10 <- left_join(lang10, lang, by = "county_fips")

}
#remove deplicated counties
lang10 <- unique(lang10)
plotLang <- function(county, language, lang)
{
  
  county1 <- county
  county1@data <- county1@data %>%
    mutate(county_fips = substring(AFFGEOID,regexpr("S", string) + 1))
  county1@data$county_fips <- as.numeric(county1@data$county_fips)
  county1@data <-left_join(county1@data, language, by = "county_fips" )
  
  #Map
  us_map <- leaflet(county) %>% addTiles()
  #customize colors
  bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, Inf)
  pal <- colorBin("YlOrRd", domain = county1$COUNT, bins = bins)
  
  us_map %>% addPolygons(
    fillColor = ~pal(county1@data[, lang]), #fill by count
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE))%>%
    addLegend(pal = pal, 
              values = ~county1$COUNT, 
              opacity = 0.7, 
              title = paste0(lang, " distribution"),
              position = "bottomright")
}

plotLang(county, lang10, "en")
plotLang(county, lang10, "und")
plotLang(county, lang10, "es")
plotLang(county, lang10, "ht")
plotLang(county, lang10, "tl")
plotLang(county, lang10, "in")
plotLang(county, lang10, "pt")
plotLang(county, lang10, "fr")
plotLang(county, lang10, "ar")
plotLang(county, lang10, "de")





