#rm(list=ls())
#setwd("~/Desktop/OpenFoodFacts")

library(shiny)
library(shinythemes)
library(data.table)
library(dplyr)
library(maps)
library(googleVis)
library(leaflet)
library(plotly)

## load data
openfood_flying <- fread(input = './data/openfood_flying.csv')
openfood_explore <- fread(input = './data/openfood_explore.csv')

nutrients <- c('fat_100g','carbohydrates_100g','energy_100g',
               'cholesterol_100g','sugars_100g', 'fiber_100g', 
               'proteins_100g','salt_100g','alcohol_100g', 'caffeine_100g')

## create data for origin map
map.org.df <- openfood_flying[,
                              .(food.count=.N), 
                              by=country_org][!country_org=="",
                                              .(country_org,
                                                food.count,
                                                Rank = rank(-food.count))]

country_orgname <- c("United Kingdom","United States","Côte d'Ivoire","Macedonia (FYROM)")
country_newname <- c("UK", "USA", "Ivory Coast", "Republic of Macedonia")

map.org.df[country_org %in% country_orgname, 
           country_org := country_newname]

map_data_org <- map("world", fill = TRUE, regions = map.org.df[,country_org], 
                    plot = FALSE)

setkey(map.org.df, "country_org")

map_data_org$food.count <- map.org.df[gsub(":.*", "", map_data_org$names), food.count]

map_data_org$Rank <- map.org.df[gsub(":.*", "", map_data_org$names), Rank]

map.org.df$Rank <- as.integer(map.org.df$Rank)


## create data for sales map 
map.sales.df <- openfood_explore[,
                                 .(food.count=.N),
                                 by=country_sales][!country_sales=="",
                                                   .(country_sales,
                                                     food.count,
                                                     Rank = rank(-food.count))]
 country_salesname <- c("United Kingdom","United States","Côte d'Ivoire","Macedonia (FYROM)")
 country_newname <- c("UK", "USA", "Ivory Coast", "Republic of Macedonia")

 map.sales.df[country_sales %in% country_salesname, 
              country_sales := country_newname]

map_data_sales <- map("world", fill = TRUE, regions = map.sales.df[,country_sales], 
                      plot = FALSE)

setkey(map.sales.df, "country_sales")

map_data_sales$food.count <- map.sales.df[gsub(":.*", "", map_data_sales$names), food.count]

map_data_sales$Rank <- map.sales.df[gsub(":.*", "", map_data_sales$names), Rank]

map.sales.df$Rank <- as.integer(map.sales.df$Rank)

country_rename <- c("UK", "USA", "Ivory Coast", "Republic of Macedonia")

## create data for ranking
ranking <- NULL
for(ingredient in nutrients){
  temp <- openfood_explore[,.(ingredient=ingredient,
                              Mean=mean(get(ingredient), na.rm = TRUE),
                              Mdian=median(get(ingredient), na.rm = TRUE),
                              Min=min(get(ingredient), na.rm = TRUE),
                              Max=max(get(ingredient), na.rm = TRUE),
                              Obs=.N),
                           by=country_sales]
  
  if(is.null(ranking)){
    ranking <- temp
  } else {
    ranking <- rbind(ranking, temp)
  }
}


