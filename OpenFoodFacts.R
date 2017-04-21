rm(list=ls())
setwd("~/Desktop/OpenFoodFacts")

#### Load libaries and data 
library(data.table)
library(tidyr)
library(dplyr)
library(dtplyr)
library(ggmap)
library(ggplot2)
library(plotly)
library(VIM)

openfood <- fread(input = './data/en.openfoodfacts.org.products.tsv', 
                  sep = "\t", na.strings = "")
dim(openfood)
summary(openfood)
head(openfood,2)

#nutrient <- openfood[,64:161]
#aggr(nutrient)

#### Data cleaning and manipulation
### Select Variables
openfood_df <- openfood[, .(code, url, product_name, brands, 
                            origins,stores,countries_en,
                            traces_en,
                            pnns_groups_1, pnns_groups_2,
                            serving_size,
                            energy_100g,
                            fat_100g,
                            cholesterol_100g,
                            carbohydrates_100g, 
                            sugars_100g, 
                            fiber_100g, 
                            proteins_100g,
                            salt_100g,
                            alcohol_100g, 
                            caffeine_100g)] 

### Remove null rows 
#remove_na <- function(data,x){
#  data[!(is.na(data$x) | data$x==""), ]
#}
openfood_df <- openfood_df[!(is.na(openfood_df$code) | openfood_df$code==""), ]
openfood_df <- openfood_df[!(is.na(openfood_df$url) | openfood_df$url==""), ]
openfood_df_all <- openfood_df[!(is.na(openfood_df$countries_en) | openfood_df$countries_en==""), ]
openfood_df_flying <- openfood_df[!(is.na(openfood_df$origins) | openfood_df$origins==""), ]

### Create data sets 
## openfood_flying data set:
# clean origin country name 
# create place holder
name_var <- c("lon", "lat", "loctype", "address", "country")

# assign NA to place holder
openfood_df_flying[, name_var] <- NA

# set data type
openfood_df_flying <- openfood_df_flying %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat),
         loctype = as.character(loctype),
         address = as.character(address),
         country = as.character(country))

# create geolocation info for origins
openfood_df_flying <- fread(input = 'openfood_df_flying.csv')

for(i in 1:nrow(openfood_df_flying)) {
  cat(i, ": ", openfood_df_flying$origins[i])
  if(all(is.na(openfood_df_flying[i, name_var, with=FALSE]))) {
    geo <- geocode(openfood_df_flying$origins[i], output = "more")
    name <- intersect(names(geo), name_var)
    idx <- openfood_df_flying$origins == openfood_df_flying$origins[i]
    openfood_df_flying[idx, name] <- geo[, name]
  }
}

fwrite(openfood_df_flying, 
       file = "openfood_df_flying.csv")

# split multiple countries of sale
food_flying <- fread(input = './data/openfood_df_flying.csv')

for(i in 1:nrow(food_flying)) {
  countries <- unlist(strsplit(food_flying$countries_en[i], split = ","))
  if(length(countries) > 1) {
    print(countries)
    food_flying[i, "countries_en"] <- countries[1]
    temp <- food_flying[i]
    for (country in countries[2:length(countries)]) {
      temp$countries_en <- country
      food_flying <- rbind(food_flying, temp)
    }
  }
}

setnames(food_flying, name_var, paste0(name_var, "_org"))
setnames(food_flying, "countries_en", "country_sales")

fwrite(food_flying, 
       file = "openfood_flying.csv")


## openfood_explore data set:
openfood_explore <- openfood_df_all

for(i in 1:nrow(openfood_explore)) {
  countries <- unlist(strsplit(openfood_explore$countries_en[i], split = ","))
  if(length(countries) > 1) {
    print(countries)
    openfood_explore[i, "countries_en"] <- countries[1]
    temp <- openfood_explore[i]
    for (country in countries[2:length(countries)]) {
      temp$countries_en <- country
      openfood_explore <- rbind(openfood_explore, temp)
    }
  }
}

setnames(openfood_explore, "countries_en", "country_sales")

fwrite(openfood_explore, 
       file = "openfood_explore.csv")

############# ShinyApp 
openfood_flying <- fread(input = './data/openfood_flying.csv')
openfood_explore <- fread(input = './data/openfood_explore.csv')

### (1) tabPanel("Home"): map 
library(maps)
library(googleVis)
library(leaflet)

# map of origins
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

country_rename <- c("UK", "USA", "Ivory Coast", "Republic of Macedonia")

pal <- colorNumeric("Blues", domain = NULL)

labels.org <- sprintf(
  "Rank %g <br/>%s : Food Count is %g",  #<strong>
  map_data_org$Rank, map_data_org$names, map_data_org$food.count) %>% 
  lapply(htmltools::HTML)

leaflet(map_data_org) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1, color = ~pal(-Rank),
              label = labels.org,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

# map_org = gvisGeoChart(map.org.df, "country_org", "Rank",
#                       hovervar = "country_org",
#                       options = list(colors="['cornflowerblue', 'white']",
#                                      height=350, dataMode="regions"))
# plot(map_org)

# map of sales
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


country_rename <- c("UK", "USA", "Ivory Coast", "Republic of Macedonia")

pal <- colorNumeric("Blues", domain = NULL)

labels.sales <- sprintf(
  "Rank %g <br/>%s : Food Count is %g",  #<strong>
  map_data_sales$Rank, map_data_sales$names, map_data_sales$food.count) %>% 
  lapply(htmltools::HTML)

leaflet(map_data_sales) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1, color = ~pal(-Rank),
              label = labels.sales,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

#map.sales.df <- as.data.frame(openfood_explore[,.(food.count=length(code)), 
#                                             by=country_sales])
#
#map_sales = gvisGeoChart(map.sales.df, "country_sales", "food.count",
#                       options = list(height=350, dataMode="regions"))
#
#plot(map_sales)

### (2) tabPanel("Nutrient explorer") 
## for nutrient facts explore, Data:
nutrientloc <- openfood_explore[country_sales=="France"]

## for nutrient facts explore, Plot:
# select Ingrediens 
ingredients.plt <- c(
                 'fat_100g',
                 'carbohydrates_100g' 
                 )
#'energy_100g',
#'cholesterol_100g',
#'sugars_100g', 
# 'fiber_100g', 
# 'proteins_100g',
# 'salt_100g',
# 'alcohol_100g', 
# 'caffeine_100g'

nutrientplt <- NULL
for(ingredient in ingredients.plt){
  temp <- nutrientloc[,.(val=get(ingredient),
                         ingredient=ingredient)]
  
  if(is.null(nutrientplt)){
    nutrientplt <- temp
  } else {
    nutrientplt <- rbind(nutrientplt, temp)
  }
}

nutrientbp <- plot_ly(nutrientplt, y = ~val, color = ~ingredient, type = "box")
nutrientbp

## for nutrient facts explore, Satistical summary:
# select all nutrients
ingredients <- c('energy_100g',
                 'fat_100g',
                 'cholesterol_100g',
                 'carbohydrates_100g', 
                 'sugars_100g', 
                 'fiber_100g', 
                 'proteins_100g',
                 'salt_100g',
                 'alcohol_100g', 
                 'caffeine_100g')

# calculate statstical summary for each ingredient by country
nutrientexp <- NULL
for(ingredient in ingredients){
  temp <- openfood_explore[,.(ingredient=ingredient,
                         Mean=mean(get(ingredient), na.rm = TRUE),
                         Mdian=median(get(ingredient), na.rm = TRUE),
                         Min=min(get(ingredient), na.rm = TRUE),
                         Max=max(get(ingredient), na.rm = TRUE),
                         Obs=.N),
                      by=country_sales]
  
  if(is.null(nutrientexp)){
    nutrientexp <- temp
  } else {
    nutrientexp <- rbind(nutrientexp, temp)
  }
}
