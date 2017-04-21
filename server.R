library(shiny)
library(dplyr)
library(data.table)
library(maps)
library(leaflet)
library(plotly)
library(RColorBrewer)
# library(scales)
# library(lattice)

#### leaft setup
labels.org <- sprintf(
  "Rank %g <br/>%s : Food Count is %g",  #<strong>
  map_data_org$Rank, map_data_org$names, map_data_org$food.count) %>% 
  lapply(htmltools::HTML)

labels.sales <- sprintf(
  "Rank %g <br/>%s : Food Count is %g",  #<strong>
  map_data_sales$Rank, map_data_sales$names, map_data_sales$food.count) %>% 
  lapply(htmltools::HTML)

pal <- colorNumeric("Blues", domain = NULL)

#### shiny app 
function(input, output, session) { 
  
  ## Interactive Food Distribution Map ###########################################
  
  output$map <- renderLeaflet({
    if(input$country == "Origins") {
      # Create the map for origins
      leaflet(map_data_org) %>%
        setView(lat = 40, lng = 40, zoom = 2) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1, color = ~pal(-Rank),
                    label = labels.org,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    } else {
      # Create the map for origins
      leaflet(map_data_sales) %>%
        setView(lat = 40, lng = 40, zoom = 2) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1, color = ~pal(-Rank),
                    label = labels.sales,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    }

  })
  
  output$table <- renderTable({
    if(input$country == "Origins") {
      map.org.df[,.(Rank, Country=country_org, "Food Count"=food.count)][Rank<16][order(Rank)] 
    } else {
      map.sales.df[,.(Rank, Country=country_sales, "Food Count"=food.count)][Rank<16][order(Rank)]
    }
  })
  
  output$boxplot <- renderPlotly({
    nutrientloc <- openfood_explore[country_sales==input$country_sales]
    
    nutrientplt <- NULL
    for(ingredient in input$nutrient){
      temp <- nutrientloc[,.(Value=get(ingredient),
                             ingredient=ingredient)]
      
      if(is.null(nutrientplt)){
        nutrientplt <- temp
      } else {
        nutrientplt <- rbind(nutrientplt, temp)
      }
    }
    
    plot_ly(nutrientplt, y = ~Value, color = ~ingredient, type = "box")
  })
  
  output$summary <- renderTable({
  
    nutrientexp <- NULL
    for(ingredient in input$nutrient){
      temp <- openfood_explore[country_sales == input$country_sales,
                               .(ingredient=ingredient,
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
    
    nutrientexp
  }, width = "100%")
  
  
  output$food <- renderDataTable({
    openfood_explore[country_sales == input$country_sales,
                     .('ID'=code,
                       'Link'= url,
                       'Name'=product_name,
                       'Brand'=brands,
                       'Category'=traces_en,
                       'Origins'=origins,
                       'Stores'=stores)
                     ]
  })
  
  
  output$barchart <- renderPlot({
    
    rankingplt <- ranking[ingredient==input$nutrient_rank,][Obs>1000][order(-Mean)][1:10]
    
    ggplot(rankingplt, aes(x=reorder(country_sales, -Mean), y=Mean, fill=country_sales)) +
      geom_bar(stat="identity", position="dodge", fill="lightblue")+
      theme_bw()+
      labs(x="Countries", y="Value")
  })
  
  
  output$rank <- renderTable({
    
    rankingplt <- ranking[ingredient==input$nutrient_rank,][Obs>1000][order(-Mean)][1:10]
    
  }, width = "100%")
  

}

  

  
 
   
  
