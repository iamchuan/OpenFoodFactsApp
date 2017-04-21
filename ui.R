library(leaflet)

### create somes vars
countrysales_name <- map.sales.df$country_sales


navbarPage(theme=shinytheme("spacelab"),
           
           "Open Food Facts Visualization",id="nav",
           
           tabPanel("Food map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",

                                      h2("Region explorer", img(src="openfood_icon.png", width=("20%"))),
                                      
                                      selectizeInput("country", "Country", 
                                                     choices = c("Origins", "Sales")),
                                      tableOutput("table")
                                      )
                    )
           ),
  
           tabPanel("Nutrients explorer",
                    
                    fluidRow(
                      
                      column(2,
                             selectInput("country_sales", "Country of Sales", 
                                         choices = countrysales_name , multiple=FALSE)
                             ),
                      column(8,
                             selectInput("nutrient", "Nutrients in 100g food",
                                         selected = c("sugars_100g","proteins_100g"),
                                         choices = nutrients, multiple=TRUE)
                             )
                      ),
                    
                    tabsetPanel(
                      tabPanel("Plot", plotlyOutput("boxplot")), 
                      tabPanel("Summny", tableOutput("summary")),
                      tabPanel("Food Information", dataTableOutput("food"))
                    )
           ),
           
           tabPanel("Ranking",
                     fluidRow(
                      column(5,
                             selectInput("nutrient_rank", "Nutrients", 
                                         choices = nutrients , multiple=FALSE))
                      ),
                      
                      tabsetPanel(
                        tabPanel("Plot", plotOutput("barchart")), 
                        tabPanel("Summny", tableOutput("rank"))
                      )
                    
                    ),
           
           tabPanel("About",
                    withMathJax(),
                    fluidRow(
                      
                      
                      
                      column(10, offset = 1,
                             HTML('<center><img src="about.002.jpeg", width="80%"></center>'),
                             
                             h4("Data Source:"),
                             p(a("Open Food Facts", href="https://world.openfoodfacts.org/"), 
                               " is a free, open, collbarative database of food products from around the world, with ingredients, allergens, nutrition facts and all the tidbits of information we can find on product labels.It is a non-profit association of volunteers."),
                             p("By now, 5000+ contributors have added 100 000+ products from 150 countries using our Android, iPhone or Windows Phone app or their camera to scan barcodes and upload pictures of products and their labels."),
                             HTML('<center><img src="openfoodproject.jpeg", width="28%"></center>'),
                             
                             h4("Tools:"),
                             p("Built using Shiny by RStudio"),
                             p("Code available on", a("GitHub", href="https://github.com/iamchuan?tab=repositories")),
                             
                             p("Author: Chuan Hong", a("LinkedIn", href="https://www.linkedin.com/in/iamchuan/"))
                             )
                      
                    )
           )
           
          )



                      
                    