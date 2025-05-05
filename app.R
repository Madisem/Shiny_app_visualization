#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Loading my smartPCA output
evecDat <- read.table("data/v54_TAK_SBB_HO_PCA.evec", col.names = c("Sample","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","Pop"))
#loading countries with sub continental region data
meta <- read.csv("data/metadata_with_regions.csv")   

evecDat_plotting <- merge(evecDat, meta, by = "Pop")
#loading the dataframe I created in visualization project - the world country maps warped on PCA data of the populations
warped_df <- read.csv("data/warped_df_centred.csv")

ui <- fluidPage(
  titlePanel("Interactive PCA and Warped Map Visualization"),
  fluidRow(
  column(width = 3,
         selectInput("region", "Select Region for PCA:",
                     choices = c("All", sort(unique(evecDat_plotting$Region))),
                     selected = "All"),
         selectInput("warp_region", "Select Region for Warped PCA Map:",
                     choices = c("All", sort(unique(evecDat_plotting$Region))),
                     selected = "All")
  ),
  
  # Main panel content
  column(width = 9,
         h4("🧬 Interactive PCA Scatterplot"),
         plotOutput("pcaPlot", height = "600px", width = "700px"),
         br(),
         
         h4("🧬 PCA Warped with Country Shapes"),
         plotOutput("pcaWarpMap", height = "500px"),
         br(),
         h4("📍 Real-World Map of Eurasia"),
         tags$img(src = "map-of-eurasia.png", width = "80%", height="600px", style = "text-align:center"),
         br(), 
         h4("🌍 Countries Included in PCA Analyses"),
         tags$img(src = "map_of_countries.png", width = "80%", heigh="600px", style = "text-align:center"),
         br(), br(),
  )
)
)

# Server
server <- function(input, output) {
  
  region_data <- reactive({
    if (input$region == "All") {
      evecDat_plotting
    } else {
      filter(evecDat_plotting, Region == input$region)
    }
  })
  warp_region_data <- reactive({
    req(input$warp_region)
    
    if (input$warp_region == "All") {
      warped_df
    } else {
      filter(warped_df, Country %in% evecDat_plotting$Country[evecDat_plotting$Region == input$warp_region])
    }
  })
  
  output$pcaPlot <- renderPlot({
    ggplot(region_data(), aes(PC1, PC2)) +
      geom_point(aes(color = Country, shape = Group), alpha = 0.6, size = 2) +
      theme_minimal() +
      labs(title = paste("PCA Plot - Region:", input$region),
           x = "PC1", y = "PC2") +
      theme(legend.position = "bottom")
  })
  output$pcaWarpMap <- renderPlot({
    df <- warp_region_data()
  
  # Match points for the same region
    region_points <- evecDat_plotting %>% filter(Country %in% df$Country)
    ggplot() +
    geom_polygon(data = df, aes(x = PCAx, y = PCAy, group = group, fill = Country),
                 color = "black", alpha = 0.7) +
    geom_point(data = region_points, aes(x = PC1, y = PC2, color = Country),
               size = 1, alpha = 0.4, show.legend = FALSE) +
    coord_cartesian(
      xlim = range(df$PCAx, na.rm = TRUE),
      ylim = range(df$PCAy, na.rm = TRUE)
    ) +
    theme_minimal() +
    labs(title = paste("Warped PCA Map - Region:", input$warp_region),
         fill = "Country") +
    theme(
      legend.key.size = unit(0.2, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8)
    )
  })
}
# Run App
shinyApp(ui = ui, server = server)