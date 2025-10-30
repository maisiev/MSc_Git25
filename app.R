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
library(matrixStats)


library(shinythemes)

options(shiny.maxRequestSize = 100*1024^2)

#load GTEx data and creates pc vector
dat <- "../originals/GTEx/GTEx_gene_reads.csv"
pcs <- paste0("PC", 1:10)

#get PCA
get_pca <- function(x){
  dat <- read.csv(x, row.names = 1)
  lib.sizes <- colSums(dat)
  dat <- t(apply(dat, 1, \(x) x/lib.sizes))
  dat <- log(dat + 1)
  dat <- dat[rowVars(dat) > 0,]  
  pca <- as.data.frame(prcomp(t(dat), scale. = TRUE)$x)
  pca
}

#plots PCA
plot_pca <- function(pca, x, y){
  # Scatter plot with ggplot2
  ggplot(pca, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(color = "#FF68A1") +  
    labs(title = paste("Scatter Plot of", x, "vs", y),
         x = x, y = y) +  
    theme_grey() 
}
# Define UI for application that draws a histogram
ui <- fluidPage(
 


    # Application title
    titlePanel("PCA Explorer"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose CSV file", accept = ".csv"),
            selectInput(inputId = 'xaxis', label = 'Select PC for X-axis',
                        choices = pcs,
                        selected = pcs[1]
                        ),
            selectInput(inputId = 'yaxis', label = 'Select PC for Y-axis',
                        choices = pcs,
                        selected = pcs[2])
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("pcaPlot")
          
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    pca_data <- reactive({
      req(input$file)
      get_pca(input$file$datapath)
    })
    observe({
      req(pca_data())
      pcs <- colnames(pca_data())
      updateSelectInput(session, "xaxis", choices = pcs, selected = pcs[1])
      updateSelectInput(session, "yaxis", choices = pcs, selected = pcs[2])
    })
    # Render the scatter plot of the selected PCA components
    output$pcaPlot <- renderPlot({
      req(pca_data(), input$xaxis, input$yaxis)
      plot_pca(pca_data(), input$xaxis, input$yaxis)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
