library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(GGally)
library(class)
library(caret)
library(bslib)
library(ggthemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = bs_theme(bootswatch = "darkly"),
    # Application title
    titlePanel("Wine Quality vs Other Features Plots"),
    headerPanel(h3("Arman Azhand & Eric Graham")),
    headerPanel(h4("aazhand@mail.smu.edu & egraham@mail.smu.edu")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectX", label = h4("Variable to compare quality versus"),
                        choices = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    train = read.csv("../Wine Train.csv", header = TRUE)
    winelocations = read_excel("../Wine Types And Locations.xlsx")
    train = left_join(train, winelocations, by = "ID")
    train$type = tools::toTitleCase(train$type)
    train$location = tools::toTitleCase(train$location)
    train$location = gsub("Califormia", "California", train$location)
    train$type = as.factor(train$type)
    train$location = as.factor(train$location)
    
    trainWineNoNA = train %>% filter(!is.na(type))
    
    train_indices = sample(1:nrow(trainWineNoNA), 0.7 * nrow(trainWineNoNA))
    imputeTrain = trainWineNoNA[train_indices,]
    imputeTest = trainWineNoNA[-train_indices,]
    
    trainWineNA = train %>% filter(is.na(type))
    
    classifications = knn(trainWineNoNA[,2:12], trainWineNA[,2:12], trainWineNoNA$type, prob = TRUE, k = 5)
    trainWineNA$type = factor(as.character(classifications))
    
    train$type[is.na(train$type)] <- trainWineNA$type[match(
      train$ID[is.na(train$type)],
      trainWineNA$ID
    )]
    
    data = reactive({
      req(train)
      return(train)
    })
    
    observe({
      req(data())
      xVars = data() %>% dplyr::select(-c(ID, quality))
      updateSelectInput(session, "selectX", choices = names(xVars))
    })
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        req(data())
        xVariable = input$selectX
        print(xVariable)
        df = data() %>% select(c(input$selectX, "quality"))
        if(is.numeric(df[,1])) {
          print("num plot")
          xlabel = gsub("\\.", " ", xVariable)
          xlabel = gsub("\\b([a-z])", "\\U\\1", xlabel, perl = TRUE)
          renderp = df %>% ggplot(aes(y = get(names(df)[1]), x = as.factor(quality))) +
            geom_boxplot(fill = "pink") +
            theme_solarized() +
            theme(legend.position="none",
                  axis.text.x = element_text(face="bold", size = 15, angle = 0),
                  axis.text.y = element_text(face="bold", size = 15, angle = 0)) +
            labs(title = paste("Boxplot of", xlabel ,"for each quality score"), y = xlabel, x = "Quality Score")
        } else if (is.factor(df[,1])) {
          print("fac plot")
          renderp = df %>% ggplot(aes(x = quality, fill = get(names(df)[1]))) +
            geom_histogram(aes(y = stat(density)), binwidth = 1, color = "black") +
            scale_y_continuous(labels = scales::percent) +
            theme_solarized() +
            facet_wrap(vars(get(names(df)[1]))) +
            theme(legend.position="none", 
                  axis.text.x = element_text(face="bold", size = 15, angle = 0),
                  axis.text.y = element_text(face="bold", size = 15, angle = 0))
            if (input$selectX == "type") {
              renderp = renderp + scale_fill_manual(values = c("red", "white")) +
                labs(title = "Histogram of quality grouped by wine type", x = "Quality Score")
            } else if (input$selectX == "location") {
              renderp = renderp + scale_fill_manual(values = c("gold", "maroon")) +
                labs(title = "Histogram of quality grouped by wine's location", x = "Quality Score")
            }
        } else {
          print("no plot")
          print(class(df[,1]))
          renderp = ggplot()
        }
        renderp
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
