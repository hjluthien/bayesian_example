library(shiny)
library(shinyWidgets)
library(here)
library(pracma)
#library(shinydashboard)

estimate_bias <- function(dataset, prior) {
  for(data in dataset) {
    prior <- bayes(data, prior)
  }
  return(prior)
}

bayes <- function(data, prior) {
  #if(abs(sum(prior) - 1) < 0.001) stop("Prior probability does not sum to 1!")
  length <- length(prior)
  
  if(data == "H") {
    likelihood <- seq(0, 1, length.out = length)
  } else if(data == "T") {
    likelihood <- seq(1, 0, length.out = length)
  } else stop("Invalid data!")
  
  posterior <- (likelihood * prior) / sum(likelihood * prior)
  
  return(posterior)
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("What is the bias of this coin?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input:  ----
      numericInput(inputId = "spacing",
                  label = "Enter spacing",
                  value = 1001,
                  min = 1,
                  max = 100000),
      
      textInput(inputId = "data",
                    label = "Please enter own data",
                value = ""),
    
    selectInput(inputId = "prior_dist",
                label = "Select Prior dist",
                choices = c("Flat", "Normal"),
                selected = "Flat"),
   
    numericInput(inputId = "mean",
                 label = "Please enter mean",
                 value = 0.3,
                 min = 0,
                 max = 1),
    
    numericInput(inputId = "sd",
                 label = "Please enter standard deviation",
                 value = 0.005,
                 min = 0,
                 max = 1)),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      # h3("What is the bias of this coin?"), br(),
       
       
       
       h3("A statistical statement appeared in 'The Guardian' on Friday January 4, 2002:"), br(),

       h4("Spun on edge 250 times, a Belgian one-euro coin came up heads 140 times and tails 110. 
         ‘It looks very suspicious to me,’ said Barry Blight, a statistics lecturer at the London School of Economics. 
         ‘If the coin were unbiased, the chance of getting a result as extreme as that would be less than 7%."), br(),
       
       h3("But do these data give evidence that the coin is biased rather than fair?"), br(),
       
       p("Bias = P(Heads)"), br(),
      
      # Output: Histogram ----
      plotOutput("plot"), br(),
      
      h3("Prior Probability (Assumptions)"), br(),
      
      h4("Strong priors (assumptions) will pull result towards the prior, 
         where as weak ones will be more dominated by the data"),br(),
      
      h4("You can use any distribution as a prior - step, tophat, delta etc. 
         Which distribution you need will depend on the nature of the problem you're trying to solve."), br(),
      
      plotOutput("prior_plot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  bias_prior <- reactive({
    
    if (input$prior_dist == "Flat") {
      rep(1 / as.numeric(input$spacing), 
                                  as.numeric(input$spacing))
    
  
     } else {
       dnorm(seq(0, 1, length.out = as.numeric(input$spacing)),
             input$mean, input$sd)/sum(dnorm(seq(0, 1, length.out = as.numeric(input$spacing)),
                                             input$mean, input$sd))
       }
  })
  
  result <- reactive({
  
  if (input$data != "") {
    
    dataset <-  strsplit(input$data, "")[[1]]
    
  } else {
  
    dataset <- c( rep("H", 140), rep("T", 110))
  
  }
  estimate_bias(dataset, bias_prior())
  })
  
  output$plot <- renderPlot({ 
    validate(
      need(input$spacing > 1, "Please enter spacing greater than 1")
    )
     plot(seq(0, 1, length.out = as.numeric(input$spacing)), 
          result(), 
          xlab = "Bias", 
          ylab = "Probability") })
  
  output$prior_plot <- renderPlot({
    validate(
      need(input$spacing > 1, "Please enter spacing greater than 1")
    )
    plot(seq(0, 1, length.out = as.numeric(input$spacing)), 
         bias_prior(), 
         xlab = "Bias", 
         ylab = "Prior Probability")#, 
        # ylim = c(0,1))
  })
}

shinyApp(ui, server)