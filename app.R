## Dataset generation for Rasch, 1PL, 2PL, 3PL and 4PL models of IRT (09.11.2020)

library(shiny)
library(shinyWidgets)
library(ltm)

ui <- fluidPage(
  
  titlePanel(strong("Generating a Dataset for IRT Logistc Models")),
  hr(),

  sidebarPanel(width = 3,

    p(strong(em(h4("Define the parameters of IRT models and select a model", 
                   style = "color:darkgreen")))),
    hr(),
    numericInput("k", "Total number of items in the dataset:", 
                 value = 20, width = 150),
    numericInput("n", "Total observations in the dataset:", 
                 value = 500, width = 150),
    numericRangeInput("a", "Limits of 'a' parameter:", 
                 value = c(1, 1), width = 200),
    numericRangeInput("b", "Limits of 'b' parameter:", 
                 value = c(-3, 3), width = 200), 
    numericInput("c", "Upper-limit of 'c' parameter:", 
                 value = 0, width = 200),
    numericInput("d", "Lower-limit of 'd' parameter:", 
                 value = 1, width = 200),
    hr(),

    radioButtons("model", "IRT Logistic Models:", 
                 c("Rasch" = "rasch", "1PL" = "1pl", "2PL" = "2pl", "3PL" = "3pl"),
                 selected = "2pl", width = 150),
    hr(),

    actionButton("go", "Generate", class = "btn-primary"),
    downloadButton("down", "Download", class = "btn-success")
    
  ),

  mainPanel(

    tabsetPanel(type = "pills",
      tabPanel("Dataset", tableOutput("table1")),
      tabPanel("Item Parameters", verbatimTextOutput("text1")),
      tabPanel("Model Comparisions", verbatimTextOutput("text2")),     
      tabPanel("Help",
        p(h4(strong("How to use this application:", style = "color:darkgreen"))),
        hr(),
        p(em("With this application; you can generate a 1-0 dataset and download it. 
           Also, you can estimate the IRT item parameters and compare the models each other.
           In order to use this application, 'shiny' and 'shinyWidgets' and 'ltm' packages must be installed in your computer.")),
        br(),
        p(h4(strong("Item Parameters:", style = "color:darkgreen"))),
        p("You can define the interval of different parameters according to your preference of the model."),
        p(strong("a:"), "Indicator of discrimination for 2PL and 3PL models. Recommended at least around 1.
                         You can define the lower and upper limits of this parameter."),
        p(strong("b:"), "Indicator of difficulties for all logistic models. Recommended mostly between [-3, 3].
                         You can define the lower and upper limits of this parameter."),
        p(strong("c:"), "Indicator of poseudo-quessing for 3PL and 4PL models. Recommended lower than .35.
                         You can define the upper limit of this parameter."),
        p(strong("d:"), "Indicator of upper-possibility level which is under 1 for using 4PL models. 
                         In this application, as a limitation, it can't be provided 4PL estimations.
                         You can define the lower limit of this parameter."),
        br(),
        p(h4(strong("Models:", style = "color:darkgreen"))), 
        p("You can select the different logistic models for parameter estimations. 
           Also you can compare different models and decide the best model for model-data fit."),
        br(),
        p(h4(strong("Buttons:", style = "color:darkgreen"))),
        p(strong("Generate:"), "You can generate a 1-0 dataset with this button, according to your parameter definitions."),
        p(strong("Download:"), "You can download the generated dataset into the working-directory of your computer 
                               with the extension name of 'IRTdata.csv'."),
        br(),
        p(h4(strong("Panels:", style = "color:darkgreen"))),
        p(strong("Dataset:"), "You can display the first 25 rows of the generated dataset in this panel."),
        p(strong("Item Parameters:"), "According to your selections, you can display the estimations of the item parameters in this panel. 
                                       These estimations were obtained by using 'ltm' package from R."),
        p(strong("Model Comparisions:"), "You can display the ANOVA results for model comparisions in this panel.")   
      
      )
    )

  ) 
)

server <- function(input, output, session){

  values <- reactiveValues()

  observe({

    input$go

    isolate({

      f.IRT.response <- function(n, k, theta, a, b, c, d){

        P <- matrix(nrow = n, ncol = k)
        M <- matrix(nrow = n, ncol = k)

        for(i in 1:k){
          for(j in 1:n){
            P[j, i] <- c[i] + (d[i] - c[i]) / (1 + exp(- a[i]* (theta[j] - b[i])))
          }
        }

        rv <- matrix(runif(n*k), n, k)
        M <- as.data.frame((P >= rv)*1)
        names(M) <- paste0("i", 1:k)
        M
      }
   
      values$df <<- f.IRT.response(n = input$n, k = input$k, theta = rnorm(input$n), 
                                a = runif(input$k, input$a[1], input$a[2]),
                                b = runif(input$k, input$b[1], input$b[2]),
                                c = runif(input$k, 0, input$c),
                                d = runif(input$k, input$d, 1)
                                )

    })

    data <- values$df 
    model.Rasch <<- rasch(data, constraint = cbind(ncol(data) + 1, 1))
    model.1PL <<- rasch(data)
    model.2PL <<- ltm(data ~ z1)
    model.3PL <<- tpm(data)

  })


  output$table1 <- renderTable({

    input$go

    head(values$df, 25)

  }, rownames = TRUE, digits = 0)


  output$text1 <- renderPrint({
   
    input$go

    if(input$model == "rasch"){ 
      print(model.Rasch) 
      } else if (input$model == "1pl"){
      print(model.1PL)
      } else if (input$model == "2pl"){
      print(model.2PL)} else {
      print(model.3PL)}

  })

  
  output$text2 <- renderPrint({

    input$go

    print(anova(model.Rasch, model.2PL))
    print(anova(model.Rasch, model.3PL))
    print(anova(model.2PL, model.3PL))

  })


  output$down <- downloadHandler(
    filename = function() {
      paste0("IRTdata", ".csv")
    },
    content = function(file) {
      write.csv2(values$df, file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
