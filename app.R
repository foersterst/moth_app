# Web app to predict dry body masses in (geometrid) moths using the predictive equations
# presented in Foerster et al. (2023)

library(shiny)
library(tidyverse)


# Define UI
ui <- fluidPage(title = "Foerster et al (2023) App",
  titlePanel(h3("Predicting dry body masses in (geometrid) moths", align = "center")),
  br(),
  p("This app implements the predictive equations presented in Foerster et al. (2023) to convert linear measurements of
                  corporal structures (e.g., wing length, maximum wingspan, body length, etc.) into dry body masses (mg). The methods
                  implemented in this app are specially suitable to be applied in geometrid moths."),
  h4("Instructions"),
  p("1) Upload you data as a CSV file, 2) Select the method, 3) Tell the app where in your file the predictors are,
    4) Click on \"Predict\". There is a specific set of models for each sex.
    Use the models for males if you don't have data for sexes in your entries."),
  p("The models require one or two predictors, always in mm (not log transformed). The predictors are abbreviated as follows:
                  (AB) abdomen width, (BL) body length, (MW) maximum wingspan, (WL) wing length, (WT) wingspan tips. See the
                  paper for a better description of how to collect these measurements. The notation is quite intuitive (I hope).
    For example, the method \"Females MW and AB\" predicts dry masses in female moths using maximum wingspan and abdomen width
    as predictors. Predicted values are always in mg."),
  p("Predicted values will be displayed in the last column of you table. Scroll the output table horizontally if it is too wide."),
  h4("Citation"),
  p("Foerster, S. Í. A., Javoiš, J., Holm, S., & Tammaru, T. (2023). Predicting insect body masses based on linear measurements: A phylogenetic case study on geometrid moths. Biological Journal of the Linnean Society, blad069. https://doi.org/10.1093/biolinnean/blad069"),
  br(),
  p("Contact: stenio.foerster@ut.ee"),
  br(),

  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # File input for CSV upload
      fileInput("file", "1. Upload your CSV File",
        accept = c(
          "text/csv", "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),

      # Select method
      selectInput("method", "2. Select Method",
        choices = c(
          "Females MW and AB",
          "Females AB",
          "Females BL",
          "Females MW",
          "Females WL",
          "Females WT",
          "Males MW and AB",
          "Males AB",
          "Males BL",
          "Males MW",
          "Males WL",
          "Males WT"
        ), multiple = F, selected = "Males MW"
      ),

      # Conditional display of variable selection
      uiOutput("varSel"),
      actionButton(inputId = "go", label = "3. Predict", icon = icon("calculator"))
    ),

    # Main panel
    mainPanel(
      # width = 10,
      # Output for displaying the selected variables
      DT::dataTableOutput("tabOut")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive function to read uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Dynamic UI for variable selection based on method
  output$varSel <- renderUI({
    method <- input$method

    if (grepl(pattern = "MW and AB", x = input$method, ignore.case = F)) {
      tagList(
        selectInput("var1", "Select Maximum Wingspan", choices = names(data())),
        selectInput("var2", "Select Abdomen Width", choices = names(data()))
      )
    } else if (grepl(pattern = "BL$", x = input$method, ignore.case = F)) {
      selectInput("var1", "Select Body Length", choices = colnames(data()))
    } else if (grepl(pattern = "MW$", x = input$method, ignore.case = F)) {
      selectInput("var1", "Select Maximum Wingspan", choices = colnames(data()))
    } else if (grepl(pattern = "WL$", x = input$method, ignore.case = F)) {
      selectInput("var1", "Select Wing Length", choices = colnames(data()))
    } else if (grepl(pattern = "AB$", x = input$method, ignore.case = F)) {
      selectInput("var1", "Select Abdomen Width", choices = colnames(data()))
    } else if (grepl(pattern = "WT$", x = input$method, ignore.case = F)) {
      selectInput("var1", "Select Wingspan Tips", choices = colnames(data()))
    } else {
      character(0)
    }
  })

  # Make the predictions

  # 4. Read the uploaded CSV file and apply the predictive equations
  # rdf <- reactive({


  rdf <- eventReactive(input$go, {
    if (input$go > 0) {
      req(input$file)


      d <- read.csv(input$file$datapath)

      if (input$method == "Females MW and AB") {
        mw <- input$var1
        ab <- input$var2
        d$Predicted_values <- exp(-5.61 + (2.33 * log(d[, mw]) + (0.56 * log(d[, ab]))))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Males MW and AB") {
        mw <- input$var1
        ab <- input$var2
        d$Predicted_values <- exp(-4.11 + (1.81 * log(d[, mw]) + (0.70 * log(d[, ab]))))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Females MW") {
        mw <- input$var1
        d$Predicted_values <- exp(-6.60 + (2.76 * log(d[, mw])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Females WL") {
        wl <- input$var1
        d$Predicted_values <- exp(-4.88 + (2.76 * log(d[, wl])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Females BL") {
        bl <- input$var1
        d$Predicted_values <- exp(-3.60 + (2.68 * log(d[, bl])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Females AB") {
        ab <- input$var1
        d$Predicted_values <- exp(1.31 + (1.67 * log(d[, ab])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Males MW") {
        mw <- input$var1
        d$Predicted_values <- exp(-6.33 + (2.62 * log(d[, mw])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Males WL") {
        wl <- input$var1
        d$Predicted_values <- exp(-4.56 + (2.54 * log(d[, wl])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Males BL") {
        bl <- input$var1
        d$Predicted_values <- exp(-3.25 + (2.30 * log(d[, bl])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Males AB") {
        ab <- input$var1
        d$Predicted_values <- exp(1.33 + (1.53 * log(d[, ab])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Males WT") {
        wt <- input$var1
        d$Predicted_values <- exp(-5.64 + (2.43 * log(d[, wt])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else if (input$method == "Females WT") {
        wt <- input$var1
        d$Predicted_values <- exp(-5.86 + (2.60 * log(d[, wt])))
        d$Predicted_values <- round(d$Predicted_values, 4)
        return(d)
      } else {
        d$Predicted_values <- NA
        return(d)
      }
    } else {
      return(NULL)
    }
  })

  # 5. Render the prediction table
  output$tabOut <- DT::renderDataTable(rdf(),
    server = F,
    extensions = "Buttons",
    options = list(
      scrollX = T,
      pageLength = 10, dom = "Bfrtip",
      buttons = list("copy", "csv", "excel")
    )
  )
}


# Run the application
shinyApp(ui, server)
