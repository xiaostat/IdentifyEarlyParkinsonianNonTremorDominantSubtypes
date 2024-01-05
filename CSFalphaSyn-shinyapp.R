library(shinydashboard)
library(shiny)
library(mlr)
library(e1071)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("REF_subset_quick", tabName = "REF_subset_quick", icon = icon("th")),
      menuItem("summary", tabName = "Summary", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      
      tabItem(tabName = "REF_subset_quick",
              fluidRow(
                box(width=12,
                    h3("For PD patients with recent clinical assessment and lumbar puncture"),
                    tags$hr(),
                    h4("Predicted results:"),
                    tableOutput("result"),
                ),
                box(h3("feature input"),
                    width=12,
                    tags$hr(),
                    h4("Enter all the information and click the “submit” button."),
                    actionButton("submit", "submit")
                ),
                box(width=12,
                    column(width = 6,
                           radioButtons("gen", "gender",
                                        choices = c("male" = "0", "female" = "1"),
                                        selected = "0"),
                           radioButtons("MCI_testscores", "MCI",
                                        choices = c("normal" = "0", "MCI" = "1"),
                                        selected = "0"),
                           radioButtons("NP1APAT", "apathy or not",
                                        choices = c("Not apathy" = "0", "apathy" = "1"),
                                        selected = "0")),
                    column(width = 6,
                           radioButtons("fampd_new", "Fampd",
                                        choices = c("no Fampd" = "0", "Fampd" = "1"),
                                        selected = "0"),
                           radioButtons("DOMSIDERight", "DOMSIDE",
                                        choices = c("not right" = "0", "right" = "1"),
                                        selected = "0"),
                           radioButtons("NP1FATG", "Fatigue or not",
                                        choices = c("Not Fatigue" = "0", "Fatigue" = "1"),
                                        selected = "0")),
                    
                    column(width = 12,
                           sliderInput("EDUCYRS", "Years of education", value = 0, min = 0, max = 30),
                           sliderInput("ageonset", "ageonset", value = 0, min = 0, max = 30),
                    ))
                ,
                box(width=12,
                    sliderInput("ess", "Epworth Sleepiness Scale Score", value = 0, min = 0, max = 24),
                    sliderInput("MSEADLG", "Modified Schwab & England ADL Score", value = 0, min = 0, max = 100, step = 5),
                    
                    sliderInput("stai", "State-Trait Anxiety Index Total Score", value = 20, min = 20, max = 160),
                    numericInput("asyn", "cerebrospinal fluid α-synuclein(pg/mL)", value = 1, min = 0, max = 9999),
                    sliderInput("tremor", "tremor score", value = 0, min = 0, max = 40),
                    sliderInput("SDMTOTAL", "SDMT", value = 0, min = 0, max = 40),
                    sliderInput("rigidity", "rigidity", value = 0, min = 0, max = 40),
                    sliderInput("lns", "LNS", value = 0, min = 0, max = 40),
                    sliderInput("scopa", "Scales-for-Outcomes-in-Parkinson's-Disease-Autonomic Score", value = 0, min = 0, max = 69),
                    sliderInput("quip", "Quip", value = 0, min = 0, max = 69),
                    sliderInput("sft", "SFT", value = 0, min = 0, max = 69),
                    sliderInput("bjlot", "BJLOT", value = 0, min = 0, max = 69)
                )
              )
      )
      
      
      ,
      tabItem(tabName = "Summary",
              fluidRow(
                box(width=12,
                    h3("For PD patients with recent clinical assessment and lumbar puncture"),
                    tags$hr(),
                    h4("Summary"),
                    tableOutput("values")
                )
              )
      )
      
      
      
    )))


PSVMmodel <- readRDS("PSVMmodel.rds")



server <- function(input, output) {
  sliderValues <- reactive({
    
    data.frame(
      Name = c("tremor score",
               "MCI",
               "Apathy or not",
               "Fatigue or not",
               "Years of education",
               "ageonset",
               "Epworth Sleepiness Scale Score",
               "Modified Schwab & England ADL Score",
               "DOMSIDERight",
               "State-Trait Anxiety Index Total Score",
               "cerebrospinal fluid α-synuclein(pg/mL)",
               "SDMTOTAL",
               "rigidity" ,
               "gen",
               "lns",
               "Scales-for-Outcomes-in-Parkinson's-Disease-Autonomic Score",
               "fampd_new",
               "quip",
               "sft",
               "bjlot" ),
      Value = as.character(c(
        as.numeric(input$tremor), 
        input$MCI_testscores,
        as.numeric(input$NP1APAT), 
        as.numeric(input$NP1FATG), 
        as.numeric(input$EDUCYRS), 
        as.numeric(input$ageonset), 
        as.numeric(input$ess), 
        as.numeric(input$MSEADLG), 
        as.numeric(input$DOMSIDERight), 
        as.numeric(input$stai), 
        as.numeric(input$asyn), 
        as.numeric(input$SDMTOTAL), 
        as.numeric(input$rigidity), 
        as.numeric(input$gen), 
        as.numeric(input$lns), 
        as.numeric(input$scopa), 
        as.numeric(input$fampd_new), 
        as.numeric(input$quip), 
        as.numeric(input$sft), 
        as.numeric(input$bjlot) 
      )
      ),stringsAsFactors = FALSE)
    
    
  })
  
  output$values <- renderTable({
    sliderValues()
  })
  
  
  observeEvent(input$submit, {
    # 构建新数据
    new_data <- data.frame(
      tremor = ((as.numeric(input$tremor)-4.309192)/3.124368),
      MCI_testscores = as.numeric(input$MCI_testscores),
      NP1APAT = as.numeric(input$NP1APAT),
      NP1FATG = as.numeric(input$NP1FATG),
      EDUCYRS = ((as.numeric(input$EDUCYRS)-15.5571)/2.978779),
      ageonset = ((as.numeric(input$ageonset)-59.96269)/9.892851),
      ess = ((as.numeric(input$ess)-5.799443)/3.468759),
      MSEADLG = ((as.numeric(input$MSEADLG)-93.20334)/5.948136),
      DOMSIDERight = as.numeric(input$DOMSIDERight),
      stai = ((as.numeric(input$stai)-64.89136)/18.5482),
      asyn = ((log(as.numeric(input$asyn))-7.284785)/0.3799623),
      SDMTOTAL = ((as.numeric(input$SDMTOTAL)-41.01671)/9.631173),
      rigidity = ((as.numeric(input$rigidity)-3.749304)/2.64651),
      gen = as.numeric(input$gen),
      lns = ((as.numeric(input$lns)-10.57939)/2.716331),
      scopa = ((as.numeric(input$scopa)-9.543175)/6.184112),
      fampd_new = as.numeric(input$fampd_new),
      quip = ((as.numeric(input$quip)-0.2980501)/0.6539708),
      sft = ((as.numeric(input$sft)-48.99721)/11.89866),
      bjlot = ((as.numeric(input$bjlot)-12.82173)/2.123683)
    )
    
    # 重新命名列名
    colnames(new_data) <- c("tremor", "MCI_testscores", "NP1APAT", "NP1FATG", "EDUCYRS", "ageonset", "ess", "MSEADLG", "DOMSIDERight", "stai", "asyn", "SDMTOTAL", "rigidity", "gen", "lns", "scopa", "fampd_new", "quip", "sft", "bjlot")
    PSVM_pred <- predict(PSVMmodel, newdata = new_data)
    # 输出结果
    
    output$result <- renderText({
      result <- PSVM_pred#$data$response
      if(result == "td") {
        modified_result <- "Tremor-Dominant subtype"
      } else if(result == "no_td") {
        modified_result <- "Non-Tremor-Dominant subtype"
      } else {
        modified_result <- result
      }
      paste("The patient's motor subtype is:",
            HTML(paste("<span style='color: red; font-weight: bold; font-size: 20px;'>",
                       modified_result,
                       "</span>")))
    })
    
  })
  
}


shinyApp(ui, server)
