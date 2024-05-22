library(DT)
library(shiny)
library(shinydashboard)

library(RISCA)
library(ggiraph)
library(highcharter)
library(ggplot2)
library(plotly)
library(shinyjs)
library(RSelenium)

##########################################TEMP###############################################
load("RES.RData")
table_resultats <- read.csv("table_resultats.csv", sep = ";")
tmp <- table_resultats$X
table_resultats <- data.frame(table_resultats[,-1])
row.names(table_resultats) <- tmp
rm(tmp)
nb_ech_bootstrap <- 1000
nb_BDD_completes <- 5
TIME <- read.csv("TIME.csv")
#############################################################################################

#load(url("https://urldefense.com/v3/__https://github.com/Pelras-A/ProgressionFreeSurvivalPrediction-PancreaticCancer/raw/main/RES.RData__;!!CWQwuGU35Jow!txF10dt6RX5IgqQV0pDATAqf4Y-T6uuoKl0eybS-D7is0GtZf0yvSt0Qcrbgmwl1MjnqAIhWJyPyJXtat9ZvjDJzj3vAYQcWJg$"))
#
#table_resultats <- read.csv("https://urldefense.com/v3/__https://raw.githubusercontent.com/Pelras-A/ProgressionFreeSurvivalPrediction-PancreaticCancer/main/table_resultats.csv__;!!CWQwuGU35Jow!txF10dt6RX5IgqQV0pDATAqf4Y-T6uuoKl0eybS-D7is0GtZf0yvSt0Qcrbgmwl1MjnqAIhWJyPyJXtat9ZvjDJzj3sTdxCHtA$")
#tmp <- table_resultats$X
#table_resultats <- data.frame(table_resultats[,-1])
#row.names(table_resultats) <- tmp
#rm(tmp)
#
#nb_ech_bootstrap <- 1000
#nb_BDD_completes <- 5
#TIME <- read.csv("https://urldefense.com/v3/__https://raw.githubusercontent.com/Pelras-A/ProgressionFreeSurvivalPrediction-PancreaticCancer/main/TIME.csv__;!!CWQwuGU35Jow!txF10dt6RX5IgqQV0pDATAqf4Y-T6uuoKl0eybS-D7is0GtZf0yvSt0Qcrbgmwl1MjnqAIhWJyPyJXtat9ZvjDJzj3vIf-BsCQ$")

ui <- dashboardPage(
    dashboardHeader(title = "KP-L3"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Welcome", tabName = "one"),
            menuItem("Calculator", tabName = "two")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "one",
                    p("Welcome to the KP-L3 application, a decision-making tool. 
                      This application is devoted to the calculation of the free progression survival 
                      after a second line treatment of pancreatic adenocarcinoma.
                      To get started, click on the calculator widget.")),
            tabItem(tabName = "two",
                    box(
                      column(
                             width = 6,
                             radioButtons(inputId = "SEXE",
                                          label = "Sex",
                                          choiceNames = c("M", "F"),
                                          choiceValues = c(1, 0)),
                             sliderInput(inputId = "AGE",
                                         label = "Age in years",
                                         min = 18,
                                         max = 99,
                                         value = 30),
                             radioButtons(inputId = "RP",
                                          label = "Primary resection",
                                          choiceNames = c("Yes", "No"),
                                          choiceValues = c(1, 0)),
                             radioButtons(inputId = "MPL3",
                                          label = "Lung metastasis at third line prescription",
                                          choiceNames = c("Yes", "No"),
                                          choiceValues = c(1, 0)),
                             radioButtons(inputId = "CL3",
                                          label = "Carcinosis at third line prescription",
                                          choiceNames = c("Yes", "No"),
                                          choiceValues = c(1,0)),
                             selectInput(inputId = "MHL3",
                                            label = "Hepatic metastasis at third line prescription",
                                            choices = c("No","Isolated","With others"))
                             ),
                      column(
                             width = 6,
                             fluidRow(
                               selectInput(inputId = "NMSD",
                                              label = "Number of metastatic sites at diagnosis",
                                              choices = c("0","1","2 or more")),
                               sliderInput(inputId = "SSP1",
                                           label = "Duration of the first line treatment in months",
                                           value = 3,
                                           min = 0.1,
                                           max = 50,
                                           step = 0.1),
                               radioButtons(inputId = "PL1",
                                            label = "Protocol followed during the first line treatment",
                                            choiceNames = c("Folfirinox", "Others"),
                                            choiceValues = c(1, 0)),
                               sliderInput(inputId = "SSP2",
                                           label = "Duration of the second line treatment in months",
                                           value = 2,
                                           min = 0.1,
                                           max = 50,
                                           step = 0.1)
                               )
                             )
                      ),
                    
                        box(title = "Predicted survival curve",
                            plotlyOutput("KM"),
                            div(style = "height:50px"),
                            verbatimTextOutput("EV"))
                    )
            )
        )
    )

server <- function(input, output){
    
    
    model_final <- function(X){
        
        model <- X
        
        for(cov in names(X)){
            
            model[,cov] <- X[,cov]*table_resultats[cov,"AVG"]
            
        }
        
        sum.model <- rowSums(model, na.rm = TRUE)
        
        pred.temp <- 0
        
        for(i in 1:(nb_ech_bootstrap*nb_BDD_completes)){
            
            tmp <- exp(matrix(exp(sum.model))%*%t(as.matrix(-1*RES$H0B[,i+1])))
            tmp[is.na(tmp)] <- 1
            pred.temp <- pred.temp + tmp
            
        }
        
        pred <- pred.temp/(nb_ech_bootstrap*nb_BDD_completes)
        
        
        table.temp <- data.frame(time = c(0,TIME$time), pred = c(1,pred[1,]))
        
        list(model = sum.model, table = table.temp)
        
    }
    
    
    graph <- function(X){
      
      graphic <- plot_ly(data = model_final(X)$table, x = ~ round(time,digits = 2), y = ~ round(pred,digits = 3)) %>%
        layout(title = "",
              xaxis = list(title = "Time (months)"),
              yaxis = list(title = "Free-progression survival")) %>% config(
        toImageButtonOptions = list(
          format = "svg",
          filename = "myplot",
          width = 600,
          height = 700))
      add_lines(graphic, line= list(shape = 'hv'))
    }
    
    x <- reactive(input$NMSD)
    NMSD1 <- reactive( if (x()=="1") 1 else 0 )
    NMSD2 <- reactive( if (x()=="2 or more") 1 else 0 )
    
    y <- reactive(input$MHL3)
    MHL3non <- reactive( if (y()=="No") 1 else 0 )
    MHL3isolee <- reactive( if (y()=="Isolated") 1 else 0 )
    
    X <- reactive(data.frame(Sexe = as.integer(input$SEXE), 
                             Age_L3 = input$AGE, 
                             log_SSP1 = log(input$SSP1), 
                             log_SSP2 = log(input$SSP2),
                             Resection.primitif = as.integer(input$RP),
                             Protocole_L1groupebis = as.integer(input$PL1),
                             Meta.pulm_L3 = as.integer(input$MPL3),
                             Carcinose_L3 = as.integer(input$CL3),
                             Nbre.de.site.metastatique_dg_1 = NMSD1(),
                             Nbre.de.site.metastatique_dg_2etplu = NMSD2(),
                             Meta.hep_L3_isolee = MHL3isolee(),
                             Meta.hep_L3_non = MHL3non()
    ))
    
    g <- reactive(
        graph(X = X())
    )
    
    output$KM <- renderPlotly({
        g()
    })
    
    MST <- reactive(rmst(times = model_final(X())$table$time, 
                    surv.rates = model_final(X())$table$pred, 
                    max.time = max(model_final(X())$table$time),
                    type = "s"))
    
    output$EV <- renderText(paste0("Expected life-time without progression = ", round(MST(), digits = 1), " month(s)"))
    
    output$png <- downloadHandler(
        filename = "Survival_PANCREAS_L3_plot.png",
        content = function(file){
            png(file)
            ggsave(file, plot = g(), device = "png")
        })
    observeEvent(input$btn_go, {
      shinyjs::show(id = "hiddenbox")
    })
    
#    data_pred <- reactive(data.frame(Time = format(model_final(X())$table$time, nsmall = 2, digits = 1), Survival = format(model_final(X())$table$pred, digits = 1, nsmall = 3)))
#    
#    output$table_pred <- renderDataTable({
#        datatable(data_pred(),
#                  rownames = FALSE,
#                  options = list(pageLength = length(TIME$time)+1,
#                                 scrollY = "250px",
#                                 dom= 't'),
#                  style = "bootstrap4")
#    })
#    
#    output$csv <- downloadHandler(
#        filename = "Survival_PANCREAS_L3",
#        content = function(file){
#            write.csv(data_pred(), file, row.names = FALSE)
#        })
    
}


shinyApp(ui, server)

