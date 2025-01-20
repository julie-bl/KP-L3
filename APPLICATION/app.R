
library(shiny)
library(shinydashboard)
library(DT)
library(RISCA)
library(plotly)
library(shinyjs)
library(shinycssloaders)


#############################################################################################

#Survival

load("RES.RData")
table_resultats <- read.csv("table_resultats.csv")
tmp <- table_resultats$X
table_resultats <- data.frame(table_resultats[,-1])
row.names(table_resultats) <- tmp
rm(tmp)
TIME <- read.csv("TIME.csv")



#Free progression survival

load("RES_SP.RData")
table_resultats_SP <- read.csv("table_resultats_SP.csv")
tmp <- table_resultats_SP$X
table_resultats_SP <- data.frame(table_resultats_SP[,-1])
row.names(table_resultats_SP) <- tmp
rm(tmp)
TIME_SP <- read.csv("TIME_SP.csv")



nb_ech_bootstrap <- 1000
nb_BDD_completes <- 5

#############################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Prognosis of patients with unresecable pencreatic cancer from the third line treatment",
                  titleWidth = "100%"),
  dashboardSidebar(
    title = p(a(tags$img(src='Logo-Horizontal-CHU-Poitiers_Couleurs.jpg', width=230,align = "center"),
                target="_blank", href="https://www.chu-poitiers.fr", class="hidden-xs"),
              style="padding-left:0px !important"),
    sidebarMenu(
      menuItem("Calculator", tabName = "one"),
      menuItem("Read Me", tabName = "two")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "one",
              tabBox(tabPanel(h4("The patient characteristics",style = "color: #2874A6;")),
                     fluidRow(
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
                  radioButtons(inputId = "NMSD",
                               label = "Metastatic site(s) at diagnosis",
                               choiceNames = c("Yes","No"),
                               choiceValues = c(1, 0)),
                  sliderInput(inputId = "SSP1",
                              label = "Duration of the first line treatment in months",
                              value = 3,
                              min = 0.1,
                              max = 50,
                              step = 0.1),
                  radioButtons(inputId = "PL1",
                               label = "Protocol followed during the first line treatment",
                               choiceNames = c("Folfirinox", "Others"),
                               choiceValues = c(1, 0))
                  
                ),
                column(
                  width = 5,
                  fluidRow(
                    sliderInput(inputId = "SSP2",
                                label = "Duration of the second line treatment in months",
                                value = 2,
                                min = 0.1,
                                max = 50,
                                step = 0.1),
                    radioButtons(inputId = "ET",
                                 label = "Depletion of therapeutic resources at third line prescription",
                                 choiceNames = c("Yes", "No"),
                                 choiceValues = c(1,0)),
                    radioButtons(inputId = "ECOGPS",
                                 label = "ECOG PS at third line prescription",
                                 choiceNames = c("0-1", "≥2"),
                                 choiceValues = c(0,1)),
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
                  )
                )
                
                
              )
              ),
                
              tabBox(
                tabPanel(h4("Overall survival prediction",style = "color: #2874A6;"),
                         plotlyOutput("CURVE_Survival")%>% withSpinner(),
                         div(style = "height:50px"),
                         verbatimTextOutput("EV")
                ),
                tabPanel(h4("Free progression survival prediction",style = "color: #2874A6;"),
                         plotlyOutput("CURVE_SurvivalSP")%>% withSpinner(),
                         div(style = "height:50px"),
                         verbatimTextOutput("EV_SP"))
              )
      ),
      tabItem(tabName = "two",
              h5("In Evrard et al. (preprint available here), two multivariate models were proposed to predict of both 
                 the progression-free survival (PFS) and overall survival (OS)"),
              h5("The study was based on a French multicenter cohort constituted by French patients treated for 
                 unresectable pancreatic adenocarcinoma."),
              h5("The following factors contributing to the predictions: gender, age, surgery of the primary tumor, 
                 metastatic  site at diagnosis, Folfirinox as first-line therapy, durations of first and second-line 
                 treatments; and at third-line treatment: depletion therapeutic (defined as a patient having already 
                 received 5-fluoruracil, oxaliplatine, irinotecan, gemcitabine and taxane at the beginning of L3), 
                 ECOG PS level, liver and/or lung metastasis and carcinosis."),
              h5("It allowed for acceptable discrimination between event and event-free patients at 6 months post 
                 third-line initiation (area under the ROC curve of 0.83 [95%CI: 0.75 - 0.90] for the PFS and 
                 0.73 [95%CI: 0.65 - 0.81] for the OS)."),
              h5("The related online calculator is available via this web-based application. It could help in informing 
                 both the physician and patient of the disease prognosis.")
              )
    ),
    
  )
)


server <- function(input, output){
  
  model_final <- function(X,type=1){
    
    model <- X
    
    if(type==1){
      for(cov in names(X)){
        
        model[,cov] <- X[,cov]*table_resultats[cov,"AVG"]
        RES_tmp <- RES
        TIME_tmp <- TIME
        
      }
    }
    
    if(type==2){
      for(cov in names(X)){
        
        model[,cov] <- X[,cov]*table_resultats_SP[cov,"AVG"]
        RES_tmp <- RES_SP
        TIME_tmp <- TIME_SP
      }
    }
    
    sum.model <- rowSums(model, na.rm = TRUE)
    
    pred.temp <- 0
    
    for(i in 1:(nb_ech_bootstrap*nb_BDD_completes)){
      
      tmp <- exp(matrix(exp(sum.model))%*%t(as.matrix(-1*RES_tmp$H0B[,i+1])))
      tmp[is.na(tmp)] <- 1
      pred.temp <- pred.temp + tmp
      
    }
    
    pred <- pred.temp/(nb_ech_bootstrap*nb_BDD_completes)
    
    
    table.temp <- data.frame(time = c(0,TIME_tmp$time), pred = c(1,pred[1,]))
    
    list(model = sum.model, table = table.temp)
    
  }
  
  
  graph <- function(X,type=1){
    
    if(type==1){
      t <- "Overall survival"
    }
    
    if(type==2){
      t <- "Free-progression survival"
    }
    
    graphic <- plot_ly(data = model_final(X,type)$table, x = ~ round(time,digits = 2), y = ~ round(pred,digits = 3)) %>%
      layout(
             xaxis = list(title = "Time (months)"),
             yaxis = list(title = t)) %>% config(
               toImageButtonOptions = list(
                 format = "svg",
                 filename = "myplot",
                 width = 600,
                 height = 700))
    add_lines(graphic, line= list(shape = 'hv'))
    
  }
  
  
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
                           Nbre.de.site.metastatique_dg = as.integer(input$NMSD),
                           Meta.hep_L3_isolee = MHL3isolee(),
                           Meta.hep_L3_non = MHL3non(),
                           Epuisement.therap = as.integer(input$ET),
                           OMS_L3.regroupe = as.integer(input$ECOGPS)
                           
  ))
  
  
  #SURVIVAL
  
  g_Survival <- reactive(
    graph(X = X(), type=1)
  )
  
  output$CURVE_Survival <- renderPlotly({
    g_Survival()
  })
  
  MST <- reactive(rmst(times = model_final(X(),type=1)$table$time,
                       surv.rates = model_final(X(),type=1)$table$pred,
                       max.time = max(model_final(X(),type=1)$table$time),
                       type = "s"))
  
  output$EV <- renderText(paste0("Life expectancy = ", round(MST(), digits = 1), " month(s)", "\n", "\n",
                                 "Overall survival at 3 months = ", round(model_final(X(),type=1)$table[model_final(X(),type=1)$table$time == TIME[50,"time"],"pred"], digits = 3), "\n", "\n",
                                 "Overall survival at 6 months = ", round(model_final(X(),type=1)$table[model_final(X(),type=1)$table$time == TIME[94,"time"],"pred"], digits = 3), "\n", "\n",
                                 "Overall survival at 12 months = ", round(model_final(X(),type=1)$table[model_final(X(),type=1)$table$time == TIME[136,"time"],"pred"], digits = 3)))
  
  
  
  #FREE PROGRESSION SURVIVAL
  
  g_SurvivalSP <- reactive(
    graph(X = X(), type=2)
  )
  
  output$CURVE_SurvivalSP <- renderPlotly({
    g_SurvivalSP()
  })
  
  MST_SP <- reactive(rmst(times = model_final(X(),type = 2)$table$time,
                          surv.rates = model_final(X(),type = 2)$table$pred,
                          max.time = max(model_final(X(),type = 2)$table$time),
                          type = "s"))
  
  output$EV_SP <- renderText(paste0("Life expectancy without progression = ", round(MST_SP(), digits = 1), " month(s)", "\n", "\n",
                                    "Free-progression survival at 3 months = ", round(model_final(X(),type = 2)$table[model_final(X(),type = 2)$table$time == TIME_SP[63,"time"],"pred"], digits = 3), "\n", "\n",
                                    "Free-progression survival at 6 months = ", round(model_final(X(),type = 2)$table[model_final(X(),type = 2)$table$time == TIME_SP[89,"time"],"pred"], digits = 3), "\n", "\n",
                                    "Free-progression survival at 12 months = ", round(model_final(X(),type = 2)$table[model_final(X(),type = 2)$table$time == TIME_SP[106,"time"],"pred"], digits = 3)))
  
  observeEvent(input$btn_go, {
    shinyjs::show(id = "hiddenbox")
  })
}


shinyApp(ui, server)