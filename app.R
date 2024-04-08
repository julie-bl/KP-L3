library(readxl)
library(ggplot2)
library(shiny)
library(shinydashboard)

table_resultats <- read_excel("J:/4-Méthodologiste/Antoine/pancreas/Resultats/table_resultats.xlsx")
nb_ech_bootstrap <- 1000
nb_BDD_completes <- 5
TIME <- data.frame(time = c(0.1973684,  0.2960526,  0.3618421,  0.3947368,  0.4605263,  0.4934211,  0.5592105,  0.6250000,  0.7565789,
                            0.7894737,  0.8552632,  0.9210526,  1.0197368,  1.0526316,  1.0855263,  1.1513158,  1.1842105,  1.2171053,
                            1.2500000,  1.3157895,  1.3815789,  1.4144737,  1.4473684,  1.4802632,  1.5131579,  1.5460526,  1.5789474,
                            1.6118421,  1.6447368,  1.6776316,  1.7105263,  1.7763158,  1.8092105,  1.8421053,  1.9078947,  1.9407895,
                            1.9736842,  2.0065789,  2.0723684,  2.1052632,  2.1381579,  2.2039474,  2.2368421,  2.2697368,  2.3026316,
                            2.3355263,  2.3684211,  2.4013158,  2.4342105,  2.4671053,  2.5000000,  2.5328947,  2.5657895,  2.6644737,
                            2.6973684,  2.7302632,  2.7631579,  2.7960526,  2.8618421,  2.8947368,  2.9276316,  2.9605263,  2.9934211,
                            3.0263158,  3.0921053,  3.1907895,  3.2894737,  3.4539474,  3.5855263,  3.7500000,  3.9473684,  3.9802632,
                            4.1118421,  4.1447368,  4.4407895,  4.4736842,  4.5065789,  4.6381579,  4.7039474,  4.9013158,  5.0657895,
                            5.2302632,  5.4605263,  5.4934211,  5.5921053,  5.6578947,  5.7565789,  5.7894737,  5.8223684,  6.2171053,
                            6.2828947,  6.4473684,  6.4802632,  6.6776316,  6.7105263,  6.8421053,  6.9407895,  7.0394737,  7.5328947,
                            7.6315789,  7.8289474,  8.3223684,  8.8486842,  9.6381579, 10.3289474, 10.9539474, 12.3026316, 13.4539474,
                            14.0460526, 14.8684211, 18.6184211, 22.8289474, 48.6184211))
load("J:/4-Méthodologiste/Antoine/pancreas/Resultats/RES.RData")


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
    
    median <- table.temp[table.temp$pred==max(table.temp[table.temp$pred<=0.5,]$pred),]$time
    
    list(model = sum.model, table = table.temp, median = median)
    
}


graph <- function(X, median){
    
    if(median == TRUE){
        
        ggplot(model_final(X)$table, aes(x = time, y = pred)) +
            geom_step(color ="dodgerblue1" )+
            labs(
                x = "Survival time (months)",
                y = "Progression-free survival") +
            geom_segment(aes(x=model_final(X)$median, xend=model_final(X)$median, y=-Inf,  yend= 0.5),  linetype=2) +
            geom_segment(aes(x=-Inf, xend=model_final(X)$median, y=0.5, yend=0.5),  linetype=2) +
            theme_minimal() +
            ylim(c(0,1))
        
    }else{
        
        ggplot(model_final(X)$table,aes(x = time, y = pred)) +
            geom_step(color ="dodgerblue1" )+
            labs(
                x = "Survival time (months)",
                y = "Progression-free survival") + 
            theme_minimal() +
            ylim(c(0,1))
        
    }
    
}

ui <- dashboardPage(
    dashboardHeader(title = "Survie L3 3 mois"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            
            box(title = "Characteristics",
                width = 2,
                radioButtons(inputId = "SEXE",
                             label = "Sex",
                             choiceNames = c("M", "F"),
                             choiceValues = c(1, 0)),
                sliderInput(inputId = "AGE",
                            label = "Age",
                            min = 0,
                            max = 110,
                            value = 30),
                numericInput(inputId = "SSP1",
                             label = "Free progression survival during the first line treatment",
                             value = 3,
                             min = 0,
                             max = 40,
                             step = 0.1,
                             width = 75),
                numericInput(inputId = "SSP2",
                             label = "Free progression survival during the second line treatment",
                             value = 2,
                             min = 0,
                             max = 40,
                             step = 0.1,
                             width = 75),
                radioButtons(inputId = "RP",
                             label = "Primary resection",
                             choiceNames = c("Yes", "No"),
                             choiceValues = c(1, 0)),
                radioButtons(inputId = "PL1",
                             label = "Grouped during the first line treatment protocol",
                             choiceNames = c("Folfirinox", "Others"),
                             choiceValues = c(1, 0)),
                radioButtons(inputId = "MPL3",
                             label = "Lung metastasis during the third line treatment",
                             choiceNames = c("Yes", "No"),
                             choiceValues = c(1, 0)),
                radioButtons(inputId = "CL3",
                             label = "Carcinosis during the third line treatment",
                             choiceNames = c("Yes", "No"),
                             choiceValues = c(1,0)),
                selectInput(inputId = "NMSD",
                            label = "Number of metastatic sites at diagnosis",
                            choices = c("0","1","2 or more")),
                selectInput(inputId = "MHL3",
                            label = "Hepatic metastasis during the third line treatment",
                            choices = c("No","Isolated","With others"))
                
            ),
            
            box(title = "Predicted survival curve",
                plotOutput("KM")),
            #box(title = "Esperance de vie",
            #    ),
            box(radioButtons(inputId = "MED",
                             label = "Show median survival",
                             choiceNames = c("Yes", "No"),
                             choiceValues = c(TRUE, FALSE)))
            
        )
    )
)

server <- function(input, output){
    
    x <- reactive(input$NMSD)
    NMSD1 <- reactive( if (x()=="1") 1 else 0 )
    NMSD2 <- reactive( if (x()=="2 or more") 1 else 0 )
    
    y <- reactive(input$MHL3)
    MHL3non <- reactive( if (y()=="No") 1 else 0 )
    MHL3isolee <- reactive( if (y()=="Isolated") 1 else 0 )
    
    g <- reactive(
        graph(X = data.frame(Sexe = as.integer(input$SEXE), 
                             Age_L3 = input$AGE, 
                             log_SSP1 = log(input$SSP1), 
                             log_SSP2 = log(input$SSP2),
                             Resection.primitif = as.integer(input$RP),
                             Protocole_L1groupebis = as.integer(input$PL1),
                             Meta.pulm_L3 = as.integer(input$MPL3),
                             Carcinose_L3 = as.integer(input$CL3),
                             Nbre.de.site.metastatique_dg_1 = NMSD1(),
                             Nbre.de.site.metastatique_dg_2etplu = NMSD2(),
                             Meta.hep_L3_isolée = MHL3isolee(),
                             Meta.hep_L3_non = MHL3non()
        ),
        median = input$MED)
    )
    
    output$KM <- renderPlot({
        g()
    })
    
    
    
    
}


shinyApp(ui, server)
