########################################################################
## FILENAME    : ui.R
## DEVELOPER   : Richard Bryant (bryanri3)
## DATE        : 17-Jul-2020
## R           : R-3.4.3
## PLATFORM    : GNU/Linux
## PROTOCOL    : NA
## DESCRIPTION : App code for LEE-PRO (many studies)
########################################################################

library(shiny)
library(shinythemes)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(survival)
library(GGally)

library(htmlwidgets)
library(DT)
library(data.table)
library(plotly)
library(gridExtra)
library(stringr)

options(datatable.showProgress=T)

fluidPage(
  
  theme = shinytheme("journal"),
  # Application title
  titlePanel("BC-PRO App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "study",
        label = "Study",
        choices = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Study 6")
      ),
      
      fluidRow(
        column(12, uiOutput("varSel"),
               uiOutput("valueSel"),
               uiOutput("singleSelection"),
               uiOutput("SliderRng"))
      ),
      
      uiOutput("paraSelect"),
      uiOutput("compvarm"),
      
      fluidRow( 
        column(4, uiOutput("compvarm_yn1") ),
        column(4, uiOutput("compvarm_sw1") )
      ),
      
      uiOutput("compvarm_sd1"),
      
      fluidRow( 
        column(7, uiOutput("compvarm_gr1") ),
        column(3, uiOutput("compvarm_tx1") ),
        column(1, uiOutput("compvarm_keep1") )
      ),
      
      uiOutput("compvarm_yn2"),
      uiOutput("compvarm_sd2"),
      
      fluidRow(
        column(8, uiOutput("compvarm_gr2") ),
        column(4, uiOutput("compvarm_tx2") )
      ),
      
      width = 3
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("PRO",
          fluidRow(
            column(4, uiOutput("proParam1")),
            column(4, selectInput(inputId = "proLevel",
                                    label = "Level",
                                    choices = c("Overall", "Individual"))),
            column(4, uiOutput("proSelector"))
          ),
          fluidRow(
            column(6, plotOutput("plotPro")),
            column(6, plotOutput("plotProLine"))
          ),
          fluidRow(
            column(6, plotOutput("plotProChg")),
            column(6, plotOutput("plotProPctChg"))
          )
        ),
        
        tabPanel("Efficacy",
           fluidRow(
             splitLayout(cellWidths = c("50%", "50%"),  plotOutput("Forest"),plotOutput("varPlot"))
           ),
           br(),
           br(),
           plotOutput("kmPlot"),
           br(),
           br(),
           verbatimTextOutput(outputId = "kmText")
        ),
        
        tabPanel("Safety",
            checkboxInput("aesiOnly", label = "Show AESI only", value = FALSE),
            plotOutput("aesiPlot"),
            DT::dataTableOutput('aesiTab')
        ),
        
        tabPanel("Bubble Plot",
            fluidRow(
              column(6, uiOutput("proDeter")),
              column(6, uiOutput("socOption"))
            ),
            plotOutput("bubblePlot")
        ),
        
        tabPanel("Heatmap",
           fluidRow(
             column(6, uiOutput("proParam2")),
             column(6, selectInput(inputId = "sortingOption",
                                    label = "Sort by:",
                                    choices = c("OS", "PFS", "AESI"),
                                    selected = "OS"))
           ),
           plotOutput("heatmap")
        )
      )
    )
  )
)
