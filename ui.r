library("shiny")
library("shinyBS")
library("shinyWidgets")
#library("highcharter")
library("plotly")
library("rhandsontable")
library("shinyjs")
library("colourpicker")
library("plotly")
library(shinyMatrix)

#library("shinythemes")

shinyUI(
  navbarPage(
    title=HTML("<b style='color:black;'>The Chi-Squared Test</b>"),
    windowTitle="Chi-Squared Test",
    tabPanel("Test of Independence/Homogeneity",
    sidebarLayout(
      sidebarPanel(
        fluidRow(column(6, selectInput("how", "Enter Data:", 
                   list("From Textbook" = "textb", "Contingency Table"="cont", "Individual Observations"="ind"), 
                   selectize = FALSE)),
                 column(6, conditionalPanel(condition="input.how=='textb'", selectInput("dataset","Dataset:", 
                   choices=list(
                     "Airline Flight Status" = "airline",
                     "Internet Device" = "device",
                     "Income & Education Level" = "incomeedu",
                     "Happiness & Income"="happ",
                     "Political Party & Gender (2018)"="partyid",
                     "Happiness & Gender (2018)"="gen",
                     "Vaccine Headache"="head",
                     "Religiosity & Gender" ="rel",
                     "Alligator Food Choice"="alli",
                     "Diamond Rating & Clarity" = "diamonds"), selectize = FALSE)))
        ),
        conditionalPanel(condition="input.how=='cont' || input.how=='textb'",  
          textInput("varname1", "Row Variable Name:", value="", placeholder = "Income"),
          textInput("cat1", "Category Labels:", value="", placeholder = "Below Average, Average, Above Average"),
          bsTooltip(id="cat1", title="Separated by a comma", trigger = "focus"),
          textInput("varname2", "Column Variable Name:", value="", placeholder = "Happiness"),
          textInput("cat2", "Category Labels:", value="", placeholder = "Not Happy, Happy, Very Happy"),
          bsTooltip(id="cat2", title="Separated by a comma", trigger = "focus"),
          h5(tags$b("Enter Counts for Contingency Table:")),
          conditionalPanel(condition="input.how=='cont'", rHandsontableOutput("hot2")),
          conditionalPanel(condition="input.how=='textb'", rHandsontableOutput("hot2textb")),
          br()
        ),
        conditionalPanel(condition="input.how=='ind'",
          fluidRow(column(6, textInput("varname1_ind", "Name of 1st Variable:", value="", placeholder = "Income")),
                   column(6,textInput("varname2_ind", "Name of 2nd Variable:", value="", placeholder = "Happiness"))),
          fluidRow(column(12, h5(tags$b("Enter observations for each variable or copy and paste from a spreadsheet:"),
          actionLink("helphot3", label="", icon=icon("question-circle"))))),
          bsPopover("helphot3", title="Entering Data:", content="Copying and pasting works in the usual way. If you are entering data manually and have more than 50 observations, right-click on the last row to add more.", trigger="hover"),
          rHandsontableOutput("hot3"),
          br(),
          actionBttn("submit", "Submit", color='primary', style='simple', size='sm')
        ),
        h5(tags$u(tags$b("Options:"))),
        awesomeCheckbox("stack", "Stacked Barchart"),
        awesomeCheckbox("freq", "Show Counts on Barchart"),
        awesomeCheckbox("cond", "Show Conditional Distribution", TRUE),
        awesomeCheckbox("expect", "Show Expected Cell Counts"),
        awesomeCheckbox("res","Show Residuals"),
        awesomeCheckbox("stdres","Show Standardized Residuals"),
        downloadBttn("chiplot", "Download Chi-Square Distribution Plot", style='simple', size='xs')
      ), #end sidebarPanel second tab
      mainPanel(
        fluidRow(
          column(6, h5(HTML(" <u> <b> Observed Contingency Table: </b> </u>")),
                    #verbatimTextOutput("freqtab"),
                    tableOutput("freqtab"),
                    conditionalPanel('input.expect',   
                      h5(HTML(" <u> <b> Expected Cell Counts: </b> </u>")),
                      #verbatimTextOutput("expecttab")
                      tableOutput("expVal")
                    ),
                 conditionalPanel('input.cond',
                   h5(HTML(" <u> <b> Conditional Distribution (in %): </b> </u>")),   
                   tableOutput("condtab"))
          ),
          column(6, plotlyOutput("barchart", height=350))
        ),
        fluidRow(
          column(6, conditionalPanel('input.res',   
                                     h5(HTML(" <u> <b> Residuals (Observed - Expected): </b> </u>")),
                                     tableOutput("resid"))
          ),
          column(6, conditionalPanel('input.stdres',   
                                     h5(HTML(" <u> <b> Standardized Residuals: </b> </u>")),
                                     tableOutput("stdresid"))
          )
        ),
        h5(HTML(" <u> <b> Pearson's Chi-Squared Test:  </u> </b>")),
        tableOutput("X2test"),
        plotOutput("mytestplot", height=280)
      ) #end mainpanel
    ) #end sidebarLayout first tab
  ), #end tabPanel first tab
  tabPanel("Goodness of Fit",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, selectInput("how1", "Enter Data:", 
            list("From Textbook" = "textb", "Contingency Table"="cont", "Individual Observations"="ind"), 
            selectize = FALSE)),
          column(6, 
            conditionalPanel(condition="input.how1=='textb'", 
              selectInput("dataset1","Dataset:", 
                choices=list("Inheritance"="inherit",
                "Benford"="benford",
                "Birthdays"="birthday"
                )
              )
            ),
            conditionalPanel(condition="input.how1=='cont'",
              numericInput("ncat", "Number of Categories:", 4,2,12)
            ),
            conditionalPanel(condition="input.how1=='ind'",
              helpText("Enter or copy/paste individual observations in the box below.")
            )
          )
        ),
        conditionalPanel("input.how1=='cont' || input.how1=='textb' || input.how1=='ind'",
          conditionalPanel("input.how1=='cont'", 
            helpText("Enter category labels, counts and hypothesized proportions by clicking in the cells:")
          ),
          conditionalPanel(condition="input.how1=='ind'",
            textAreaInput("indobs", "Individual Categorical Observations:"),
            helpText("The table below shows the observed counts. Click on the cells in the last row to enter the hypothesized proportions, then press submit")
          ),
          uiOutput("inmatrix1"),
          conditionalPanel("input.how1=='cont' || input.how1=='ind'", 
            fluidRow(
              column(7, actionBttn("submit1", "Submit", color='primary', style='simple', size='sm')),
              column(5, materialSwitch("helpi", "Help"))
            ),
            conditionalPanel('input.helpi',
            helpText("The first row holds the category labels, which you can overwrite using your own labels. 
                   Enter observed counts for the categories by clicking into the cells of the second row.
                   Enter the population proportions for the categories under the null hypothesis by clicking in the cells of the third row.
                   (Make sure the proportions add up to 1; if they don't, they are scaled so that they add up to 1.)")
            ),
            br()
          )
        ),
        h5(tags$b("Options:")),
        awesomeCheckbox("stdres2","Show Standardized Residuals"),
        width=5
      ), #end sidebarPanel second tab
      mainPanel(
        tableOutput("obscounts"),
        tableOutput("X2test2"),
        plotOutput("mytestplot2"),
        width=7
      ) #end mainpanel
    ) #end sidebarLayout second tab
  ) #end tabPanel second tab
 )# end navbar    
) #endUI