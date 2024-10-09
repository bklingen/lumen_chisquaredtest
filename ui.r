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
library("prompter")
#library("shinythemes")

shinyUI(
  navbarPage(
    title=HTML("<b style='color:black;'>The Chi-Squared Test</b>"),
    header = tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      .rounded-corners {
        border-radius: 8px;
      }
      .column1 {
        float: left;
        width: 100px;
        padding: 2px;
        margin-left: 15px;
        margin-bottom: 6px;
      }
      .column2 {
        float: left;
        width: 130px;
        margin-top: 4px;
        padding: 2px;
      }
      .image2 {
         margin-top: 5px;
      }
      .custom-hr {
      border: 0;
      border-top: 1px solid #808080;
      margin: 10px 0;
      }
      
      "))
    ),
    windowTitle="Chi-Squared Test",
    tabPanel("Test of Independence or Homogeneity",
    sidebarLayout(
      sidebarPanel(
        fluidRow(column(6, selectInput("how", "Enter Data:", 
                   list("From Textbook" = "textb", "Upload CSV File" = "upload", "Contingency Table"="cont", "Individual Observations"="ind"), 
                   selectize = FALSE)),
                 column(6, 
                     conditionalPanel(condition="input.how=='textb'", selectInput("dataset","Dataset:", 
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
                       "Diamond Rating & Clarity" = "diamonds"), selectize = FALSE)
                     ),
                     conditionalPanel(condition="input.how=='upload'",
                                      fileInput("file", 
                                                label = tags$span(
                                                  "Choose CSV File:", 
                                                  tags$span(
                                                    icon(
                                                      name = "question-circle",
                                                    ) 
                                                  ) %>%
                                                    add_prompt(message = "File needs to contain two columns that correspond to two categorical variables.", position = "right")
                                                ), 
                                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                     )
                  )
        ),
        conditionalPanel(condition = "input.how == 'upload'", 
                         # helpText("File needs to be in long format, containing one column with the variable of interest, and another identifying group membership."),
                         tags$b("Uploaded Dataset:"),
                         rHandsontableOutput("hotUpload"),
                         br(),
                         fluidRow(
                           column(6, selectInput("var1", "Row Variable:", choices=NULL, selectize=FALSE)),
                           column(6, selectInput("var2", "Column Variable:", choices=NULL, selectize=FALSE))
                         )
        ),
        conditionalPanel(condition="input.how=='cont' || input.how=='textb'",  
          fluidRow(
            column(6, textInput("varname1", "Row Variable Name:", value="", placeholder = "Income")),
            column(6, textInput("cat1", "Category Labels:", value="", placeholder = "Below Average, Average, Above Average"),
                      bsTooltip(id="cat1", title="Separate labels by a comma", trigger = "focus"))
          ),
          fluidRow(
            column(6, textInput("varname2", "Column Variable Name:", value="", placeholder = "Happiness")),
            column(6, textInput("cat2", "Category Labels:", value="", placeholder = "Not Happy, Happy, Very Happy"),
                      bsTooltip(id="cat2", title="Separate labels by a comma", trigger = "focus"))
          ),
          h5(tags$b("Enter Counts for Contingency Table:")),
          conditionalPanel(condition="input.how=='cont'", rHandsontableOutput("hot2")),
          conditionalPanel(condition="input.how=='textb'", rHandsontableOutput("hot2textb")),
          br()
        ),
        conditionalPanel(condition="input.how=='ind'",
          fluidRow(column(6, textInput("varname1_ind", "Name of 1st Variable:", value="", placeholder = "Income")),
                   column(6, textInput("varname2_ind", "Name of 2nd Variable:", value="", placeholder = "Happiness"))),
          fluidRow(column(12, h5(tags$b("Enter observations for each variable or copy and paste from a spreadsheet:"),
          actionLink("helphot3", label="", icon=icon("question-circle"))))),
          bsPopover("helphot3", title="Entering Data:", content="Copying and pasting works in the usual way. If you are entering data manually and have more than 50 observations, right-click on the last row to add more.", trigger="hover"),
          rHandsontableOutput("hot3"),
          br(),
          actionBttn("submit", "Submit", color='primary', style='simple', size='sm')
        ),
        h5(tags$u(tags$b("Options:"))),
        fluidRow(
          column(6, awesomeCheckbox("stack", "Stacked Barchart")),
          column(6, awesomeCheckbox("freq", "Show Counts on y-Axis"))
        ),
        fluidRow(
          column(6, awesomeCheckbox("cond", "Conditional Distribution", TRUE)),
          column(6, awesomeCheckbox("expect", "Expected Cell Counts"))
        ),
        pickerInput(
          inputId = "res",              # Input ID
          label = "Select Type of Residual to Display:",        # Label above the dropdown
          choices = list("Raw Residual" = 1,    # Choices for the dropdown
                         "Standardized (Pearson) Residual" = 2, 
                         "Adjusted Standardized (Pearson) Residual" = 3),
          multiple = TRUE                  # Allow multiple selections
        ),
        # h5(tags$u("Residuals:")),
        # fluidRow(
        #   column(4, awesomeCheckbox("res","Raw")),
        #   column(4, awesomeCheckbox("stdres", 
        #                             label = tags$span(
        #                               "Standardized",
        #                               tags$span(
        #                                 icon(
        #                                   name = "question-circle",
        #                                 ) 
        #                               ) %>%
        #                                 add_prompt(message = "File ne.", position = "right")
        #                             ))
        #   ),
        #   column(4, awesomeCheckbox("adj", "Adjusted"))
        # ),
        
        downloadButton("chiplot", "Download Chi-Square Distribution Plot", style = "font-size: 12px; padding: 2px 10px;"),
        tags$hr(class = "custom-hr"),
        h5(tags$b("Available on Mobile:")),
        div(class="row",
            div(class="column1",
                a(img(src="app-inference.png", width = "85px", class = "rounded-corners"), 
                  href='https://artofstat.com/mobile-apps', 
                  target="_blank"),
            ),
            # div(class="column2",
            #     a(img(src="AppStoreLogoApple.png", width="125px"), 
            #       href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757?platform=iphone', 
            #       target="_blank"),
            #     br(),
            #     a(img(src="AppStoreLogoAndroid1.png",width="125px",class="image2"), 
            #       href='https://play.google.com/store/apps/details?id=com.artofstat.exploredata', 
            #       target="_blank"
            #     ),
            # )
        ),
        tags$p(
          "More information ",
          tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank")
        ),
        # h5(tags$b("Check out our textbook:")),
        # a(img(src='textbookFullCover.png', width="150px"), href='http://www.artofstat.com'),     
      ), #end sidebarPanel second tab
      mainPanel(
        fluidRow(
          column(7, h5(HTML(" <u> <b> Observed Contingency Table: </b> </u>")),
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
          column(5, plotlyOutput("barchart", height=350))
        ),
        fluidRow(
          column(6, conditionalPanel("input.res.indexOf('1') > -1",   
                                     h5(HTML(" <u> <b> Raw Residuals: Obs - Exp </b> </u>")),
                                     tableOutput("resid"))
          ),
          column(6, conditionalPanel("input.res.indexOf('2') > -1",   
                                     h5(HTML(" <u> <b> Standardized (Pearson) Residuals: (Obs - Exp) / √Exp </b> </u>")),
                                     tableOutput("stdresid"))
          )
        ),
        fluidRow(
          column(6, ),
          column(6, conditionalPanel("input.res.indexOf('3') > -1",   
                                     h5(HTML(" <u> <b> Adjusted Standardized Residuals: </b> </u>")),
                                     tableOutput("adjresid"))
          )
        ),
        
        h5(HTML(" <u> <b> Pearson's Chi-Squared Test:  </u> </b>")),
        tableOutput("X2test"),
        plotOutput("mytestplot", height=280)
      ) #end mainpanel
    ) #end sidebarLayout first tab
  ), #end tabPanel first tab
  tabPanel("Goodness of Fit Test for a Single Categorical Variable",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, selectInput("how1", "Enter Data:", 
            list("From Textbook" = "textb", "Upload CSV File" = "upload", "Contingency Table"="cont", "Individual Observations"="ind"), 
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
              helpText("Enter individual observations, separated by comma, in the box below, or copy/paste values from a spreadsheet")
            ),
            conditionalPanel(condition="input.how1=='upload'",
                             fileInput("file1", 
                                       label = tags$span(
                                         "Choose CSV File:", 
                                         tags$span(
                                           icon(
                                             name = "question-circle",
                                           ) 
                                         ) %>%
                                           add_prompt(message = "File needs to contain a column with the values of a categorical variable.", position = "right")
                                       ), 
                                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
            )
          )
        ),
        conditionalPanel("input.how1=='cont'", 
          helpText("Enter category labels, counts and hypothesized proportions by clicking in the corresponding cells:")
        ),
        conditionalPanel(condition="input.how1=='ind' || input.how1=='upload'",
          conditionalPanel(condition="input.how1=='ind'",
            textAreaInput("indobs", "Categorical Observations:", placeholder="Green, Green, Yellow, Green, Yellow, Yellow, Green, Yellow, Yellow")
          ),
          conditionalPanel(condition = "input.how1 == 'upload'", 
                           # helpText("File needs to be in long format, containing one column with the variable of interest, and another identifying group membership."),
                           tags$b("Uploaded Dataset:"),
                           rHandsontableOutput("hotUpload1"),
                           br(),
                           selectInput("var", "Select Variable to Analyze:", choices=NULL, selectize=FALSE)
          ),
          helpText("The table below shows the observed counts. Click on the cells in the last row to enter the hypothesized proportions, then press submit.")
        ),
        uiOutput("inmatrix1"),
        conditionalPanel("input.how1=='cont' || input.how1=='ind' || input.how1=='upload'", 
          fluidRow(
            column(7, actionBttn("submit1", "Submit", color='primary', style='simple', size='sm')),
            column(5, materialSwitch("helpi", "Help"))
          ),
          conditionalPanel('input.helpi',
          helpText("The first row holds the category labels, which you can overwrite using your own labels. 
                 Enter observed counts for the categories by clicking into the cells of the second row.
                 Enter the population proportions for the categories under the null hypothesis by clicking in the cells of the third row.
                 (Make sure the proportions add up to 1; if they don't, they are scaled so that they will add up to 1.)")
          ),
          br()
        ),
        h5(tags$b("Options:")),
        awesomeCheckbox("stdres1","Show Standardized (Pearson) Residuals"),
        awesomeCheckbox("adjres1","Show Adjusted Standardized Residuals"),
        tags$hr(class = "custom-hr"),
        h5(tags$b("Available on Mobile:")),
        div(class="row",
            div(class="column1",
                a(img(src="app-inference.png", width = "85px", class = "rounded-corners"), 
                  href='https://artofstat.com/mobile-apps', 
                  target="_blank"),
            ),
            # div(class="column2",
            #     a(img(src="AppStoreLogoApple.png", width="125px"), 
            #       href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757?platform=iphone', 
            #       target="_blank"),
            #     br(),
            #     a(img(src="AppStoreLogoAndroid1.png",width="125px",class="image2"), 
            #       href='https://play.google.com/store/apps/details?id=com.artofstat.exploredata', 
            #       target="_blank"
            #     ),
            # )
        ),
        tags$p(
          "More information ",
          tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank")
        ),
        # h5(tags$b("Check out our textbook:")),
        # a(img(src='textbookFullCover.png', width="150px"), href='http://www.artofstat.com'),     
        
        width=5
      ), #end sidebarPanel second tab
      mainPanel(
        tableOutput("obscounts"),
        tableOutput("X2test2"),
        plotOutput("mytestplot2", height=280),
        width=7
      ) #end mainpanel
    ) #end sidebarLayout second tab
  ) #end tabPanel second tab
 )# end navbar    
) #endUI