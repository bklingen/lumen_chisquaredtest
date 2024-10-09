library(ggplot2)
#library(grid)
library("RColorBrewer")
library("stringr")
#library("highcharter")
library("plotly")
#library("rlang")
library("shinyjs")
library("rhandsontable")
library("readr")
library("shinyMatrix")
options(scipen=999)

mycol <- c(brewer.pal(12, "Set3")[c(7,12,5,4,6,3,1,10,2,9,8,11)], brewer.pal(8, "Accent"), brewer.pal(8, "Set2"), brewer.pal(9, "Pastel1"),brewer.pal(8, "Pastel2"))
mycol1 <- c(brewer.pal(9, "Pastel1"), brewer.pal(8, "Set2"), brewer.pal(8, "Accent"), brewer.pal(12, "Set3"),brewer.pal(8, "Set1")) 
#c(brewer.pal(8, "Pastel1"), colours()[seq(2,151,5)]) #brewer.pal(8, "Set2")[c(5,6,7,4,3,2,1)]) #brewer.pal(8, "YlOrRd")[c(2,4,6,8,1,3,5,7)] #brewer.pal(8, "Accent")#[c(3,2,7,4,6,5,1,8)]
mycol2 <- brewer.pal(8, "BrBG")[4]  # "#CC9933"  #brewer.pa l(8, "YlOrRd")[2]
addspace <- 2 #for adding space to columns of table for better rendering

X2 <- function(a,b) {  #computes X2 statistic for long data
  tab <- table(a,b)
  mr <- rowSums(tab)
  mc <- colSums(tab)
  n <- sum(tab)
  exp <- outer(mr,mc)/n
  sum((tab-exp)^2/exp)
}

PvalRound <- function(p) {
  p0 = unlist(p)
  if(is.na(p0)) return(NA)
  if(!is.finite(p0)) return(NA)
  if(p0 > 0.9999) return("1.000")
  else if(p0 <= 0.0001) return("< 0.0001")
  else return(format(round(p0 + 10^(-12), 4), nsmall=4))
}

shinyServer(function(input, output, session) {

rowlabels <- reactive({
  if(input$varname1=="") varn1 <- "Var1" else varn1 <- input$varname1
  if(input$cat1=="") labels1 <- c("Row1", "Row2", "Row3") else labels1 <- str_trim(unlist(strsplit(input$cat1,",")))
  if(length(labels1)<2) labels1 <- c(labels1, " ")
  return(list(name=varn1,lab=labels1))
})
  
collabels <- reactive({
  if(input$varname2=="") varn2 <- "Var2" else varn2 <- input$varname2
  if(input$cat2=="") labels2 <- c("Col1", "Col2", "Col3") else labels2 <- str_trim(unlist(strsplit(input$cat2,",")))
  if(length(labels2)<2) labels2 <- c(labels2, " ")
  return(list(name=varn2,lab=labels2))
})

observeEvent(list(input$how), {
  if(input$how == 'cont') {
    updateTextInput(session, 'varname1', value="", placeholder="Var1")
    updateTextInput(session, 'varname2', value="", placeholder="Var2")
    updateTextInput(session, 'cat1', value="", placeholder="Row1, Row2, Row3")
    updateTextInput(session, 'cat2', value="", placeholder="Col1, Col2, Col3")
  }
}, ignoreInit = TRUE)

mytable <- reactiveValues(DF=NULL, condDF=NULL, chart=NULL)

uploadedData = reactiveVal(NULL)

observeEvent(list(input$file), {
  myfile <- input$file
  if(is.null(myfile)) {
    uploadedData(NULL)
  } else {
    df <- read_csv(myfile$datapath)
    #numeric_columns_index <- sapply(df, function(x) is.numeric(x))
    coln <- colnames(df)
    #mychoices <- as.list(c("", coln[numeric_columns_index]))
    #names(mychoices) <- c("Choose One", coln[numeric_columns_index])
    mychoicesAll <- as.list(c("", coln))
    names(mychoicesAll) <- c("Choose One", coln)
    updateSelectInput(session, "var1", choices=mychoicesAll, selected=mychoicesAll[1])
    updateSelectInput(session, "var2", choices=mychoicesAll, selected=mychoicesAll[1])
    uploadedData(df)
  }
},ignoreInit = TRUE)

observeEvent(list(input$var1, input$var2), {
  if(input$how != 'upload') return(NULL)
  df <- req(uploadedData())
  if(nrow(df > 0) > 0) {
    var1 <- req(input$var1)
    var2 <- req(input$var2)
    if(var1 == var2) {
      showModal(modalDialog(
        title = "Attention!",
        "You selected the same variable twice. Please choose two different variables from your dataset.",
        easyClose = FALSE,  # Allows closing by clicking outside the popup
        footer = modalButton("Close")  # Add a close button
      ))
      updateSelectInput(session,"var2", selected = '')
      updateSelectInput(session,"var1", selected = '')
      return(NULL)
    }
    df <- df %>% dplyr::select(all_of(c(var1, var2))) %>% na.omit() %>% mutate(across(everything(), as.factor))
    #colnames(df) = c("var1", "var2")
    updateTextInput(session, 'varname1', value=var1)
    updateTextInput(session, 'varname2', value=var2)
    updateTextInput(session, 'cat1', value=paste(levels(df$var1), collapse=", "))
    updateTextInput(session, 'cat2', value=paste(levels(df$var2), collapse=", "))
    mytable$DF <- table(df)
  } else {
    updateTextInput(session, 'varname1', value="", placeholder="Var1")
    updateTextInput(session, 'varname2', value="", placeholder="Var2")
    updateTextInput(session, 'cat1', value="", placeholder="Row1, Row2, Row3")
    updateTextInput(session, 'cat2', value="", placeholder="Col1, Col2, Col3")
  }
}, ignoreInit = TRUE)

output$hotUpload <- renderRHandsontable({
  if(input$how != "upload") return(NULL)
  DF <- uploadedData()
  if (is.null(DF)) {
    return(NULL)
  }
  mytab <-  rhandsontable(DF, rowHeaders=NULL, height=160, readOnly = TRUE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    # hot_cols(type='numeric', manualColumnResize=TRUE
    # , renderer = 
    #       "function(instance, td, row, col, prop, value, cellProperties) {
    #       Handsontable.renderers.TextRenderer.apply(this, arguments);
    #       td.style.color = 'black';
    #       }"
    # ) %>%
    hot_table(stretchH="all", colHeaders = TRUE, columnSorting = TRUE) 
  return(mytab)
})


output$hot2 <- renderRHandsontable({
  if(input$how != "cont") return(NULL) 
  rlabs <- req(rowlabels()$lab)
  clabs <- req(collabels()$lab)
  r <- length(rlabs)
  c <- length(clabs)
  if (is.null(input$hot2))  DF <- matrix(NA_integer_, r, c)
  else DF <- hot_to_r(input$hot2)
  if(nrow(DF)<r) DF <- rbind(DF, matrix(NA_integer_, ncol=c, nrow=r-nrow(DF)))
  if(nrow(DF)>r) DF <- DF[1:r,]
  if(ncol(DF)<c) DF <- cbind(DF, matrix(NA_integer_, nrow=r, ncol=c-ncol(DF)))
  if(ncol(DF)>c) DF <- DF[,1:c]
  colnames(DF) <- clabs
  rownames(DF) <- rlabs
  mytable$DF <- DF
  mytab <-  rhandsontable(DF, rowHeaderWidth = 75) %>%
    hot_col(col=1:dim(DF)[2], type="numeric", format='0,0') %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(manualColumnResize=TRUE) %>%
    hot_table(stretchH="all") #doesn't render nicely witrhin wellpanel
  return(mytab)
}) # %>% debounce(600)
outputOptions(output, "hot2", priority = 12)

datatextb <- eventReactive(list(input$how, input$dataset), {
  switch(input$dataset,
    "happ" = {
      updateTextInput(session, "varname1", value="Income")
      updateTextInput(session, "cat1", value="Above Average, Average, Below Average")
      updateTextInput(session, "varname2", value="Happiness")
      updateTextInput(session, "cat2", value="Not Happy, Pretty Happy, Very Happy")
      m <- matrix(c(29,83,104,178,494,314,135,277,119), ncol=3)
      rownames(m) <- c("Above Average", "Average", "Below Average")
      colnames(m) <- c("Not Happy", "Pretty Happy", "Very Happy")
      names(attr(m,"dimnames")) <- c("Income", "Happiness")
    },
    "gen" = {
      updateTextInput(session, "varname1", value="Gender")
      updateTextInput(session, "cat1", value="Female, Male")
      updateTextInput(session, "varname2", value="Happiness")
      updateTextInput(session, "cat2", value="Not Happy, Pretty Happy, Very Happy")
      m <- matrix(c(180, 156, 724,583,390,311), ncol=3)
      rownames(m) <- c("Female", "Male")
      colnames(m) <- c("Not Happy", "Pretty Happy", "Very Happy")
      names(attr(m,"dimnames")) <- c("Gender", "Happiness")
    },
    "partyid" = {
      updateTextInput(session, "varname1", value="Gender")
      updateTextInput(session, "cat1", value="Female, Male")
      updateTextInput(session, "varname2", value="Party Identification")
      updateTextInput(session, "cat2", value="Democrat, Independent, Republican")
      m <- matrix(c(439,292,525,455,275,252), ncol=3)
      rownames(m) <- c("Female", "Male")
      colnames(m) <- c("Not Happy", "Pretty Happy", "Very Happy")
      names(attr(m,"dimnames")) <- c("Gender", "Happiness")
    },
    "rel" = {
      updateTextInput(session, "varname1", value="Gender")
      updateTextInput(session, "cat1", value="Female, Male")
      updateTextInput(session, "varname2", value="Religiosity")
      updateTextInput(session, "cat2", value="very, moderately, slighly, not")
      m <- matrix(c(145, 227, 359, 514, 268, 305, 275, 235), ncol=4)
      rownames(m) <- c("Female", "Male")
      colnames(m) <- c("very", "moderately", "slightly", "not")
      names(attr(m,"dimnames")) <- c("Gender", "Religiosity")
    },
    "alli" = {
      updateTextInput(session, "varname1", value="Lake")
      updateTextInput(session, "cat1", value="Hancock, Trafford")
      updateTextInput(session, "varname2", value="Primary Food")
      updateTextInput(session, "cat2", value="Fish, Invertebrates, Birds & Reptiles, Others")
      m <- matrix(c(30,13,4,18,8,12,13,10), ncol=4)
      rownames(m) <- c("Hancock", "Trafford")
      colnames(m) <- c("Fish", "Invertebrates", "Birds & Reptiles", "Others")
      names(attr(m,"dimnames")) <- c("Lake", "Primary Food")
    },
    "head" = {
      updateTextInput(session, "varname1", value="Flu Shot")
      updateTextInput(session, "cat1", value="Active, Placebo")
      updateTextInput(session, "varname2", value="Severity of Headache")
      updateTextInput(session, "cat2", value="Mild, Moderate, Severe")
      m <- matrix(c(486,355,113,80,16,5), ncol=3)
      rownames(m) <- c("Active", "Placebo")
      colnames(m) <- c("Mild", "Moderate", "Severe")
      names(attr(m,"dimnames")) <- c("Flu Shot", "Severity of Headache")
    },
    "diamonds" = {
      updateTextInput(session, "varname1", value="Cut")
      updateTextInput(session, "cat1", value="Good Cut, Fair Cut")
      updateTextInput(session, "varname2", value="Clarity")
      updateTextInput(session, "cat2", value="IF, VVS, VS, SI, I")
      updateTextInput(session, "title2", value="Cut and Clarity of Diamonds")
      updateTextInput(session, "subtitle2", value="")
      updateTextInput(session, "caption2", value="")
      m <- matrix(c(2,1,4,3,16,8,55,30,3,2), ncol=5)
      rownames(m) <- c("Good Cut", "Fair Cut")
      colnames(m) <- c("IF", "VVS", "VS", "SI", "I")
      names(attr(m,"dimnames")) <- c("Cut", "Clarity")
    },
    "airline" = {
      updateTextInput(session, "varname1", value="Airline")
      updateTextInput(session, "cat1", value="American, Delta, Southwest")
      updateTextInput(session, "varname2", value="Flight Status")
      updateTextInput(session, "cat2", value="On-Time, Delayed, Canceled, Diverted")
      updateTextInput(session, "title2", value="")
      updateTextInput(session, "subtitle2", value="")
      updateTextInput(session, "caption2", value="")
      m <- matrix(c(42600, 4657, 296, 95,
                    51620, 4030, 150, 56,
                    69384, 9280, 1782, 128),
                    byrow=TRUE, ncol=4)
      rownames(m) <- c("American", "Delta", "Southwest")
      colnames(m) <- c("On-Time", "Delayed", "Canceled", "Diverted")
      names(attr(m,"dimnames")) <- c("Airline", "Flight Status")
    },
    "device" = {
      updateTextInput(session, "varname1", value="Device Used to Go Online")
      updateTextInput(session, "cat1", value="Mostly Cell, Mostly Other, Equally or Depends, Non - Smartphone")
      updateTextInput(session, "varname2", value="Age Bracket")
      updateTextInput(session, "cat2", value="18 - 29, 30 - 49, 50 - 64, 65+")
      updateTextInput(session, "title2", value="")
      updateTextInput(session, "subtitle2", value="")
      updateTextInput(session, "caption2", value="")
      m <- matrix(c(179, 231, 100, 42,
                    64, 94, 122, 67,
                    53, 124, 63, 34,
                    13, 38, 78, 136),
                  byrow=TRUE, ncol=4)
      rownames(m) <- c("Mostly Cell", "Mostly Other", "Equally or Depends", "Non - Smartphone")
      colnames(m) <- c("18 - 29", "30 - 49", "50 - 64", "65+")
      names(attr(m,"dimnames")) <- c("Device Used to Go Online", "Age Bracket")
    },
    "incomeedu" = {
      updateTextInput(session, "varname1", value="Education Level")
      updateTextInput(session, "cat1", value="Post-Grad Degree, College Degree, Some College, HS Grad, No HS Degree")
      updateTextInput(session, "varname2", value="Income Level")
      updateTextInput(session, "cat2", value="<$30k, $30k to $75k, >$75k")
      updateTextInput(session, "title2", value="")
      updateTextInput(session, "subtitle2", value="")
      updateTextInput(session, "caption2", value="")
      m <- matrix(c(2, 8, 46,
                    39, 113, 202,
                    131, 138, 120,
                    175, 129, 65,
                    78, 32, 8),
                  byrow=TRUE, ncol=3)
      rownames(m) <- c('Post-Grad Degree', 'College Degree', 'Some College', 'HS Grad', 'No HS Degree')
      colnames(m) <- c('<$30k', '$30k to $75k', '>$75k')
      names(attr(m,"dimnames")) <- c("Education Level", "Income Level")
    }
  )
  return(m)
})

output$hot2textb <- renderRHandsontable({
  if(input$how != "textb") return(NULL) 
  DF <- req(datatextb())
  mytable$DF <- DF
  mytab <-  rhandsontable(DF, rowHeaderWidth = 110) %>%
    hot_col(col=1:dim(DF)[2], type="numeric", format='0,0') %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(manualColumnResize=TRUE) %>%
    hot_table(stretchH="all") #doesn't render nicely witrhin wellpanel
  return(mytab)
}) #%>% debounce(1000)


output$hot3 <- renderRHandsontable({
  if(input$how != "ind") return(NULL) 
  if (!is.null(input$hot3)) {
    DF <- hot_to_r(input$hot3)
  } else {
    DF <- data.frame(rep("",50),rep("",50), stringsAsFactors = FALSE)
  }
  if(input$varname1_ind=="") vn1 <- "Var1" else vn1 <- input$varname1_ind 
  if(input$varname2_ind=="") vn2 <- "Var2" else vn2 <- input$varname2_ind 
  colnames(DF) <- c(vn1,vn2)
  mytab <-  rhandsontable(DF, height=180) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    #hot_validate_numeric(cols=c(1,2), min=0) %>%
    hot_cols(manualColumnResize=TRUE) %>%
    hot_table(stretchH="all") #doesn't render nicely
  return(mytab)
}) # %>% debounce(600)
outputOptions(output, "hot3", priority = 12)

output$freqtab <- renderTable({
  if(input$how == "textb" || input$how == "cont") {
    DF <- switch(input$how, 
                 "cont" = hot_to_r(req(input$hot2)), 
                 "textb" = hot_to_r(req(input$hot2textb))
    )
    if (any(is.na(DF)) | sum(DF)<1) {
      DF <- matrix(NA_integer_, length(rowlabels()$lab), length(collabels()$lab))
      rownames(DF) <- req(rowlabels()$lab)
      colnames(DF) <- req(collabels()$lab)
    }
    rownames(DF) <- req(rowlabels()$lab)
    colnames(DF) <- req(collabels()$lab)
    names(attr(DF,"dimnames")) <- c(rowlabels()$name, collabels()$name)
    mytable$DF <- DF
  } else if (input$how == "ind") {
    if(input$submit<1) return(NULL)
    if (!is.null(input$hot3)) {
      DF <- table(hot_to_r(input$hot3),exclude=c("", NA, NaN))
      if(any(dim(DF) < 1)) {
        DF <- as.table(matrix(NA,ncol=3,nrow=3))
        dimnames(DF) <- list(rep("",3), rep("",3))
      }
    } else {
      DF <- as.table(matrix(NA,ncol=3,nrow=3))
      dimnames(DF) <- list(rep("",3), rep("",3))
    }
    if(input$varname1_ind=="") vn1 <- "Income" else vn1 <- input$varname1_ind 
    if(input$varname2_ind=="") vn2 <- "Happiness" else vn2 <- input$varname2_ind 
    names(attr(DF,"dimnames")) <- c(vn1,vn2)
    mytable$DF <- DF
  } else if(input$how == 'upload') {
    DF <- mytable$DF 
  }
  #u <- colnames(DF)
  #colnames(DF) <- str_pad(u, width=length(u)+3)
  
  d <- dim(DF)
  DF <- addmargins(DF)
  rownames(DF)[d[1]+1] <- "Overall"
  colnames(DF)[d[2]+1] <- "Total"
  rownames(DF) <- paste0("<b>", rownames(DF), "</b>")
  #colnames(t) <- paste0("</b>", colnames((t)), "</b>")
  DF <- apply(DF,c(1,2), function(x) format(x, big.mark=","))
  DF[d[1]+1,] <- paste0("<b>", DF[d[1]+1,], "</b>")
  DF[,d[2]+1] <- paste0("<b>", DF[,d[2]+1], "</b>")
  return(DF)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
digits=0,
bordered=TRUE,
align="r",
spacing="xs"
)

output$condtab <- renderTable({
  if(input$how == 'ind' & input$submit<1) return(NULL)
  DF <- req(mytable$DF)
  marg.DF <- round(100*prop.table(margin.table(DF,2)),1)
  DF <- prop.table(DF,1)
  DF <- round(100*addmargins(DF,2),1)
  DF <- rbind(DF, c(marg.DF, 100))
  d <- dim(DF)
  rownames(DF)[d[1]] <- "Overall"
  colnames(DF)[d[2]] <- "Total"
  rownames(DF) <- paste0("<b>", rownames(DF), "</b>")
  #colnames(t) <- paste0("</b>", colnames((t)), "</b>")
  #DF <- apply(DF,c(1,2), function(x) paste0(format(x,digits=1, nsmall=1),"%"))
  DF <- apply(DF,c(1,2), function(x) format(x,digits=2, nsmall=1))
  DF[d[1],] <- paste0("<b>", DF[d[1],], "</b>")
  DF[,d[2]] <- paste0("<b>", DF[,d[2]], "</b>")
  return(DF)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
digits=1,
bordered=TRUE,
align="r",
spacing="xs"
)


reactall <- reactive({
  input$how
  input$dataset
  input$file
  input$var1
  input$var2
  input$hot2
  input$hot2textb
  input$hot3
  input$varname1
  input$varname2
  input$cat1
  input$cat2
  input$varname1_ind
  input$varname2_ind
})

###### Barchart ############################

output$barchart <- renderPlotly({
  if(input$how == 'ind' & input$submit<1) return(NULL)
  df <- as.table(req(mytable$DF))
  if(any(is.na(df))) return(NULL)
  varn <- names(dimnames(df))
  if(is.null(df)) return(NULL)
  if(all(is.na(df))) return(NULL)
  df1 <- prop.table(df,1)
  df <- as.data.frame(df)
  if(sum(df$Freq)<1) return(NULL)
  df$Freq1 <- 100*as.data.frame(df1)$Freq #row percentages
  colnames(df) <- c("V1","V2","Count","Freq1")
  levels(df$V1) <- str_replace(str_wrap(str_trim(levels(df$V1), side="left"), 10), pattern="\n", replacement="<br>")
  levels(df$V2) <- str_replace(str_wrap(str_trim(levels(df$V2), side="left"), 12), pattern="\n", replacement="<br>")
  barplot <- plot_ly(data=df, x=~V1, y=as.formula(paste0("~",ifelse(!input$freq,"Freq1","Count"))),
                           color=~V2, colors=mycol[1:nlevels(df$V2)],
                           marker = list(line = list(color = '#000000', width = 1)),
                           text = paste0("<b>Category:</b> ", df$V2,
                                         "<br><b>Count:</b> ", df$Count,
                                         "<br><b>Percent:</b> ", round(df$Freq1,2), "%"),
                           #~do.call(paste0,myhovertext),
                           textposition = 'none',
                           hoverinfo='text'
             ) %>% 
             layout(barmode=ifelse(!input$stack,"dodge","stack"), hovermode="x")
# add_annotations(text=varn[2], showarrow=FALSE, xref="paper", yref="paper", font=list(size=14),
#                 x=1.02, xanchor="left", y=0.9, yanchor="bottom"

  barplot <- barplot %>% add_bars() %>%
    layout(bargap = NULL,
           #bargroupgap = 0.1,
           #title = "Stacked Barchart",
           xaxis = list(title=NA, fixedrange=TRUE),
           yaxis = list(title=ifelse(!input$freq,"Percent (%)", "Count"), range=ifelse(!input$freq,c(0,102),NA), fixedrange=TRUE),
           #legend = list(x=0, y=1.22, yanchor="left", size=11, orientation="h"),
           #margin = list(t=60, b=50, r=20),
           #legend = list(y=0.9, yanchor="top", size=10),
           #margin = list(t=25, b=25, r=80),
           legend = list(x=0, y=-0.40, xanchor="left", yanchor="top", orientation = 'h', size=11),
           margin = list(t=30, b=100, r=20, l=20),
           font = list(size=12),
           hovermode="closest"
    ) %>%
    add_annotations(text=ifelse(!input$stack,"<b> Side-By-Side Barchart <b>", "<b> Stacked Barchart <b>"), showarrow=FALSE, font=list(size=13), 
                    x=0.5, xref='paper', xanchor='center', 
                    y=1.15, yref='paper', yanchor='center') %>%
    add_annotations(text=paste0('<b>',varn[1],"</b>"), showarrow=FALSE, font=list(size=12),
                       x=0.5, xref='paper', xanchor='center',
                       y=-0.25, yref='paper', yanchor='center') %>%
    add_annotations(text=paste0('<b>',varn[2],":</b>"), showarrow=FALSE, font=list(size=12),
                    x=0, xref='paper', xanchor='left',
                    y=-0.40, yref='paper', yanchor='bottom') %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 
      'pan2d', 'select2d', 'lasso2d', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines','hoverClosestCartesian', 'hoverCompareCartesian')) 
    #list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d',  'hoverClosestGl2d', 
    #'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines')) #'hoverClosestCartesian', 'hoverCompareCartesian',
  mytable$chart <- barplot
  return(barplot)
})

X2stat <- reactiveValues(X2=NULL, df=NULL, P=NULL, exp=NULL, res=NULL, stdres=NULL, adjres=NULL,plot=NULL)

observeEvent(list(input$how, input$dataset, input$file, input$hot2, input$hot2texb, input$hot3), {
  X2stat$X2=NULL; X2stat$df=NULL; X2stat$P=NULL; X2stat$exp=NULL; X2stat$res=NULL; X2stat$stdres=NULL; X2stat$adjres=NULL;plot=NULL
})

observeEvent(list(mytable, reactall()), {
  t <- mytable$DF
  if(any(is.na(t))) return(NULL)
  # if(input$how != 'ind') {
  #   rlabs <- rowlabels()$lab
  #   clabs <- collabels()$lab
  #   r <- length(rlabs)
  #   c <- length(clabs)
  #   if(!all(dim(t)==c(r,c))) return(NULL)
  # }
  X2 <- chisq.test(t, correct=FALSE)
  X2stat$X2 <- X2$statistic
  X2stat$df <- X2$parameter
  X2stat$P <- X2$p.value
  X2stat$exp <- X2$expected
  X2stat$res <- t - X2$expected #these are the raw residuals (obs-exp)
  X2stat$stdres <- X2$residuals #these are the Pearson residuals (obs-exp)/sqrt(exp)
  X2stat$adjres <- X2$stdres #lets call them adjusted residuals
})

output$X2test <- renderTable({
  if(input$how == 'ind' & input$submit<1) return(NULL)
  # t <- req(mytable$DF)
  # if(any(is.na(t))) return(NULL)
  # if(input$how!='ind') {
  #   rlabs <- rowlabels()$lab
  #   clabs <- collabels()$lab
  #   r <- length(rlabs)
  #   c <- length(clabs)
  #   if(!all(dim(t)==c(r,c))) return(NULL)
  # }
  # X2 <- chisq.test(t, correct=FALSE)
  # X2stat$X2 <- X2$statistic
  # X2stat$df <- X2$parameter
  # X2stat$P <- X2$p.value
  # X2stat$exp <- X2$expected
  # X2stat$res <- t - X2$expected #these are the raw residuals (obs-exp)
  # X2stat$stdres <- X2$residuals #these are the Pearson residuals (obs-exp)/sqrt(exp)
  # X2stat$adjres <- X2$stdres #lets call them adjusted residuals
  # P <- X2$p.value
  df <- data.frame("Independence<br>(or Homogeneous Distributions)", "Association<br>(or Non-Homogeneous Distributions)", 
                   format(round(req(X2stat$X2), 2),digits=2, nsmall=2, big.mark=","), 
                   format(req(X2stat$df),0), 
                   PvalRound(req(X2stat$P))
                   )
  colnames(df) <- c("Null Hypothesis", "Alternative Hypothesis", "Test Statistic X<sup>2</sup>", "df", "P-value")
  return(df)
},
rownames=FALSE,
colnames=TRUE,
sanitize.text.function = function(x) x,
align = "c",
striped=TRUE,
bordered=TRUE
)

output$mytestplot <- 	renderPlot({
  if(input$how == 'ind' & input$submit<1) return(NULL)
  t <- req(X2stat$X2)
  dg <- req(X2stat$df)
  P <- req(X2stat$P)
  max <- max(qchisq(.999,dg), 1.1*t)
  x <- seq(0, max, length.out=300)
  y <- dchisq(x, df=dg)
  df.t <- data.frame(x=x,y=y)
  basic.plot <- ggplot(data=df.t, aes(x=x, y=y)) +
    geom_area(fill="#6A51A3", alpha=0.25) + 
    geom_line(linewidth=1,  alpha=1, color="black") +   #color="#669900",
    theme_classic()  +
    theme(text=element_text(size=16),
          plot.title = element_text(size=16, hjust=0.5, margin = margin(b=10), face="bold"),
          plot.subtitle = element_text(hjust=0.5, margin = margin(t=-5, b=22)),
          plot.margin=unit(c(0.5,0.1,1,0.1),"cm"),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color="black"),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_x_continuous(breaks=pretty(seq(0,floor(max)),8), expand = expansion(add=c(1.5,1), mult = c(0, 0.02)))
    b <- P
    basic.plot <- basic.plot +
      geom_area(data = subset(df.t, x>=t), aes(x, y), fill = "#6A51A3", alpha = 0.6) +
      geom_vline(xintercept=t, linewidth=1.2, linetype=2) +
      geom_point(x=t, y=0, size=6, pch="X", color="#FF6600") +
      annotate("label", x = t, y = Inf, label = format(round(1-b,4), digits=4, nsmall=1), size=5, vjust=0.4, hjust=1.1, color = "#6A51A380", fontface="bold") +
      annotate("label", x = t, y = Inf, label = format(round(b,4), digits=4, nsmall=1), size=5, vjust=0.4, hjust=-0.2, color = "#6A51A3", fontface="bold") +
      annotate("text", label=paste0("X^2 == ", format(round(t,2),digits=2,nsmall=2)), parse=TRUE, x=t, y=0, vjust=2.1, size=4.8, color="#FF6600", alpha=1)
    subtitle = substitute(H[0]:~ "Independence, "  * X^2 ~ "" == ~b * ", " * df ~ "" == ~a * ", P-value" ~ "" == ~ c, 
                          list(a=dg, b=format(round(t,2),digits=2,nsmall=2), c=PvalRound(P)))

  main <- paste0("Chi-Squared Distribution with df = ", dg)
  plot <- basic.plot + ggtitle(label=main, subtitle=subtitle) + coord_cartesian(clip="off")
  X2stat$plot <- plot
  return(plot)
}) #end mytestplot


output$expVal <- renderTable({
  reactall()
  if(input$how == 'ind' & input$submit<1) return(NULL)
  t <- req(X2stat$exp)
  d <- dim(t)
  DF <- addmargins(t)
  rownames(DF)[d[1]+1] <- "Overall"
  colnames(DF)[d[2]+1] <- "Total"
  Overall <- DF[d[1]+1,]
  Total <- DF[,d[2]+1]
  #colnames(t) <- paste0("</b>", colnames((t)), "</b>")
  #DF <- apply(DF,c(1,2), function(x) paste0(format(x,digits=1, nsmall=1),"%"))
  DF <- apply(DF,c(1,2), function(x) format(x, digits=2, nsmall=1, big.mark=","))
  rownames(DF) <- paste0("<b>", rownames(DF), "</b>")
  DF[d[1]+1,] <- paste0("<b>", format(Overall, big.mark=","), "</b>")
  DF[,d[2]+1] <- paste0("<b>", format(Total, big.mark=","), "</b>")
  return(DF)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
bordered=TRUE,
align="r"
)


output$resid <- renderTable({
  reactall()
  if(input$how == 'ind' & input$submit<1) return(NULL)
  DF <- req(X2stat$res)
  d <- dim(DF)
  DF <- apply(DF,c(1,2), function(x) format(x, digits=2, nsmall=1, big.mark=","))
  rownames(DF) <- paste0("<b>", rownames(DF), "</b>")
  return(DF)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
bordered=TRUE,
align="r"
)

output$stdresid <- renderTable({
  reactall()
  if(input$how == 'ind' & input$submit<1) return(NULL)
  DF <- req(X2stat$stdres)
  d <- dim(DF)
  DF <- apply(DF,c(1,2), function(x) format(x, digits=2, nsmall=1, big.mark=","))
  rownames(DF) <- paste0("<b>", rownames(DF), "</b>")
  return(DF)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
digits=1,
bordered=TRUE,
align="r"
)

output$adjresid <- renderTable({
  reactall()
  if(input$how == 'ind' & input$submit<1) return(NULL)
  DF <- req(X2stat$stdres)
  d <- dim(DF)
  DF <- apply(DF,c(1,2), function(x) format(x, digits=2, nsmall=1, big.mark=","))
  rownames(DF) <- paste0("<b>", rownames(DF), "</b>")
  return(DF)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
digits=1,
bordered=TRUE,
align="r"
)


###############################################
######## Second Tab: Goodness of Fit ##########
###############################################

uploadedData1 = reactiveVal(NULL)

observeEvent(list(input$file1), {
  myfile <- input$file1
  if(is.null(myfile)) {
    uploadedData1(NULL)
  } else {
    df <- read_csv(myfile$datapath)
    #numeric_columns_index <- sapply(df, function(x) is.numeric(x))
    coln <- colnames(df)
    mychoicesAll <- as.list(c("", coln))
    names(mychoicesAll) <- c("Choose One", coln)
    updateSelectInput(session, "var", choices=mychoicesAll, selected=mychoicesAll[1])
    uploadedData1(df)
  }
},ignoreInit = TRUE)

# observeEvent(list(input$var), {
#   if(input$how1 != 'upload') return(NULL)
#   df <- req(uploadedData1())
#   if(nrow(df > 0) > 0) {
#     var <- req(input$var)
#     df <- df %>% dplyr::select(all_of(var)) %>% na.omit() %>% mutate(across(everything(), as.factor))
#     mytable$DF <- table(df)
#   } else {
#   }
# }, ignoreInit = TRUE)

output$hotUpload1 <- renderRHandsontable({
  if(input$how1 != "upload") return(NULL)
  DF <- uploadedData1()
  if (is.null(DF)) {
    return(NULL)
  }
  mytab <-  rhandsontable(DF, rowHeaders=NULL, height=160, readOnly = TRUE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    # hot_cols(type='numeric', manualColumnResize=TRUE
    # , renderer = 
    #       "function(instance, td, row, col, prop, value, cellProperties) {
    #       Handsontable.renderers.TextRenderer.apply(this, arguments);
    #       td.style.color = 'black';
    #       }"
    # ) %>%
    hot_table(stretchH="all", colHeaders = TRUE, columnSorting = TRUE) 
  return(mytab)
})

output$inmatrix1 <- renderUI({
  switch(input$how1,
    'textb' = {
      switch(input$dataset1,
        'inherit' = {
          c <- 2
          clabs <- c("Green", "Yellow")
          row1 <- c(6022, 2001)
          row2 <- c(0.75, 0.25)
          DF <- matrix(c(row1,row2), byrow=TRUE, ncol=c)
          colnames(DF) <- clabs
          rownames(DF) <- c("Counts:", "Props:")
          matrixInput("countsin", value = DF, rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=FALSE))
        },
        'benford' = {
          c <- 9
          clabs <- 1:9
          row1 <- c(39,24,20,7,6,10,11,6,7)
          row2 <- c(0.301, 0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046)
          DF <- matrix(c(row1,row2), byrow=TRUE, ncol=c)
          colnames(DF) <- clabs
          rownames(DF) <- c("Counts:", "Props:")
          matrixInput("countsin", value = DF, rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=FALSE))
        },
        'birthday' = {
          c <- 4
          clabs <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
          row1 <- c(19,21,31,13)
          row2 <- c(0.25,0.25,0.25,0.25)
          DF <- matrix(c(row1,row2), byrow=TRUE, ncol=c)
          colnames(DF) <- clabs
          rownames(DF) <- c("Counts:", "Props:")
          matrixInput("countsin", value = DF, rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=FALSE))
        }
      )
    },
    'cont' = {
      c <- input$ncat
      clabs <- paste("Cat",1:c)
      row1 <- rep("",c)
      row2 <- rep(signif(1/c,2),c)
      DF <- matrix(c(row1,row2), byrow=TRUE, ncol=c)
      colnames(DF) <- clabs
      rownames(DF) <- c("Counts:", "Props:")
      if(ncol(DF)<c) DF <- cbind(DF, matrix(NA, nrow=1, ncol=c-ncol(DF)))
      if(ncol(DF)>c) DF <- DF[,1:c]
      matrixInput("countsin", value = DF, rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=TRUE))
    },
    'ind' = {
      indobs <- str_split(req(input$indobs), pattern=" ")
      indobs <- indobs[[1]][indobs[[1]] != ""]
      
      nums1 <- str_remove_all(req(input$indobs), "^\\s*|\\s*$|,+\\s*$") #removes trailing commas and whitespace from entire string
      nums1 <- str_replace_all(nums1, '\t\n', ',') #in case data was copy and pasted in Unix
      nums1 <- str_replace_all(nums1, '\n', ',') #in case data was copy and pasted in Windows
      nums1 <- str_split(nums1, ",") #scan(text=nums1, what="character", sep=",")
      nums1 <- sapply(nums1, str_trim, simplify = TRUE) #removes trailing commas and whitespace from each element
      nums1 <- nums1[nums1 != ""] #remove empty string
      indobs <- factor(nums1, levels=unique(nums1))
      df <- table(indobs)
      c <- dim(df)
      clabs <- names(df)
      row1 <- c(df)
      row2 <- rep(signif(1/c,2),c)
      DF <- matrix(c(row1,row2), byrow=TRUE, ncol=c)
      colnames(DF) <- clabs
      rownames(DF) <- c("Counts:", "Props:")
      if(ncol(DF)<c) DF <- cbind(DF, matrix(NA, nrow=1, ncol=c-ncol(DF)))
      if(ncol(DF)>c) DF <- DF[,1:c]
      matrixInput("countsin", value = DF, rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=TRUE))
    },
    'upload' = {
      df <- req(uploadedData1())
      if(nrow(df > 0) > 0) {
        var <- req(input$var)
        df <- df %>% dplyr::select(all_of(var)) %>% na.omit() %>% mutate(across(everything(), as.factor))
        df <- table(df)
        c <- dim(df)
        clabs <- names(df)
        row1 <- c(df)
        row2 <- rep(signif(1/c,2),c)
        DF <- matrix(c(row1,row2), byrow=TRUE, ncol=c)
        colnames(DF) <- clabs
        rownames(DF) <- c("Counts:", "Props:")
        if(ncol(DF)<c) DF <- cbind(DF, matrix(NA, nrow=1, ncol=c-ncol(DF)))
        if(ncol(DF)>c) DF <- DF[,1:c]
        matrixInput("countsin", value = DF, rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=TRUE))
      } else {
        matrixInput("countsin", value = matrix(NA, ncol=4, nrow=2), rows = list(names=TRUE, editableNames=FALSE), cols=list(names=TRUE, editableNames=TRUE))
      }
    }
    
  )
})

myGoF <- reactiveValues(test=NULL)

observeEvent(list(input$how1, input$dataset1, input$submit, input$ncat), myGoF$test <- NULL, priority = 10)

output$obscounts <- renderTable({
  if(input$how1 != 'textb' & input$submit1<1) return(NULL)
  df <- req(input$countsin)
  counts <- as.numeric(df[1,])
  if(any(is.na(counts)) || any(is.null(counts)) || sum(counts)<1) return(NULL)
  probs <- as.numeric(df[2,])
  gof <- chisq.test(counts, p=probs, rescale.p = TRUE, correct=FALSE)
  myGoF$test <- gof 
  ecounts <- gof$expected
  resid <- counts-ecounts
  stdresid <- resid/sqrt(ecounts)
  adjresid <- gof$stdres
  probs0 <- probs/sum(probs)
  df1 <- rbind(counts, probs0, ecounts, resid, stdresid, adjresid)
  df1 <- df2 <- addmargins(df1,2)
  df2[1,] <- format(df1[1,], big.mark = ",")
  df2[2,] <- format(df1[2,], digits=4, nsmall=2)
  df2[3,] <- format(df1[3,], digits=2, nsmall=1, big.mark = ",")
  df2[4,] <- format(round(df1[4,],2), nsmall=2, big.interval = ",")
  df2[5,] <- round(df1[5,],2)
  df2[6,] <- round(df1[6,],2)
  df2[,dim(df2)[2]] <- paste0("<b>",df2[,dim(df2)[2]],"</b>")
  colnames(df2) <- c(colnames(df), "Total")
  rownames(df2) <- c("Observed Counts", "Hypothesized Proportions", "Expected Counts", "Residuals: Obs - Exp", "Standardized Residuals: (Obs - Exp) / √Exp", "Adjusted Standardized Residuals")
  if(!input$stdres1 & !input$adjres1) df2 <- df2[-c(5,6),]
  if(input$stdres1 & !input$adjres1) df2 <- df2[-6,]
  if(!input$stdres1 & input$adjres1) df2 <- df2[-5,]
  return(df2)
},
rownames=TRUE,
colnames=TRUE,
sanitize.text.function = function(x) x,
bordered=TRUE,
striped=TRUE,
align="r"
)

output$X2test2 <- renderTable({
  if(input$how1 != 'textb' & input$submit1<1) return(NULL)
  X2 <- req(myGoF$test)
  dg <- X2$parameter
  P <- PvalRound(X2$p.value)
  probs0 <- as.numeric(c(input$countsin[2,]))
  probs0 <- signif(probs0/sum(probs0),4)
  df <- data.frame(paste0("p<sub>",1:(dg+1), "</sub> = ", probs0, collapse=", "), format(X2$statistic,digits=2, nsmall=2, big.mark=","), 
                   format(dg,0), P)
  colnames(df) <- c("Null Hypothesis", "Test Statistic X<sup>2</sup>", "df", "P-value")
  return(df)
},
rownames=FALSE,
colnames=TRUE,
sanitize.text.function = function(x) x,
digits=4,
align = "c",
striped=TRUE,
bordered=TRUE
)

output$mytestplot2 <- 	renderPlot({
  if(input$how1 != 'textb' & input$submit1<1) return(NULL)
  X2 <- req(myGoF$test)
  t <- X2$statistic
  dg <- X2$parameter
  P <- X2$p.value
  max <- max(c(qchisq(.999,dg), t+3))
  x <- seq(0, max, length.out=300)
  y <- dchisq(x, df=dg)
  df.t <- data.frame(x=x,y=y)
  basic.plot <- ggplot(data=df.t, aes(x=x, y=y)) +
    geom_area(fill="#6A51A3", alpha=0.25) + 
    geom_line(linewidth=1,  alpha=1, color="black") +   #color="#669900",
    theme_classic()  +
    theme(text=element_text(size=16),
          plot.title = element_text(size=16, hjust=0.5, margin = margin(b=10), face="bold"),
          plot.subtitle = element_text(hjust=0.5, margin = margin(t=-5, b=22)),
          plot.margin=unit(c(0.5,0.1,1,0.1),"cm"),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color="black"),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_x_continuous(breaks=pretty(seq(0,floor(max)),8), expand = expansion(add=c(1.5,1), mult = c(0, 0.02)))
  b <- P
  basic.plot <- basic.plot +
    geom_area(data = subset(df.t, x>=t), aes(x, y), fill = "#6A51A3", alpha = 0.6) +
    geom_vline(xintercept=t, linewidth=1.2, linetype=2) +
    geom_point(x=t, y=0, size=6, pch="X", color="#FF6600") +
    annotate("label", x = t, y = Inf, label = format(round(1-b,4), digits=4, nsmall=1), size=5, vjust=0.4, hjust=1.1, color = "#6A51A380", fontface="bold") +
    annotate("label", x = t, y = Inf, label = format(round(b,4), digits=4, nsmall=1), size=5, vjust=0.4, hjust=-0.2, color = "#6A51A3", fontface="bold") +
    annotate("text", label=paste0("X^2 == ", format(round(t,2),digits=2,nsmall=2)), parse=TRUE, x=t, y=0, vjust=2.1, size=4.8, color="#FF6600", alpha=1)
  subtitle = substitute(X^2 ~ "" == ~b * ", " * df ~ "" == ~a * ", P-value" ~ "" == ~ c, 
                        list(a=dg, b=format(round(t,2),digits=2,nsmall=2), c=PvalRound(P)))
  
  main <- paste0("Chi-Squared Distribution with df = ", dg)
  plot <- basic.plot + ggtitle(label=main, subtitle=subtitle) + coord_cartesian(clip="off")
  X2stat$plot <- plot
  return(plot)
}) #end mytestplot

output$chiplot<-downloadHandler(
  filename=function(){
    'ChiSquareplot.png'
  },content=function(file){
    png(file, height=300, width=600)
    print(X2stat$plot)
    dev.off() 
  }
)



})    #end shinyserver