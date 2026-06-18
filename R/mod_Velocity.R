#' Velocity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
library(shiny)
library(rhandsontable)
library(bslib)
library(dplyr)
mod_Velocity_ui<-function(id){
  ns<-NS(id)
  page_fluid(
    card(
      card_header("Velocity Worksheet"),
      card_body(
        textInput(ns("SCID"),"What is the Permit ID?:"),
        numericInput(ns("Reaches"),"How many reaches are in your model?:",value=1,min=0,max=20,step=1,updateOn = "blur"),
        numericInput(ns("elementlength"),"What is the length of each element?:",value=0.1,min=0.1,max=1,step=0.1,updateOn = "blur"),
        numericInput(ns("outfallmgd"),"What is the design flow in MGD?:",value=0,min=0,max=1000,step=1,updateOn = "blur"),
        "Your design flow in CFS is:", textOutput(ns('flowcfs')),
        br(),
        em("NOTE: If you change something in the input boxes, make sure you click on a cell and press the *Enter*
button to update values"),
      )),
    card(
      full_screen = TRUE,card_header("Velocity Table"),
      card_body(
        style="overflow-x",
        rHandsontableOutput(ns("velocitytable")),
        downloadButton(ns("downloadtable"),"Download Table (CSV)"))
    )
  )}

#' Velocity Server Functions
#'
#' @noRd
mod_Velocity_server  <- function(id){
  moduleServer(id,function(input,output,session){
    flowcfs<-reactive({
      req(input$outfallmgd)
      as.numeric(input$outfallmgd*1.547)
    })
    permitid<-reactive({
      req(input$SCID)
    })
    output$flowcfs<-renderText({
      req(flowcfs())
      paste0(round(flowcfs(),3)," cfs")
    })
    tbl<-reactiveVal()
    calculateTable<-function(df){
      inc<-as.numeric(df$'Incremental D.A. (mi2)')
      inc[is.na(inc)]<-0

      df$'Cumulative D.A. (mi2)'<-cumsum(inc)

      len<-as.numeric(df$'Length (mi)')
      len[is.na(len)]<-0

      df$'Cumulative Length (mi)'<-NA
      if(nrow(df)>1){
        df$'Cumulative Length (mi)'[-1]<-cumsum(len[-1])
      }

      # Calculate slope: (Upper Contour - Lower Contour) / cumulative length for repeated contours
      upper<-as.numeric(df$'Upper Contour (ft)')
      lower<-as.numeric(df$'Lower Contour (ft)')

      # Initialize slope vector
      df$'Slope (ft/mi)'<-NA_real_

      # Track cumulative length for repeated contour segments
      i <- 1
      while(i <= nrow(df)){
        # Find all consecutive rows with the same upper and lower contours
        j <- i
        cum_len <- 0

        while(j <= nrow(df)){
          cum_len <- cum_len + len[j]

          # Check if next row has same contours
          if(j < nrow(df) &&
             !is.na(upper[j]) && !is.na(upper[j+1]) &&
             !is.na(lower[j]) && !is.na(lower[j+1]) &&
             upper[j] == upper[j+1] && lower[j] == lower[j+1]){
            j <- j + 1
          } else {
            break
          }
        }

        # Calculate slope for this segment
        if(!is.na(upper[i]) && !is.na(lower[i]) && cum_len > 0){
          slope_val <- (upper[i] - lower[i]) / cum_len
          # Apply the same slope to all rows in this segment
          for(k in i:j){
            df$'Slope (ft/mi)'[k] <- slope_val
          }
        }

        i <- j + 1
      }

      df$'7Q10 Incremental Flow (cfs)'<-inc*df$'Unit7Q10 (cfs/mi)'

      inc_flow<-as.numeric(df$'7Q10 Incremental Flow (cfs)')
      inc_flow[is.na(inc_flow)]<-0
      df$'Cumulative Flow (cfs)'<-flowcfs()+cumsum(inc_flow)

      df$'Velocity (ft/sec)'<-ifelse(df$'Cumulative D.A. (mi2)'>0& df$'Cumulative Flow (cfs)'>0& df$'Slope (ft/mi)' >0,(28.997*(df$'Cumulative Flow (cfs)'/df$'Cumulative D.A. (mi2)')-24.667*
                                                                                                                          (df$'Cumulative Flow (cfs)'/df$'Cumulative D.A. (mi2)')^2+0.598*
                                                                                                                          (df$'Slope (ft/mi)'*df$'Cumulative Flow (cfs)'^2)^(1/5))*0.06111,NA_real_)
      df$'Number of Elements'<-len/input$elementlength


      df$'K3'<-ifelse(df$'Slope (ft/mi)'<20,0.3,0.5)

      df$'K2'<-ifelse(df$'Cumulative Flow (cfs)'<10,
                      1.8*df$'Slope (ft/mi)'*df$'Velocity (ft/sec)',
                      ifelse(df$'Cumulative Flow (cfs)'>=10 & df$'Cumulative Flow (cfs)'<25,
                             1.3*df$'Slope (ft/mi)'*df$'Velocity (ft/sec)',
                             ifelse(df$'Cumulative Flow (cfs)'>=25,
                                    0.88*df$'Slope (ft/mi)'*df$'Velocity (ft/sec)',NA_real_)))

      df$'Alternative Velocity' <- ifelse(
        (df$'Cumulative Flow (cfs)' / df$'Cumulative D.A. (mi2)') >= 0.59,
        (28.977 * (0.62)^2 + 0.598 * (df$'Slope (ft/mi)' * (df$'Cumulative D.A. (mi2)' * 0.62)^2)^(1/5)) * 0.06111,
        NA_real_
      )
      df$'Alternative K2'<-ifelse(!is.na(df$'Alternative Velocity') & df$'Cumulative Flow (cfs)'<10,
                                  1.8*df$'Alternative Velocity'*df$'Slope (ft/mi)',
                                  ifelse(!is.na(df$'Alternative Velocity')& df$'Cumulative Flow (cfs)'>25,
                                         0.88*df$'Alternative Velocity'*df$'Slope (ft/mi)',
                                         ifelse(!is.na(df$'Alternative Velocity') & df$'Cumulative Flow (cfs)'>10 &
                                                  df$'Cumulative Flow (cfs)'<25,
                                                1.3*df$'Alternative Velocity'*df$'Slope (ft/mi)',NA_real_)))
      df

    }
    observe({
      n<-input$Reaches

      df<-data.frame(
        'Model Section'=c("HW",paste("Reach",1:n)),
        'Incremental D.A. (mi2)'=rep(NA_real_,n+1),
        'Cumulative D.A. (mi2)'=rep(NA_real_,n+1),
        'Length (mi)'=rep(NA_real_,n+1),
        'Cumulative Length (mi)'=rep(NA_real_,n+1),
        'Upper Contour (ft)'=rep(NA_real_,n+1),
        'Lower Contour (ft)'=rep(NA_real_,n+1),
        'Slope (ft/mi)'=rep(NA_real_,n+1),
        'K1'=rep(NA_real_,n+1),
        'K2'=rep(NA_real_,n+1),
        'K3'=rep(NA_real_,n+1),
        'Unit7Q10 (cfs/mi)'=rep(NA_real_,n+1),
        '7Q10 Incremental Flow (cfs)'=rep(NA_real_,n+1),
        'Cumulative Flow (cfs)'=rep(NA_real_,n+1),
        'Velocity (ft/sec)'=rep(NA_real_,n+1),
        'Alternative Velocity'=rep(NA_real_,n+1),
        "Alternative K2"=rep(NA_real_,n+1),
        'Number of Elements'=rep(NA_real_,n+1),
        check.names = FALSE
      )
      tbl(df)
    })%>%
      bindEvent(input$Reaches,ignoreNULL=FALSE)

    observe({
      req(input$velocitytable)
      df<-hot_to_r(input$velocitytable)
      df<-calculateTable(df)
      tbl(df)
    })%>%
      bindEvent(input$velocitytable)
    output$velocitytable<-renderRHandsontable({
      req(tbl())
      rhandsontable(tbl())%>%
        hot_col('Model Section',readOnly = TRUE)%>%
        hot_col('Incremental D.A. (mi2)',readOnly = FALSE)%>%
        hot_col('Cumulative D.A. (mi2)',readOnly = TRUE)%>%
        hot_col('Length (mi)',readOnly = FALSE)%>%
        hot_col('Cumulative Length (mi)',readOnly = TRUE)%>%
        hot_col('Upper Contour (ft)',readOnly=FALSE)%>%
        hot_col('Lower Contour (ft)',readOnly=FALSE)%>%
        hot_col('Slope (ft/mi)',readOnly = TRUE)%>%
        hot_col('K1',readOnly = FALSE)%>%
        hot_col('K2',readOnly=TRUE)%>%
        hot_col('K3',readOnly=TRUE,type="numeric")%>%
        hot_col('Unit7Q10 (cfs/mi)',readOnly=FALSE,type="numeric")%>%
        hot_col('7Q10 Incremental Flow (cfs)',readOnly = TRUE,type='numeric')%>%
        hot_col('Cumulative Flow (cfs)',readOnly = TRUE)%>%
        hot_col('Velocity (ft/sec)',readOnly = TRUE)%>%
        hot_col('Alternative Velocity',readOnly = TRUE,type='numeric')%>%
        hot_col("Alternative K2",readOnly = TRUE)%>%
        hot_col('Number of Elements',readOnly = TRUE)
    })
    output$downloadtable<-downloadHandler(
      filename=function(input,output){
        paste0(permitid(),"_VelocityTable_",Sys.Date(),".csv",sep="")},
      content=function(file){
        df<-tbl()
        write.csv(df,file,row.names = FALSE)
      })
  })}

## To be copied in the UI
# mod_Velocity_ui("Velocity_1")

## To be copied in the server
# mod_Velocity_server("Velocity_1")
