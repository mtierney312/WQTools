#' STORET UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(shiny)
library(bslib)
library(EPATADA)
library(rhandsontable)
library(dplyr)
mod_STORET_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    tabsetPanel(
      tabPanel(
        card(
          card_header('STORET Data Download'),
          layout_sidebar(
            sidebar=sidebar(
              textInput(ns('huc'),'HUC',value="",placeholder='010203040506',updateOn = 'blur'),
              dateRangeInput(ns('date'),"Date Range",start = Sys.Date()-365,end = Sys.Date(),min='1950-01-01',
                       max=Sys.Date()),
              textInput(ns('id'),'Organization ID',value='21SC60WQ_WQX',updateOn='blur'),
              textAreaInput(ns('site'),'Site Name(s)',value='S-855',updateOn='blur'),
              textAreaInput(ns('characteristic'),"Characteristic Name(s)",value='Escherichia coli'),
              selectInput(ns('watertype'),"Waterbody Type",choices = c('Aggregate groundwater use',
                                                                   'Aggregate surface-water-use',
                                                                   'Atmosphere',
                                                                   'Estuary',
                                                                   'Facility',
                                                                   'Stream',
                                                                   'Wetland',
                                                                   'Well',
                                                                   'Spring',
                                                                   'Lake, Reservoir, Impoundment'),multiple=TRUE),
              input_task_button(ns('downloadstoret'),'Download STORET Data'),
              textOutput(ns('record_numbers'))),rHandsontableOutput(ns('results_table')),
            downloadButton(ns('storetdownload'),"Download.csv")))),
      tabPanel(
        card(
          card_header('Clean STORET'),
          layout_sidebar(
            sidebar=sidebar(input_task_button(ns('cleanstoret'),'Clean the Storet Data'),
                            textOutput(ns('clean_records')),
                            em('This removes records that have been flagged for Result Unit, Method Speciation,
                               Sample Fraction, Measure Quantifier Code, and Activity Type')),
            rHandsontableOutput(ns('clean_table')),
            downloadButton(ns('cleandownload'),'Download.csv')
      )))))
}

#' STORET Server Functions
#'
#' @noRd
mod_STORET_server <- function(id){
  moduleServer(id, function(input, output, session){
    options(shiny.maxRequestSize=50*1024^2)
    ns <- session$ns
    storet_data<-eventReactive(input$downloadstoret,{
      to_null <- function(x) {
        # Handle vectors and single values properly
        if (is.null(x) || length(x) == 0) {
          return("null")
        }
        # Trim whitespace from all elements
        x <- trimws(x)
        # Remove empty strings
        x <- x[nzchar(x)]
        # Return "null" if nothing left, otherwise return the cleaned vector
        if (length(x) == 0) {
          return("null")
        }
        return(x)
      }
      format_siteid <- function(sites_input, org = input$id) {
        # Handle NULL or empty input
        if (is.null(sites_input) || length(sites_input) == 0 || !nzchar(trimws(sites_input))) {
          return("null")
        }

        # Parse comma-separated sites
        sites <- trimws(strsplit(sites_input, ",")[[1]])
        sites <- sites[nzchar(sites)]

        # If no valid sites or explicitly "null", return "null"
        if (length(sites) == 0 || (length(sites) == 1 && sites[1] == "null")) {
          return("null")
        }

        # Format as "ORG-SITE" for each site
        formatted_sites <- paste0(org, "-", sites)

        # Return the formatted vector
        return(formatted_sites)
      }
      format_characteristics <- function(char_input) {
        if (is.null(char_input) || length(char_input) == 0 || !nzchar(trimws(char_input))) {
          return("null")
        }
        # Split by comma and trim whitespace
        chars <- trimws(strsplit(char_input, ",")[[1]])
        chars <- chars[nzchar(chars)]
        if (length(chars) == 0) {
          return("null")
        }
        # Return as vector (not "null" string)
        return(chars)
      }
      tryCatch({
        result<-EPATADA::TADA_DataRetrieval(
        startDate = to_null(format(input$date[1])),
        endDate = to_null(format(input$date[2])),
        huc=to_null(input$huc),
        siteid = format_siteid(input$site),
        siteType = to_null(input$watertype),
        characteristicName = format_characteristics(input$characteristic),
        organization = to_null(input$id),
        applyautoclean = TRUE,
        ask=FALSE
      )
        if(is.null(result) || nrow(result) == 0){
          showNotification('No records retrieved. Check your search parameters',
                         type = 'warning', duration = 10)
          return(data.frame())
        }
        result <- TADA_IDCensoredData(result)
       # result %>%
        #  mutate(
         #   ResultMeasureValue = as.numeric(ResultMeasureValue),
          #  DetectionQuantitationLimitMeasure.MeasureValue =
           #   as.numeric(DetectionQuantitationLimitMeasure.MeasureValue),
            #ResultMeasureValue = ifelse(
             # is.na(ResultMeasureValue) | ResultMeasureValue == "",
              #DetectionQuantitationLimitMeasure.MeasureValue,
            #  ResultMeasureValue
            #),
            #TADA.ResultMeasureValue=ifelse(
             # is.na(TADA.ResultMeasureValue) | TADA.ResultMeasureValue=='',
              #DetectionQuantitationLimitMeasure.MeasureValue,
              #TADA.ResultMeasureValue
          #  ),
           # ResultMeasure.MeasureUnitCode=ifelse(
            #  is.na(ResultMeasure.MeasureUnitCode) | ResultMeasure.MeasureUnitCode=='',
             # DetectionQuantitationLimitMeasure.MeasureUnitCode,
              #ResultMeasure.MeasureUnitCode),
            #TADA.ResultMeasure.MeasureUnitCode=ifelse(
             # is.na(TADA.ResultMeasure.MeasureUnitCode) | TADA.ResultMeasure.MeasureUnitCode=='NONE' | TADA.ResultMeasure.MeasureUnitCode=='',
              #DetectionQuantitationLimitMeasure.MeasureUnitCode,
              #TADA.ResultMeasure.MeasureUnitCode)
          #)
        result<-EPATADA::TADA_RunKeyFlagFunctions(result,clean=FALSE)
        return(result)
        },error=function(e){
          showNotification(paste0('Error retrieving records:',e$message),type='error',duration=10)
          return(data.frame())
        })
    })
    output$results_table<-renderRHandsontable({
      df<-storet_data()
      req(nrow(df)>0)
      rhandsontable(df,
                    readOnly = TRUE,
                    rowHeaders = FALSE,
                    stretchH='all',
                    height=500)|>
        hot_context_menu(allowRowEdit = FALSE,allowColEdit = FALSE)|>
        hot_cols(columnSorting = TRUE)
    })
    observe({
      req(input$downloadstoret)
      df<-storet_data()
      showNotification(
        paste0('Retrieved ',nrow(df),' records'),
        type='message',
        duration = 10
      )
    })

    output$record_numbers<-renderText({
      df<-storet_data()
      paste0('Total Records: ',nrow(df))})

    output$storetdownload<-downloadHandler(
      filename = function(){
        paste0('storet',input$site,format(input$date[1]),'to',format(input$date[2]),'.csv')},
      content=function(file){
        df<-storet_data()
        write.csv(df,file,row.names=FALSE)
      }
    )
    clean_storet_data<-eventReactive(
      input$cleanstoret,{
        df<-storet_data()
      EPATADA::TADA_RunKeyFlagFunctions(df,clean=TRUE)
    })
    observe({
      req(input$cleanstoret)
      df1<-clean_storet_data()
      df2<-storet_data()
      showNotification(
        paste0('Removed ',nrow(df2)-nrow(df1),' records'),
        type='message',
        duration=10
      )
    })

    output$clean_records<-renderText({
      df<-clean_storet_data()
      paste0('Total Records: ',nrow(df))})

    output$clean_table<-renderRHandsontable({
      df<-clean_storet_data()
      rhandsontable(df,
                    rowHeaders = FALSE,
                    readOnly = TRUE,
                    stretchH='all',
                    height=500)|>
        hot_context_menu(allowRowEdit = FALSE,allowColEdit = FALSE)|>
        hot_cols(columnSorting = TRUE)
    })
    output$cleandownload<-downloadHandler(
      filename=function(){
        paste0('cleanstoret',input$site,format(input$date[1]),'to',format(input$date[2]),'.csv')
      },
      content=function(file){
        df<-clean_storet_data()
        write.csv(df,file,row.names =FALSE )
      }
    )
    return(list(
      clean_storet_data=clean_storet_data
    ))


  })
}

## To be copied in the UI
# mod_STORET_ui("STORET_1")

## To be copied in the server
# mod_STORET_server("STORET_1")
