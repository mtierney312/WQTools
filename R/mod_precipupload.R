#' precipupload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

library(dplyr)

mod_precipupload_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      card_header("Precipitation Data Upload and Analysis"),
      layout_sidebar(
        sidebar = sidebar(
          radioButtons(
            ns('storet_source'),
            'STORET Data Source',
            choices = c('Upload File' = 'upload', 'Use Cleaned STORET' = 'cleaned'),
            selected = 'cleaned'
          ),
          conditionalPanel(
            condition = "input.storet_source == 'upload'",
            ns = ns,
            fileInput(ns('storet_upload_file'), 'Upload STORET CSV File', accept = '.csv')
          ),
          uiOutput(ns("upload_message")),
          fileInput(
            ns("prism_file"),
            "Upload PRISM CSV File",
            accept = c(".csv")
          ),
          input_task_button(
            ns("process_btn"),
            "Process Data"
          ),
          textOutput(ns("merge_info"))
        ),
        card(
          card_header("Precipitation vs E. coli Correlation"),
          plotOutput(ns("correlation_plot"), height = "500px"),
          downloadButton(ns('downloadplot'),'Download Precipitation Plot')
        )
      )
    )
  )
}
#' precipupload Server Functions
#'
#' @noRd
mod_precipupload_server <- function(id, cleaned_storet_data = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Define parse_dates function once at module level
    parse_dates <- function(x, name = "Date") {
      formats <- c("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y", "%m-%d-%Y", "%Y/%m/%d", "%d-%m-%Y")
      result <- as.Date(rep(NA, length(x)))
      for (fmt in formats) {
        na_idx <- is.na(result)
        result[na_idx] <- as.Date(x[na_idx], format = fmt)
      }
      failed <- sum(is.na(result) & !is.na(x))
      if (failed > 0) warning(sprintf("%s: %d/%d dates failed", name, failed, length(x)))
      result
    }

    # Get STORET data based on source selection
    source_storet_data <- reactive({
      if(input$storet_source == 'upload') {
        req(input$storet_upload_file)
        read.csv(input$storet_upload_file$datapath)
      } else {
        req(cleaned_storet_data)
        cleaned_storet_data()
      }
    })

    # Display upload message with date range
    output$upload_message <- renderUI({
      df <- source_storet_data()

      if(is.null(df) || nrow(df) == 0) {
        return(
          div(
            style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px;",
            icon("info-circle"),
            " Please upload STORET data or clean the STORET data from the STORET tab."
          )
        )
      }

      if(!"ActivityStartDate" %in% names(df)) {
        return(
          div(
            style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
            icon("exclamation-triangle"),
            " STORET data does not contain ActivityStartDate column."
          )
        )
      }

      # Parse dates and store result
      df$ActivityStartDate <- parse_dates(df$ActivityStartDate, "ActivityStartDate")

      # Check if any dates were successfully parsed
      if(all(is.na(df$ActivityStartDate))) {
        return(
          div(
            style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;",
            icon("times-circle"),
            " Failed to parse any dates from ActivityStartDate column."
          )
        )
      }

      min_date <- min(df$ActivityStartDate, na.rm = TRUE)
      max_date <- max(df$ActivityStartDate, na.rm = TRUE)

      div(
        style = "padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
        icon("check-circle"),
        p(
          strong("Please upload PRISM data between:"),
          br(),
          sprintf("Minimum Date: %s", format(min_date, "%Y-%m-%d")),
          br(),
          sprintf("Maximum Date: %s", format(max_date, "%Y-%m-%d")),
          br(),
          sprintf("Total STORET records: %d", nrow(df))
        )
      )
    })

    # Read uploaded precipitation data
    prism_data <- reactive({
      req(input$prism_file)

      tryCatch({
        data <- read.csv(input$prism_file$datapath, stringsAsFactors = FALSE)

        if(!"Date" %in% names(data)) {
          showNotification("CSV must contain a 'Date' column", type = "error")
          return(NULL)
        }

        ppt_col <- grep("^ppt", names(data), ignore.case = TRUE, value = TRUE)

        if(length(ppt_col) == 0) {
          showNotification("CSV must contain a precipitation column starting with 'ppt'", type = "error")
          return(NULL)
        }

        names(data)[names(data) == ppt_col[1]] <- "ppt"
        data$Date <- parse_dates(data$Date, "PRISM Date")
        data <- data[!is.na(data$Date), ]

        if(nrow(data) == 0) {
          showNotification("No valid dates found", type = "error")
          return(NULL)
        }

        data$ppt <- as.numeric(data$ppt)
        data <- data[!is.na(data$ppt), ]

        if(nrow(data) == 0) {
          showNotification("No valid precipitation values found", type = "error")
          return(NULL)
        }

        showNotification(
          sprintf("Loaded %d precipitation records", nrow(data)),
          type = "message"
        )

        return(data)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    })

    # Process and merge data when button is clicked
    merged_data <- eventReactive(input$process_btn, {
      req(prism_data())

      storet <- source_storet_data()

      if(is.null(storet) || nrow(storet) == 0) {
        showNotification("Please provide STORET data", type = "error")
        return(NULL)
      }

      prism <- prism_data()
      storet$Date <- parse_dates(storet$ActivityStartDate, "STORET ActivityStartDate")

      merged <- merge(storet, prism, by = "Date", all = FALSE)

      if(nrow(merged) == 0) {
        showNotification("No matching dates found between datasets", type = "warning")
        return(NULL)
      }

      # Check if the column exists
      if(!"TADA.ResultMeasureValue" %in% names(merged)) {
        showNotification(
          paste("TADA.ResultMeasureValue column not found. Available columns:",
                paste(names(merged)[1:min(15, length(names(merged)))], collapse = ", ")),
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # Create ecoli_value column
      merged$ecoli_value <- as.numeric(as.character(merged$TADA.ResultMeasureValue))

      # Check how many non-NA values we have
      non_na_ecoli <- sum(!is.na(merged$ecoli_value))
      non_na_ppt <- sum(!is.na(merged$ppt))

      showNotification(
        paste0("Non-NA E. coli values: ", non_na_ecoli, ", Non-NA ppt values: ", non_na_ppt),
        type = "message",
        duration = 5
      )

      # Filter to complete cases
      merged <- merged %>% dplyr::filter(!is.na(ecoli_value), !is.na(ppt))

      if(nrow(merged) == 0) {
        showNotification("No complete cases after removing missing values", type = "warning")
        return(NULL)
      }

      showNotification(sprintf("Successfully merged %d observations", nrow(merged)), type = "message")
      return(merged)
    })

    output$merge_info <- renderText({
      req(input$process_btn)
      data <- merged_data()
      if(!is.null(data)) paste0("Matched records: ", nrow(data)) else ""
    })

    plotInput <- function(){
      req(merged_data())
      data <- merged_data()

      cor_value <- cor(data$ppt, data$ecoli_value, use = "complete.obs")

      par(mar = c(5, 5, 4, 2))
      plot(
        data$ppt, data$ecoli_value,
        xlab = "Precipitation (inches)", ylab = "E. coli (MPN/100mL or CFU/100mL)",
        main = sprintf("Precipitation vs E. coli\nCorrelation: %.3f (n=%d)", cor_value, nrow(data)),
        pch = 16, col = rgb(0, 0, 1, 0.5), cex = 1.2, cex.lab = 1.2, cex.main = 1.3
      )

      lm_model <- lm(ecoli_value ~ ppt, data = data)
      abline(lm_model, col = "deepskyblue", lwd = 2)
      grid()

      r_squared <- summary(lm_model)$r.squared
      legend("topleft",
             legend = c(sprintf("y = %.2fx + %.2f", coef(lm_model)[2], coef(lm_model)[1]),
                        sprintf("R2 = %.3f", r_squared)),
             bty = "n", text.col = "black", cex = 1.1)
    }

    output$correlation_plot <- renderPlot({
      plotInput()
    })

    output$downloadplot <- downloadHandler(
      filename = function(){
        paste('precipplot', Sys.Date(), '.png', sep = '')
      },
      content = function(file){
        png(file)
        plotInput()
        dev.off()
      })
  })
}
## To be copied in the UI
# mod_precipupload_ui("precipupload_1")

## To be copied in the server
# mod_precipupload_server("precipupload_1")
