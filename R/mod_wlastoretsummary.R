#' wlastoretsummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(bslib)
library(shiny)
library(dplyr)
library(tidyr)
library(rhandsontable)
mod_wlastoretsummary_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      card_header('WLA STORET Summary'),
      layout_sidebar(
        sidebar = sidebar(
          radioButtons(
            ns('data_source'),
            'Data Source',
            choices = c('Upload File' = 'upload', 'Use Cleaned STORET' = 'cleaned'),
            selected = 'cleaned'
          ),
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            ns = ns,
            fileInput(ns('upload_file'), 'Upload CSV File', accept = '.csv')
          ),
          input_task_button(ns('generate_summary'), 'Generate Summary'),
          textOutput(ns('summary_info')),
          em('* < 3 Years of Data')
        ),
        card(
          card_header('Temperature Analysis'),
          tableOutput(ns('temp_summary'))
        ),
        card(
          card_header('Characteristic Summaries by Month'),
          rHandsontableOutput(ns('summary_table')),
          downloadButton(ns('download_summary'), 'Download Summary.csv')
        ),
        card(card_header('Number of Results by Month'),
             rHandsontableOutput(ns('count_table')))
      )
    )
  )
}

#' wlastoretsummary Server Functions
#'
#' @noRd
mod_wlastoretsummary_server <- function(id,cleaned_storet_data=NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    source_data <- reactive({
      if(input$data_source == 'upload') {
        req(input$upload_file)
        read.csv(input$upload_file$datapath)
      } else {
        req(cleaned_storet_data)
        cleaned_storet_data()
      }
    })

    # Filter data based on characteristic-specific date requirements
    filtered_data <- reactive({
      df <- source_data()
      req(nrow(df) > 0)

      # Ensure date column exists and is properly formatted
      if(!"ActivityStartDate" %in% names(df)) {
        showNotification("ActivityStartDate column not found", type = "error")
        return(NULL)
      }

      df$ActivityStartDate <- as.Date(df$ActivityStartDate)
      df$Year <- as.numeric(format(df$ActivityStartDate, "%Y"))

      # Filter out NH3 data prior to 2001
      df <- df %>%
        filter(!(grepl("Ammonia", CharacteristicName, ignore.case = TRUE) & Year < 2001))

      # Filter out phosphorus data prior to 2002
      df <- df %>%
        filter(!(grepl("phosphorus|phosphate", CharacteristicName, ignore.case = TRUE) & Year < 2002))

      df
    })

    # Temperature analysis - Modified to use monthly averages
    temp_analysis <- eventReactive(input$generate_summary, {
      df <- filtered_data()
      req(nrow(df) > 0)

      df$Month <- format(df$ActivityStartDate, "%m")
      df$MonthName <- format(df$ActivityStartDate, "%B")

      # Filter for temperature data
      temp_data <- df %>%
        filter(grepl("temperature|temp", CharacteristicName, ignore.case = TRUE))

      if(nrow(temp_data) == 0) {
        showNotification("No temperature data found", type = "warning")
        return(list(
          overall_max = NA,
          overall_month = NA,
          winter_max = NA,
          winter_month = NA
        ))
      }

      # Ensure ResultMeasureValue is numeric
      temp_data$TADA.ResultMeasureValue <- as.numeric(temp_data$TADA.ResultMeasureValue)
      temp_data <- temp_data %>% filter(!is.na(TADA.ResultMeasureValue))

      # Calculate average temperature by month across all years
      monthly_avg <- temp_data %>%
        group_by(Month, MonthName) %>%
        summarise(
          AvgTemp = mean(TADA.ResultMeasureValue, na.rm = TRUE),
          .groups = 'drop'
        )

      # Find month with highest average temperature overall
      overall_max_row <- monthly_avg %>%
        filter(AvgTemp == max(AvgTemp, na.rm = TRUE)) %>%
        slice(1)

      # Winter months (November-February: 11, 12, 01, 02)
      winter_avg <- monthly_avg %>%
        filter(Month %in% c("11", "12", "01", "02"))

      winter_max_row <- if(nrow(winter_avg) > 0) {
        winter_avg %>%
          filter(AvgTemp == max(AvgTemp, na.rm = TRUE)) %>%
          slice(1)
      } else {
        NULL
      }

      list(
        overall_max = if(nrow(overall_max_row) > 0) overall_max_row$AvgTemp else NA,
        overall_month = if(nrow(overall_max_row) > 0) overall_max_row$MonthName else NA,
        winter_max = if(!is.null(winter_max_row) && nrow(winter_max_row) > 0) winter_max_row$AvgTemp else NA,
        winter_month = if(!is.null(winter_max_row) && nrow(winter_max_row) > 0) winter_max_row$MonthName else NA
      )
    })

    # Display temperature summary
    output$temp_summary <- renderTable({
      temp_info <- temp_analysis()
      req(temp_info)

      data.frame(
        Metric = c("Highest Average Temperature (Overall)", "Month of Highest Average",
                   "Highest Average Temperature (Nov-Feb)", "Month of Highest Average (Winter)"),
        Value = c(
          ifelse(is.na(temp_info$overall_max), "N/A", paste0(round(temp_info$overall_max, 2))),
          ifelse(is.na(temp_info$overall_month), "N/A", temp_info$overall_month),
          ifelse(is.na(temp_info$winter_max), "N/A", paste0(round(temp_info$winter_max, 2))),
          ifelse(is.na(temp_info$winter_month), "N/A", temp_info$winter_month)
        )
      )
    }, striped = TRUE, hover = TRUE)

    # Generate summary table by characteristic and month
    summary_table_data <- eventReactive(input$generate_summary, {
      df <- filtered_data()
      req(nrow(df) > 0)

      # Prepare data
      df$Month <- factor(format(df$ActivityStartDate, "%B"),
                         levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
      df$Year <- format(df$ActivityStartDate, "%Y")
      df$TADA.ResultMeasureValue <- as.numeric(df$TADA.ResultMeasureValue)

      # Calculate mean by characteristic and month
      date_ranges <- df %>%
        filter(!is.na(TADA.ResultMeasureValue)) %>%
        group_by(CharacteristicName) %>%
        summarise(
          DateRange = paste0(format(min(ActivityStartDate, na.rm = TRUE), "%Y-%m-%d"),
                             " to ",
                             format(max(ActivityStartDate, na.rm = TRUE), "%Y-%m-%d")),
          .groups = 'drop'
        )

      # Calculate mean by characteristic and month
      summary <- df %>%
        filter(!is.na(TADA.ResultMeasureValue)) %>%
        group_by(CharacteristicName, Month) %>%
        summarise(
          MeanValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
          YearsOfData = n_distinct(Year),
          .groups = 'drop'
        ) %>%
        mutate(
          MeanValue = round(MeanValue, 3),
          LessThan3Years = YearsOfData < 3
        )

      # Pivot to wide format
      summary_wide <- summary %>%
        select(CharacteristicName, Month, MeanValue, LessThan3Years) %>%
        pivot_wider(
          names_from = Month,
          values_from = c(MeanValue, LessThan3Years),
          names_sep = "_"
        ) %>%
        left_join(date_ranges, by = "CharacteristicName")

      list(
        data = summary_wide,
        flag_cols = grep("LessThan3Years", names(summary_wide))
      )
    })
    count_table_data <- eventReactive(input$generate_summary, {
      df <- filtered_data()
      req(nrow(df) > 0)

      df$Month <- factor(format(df$ActivityStartDate, "%B"),
                         levels=c('January','February','March','April','May','June',
                                  'July','August','September','October','November','December'))
      df$TADA.ResultMeasureValue <- as.numeric(df$TADA.ResultMeasureValue)

      # Count by characteristic and month
      count_summary <- df %>%
        filter(!is.na(TADA.ResultMeasureValue)) %>%
        group_by(CharacteristicName, Month) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(
          names_from = Month,
          values_from = Count,
          values_fill = 0
        )

      count_summary
    })

    output$summary_table <- renderRHandsontable({
      summary_info <- summary_table_data()
      req(summary_info)

      df <- summary_info$data

      # Create display dataframe with only MeanValue columns
      month_order <- c('January','February','March','April','May','June','July','August','September','October','November','December')
      mean_cols <- paste0("MeanValue_", month_order)
      date_cols <- paste0("DateRange_", month_order)

      mean_cols <- grep("MeanValue_", names(df), value = TRUE)
      display_cols <- c("CharacteristicName", mean_cols, "DateRange")
      display_df <- df[, display_cols]

      # Rename columns to remove MeanValue_ prefix
      names(display_df) <- gsub("MeanValue_", "", names(display_df))
      names(display_df)[names(display_df) == "DateRange"] <- "Date Range"
      names(display_df) <- gsub("DateRange_", "", names(display_df))
      # Add suffix to date range columns
      date_col_names <- month_order[paste0("DateRange_", month_order) %in% display_cols]
      for(month in date_col_names) {
        col_idx <- which(names(display_df) == month)
        if(length(col_idx) > 1) {
          names(display_df)[col_idx[2]] <- paste0(month, " (Date Range)")
        }
      }

      display_df <- as.data.frame(display_df)
      display_df[] <- lapply(display_df, as.character)

      # Create a character version for display with markers
      display_with_markers <- display_df

      for(i in seq_along(summary_info$flag_cols)) {
        flag_col <- summary_info$flag_cols[i]
        value_col_name <- gsub("LessThan3Years_", "", names(df)[flag_col])

        if(value_col_name %in% names(display_with_markers)) {
          col_idx <- which(names(display_with_markers) == value_col_name)
          rows_to_highlight <- which(df[[flag_col]] == TRUE)

          # Add asterisk to values with insufficient data
          if(length(rows_to_highlight) > 0) {
            for(row_idx in rows_to_highlight) {
              current_val <- display_with_markers[row_idx, col_idx]
              if(!is.na(current_val)) {
                display_with_markers[row_idx, col_idx] <- paste0(current_val, " *")
              }
            }
          }
        }
      }


      rhandsontable(
        display_with_markers,
        readOnly = TRUE,
        rowHeaders = FALSE,
        stretchH = 'all',
        height = 500
      ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(columnSorting = FALSE)
    })
    output$count_table <- renderRHandsontable({
      count_df <- count_table_data()
      req(count_df)

      count_df <- as.data.frame(count_df)
      count_df[] <- lapply(count_df, as.character)

      rhandsontable(
        count_df,
        readOnly = TRUE,
        rowHeaders = FALSE,
        stretchH = 'all',
        height = 400
      ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(columnSorting = FALSE)
    })

    output$summary_info <- renderText({
      req(input$generate_summary)
      df <- filtered_data()
      paste0("Total records: ", nrow(df))
    })

    output$download_summary <- downloadHandler(
      filename = function() {
        paste0('wla_summary_', Sys.Date(), '.csv')
      },
      content = function(file) {
        summary_info <- summary_table_data()
        req(summary_info)

        df <- summary_info$data
        mean_cols <- grep("MeanValue_", names(df), value = TRUE)
        display_cols <- c("CharacteristicName", mean_cols, "DateRange")
        display_df <- df[, display_cols]
        names(display_df) <- gsub("MeanValue_", "", names(display_df))
        names(display_df)[names(display_df) == "DateRange"] <- "Date Range"

        # Add asterisks to values with < 3 years of data
        display_df <- as.data.frame(display_df)
        display_df[] <- lapply(display_df, as.character)

        for(i in seq_along(summary_info$flag_cols)) {
          flag_col <- summary_info$flag_cols[i]
          value_col_name <- gsub("LessThan3Years_", "", names(df)[flag_col])

          if(value_col_name %in% names(display_df)) {
            col_idx <- which(names(display_df) == value_col_name)
            rows_to_highlight <- which(df[[flag_col]] == TRUE)

            # Add asterisk to values with insufficient data
            if(length(rows_to_highlight) > 0) {
              for(row_idx in rows_to_highlight) {
                current_val <- display_df[row_idx, col_idx]
                if(!is.na(current_val) && current_val != "") {
                  display_df[row_idx, col_idx] <- paste0(current_val, " *")
                }
              }
            }
          }
        }

        write.csv(display_df, file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_wlastoretsummary_ui("wlastoretsummary_1")

## To be copied in the server
# mod_wlastoretsummary_server("wlastoretsummary_1")
