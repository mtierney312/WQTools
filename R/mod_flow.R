#' flow UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card_header card_body layout_sidebar sidebar
#' @importFrom plotly plotlyOutput renderPlotly
library(dataRetrieval)
library(hydroGOF)
library(dplyr)
library(zoo)
library(plotly)
library(ggplot2)
library(lubridate)
library(sf)
library(purrr)
library(zip)
mod_flow_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      card_header("Flow Data and Load Duration Analysis"),
      layout_sidebar(
        sidebar = sidebar(
          h4("Step 1: Get Date Range from E. coli Data"),
          radioButtons(
            ns('flow_source'),
            'E. coli Data Source',
            choices = c('Use Cleaned STORET' = 'cleaned', 'Upload File' = 'upload'),
            selected = 'cleaned'
          ),
          conditionalPanel(
            condition = "input.flow_source == 'upload'",
            ns = ns,
            fileInput(ns('flow_upload_file'), 'Upload E. coli CSV File', accept = '.csv')
          ),
          uiOutput(ns("date_range_message")),

          hr(),
          h4("Step 2: Download Flow Data"),
          # Station drainage area - OUTSIDE both conditional panels
          numericInput(
            ns("target_drainage_area"),
            "Target Location Drainage Area (sq mi):",
            value = 47.7,
            min = 0
          ),
          radioButtons(
            ns('flow_data_source'),
            'Flow Data Source',
            choices = c('Download from USGS' = 'usgs', 'Upload CSV File' = 'upload_flow'),
            selected = 'usgs'
          ),
          conditionalPanel(
            condition = "input.flow_data_source == 'usgs'",
            ns = ns,
            textAreaInput(
              ns("gage_ids"),
              "Enter USGS Gage ID(s) (comma-separated):",
              value = "",
              placeholder = "e.g., 02167705, 02167582, 02167450"
            ),
            uiOutput(ns("drainage_area_inputs")),  # These are the GAGE drainage areas
            dateRangeInput(
              ns("date_range"),
              "Select Date Range:",
              start = Sys.Date() - 365,
              end = Sys.Date()
            )
          ),
          checkboxInput(ns('nonzero'), "Check to set any negative values to .0001", value = TRUE),
          conditionalPanel(
            condition = "input.flow_data_source == 'upload_flow'",
            ns = ns,
            fileInput(
              ns('flow_csv_upload'),
              'Upload CSV Flow Data',
              accept = '.csv'
            )
          ),
          input_task_button(
            ns("download_btn"),
            "Download & Adjust USGS Data"
          ),
          conditionalPanel(
            condition = "input.flow_data_source == 'usgs'",
            ns = ns,
            downloadButton(ns('download_flow_csv'), 'Download Flow Data CSV')
          ),

          hr(),
          h4("Step 3: Select Gage for Load Analysis"),
          uiOutput(ns("gage_selector")),
          numericInput(
            ns("wq_target"),
            "Water Quality Target",
            value = 349,
            min = 0
          ),
          numericInput(
            ns("mos_percent"),
            "Margin of Safety (%):",
            value = 5,
            min = 0,
            max = 100
          ),
          input_task_button(
            ns("calculate_load_btn"),
            "Calculate Load Duration"
          ),
          downloadButton(ns('download_plots'), 'Download Plots')
        ),
        navset_card_tab(
          nav_panel(
            "Flow Comparison",
            plotly::plotlyOutput(ns("timeseries_plot"), height = "600px")
          ),
          nav_panel(
            "Drainage Adjustments",
            tableOutput(ns("drainage_adjustments_table"))
          ),
          nav_panel(
            "Flow Duration Curve",
            plotOutput(ns("duration_plot"), height = "600px")
          ),
          nav_panel(
            "Load Duration Curve",
            plotOutput(ns("load_duration_plot"), height = "600px"),
            uiOutput(ns('match_statistics'))
          ),
          nav_panel(
            "Load Reduction Summary",
            tableOutput(ns("load_reduction_table"))
          )
        )
      )
    )
  )
}

#' flow Server Functions
#'
#' @noRd
mod_flow_server <- function(id, cleaned_storet_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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

    # Get E. coli data for date range extraction
    source_ecoli_data <- reactive({
      if(input$flow_source == 'upload') {
        req(input$flow_upload_file)
        data <- read.csv(input$flow_upload_file$datapath)
        data$ActivityStartDate <- parse_dates(data$ActivityStartDate,"ActivtyStartDate")
        return(data)
      } else {
        req(cleaned_storet_data)
        data <- cleaned_storet_data()

        # Ensure ActivityStartDate is properly formatted
        if("ActivityStartDate" %in% names(data)) {
          data$ActivityStartDate <- parse_dates(data$ActivityStartDate,"ActivityStartDate")
        }

        return(data)
      }
    })

    # Display date range message
    output$date_range_message <- renderUI({
      df <- source_ecoli_data()

      if(is.null(df) || nrow(df) == 0) {
        return(
          div(
            style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px;",
            icon("info-circle"),
            " Please provide E. coli data first."
          )
        )
      }

      if(!"ActivityStartDate" %in% names(df)) {
        return(
          div(
            style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
            icon("exclamation-triangle"),
            " Data does not contain ActivityStartDate column."
          )
        )
      }

      min_date <- min(df$ActivityStartDate, na.rm = TRUE)
      max_date <- max(df$ActivityStartDate, na.rm = TRUE)

      div(
        style = "padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
        icon("check-circle"),
        p(
          strong("Suggested date range based on E. coli data:"),
          br(),
          sprintf("From: %s", format(min_date, "%Y-%m-%d")),
          br(),
          sprintf("To: %s", format(max_date, "%Y-%m-%d"))
        )
      )
    })

    # Parse gage IDs
    gage_list <- reactive({
      req(input$gage_ids)
      trimws(unlist(strsplit(input$gage_ids, ",")))
    })

    # Create dynamic drainage area inputs for each gage
    output$drainage_area_inputs <- renderUI({
      gages <- gage_list()
      if(length(gages) == 0) return(NULL)

      lapply(gages, function(gage_id) {
        numericInput(
          ns(paste0("da_", gage_id)),
          paste("Gage", gage_id, "Drainage Area (sq mi):"),
          value = 50,
          min = 0
        )
      })
    })

    # Download and adjust USGS gage data
adjusted_data <- eventReactive(input$download_btn, {
  # Handle uploaded flow data
  if(input$flow_data_source == 'upload_flow') {
    req(input$flow_csv_upload)
    req(input$target_drainage_area)

    tryCatch({
      # Read uploaded CSV
      flow_data <- read.csv(input$flow_csv_upload$datapath, stringsAsFactors = FALSE)

      # Standardize column names and parse dates
      # Try to find date column regardless of name
      date_col <- NULL
      if("time" %in% names(flow_data)) {
        date_col <- "time"
      } else if("date" %in% names(flow_data)) {
        date_col <- "date"
      } else if("Date" %in% names(flow_data)) {
        date_col <- "Date"
      }
      
      if(is.null(date_col)) {
        showNotification("CSV must contain 'date', 'Date', or 'time' column", type = "error")
        return(NULL)
      }
      
      # Parse dates with multiple format attempts
      flow_data$date <- tryCatch({
        # Get the date column values
        date_values <- flow_data[[date_col]]
        
        # Try different parsing methods
        # First try: lubridate mdy (M/D/YYYY or MM/DD/YYYY)
        parsed <- mdy(date_values, quiet = TRUE)
        
        # If that didn't work, try ymd (YYYY-MM-DD)
        if(all(is.na(parsed))) {
          parsed <- ymd(date_values, quiet = TRUE)
        }
        
        # If still not working, try as.Date with format parameter
        if(all(is.na(parsed))) {
          parsed <- as.Date(date_values, format = "%m/%d/%Y")
        }
        
        # Last resort: try default as.Date
        if(all(is.na(parsed))) {
          parsed <- as.Date(date_values)
        }
        
        parsed
      }, error = function(e) {
        return(rep(NA, nrow(flow_data)))
      })
      
      # Check if date parsing succeeded
      if(all(is.na(flow_data$date))) {
        showNotification("Could not parse dates. Please ensure dates are in M/D/YYYY, MM/DD/YYYY, or YYYY-MM-DD format", type = "error")
        return(NULL)
      }

      # Handle flow value column - try multiple possible names
      flow_col <- NULL
      if("value" %in% names(flow_data)) {
        flow_col <- "value"
      } else if("flow_cfs" %in% names(flow_data)) {
        flow_col <- "flow_cfs"
      } else if("Value" %in% names(flow_data)) {
        flow_col <- "Value"
      } else if("flow" %in% names(flow_data)) {
        flow_col <- "flow"
      }
      
      if(is.null(flow_col)) {
        showNotification("CSV must contain 'value', 'flow_cfs', or 'flow' column", type = "error")
        return(NULL)
      }
      
      flow_data$flow_uploaded <- as.numeric(flow_data[[flow_col]])

      # Handle negative values if checkbox is checked
      if(input$nonzero && any(flow_data$flow_uploaded <= 0, na.rm = TRUE)) {
        message('Negatives and Zeroes Found; Converting to 0.0001')
        flow_data$flow_uploaded[flow_data$flow_uploaded <= 0] <- 0.0001
      }

      # Select and format data
      result <- flow_data %>%
        select(date, flow_uploaded) %>%
        filter(!is.na(flow_uploaded), !is.na(date))

      message("Uploaded flow data has ", nrow(result), " rows")
      message("Date range: ", min(result$date), " to ", max(result$date))

      # Add drainage info as attribute
      attr(result, "drainage_info") <- data.frame(
        Gage_ID = "Uploaded",
        Gage_DA_sqmi = input$target_drainage_area,
        Target_DA_sqmi = input$target_drainage_area,
        DA_Ratio = 1.0
      )

      attr(result, "gage_list") <- c("uploaded")

      showNotification("Flow data uploaded successfully", type = "message")
      return(result)

    }, error = function(e) {
      message("Error reading flow CSV: ", e$message)
      showNotification(paste("Error reading flow CSV:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  } else {
    # USGS download code
    req(gage_list(), input$target_drainage_area)

    gages <- gage_list()
    if(length(gages) == 0) {
      showNotification("Please enter at least one gage ID", type = "error")
      return(NULL)
    }

    target_da <- input$target_drainage_area
    time_interval <- paste0(
      as.character(input$date_range[1]),
      "/",
      as.character(input$date_range[2])
    )

    message("Time interval: ", time_interval)
    message("Target DA: ", target_da)

    tryCatch({
      # Download and adjust all gages
      results <- lapply(gages, function(gage_id) {
        message("Processing gage: ", gage_id)

        gage_da <- input[[paste0("da_", gage_id)]]
        message("Gage DA: ", gage_da)

        if(is.null(gage_da) || gage_da == 0) {
          message("Gage DA is NULL or 0, skipping")
          return(NULL)
        }

        message("Downloading data for gage: ", gage_id)

        # Download using correct parameter names
        data <- read_waterdata_daily(
          monitoring_location_id = paste0('USGS-', as.character(gage_id)),
          parameter_code = "00060",
          statistic_id = "00003",
          time = time_interval
        )

        message("Downloaded ", nrow(data), " rows for gage ", gage_id)
        message("Column names: ", paste(names(data), collapse = ", "))

        if(is.null(data) || nrow(data) == 0) {
          message("No data returned for gage: ", gage_id)
          return(NULL)
        }

        # Drop geometry if present
        if(inherits(data, "sf")) {
          message("Dropping geometry")
          data <- st_drop_geometry(data)
        }

        if(input$nonzero && any(data$value <= 0, na.rm = TRUE)) {
          message('Negatives and zeroes found; converting to 0.0001')
          data$value[data$value <= 0] <- 0.0001
        }

        da_ratio <- target_da / gage_da
        flow_col <- paste0("flow_", gsub("[^0-9]", "", gage_id))

        message("Creating adjusted data with column: ", flow_col)

        list(
          data = data %>%
            mutate(
              date = as.Date(time),
              !!flow_col := as.numeric(value) * da_ratio
            ) %>%
            select(date, !!flow_col),
          info = data.frame(
            Gage_ID = gage_id,
            Gage_DA_sqmi = gage_da,
            Target_DA_sqmi = target_da,
            DA_Ratio = round(da_ratio, 4)
          )
        )
      })

      message("Number of results: ", length(results))
      message("NULL results: ", sum(sapply(results, is.null)))

      # Remove NULL results
      results <- results[!sapply(results, is.null)]

      if(length(results) == 0) {
        showNotification("No valid gage data downloaded", type = "error")
        return(NULL)
      }

      # Merge all data
      merged <- Reduce(
        function(x, y) merge(x, y, by = "date", all = TRUE),
        lapply(results, `[[`, "data")
      )

      message("Merged data has ", nrow(merged), " rows")

      attr(merged, "drainage_info") <- do.call(rbind, lapply(results, `[[`, "info"))
      attr(merged, "gage_list") <- gages

      showNotification(sprintf("Downloaded %d gage(s)", length(results)), type = "message")
      merged

    }, error = function(e) {
      message("Error occurred: ", e$message)
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      NULL
    })
  }
})
    #End of Reactive


    # Gage selector UI
    output$gage_selector <- renderUI({
      req(adjusted_data())
      data <- adjusted_data()
      gages <- attr(data, "gage_list")

      if(is.null(gages) || length(gages) == 0) return(NULL)

      selectInput(
        ns("selected_gage"),
        "Select Gage for Load Analysis:",
        choices = gages,
        selected = gages[1]
      )
    })

    # Render drainage adjustments table
    output$drainage_adjustments_table <- renderTable({
      req(adjusted_data())
      data <- adjusted_data()
      attr(data, "drainage_info")
    })

    # Create time series plot
    output$timeseries_plot <- renderPlotly({
      req(adjusted_data())
      data <- adjusted_data()

      flow_cols <- setdiff(names(data), "date")
      fig <- plot_ly(data, x = ~date)

      # Add each gage's flow data
      colors <- c('darkorange', 'deepskyblue', 'red', 'purple', 'darkseagreen4', 'coral')

      for(i in seq_along(flow_cols)) {
        col <- flow_cols[i]
        gage_name <- paste("Gage", gsub("flow_", "", col), "(adjusted)")

        fig <- fig %>%
          add_trace(
            y = data[[col]],
            name = gage_name,
            mode = 'lines',
            line = list(color = colors[i %% length(colors) + 1], width = 1.5)
          )
      }

      fig <- fig %>%
        layout(
          title = "Adjusted Flow Comparison Over Time",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Adjusted Flow (cfs)"),
          hovermode = "x unified",
          legend = list(x = 0.1, y = 1)
        )

      fig
    })

    # Prepare flow data for selected gage
    flow_sorted <- reactive({
      req(adjusted_data())
      req(input$selected_gage)

      data <- adjusted_data()
      # Handle uploaded data
      if(input$flow_data_source == 'upload_flow') {
        col_name <- "flow_uploaded"
      } else {
        col_name <- paste0("flow_", gsub("[^0-9]", "", input$selected_gage))
      }

      if(!col_name %in% names(data)) {
        showNotification("Selected gage not found in data", type = "error")
        return(NULL)
      }

      flow_data <- data %>%
        select(date, flow_site_cfs = !!col_name) %>%
        filter(!is.na(flow_site_cfs)) %>%
        arrange(desc(flow_site_cfs)) %>%
        mutate(
          rank = row_number(),
          n = n(),
          exceed_p = (rank / (n + 1)) * 100,
          hydro_cat = case_when(
            exceed_p <= 10 ~ "High Flows",
            exceed_p <= 40 ~ "Moist Conditions",
            exceed_p <= 60 ~ "Mid-Range Flows",
            exceed_p <= 90 ~ "Dry Conditions",
            TRUE ~ "Low Flows"
          )
        )

      mean_flow <- mean(flow_data$flow_site_cfs, na.rm = TRUE)
      flow_data$mean_daily_flow_cfs <- mean_flow

      return(flow_data)
    })

    # Create flow duration curve
    flowdurationplot <- reactive({
      req(flow_sorted())

      flow_data <- flow_sorted()

      fdc_colors <- c(
        "High Flows" = "deepskyblue4",
        'Moist Conditions' = 'deepskyblue',
        'Mid-Range Flows' = 'green3',
        'Dry Conditions' = 'darkgoldenrod1',
        'Low Flows' = 'red3'
      )

      cat_labels <- data.frame(
        x = c(5, 25, 50, 75, 95),
        label = c("High Flows", "Moist Conditions", "Mid-Range Flows",
                  "Dry Conditions", "Low Flows")
      )

      p <- ggplot(flow_data, aes(x = exceed_p, y = flow_site_cfs, color = hydro_cat)) +
        geom_line(
          data = flow_data,
          aes(x = exceed_p, y = mean_daily_flow_cfs),
          color = "grey50",
          linewidth = 0.8,
          linetype = "dashed"
        ) +
        geom_line(linewidth = 1) +
        geom_vline(
          xintercept = c(10, 40, 60, 90),
          linetype = "solid",
          color = "dodgerblue",
          linewidth = 0.6
        ) +
        annotate(
          "text",
          y = min(flow_data$flow_site_cfs),
          x = cat_labels$x,
          size = 5,
          hjust = 0.5,
          label = cat_labels$label
        ) +
        scale_y_log10(labels = scales::comma) +
        scale_x_continuous(
          breaks = seq(0, 100, 10),
          labels = paste0(seq(0, 100, 10), "%"),
          limits = c(0, 100)
        ) +
        scale_color_manual(values = fdc_colors) +
        labs(
          title = paste("Flow Duration Curve - Gage", input$selected_gage),
          x = "Percent of Time Flow is Equaled or Exceeded",
          y = "Daily Mean Flow (cfs)",
          color = "Hydrologic Category"
        ) +
        theme_classic(base_size = 15) +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      return(p)
    })
    
    output$duration_plot <- renderPlot({
      print(flowdurationplot())
    })

    # Calculate load duration when button is clicked
    load_results <- eventReactive(input$calculate_load_btn, {
      req(flow_sorted())
      req(source_ecoli_data())
      req(input$wq_target)
      req(input$mos_percent)

      flow_data <- flow_sorted()
      ecoli_data <- source_ecoli_data()

      # Apply margin of safety to target
      WQ_TARGET <- input$wq_target * (1 - (input$mos_percent / 100))
      CONV_FACTOR <- 24465758.4  # Conversion factor for loads

      # Match E. coli to flow data
      ecoli_with_flow <- ecoli_data %>%
        rename(date = ActivityStartDate) %>%
        inner_join(flow_data %>% select(date, flow_site_cfs, exceed_p, hydro_cat), by = "date")

      # Check if we have the E. coli value column
      if("TADA.ResultMeasureValue" %in% names(ecoli_with_flow)) {
        ecoli_with_flow <- ecoli_with_flow %>%
          mutate(ecoli_value = as.numeric(TADA.ResultMeasureValue))
      } else if("ecoli_value" %in% names(ecoli_with_flow)) {
        ecoli_with_flow <- ecoli_with_flow %>%
          mutate(ecoli_value = as.numeric(ecoli_value))
      } else {
        showNotification("Cannot find E. coli value column", type = "error")
        return(NULL)
      }

      ecoli_with_flow <- ecoli_with_flow %>%
        filter(!is.na(ecoli_value), !is.na(flow_site_cfs)) %>%
        mutate(inst_load = ecoli_value * flow_site_cfs * CONV_FACTOR)

      # Calculate target curve
      target_curve <- flow_data %>%
        mutate(target_load = WQ_TARGET * flow_site_cfs * CONV_FACTOR)

      # Calculate existing loads at category midpoints
      midpoints <- c(
        "High Flows" = 0.95,
        "Moist Conditions" = 0.75,
        "Mid-Range Flows" = 0.50,
        "Dry Conditions" = 0.25,
        "Low Flows" = 0.05
      )

      existing_loads <- purrr::map_dfr(names(midpoints), function(cat) {
        pct <- midpoints[cat]
        mid_flow <- quantile(flow_data$flow_site_cfs, probs = pct, na.rm = TRUE)

        # Get E. coli samples in this category
        cat_ecoli <- ecoli_with_flow %>% filter(hydro_cat == cat)

        p90_conc <- if (nrow(cat_ecoli) > 0) {
          quantile(cat_ecoli$ecoli_value, 0.90, na.rm = TRUE)
        } else {
          NA
        }

        existing_load <- if (!is.na(mid_flow) && !is.na(p90_conc)) {
          p90_conc * mid_flow * CONV_FACTOR
        } else {
          NA
        }

        data.frame(
          hydro_cat = cat,
          exceed_p = pct * 100,
          mid_flow_cfs = mid_flow,
          p90_conc = p90_conc,
          existing_load = existing_load,
          stringsAsFactors = FALSE
        )
      })

      # Calculate percent reduction needed
      percent_reduction <- existing_loads %>%
        filter(!is.na(existing_load)) %>%
        mutate(
          target_load = WQ_TARGET * mid_flow_cfs * CONV_FACTOR,
          load_reduction = existing_load - target_load,
          pct_reduction = round((existing_load - target_load) / existing_load * 100, 1),
          pct_reduction = ifelse(pct_reduction < 0, 0, pct_reduction),
          meets_standard = ifelse(pct_reduction == 0, "No Reduction Needed",
                                  paste0(pct_reduction, "%"))
        ) %>%
        select(hydro_cat, exceed_p, mid_flow_cfs, p90_conc,
               existing_load, target_load, load_reduction, pct_reduction, meets_standard)

      list(
        target_curve = target_curve,
        existing_loads = existing_loads,
        ecoli_loads = ecoli_with_flow,
        percent_reduction = percent_reduction,
        wq_target = WQ_TARGET,
        conv_factor = CONV_FACTOR
      )
    })

    # Render load duration curve
    plotloadduration <- reactive({
      req(input$calculate_load_btn)
      req(load_results())

      results <- load_results()
      target_curve <- results$target_curve
      existing_loads <- results$existing_loads
      ecoli_loads <- results$ecoli_loads

      fdc_colors <- c(
        "High Flows" = "deepskyblue4",
        'Moist Conditions' = 'deepskyblue',
        'Mid-Range Flows' = 'green3',
        'Dry Conditions' = 'darkgoldenrod1',
        'Low Flows' = 'red3'
      )

      cat_labels <- data.frame(
        x = c(5, 25, 50, 75, 95),
        label = c("High Flows", "Moist Conditions", "Mid-Range Flows",
                  "Dry Conditions", "Low Flows")
      )

      # Define category boundaries
      cat_boundaries <- data.frame(
        hydro_cat = c("High Flows", "Moist Conditions", "Mid-Range Flows",
                      "Dry Conditions", "Low Flows"),
        x_start = c(0, 10, 40, 60, 90),
        x_end = c(10, 40, 60, 90, 100),
        stringsAsFactors = FALSE
      )

      # Add boundary info to existing loads
      existing_loads_plot <- existing_loads %>%
        filter(!is.na(existing_load)) %>%
        left_join(cat_boundaries, by = "hydro_cat")

      # Filter out NA values from ecoli_loads
      ecoli_loads_plot <- ecoli_loads %>%
        filter(!is.na(inst_load), !is.na(exceed_p))

      # Calculate y-axis limits
      all_values <- c(
        ecoli_loads_plot$inst_load,
        target_curve$target_load,
        existing_loads_plot$existing_load
      )

      y_min <- min(all_values, na.rm = TRUE) * 0.5
      y_max <- max(all_values, na.rm = TRUE) * 2

      p <- ggplot() +
        # Target load curve
        geom_line(
          data = target_curve,
          aes(x = exceed_p, y = target_load, color = "Target Load w/ MOS"),
          linewidth = 1
        ) +
        # Existing load horizontal segments
        geom_segment(
          data = existing_loads_plot,
          aes(
            x = x_start,
            xend = x_end,
            y = existing_load,
            yend = existing_load,
            color = "Existing Load"
          ),
          linewidth = 1.2
        ) +
        # Measured E. coli points
        geom_point(
          data = ecoli_loads_plot,
          aes(x = exceed_p, y = inst_load, shape = "Measured E. coli"),
          color = "steelblue",
          size = 2.5
        ) +
        # Category boundary lines
        geom_vline(
          xintercept = c(10, 40, 60, 90),
          linetype = "solid",
          color = "dodgerblue",
          linewidth = 0.6
        ) +
        # Category labels at bottom
        annotate(
          "text",
          x = cat_labels$x,
          y = rep(y_min * 1.2, 5),
          label = cat_labels$label,
          size = 5,
          hjust = 0.5
        ) +
        scale_y_log10(
          labels = scales::scientific,
          limits = c(y_min, y_max)
        ) +
        scale_x_continuous(
          breaks = seq(0, 100, 10),
          labels = paste0(seq(0, 100, 10), "%"),
          limits = c(0, 100),
          expand = expansion(add = c(0.01, 0.01))
        ) +
        scale_color_manual(
          name = NULL,
          values = c(
            "Target Load w/ MOS" = "darkgreen",
            "Existing Load" = "magenta3"
          )
        ) +
        scale_shape_manual(
          name = NULL,
          values = c("Measured E. coli" = 17)
        ) +
        labs(
          title = paste("Load Duration Curve - Gage", input$selected_gage),
          x = expression("Percent of Time Intervals That Flows" >=""),
          y = "E. coli Load (#/day)"
        ) +
        theme_classic(base_size = 15) +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      return(p)
    })
    
    output$load_duration_plot <- renderPlot({
      print(plotloadduration())
    })

    # Render load reduction table
    formatted_table <- reactive({
      req(load_results())

      results <- load_results()

      # Format the table nicely
      results$percent_reduction %>%
        mutate(
          hydro_cat = factor(hydro_cat, levels = c("High Flows", "Moist Conditions",
                                                   "Mid-Range Flows", "Dry Conditions", "Low Flows")),
          mid_flow_cfs = round(mid_flow_cfs, 2),
          p90_conc = round(p90_conc, 1),
          existing_load = scales::scientific(existing_load, digits = 3),
          target_load = scales::scientific(target_load, digits = 3),
          load_reduction = scales::scientific(load_reduction, digits = 3)
        ) %>%
        arrange(hydro_cat) %>%
        rename(
          `Hydrologic Category` = hydro_cat,
          `Percentile` = exceed_p,
          `Midpoint Flow (cfs)` = mid_flow_cfs,
          `90th %ile E. coli` = p90_conc,
          `Existing Load (#/day)` = existing_load,
          `Target Load (#/day)` = target_load,
          `Load Reduction (#/day)` = load_reduction,
          `% Reduction Needed` = pct_reduction,
          `Status` = meets_standard
        )
    })

    # Render the table in a separate call
    output$load_reduction_table <- renderTable({
      formatted_table()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$match_statistics <- renderUI({
      req(load_results())

      results <- load_results()
      ecoli_total <- nrow(source_ecoli_data())
      ecoli_matched <- nrow(results$ecoli_loads)

      div(
        style = "padding: 10px; margin-top: 10px; background-color: #e7f3ff; border: 1px solid #bee5eb; border-radius: 4px;",
        icon("info-circle"),
        p(
          strong("Matching Statistics:"),
          br(),
          sprintf("E. coli records matched to flow: %d of %d (%.1f%%)",
                  ecoli_matched,
                  ecoli_total,
                  (ecoli_matched/ecoli_total)*100)
        )
      )
    })

    output$download_plots <- downloadHandler(
      filename = function() {
        paste0("load_duration_analysis_", input$selected_gage, "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        # Create temporary directory
        temp_dir <- tempdir()
        temp_files <- character(0)
        tryCatch({
          req(flowdurationplot())
          fdc_file <- file.path(temp_dir, "01_flow_duration_curve.png")
          ggsave(fdc_file, plot = flowdurationplot(), width = 10, height = 6, dpi = 300)
          temp_files <- c(temp_files, fdc_file)
        }, error = function(e) {
          message("Could not save flow duration curve: ", e$message)
        })

        # Save load duration curve
        tryCatch({
          req(plotloadduration())
          ldc_file <- file.path(temp_dir, "02_load_duration_curve.png")
          ggsave(ldc_file, plot = plotloadduration(), width = 10, height = 6, dpi = 300)
          temp_files <- c(temp_files, ldc_file)
        }, error = function(e) {
          message("Could not save load duration curve: ", e$message)
        })

        # Save load reduction table as CSV
        tryCatch({
          req(formatted_table())
          table_file <- file.path(temp_dir, "03_load_reduction_summary.csv")
          write.csv(formatted_table(), file = table_file, row.names = FALSE)
          temp_files <- c(temp_files, table_file)
        }, error = function(e) {
          message("Could not save table: ", e$message)
        })

        # Create zip file
        if (length(temp_files) > 0) {
          zip::zip(zipfile = file, files = temp_files, mode = "cherry-pick")
        } else {
          showNotification("No files available to download", type = "error")
        }
      }
    )
    
    output$download_flow_csv <- downloadHandler(
      filename = function() {
        if (input$flow_data_source == 'usgs' && !is.null(input$selected_gage)) {
          paste0("flow_data_", input$selected_gage, "_", Sys.Date(), ".csv")
        } else {
          paste0("flow_data_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        req(adjusted_data())

        data <- adjusted_data()

        # If a gage is selected, export only that gage's data
        if (!is.null(input$selected_gage)) {
          if (input$flow_data_source == 'usgs') {
            col_name <- paste0("flow_", gsub("[^0-9]", "", input$selected_gage))

            if (col_name %in% names(data)) {
              export_data <- data %>%
                select(date, flow_cfs = !!col_name) %>%
                filter(!is.na(flow_cfs))

              write.csv(export_data, file, row.names = FALSE)
              showNotification("Flow data downloaded successfully", type = "message")
            } else {
              showNotification("Selected gage data not found", type = "error")
            }
          } else if (input$flow_data_source == 'upload_flow') {
            # For uploaded data, export with flow_cfs column
            if ("flow_uploaded" %in% names(data)) {
              export_data <- data %>%
                select(date, flow_cfs = flow_uploaded) %>%
                filter(!is.na(flow_cfs))

              write.csv(export_data, file, row.names = FALSE)
              showNotification("Flow data downloaded successfully", type = "message")
            } else {
              showNotification("Uploaded flow data not found", type = "error")
            }
          }
        } else {
          # If no gage selected, export all flow data
          write.csv(data, file, row.names = FALSE)
          showNotification("Flow data downloaded successfully", type = "message")
        }
      }
    )
  })
}



## To be copied in the UI
# mod_flow_ui("flow_1")

## To be copied in the server
# mod_flow_server("flow_1")
