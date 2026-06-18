#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Hide Velocity tab initially
  hideTab(inputId = "main_navbar", target = "velocity")
  hideTab(inputId = "main_navbar", target = "precip")
  hideTab(inputId = "main_navbar", target = "summary")
  hideTab(inputId = "main_navbar", target = "download")
  hideTab(inputId='main_navbar',target='flowcomp')

  # Show Velocity tab when WLA is selected
  observe({
    if(input$task == "WLA") {
      showTab(inputId = "main_navbar", target = 'summary')
      showTab(inputId = "main_navbar", target = "velocity")
      showTab(inputId = "main_navbar", target = 'download')
      hideTab(inputId='main_navbar',target='precip')
      hideTab(inputId='main_navbar',target='flowcomp')
    } else {
      showTab(inputId = "main_navbar", target='precip')
      showTab(inputId = "main_navbar", target='download')
      showTab(inputId = "main_navbar", target='summary')
      showTab(inputId='main_navbar',target='flowcomp')
      hideTab(inputId='main_navbar',target='velocity')
    }
  }) %>%
    bindEvent(input$task)

  # Call the Velocity module server
  mod_Velocity_server("mod_Velocity_module")

  storet_outputs <- mod_STORET_server("mod_STORET_module")

  # Pass the cleaned data to the modules
  mod_wlastoretsummary_server(
    "mod_wlastoretsummary_module",
    cleaned_storet_data = storet_outputs$clean_storet_data
  )
  mod_precipupload_server(
    'mod_precipupload_module',
    cleaned_storet_data=storet_outputs$clean_storet_data
  )
  mod_flow_server(
    'mod_flow_module',
    cleaned_storet_data=storet_outputs$clean_storet_data
  )
}
