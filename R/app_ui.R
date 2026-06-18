#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(EPATADA)
library(shiny)
library(bslib)
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "Water Quality Modeling",
      id = "main_navbar",
      nav_panel(
        title = "Start",
        value = "start",
        card(
          card_header("Which task would you like to start?"),
          card_body(
            radioButtons("task",
                         choices = c("WLA", "TMDL"),
                         label = "Choose a Task:")
          )
        )
      ),
      nav_panel(
        title = "Velocity",
        value = "velocity",
        mod_Velocity_ui("mod_Velocity_module")
      ),
      nav_panel(title="STORET Data Download",
                value='download',
                mod_STORET_ui("mod_STORET_module")),
      nav_panel(title='WLA STORET Summary',
                value='summary',
                mod_wlastoretsummary_ui('mod_wlastoretsummary_module')),
      nav_panel(title="Precipitation Graph",
                value='precip',
                mod_precipupload_ui('mod_precipupload_module')),
      nav_panel(title='Flow Comparison',
                value='flowcomp',
                mod_flow_ui('mod_flow_module')
      )))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "WQTools"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
