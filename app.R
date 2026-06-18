#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
getwd()
list.files('R')
library(shiny)
library(golem)
options(shiny.autoload.r = FALSE)
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
options(golem.app.prod = TRUE)
run_app()
