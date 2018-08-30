library(shiny)
library(miniUI)

#' Shiny gadget interface.
#'
#' Creates 2 tabs and 4 rows of buttons and 1 text field.
#'
#' There are 2 tabs: one for buttons and text output and one for html output for diff table.
interface <- shiny::fluidPage(
  miniUI::gadgetTitleBar("RMD update"),
  shiny::tabsetPanel(
    shiny::tabPanel( "Functions",
                     shiny::fluidRow(
                       miniUI::miniButtonBlock(
                         shiny::actionButton("upd", "Update", icon=shiny::icon("backward")),
                         shiny::actionButton("fupd", "Force update", icon=shiny::icon("fast-backward")))
                     ),
                     shiny::fluidRow(
                       miniUI::miniButtonBlock(
                         shiny::actionButton("prv", "Find prev", icon=shiny::icon("arrow-left")),
                         shiny::actionButton("nxt", "Find next", icon=shiny::icon("arrow-right")))
                     ),
                     shiny::fluidRow(
                       miniUI::miniButtonBlock(
                         shiny::actionButton("tprv", "Find text prev", icon=shiny::icon("arrow-left")),
                         shiny::actionButton("tnxt", "Find text next", icon=shiny::icon("arrow-right")))
                     ),
                     shiny::fluidRow(
                       style="overflow: scroll",
                       shiny::textOutput("changed")),
                     shiny::fluidRow(
                       miniUI::miniButtonBlock(
                         shiny::actionButton("odiff", "Opel diff file in browser", color="green")))
    ),
    shiny::tabPanel( "Diff", shiny::htmlOutput("diff"))
  )
)
