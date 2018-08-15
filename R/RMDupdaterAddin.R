library(shiny)
library(miniUI)
library(rstudioapi)
library(googledrive)

RMDupdaterAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Detect changed blocks"),
    miniUI::miniButtonBlock(
      shiny::actionButton("tst", "TEST BUTT", icon = shiny::icon("plus")),
      border = "bottom"
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("nxt", "Find next", icon = shiny::icon("arrow-right")),
      border = "bottom"
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("prv", "Find prev", icon = shiny::icon("arrow-left")),
      border = "bottom"
    )
  )


  server <- function(input, output, session) {
    myChanges <- readLines(rstudioapi::selectFile(caption = "Select File", label = "Select", path = NULL,
                   filter = "*.changes", existing = TRUE))
    context <- rstudioapi::getActiveDocumentContext()
    original <- context$contents
    print(original[16])
#    print(myChanges)
    iter <- 1

    shiny::observeEvent(input$tst, {
      print(rstudioapi::getActiveProject())
      file <- rstudioapi::selectFile(caption = "Select File", label = "Select", path = NULL,
                                     filter = "*.changes", existing = TRUE)
      print(file)
      result <- googledrive::drive_upload(file, name = "experimental")
      str(result[[2]])
    })

    shiny::observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      shiny::stopApp()
    })

    shiny::observeEvent(input$prv, {
      # Return the brushed points. See ?shiny::brushedPoints.
      shiny::stopApp()
    })

    shiny::observeEvent(input$nxt, {
      if (iter > length(myChanges)){
        print("END OF CHANGES FILE. USE 'FIND PREV' OR 'DONE'")
      } else {
        contextLength <- 0
        patern <- c("")
        res.patern <- c("")
        if (myChanges[iter] == "# CONTEXT") {
          iter <<- iter + 1
          innerIteration <- 0
          while (myChanges[iter] != "# CHANGED BLOCK"){
            innerIteration <- innerIteration + 1
            patern[innerIteration] <- myChanges[iter]
            iter <<- iter + 1
          }
          iter <<- iter + 1
          contextLength <- innerIteration
          if (contextLength == 1 & patern[1] == ""){
            contextLength <<-0
            innerIteration <<-0
          }
          while (myChanges[iter] != "# END"){
            innerIteration <- innerIteration + 1
            patern[innerIteration] <- myChanges[iter]
            res.patern[innerIteration - contextLength] <- myChanges[iter]
            iter <<- iter + 1
          }
          pLength <- length(patern)
          oLength <- length(original)
          pResLength <- length(res.patern)
          candidate <- seq.int(length=oLength-pLength+1)
          res.candidate <- seq.int(length=oLength-pResLength+1)
          for (j in seq.int(length=pResLength)) {
            res.candidate <- res.candidate[res.patern[j] == original[res.candidate + j - 1]]
          }
          for (i in seq.int(length=pLength)) {
            candidate <- candidate[patern[i] == original[candidate + i - 1]]
          }
          if (length(candidate) > 0){
            rstudioapi::setSelectionRanges(rstudioapi::document_range(rstudioapi::document_position(candidate[1]+contextLength, 1),
                                                                      rstudioapi::document_position(candidate[1]+pLength-1, nchar(patern[pLength])+1)),
                                           id = NULL)
          }
          else if (length(res.candidate) > 0){
            rstudioapi::setSelectionRanges(rstudioapi::document_range(rstudioapi::document_position(res.candidate[1], 1),
                                                                      rstudioapi::document_position(res.candidate[1]+pResLength-1, nchar(res.patern[pResLength])+1)),
                                           id = NULL)
          }
          else {
            print("NOT FOUND")
            print(res.patern)
          }
        }
        iter <<- iter + 1
      }
    })
  }

  viewer <- shiny::paneViewer()
  shiny::runGadget(ui, server)
}

RMDupdaterAddin()
