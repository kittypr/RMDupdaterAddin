#title: "RSCH-1369 Developer Ecosystem Survey 2018. Full Report"

library(shiny)
library(miniUI)
library(rstudioapi)
library(googledrive)
library(knitr)

find <- function(patern, original){
  pLength <- length(patern)
  oLength <- length(original)
  candidate <- seq.int(length=oLength-pLength+1)
  for (i in seq.int(length=pLength)) {
    candidate <- candidate[patern[i] == original[candidate + i - 1]]
  }
  candidate
}


RMDupdaterAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("RMD update"),
    miniUI::miniButtonBlock(
      shiny::actionButton("upd", "Update", icon = shiny::icon("arrow-top")),
      border = "bottom"
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("chc", "Check out", icon = shiny::icon("arrow-bottom")),
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
    myChanges <- NaN
    context <- NaN
    original <- NaN
    iter <- 1

    shiny::observeEvent(input$upd, {
# temporary, testing some functions
      # extracting report name
      current.report <- rstudioapi::getActiveDocumentContext()
      report.path <- current.report$path

      title.string <- file.new$contents[1] # CHANGE BEFORE RELEASE to 2
      title <- strsplit(title.string, split = '"', fixed = TRUE)
      report.name <- title[[c(1,2)]]
      report.name.draft <- paste0("\"", report.name, ". Draft\"")
      report.name <- paste0("\"",report.name, "\"")

      project.path <- rstudioapi::getActiveProject() # path for sync_reports
      sync.path <- paste0(project.path, "/sync_reports.sh")
      sync.info <- readLines(sync.path)
      regular.exp <- paste0("^# ", report.name)
      result <- grep(regular.exp, sync.info)
      print(report.name)
      print(sync.info)
      print(result)
      if (length(result) == 0){
        choice <- menu(c("Yes"), title = "Report wasn`t found in sync_reports.sh. Do you want create new finish and draft?")
        if (choice == 1){
          odt.report <- gsub(".R", ".odt", report.path, fixed=TRUE) # CHANGE BEFORE RELEASE to .rmd
          result <- googledrive::drive_upload(odt.report, name = report.name, type = "document")
          print(result)
          finish <- result[[2]]
          result <- googledrive::drive_upload(odt.report, name = report.name.draft, type = "document")
          draft <- result[[2]]
          finish.link <- paste0(" https://docs.google.com/document/d/", finish, "/")
          finish.string <- paste0("# ", report.name, finish.link)
          draft.string <- paste0("gdrive update ", draft, " ", odt.report, " --name ", report.name.draft)
          print(finish.string)
          print(draft.string)
          cat("\n", finish.string, draft.string,file=sync.path,sep="\n",append=TRUE)
        }
      }



#      file <- rstudioapi::selectFile(caption = "Select File", label = "Select", path = NULL,
#                                     filter = "*.changes", existing = TRUE)
#      print(file)
#      result <- googledrive::drive_upload(file, name = "experimental")
#      str(result[[2]]) # gdoc id
#      file.new <- rstudioapi::getActiveDocumentContext()
#      title.string <- file.new$contents[1]
#      print(title.string)
#      str(file.new$path) # rmd path
#      title <- strsplit(title.string, split = '"', fixed = TRUE)
#      print(title.string)
#      print(title[[c(1,2)]]) # report name
#      print("hi")
#      res <- shell("RMDupdater.py C:\\Users\\jetbrains\\Documents\\research-scripts-master\\research-scripts-master\\RSCH\\rsch_1369_deveco18_report\\reports\\full_report\\full_report.md 1wg3wTu2tv1gs_eCoUFZUkSNyyBVaoCeTYOIKZjGSKZU", intern = TRUE)
#      print(res) #  answer
    })

    shiny::observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      shiny::stopApp()
    })

    shiny::observeEvent(input$chc, {
      myChanges <<- readLines(rstudioapi::selectFile(caption = "Select File", label = "Select", path = NULL,
                                                    filter = "*.changes", existing = TRUE))
      context <<- rstudioapi::getActiveDocumentContext()
      original <<- context$contents
    })

    shiny::observeEvent(input$prv, {
# temporary unavailable
# TODO: just do it
      shiny::stopApp()
    })

    shiny::observeEvent(input$nxt, {
      if (length(myChanges) == 1){
        print("FILE WAS NOT CHOSEN")
      }
      else if (iter > length(myChanges)){
        print("END OF CHANGES FILE. USE 'FIND PREV' OR 'DONE'")
      }
      else {
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
          pResLength <- length(res.patern)
          candidate <- find(patern = patern, original = origina)
          res.candidate <- find(patern = res.patern, original = original)
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
