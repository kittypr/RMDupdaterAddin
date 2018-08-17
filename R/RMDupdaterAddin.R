#title: "RSCH-1369 Developer Ecosystem Survey 2018. Full Report"

library(shiny)
library(miniUI)
library(rstudioapi)
library(googledrive)
library(knitr)


find <- function(patern, original){
  pattern.length <- length(patern)
  original.length <- length(original)
  candidate <- seq.int(length=original.length-pattern.length+1)
  for (i in seq.int(length=pattern.length)) {
    candidate <- candidate[patern[i] == original[candidate + i - 1]]
  }
  candidate
}

upload <- function(odt.report, report.name, report.name.draft, sync.path){
  result <- googledrive::drive_upload(odt.report, name = report.name, type = "document")
  fair <- result[[2]] # gdoc id for clean copy
  result <- googledrive::drive_upload(odt.report, name = report.name.draft, type = "document")
  draft <- result[[2]] # gdoc id for draft

  fair.link <- paste0(" https://docs.google.com/document/d/", fair, "/")
  fair.string <- paste0("# ", report.name, fair.link)
  draft.string <- paste0("gdrive update ", draft, " ", odt.report, " --name ", report.name.draft)
  cat("\n", fair.string, draft.string,file=sync.path,sep="\n",append=TRUE)
}

compare <- function(echo.md.path, draft.id){
  res <- shell(paste0("RMDupdater.py ", echo.md.path, " ", draft.id), intern = TRUE) # getting answer from python
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

      title.string <- current.report$contents[1] # CHANGE BEFORE RELEASE to 2
      title <- strsplit(title.string, split = "\"", fixed = TRUE)
      report.name <- title[[c(1,2)]]
      report.name.draft <- paste0("\"", report.name, ". Draft\"")
      report.name <- paste0("\"", report.name, "\"")

      project.path <- rstudioapi::getActiveProject() # path for sync_reports
      sync.path <- paste0(project.path, "/sync_reports.sh")
      sync.info <- readLines(sync.path)

      # looking for report info in sync_reports.sh
      regular.exp <- paste0("^# ", report.name)
      result <- grep(regular.exp, sync.info)

      # building path to odt, TODO: add manual selection if not exists
      odt.report <- gsub(".R", ".odt", report.path, fixed=TRUE) # CHANGE BEFORE RELEASE to .rmd

      if (length(result) == 0){ # report info wasnt found
        print("Files were not found in sync_reports.sh.")
        choice <- menu(c("Yes"), title = "Do you want create new fair and draft?")
        if (choice == 1){
          upload(odt.report, report.name, report.name.draft, sync.path)
        }
        print("Uploaded successfully")
        shiny::stopApp()
      }
      else {
        draft.string <- sync.info[result[1] + 1]
        draft.id <- strsplit(draft.string, split = " ", fixed = TRUE)[[c(1,3)]]
        print("Draft info found. Comparison process . . .")
        # KNITTING ECHO MD HERE
        # PATH TO ECHO MD HERE
        #compare(report.path, draft.id)
        choice <- menu(c("Yes"), title = "Do you want update draft?")
        if (choice == 1){
          googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
        }
        print("Updated successfully")
        shiny::stopApp()
      }
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

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }

  viewer <- shiny::paneViewer()
  shiny::runGadget(ui, server)
}

RMDupdaterAddin()


