#title: "RSCH-1369 Developer Ecosystem Survey 2018. Full Report"

library(shiny)
library(miniUI)
library(rstudioapi)
library(googledrive)
library(knitr)


Find <- function(patern, original){
  pattern.length <- length(patern)
  original.length <- length(original)
  candidate <- seq.int(length=original.length-pattern.length+1)
  for (i in seq.int(length=pattern.length)) {
    candidate <- candidate[patern[i] == original[candidate + i - 1]]
  }
  candidate
}

Upload <- function(odt.report, report.name, report.name.draft, sync.path){
  result <- googledrive::drive_upload(odt.report, name = report.name, type = "document")
  fair <- result[[2]] # gdoc id for clean copy
  result <- googledrive::drive_upload(odt.report, name = report.name.draft, type = "document")
  draft <- result[[2]] # gdoc id for draft

  fair.link <- paste0(" https://docs.google.com/document/d/", fair, "/")
  fair.string <- paste0("# ", report.name, fair.link)
  draft.string <- paste0("gdrive update ", draft, " ", odt.report, " --name ", report.name.draft)
  cat("\n", fair.string, draft.string,file=sync.path,sep="\n",append=TRUE)
}

Compare <- function(echo.md.path, draft.id){
  answer <- shell(paste0("RMDupdater.py ", echo.md.path, " ", draft.id), intern = TRUE) # getting answer from python
}

PerformRefactor <- function(contents, from, to, useWordBoundaries = FALSE) {

  reFrom <- from
  reTo <- to
  matches <- gregexpr(reFrom, contents, fixed = TRUE)

  changes <- sum(unlist(lapply(matches, function(x) {
    if (x[[1]] == -1) 0 else length(x)
  })))

  refactored <- unlist(lapply(contents, function(x) {
    gsub(reFrom, reTo, x, fixed = TRUE)
  }))

  list(
    refactored = refactored,
    changes = changes
  )
}

Echo <- function(content, context){
  # copying content of current report and replace all ECHO=FALSE to ECHO=TRUE, return  changed content
  file.create("report_copy.rmd")
  spec <- PerformRefactor(content, from = "knitr::opts_chunk$set(echo = FALSE)", to = "knitr::opts_chunk$set(echo = TRUE)") # CHANGE BEFORE RELIASE
  spec$refactored  # return as character vector
  #  transformed <- paste(spec$refactored, collapse = "\n")  # return as string witn \n
}

CopyAndCompare <- function(echo.true.report, draft.id){
  file.create("report_copy.rmd")
  out <- file(description="report_copy.rmd", open="w", encoding="UTF-8")
  writeLines(echo.true.report, con=out)
  close(con=out)
  knitr::knit(input = "report_copy.rmd", output = "echo_report.md")
  answer <- Compare(echo.md.path = "echo_report.md", draft.id = draft.id)
  file.remove(c("report_copy.rmd", "echo_report.md"))
  answer
}

RMDupdaterAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("RMD update"),
    miniUI::miniButtonBlock(
      shiny::actionButton("upd", "Update", icon = shiny::icon("backward")),
      border = "bottom"
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("nxt", "Find next", icon = shiny::icon("arrow-right")),
      border = "bottom"
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("prv", "Find prev", icon = shiny::icon("arrow-left")),
      border = "bottom"
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("fupd", "Force update", icon = shiny::icon("fast-backward")),
      border = "top"
    )
  )


  server <- function(input, output, session) {
    myChanges <- NaN
    context <- NaN
    original <- NaN
    iter <- 1

    draft.id <- NaN
    current.report <- NaN
    report.path <- NaN
    report.name <- NaN
    report.name.draft <- NaN
    result <- NaN
    odt.report <- NaN
    sync.path <- NaN
    sync.info <- NaN

    GetIformation <- function(){
      # extracting report name
      current.report <<- rstudioapi::getActiveDocumentContext()
      report.path <<- current.report$path

      title.string <- current.report$contents[2]  # CHANGE BEFORE RELEASE to 2
      title <- strsplit(title.string, split = "\"", fixed = TRUE)
      report.name <<- title[[c(1,2)]]
      report.name.draft <<- paste0("\"", report.name, ". Draft\"")
      report.name <<- paste0("\"", report.name, "\"")

      project.path <- rstudioapi::getActiveProject() # path for sync_reports
      sync.path <<- paste0(project.path, "/sync_reports.sh")
      sync.info <<- readLines(sync.path)

      # looking for report info in sync_reports.sh
      regular.exp <- paste0("^# ", report.name)
      result <<- grep(regular.exp, sync.info)

      # building path to odt
      odt.report <<- gsub(".rmd$", ".odt", report.path) # CHANGE BEFORE RELEASE to .rmd
      if ( ! file.exists(odt.report)){
        print(paste0("File ", odt.report, " was not found."))
        if (menu(c("Yes"), title = "Select it manually?") == 1){
          odt.report <<- rstudioapi::selectFile(caption = "Select knitted report:", label = "Select", path = NULL,
                                             filter = "*.odt", existing = TRUE)
        }
        else {
          shiny::stopApp()
        }
      }
    }


    shiny::observeEvent(input$upd, {
      GetIformation()
      if (length(result) == 0){  # report info wasnt found
        print(paste0("Information associated with ", report.name, " was not found in sync_reports.sh."))
        choice <- menu(c("Yes"), title = "Do you want create new fair and draft?")
        if (choice == 1){
          Upload(odt.report, report.name, report.name.draft, sync.path)
          print("Uploaded successfully")
        }
        shiny::stopApp()
      }
      else {
        draft.string <- sync.info[result[1] + 1]
        draft.id <<- strsplit(draft.string, split = " ", fixed = TRUE)[[c(1,3)]]
        print("Draft info found. Comparison process . . .")
        echo.true.report <- Echo(content = current.report$contents, current.report)
        answer <- CopyAndCompare(echo.true.report, draft.id)
        if (answer[1] == "OUTDATED BLOCKS FOUNDED"){
          print("Changes detected. Please use 'Find next' button to see outdated blocks.")
          print("You can ignore changes and use 'Force update' button.")
        }
        else if (answer[1] == "UP TO DATE"){
          print("RMDupdater didn't detect any changes.")
          choice <- menu(c("Yes"), title = "Do you want update draft?")
          if (choice == 1){
            googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
            print("Updated successfully")
          }
          shiny::stopApp()
        }
        else {
          print("Some errors occurred:")
          print(answer)
        }
      }
    })

    shiny::observeEvent(input$fupd, {
      if (is.nan(draft.id)){
        GetIformation()
        if (length(result) == 0){  # report info wasnt found
          print("Files were not found in sync_reports.sh.")
          print("Use 'Update' button.")
        }
        else {
          draft.string <- sync.info[result[1] + 1]
          draft.id <<- strsplit(draft.string, split = " ", fixed = TRUE)[[c(1,3)]]
          choice <- menu(c("Yes"), title = "Do you want update draft?")
          if (choice == 1){
            googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
            print("Updated successfully")
          }
          shiny::stopApp()
        }
      }
      else {
        choice <- menu(c("Yes"), title = "Do you want update draft?")
        if (choice == 1){
          googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
          print("Updated successfully")
        }
        shiny::stopApp()
      }
    })

    shiny::observeEvent(input$prv, {
      # temporary unavailable
      # TODO: just do it
      shiny::stopApp()
    })

    shiny::observeEvent(input$nxt, {
      if (iter == 1){
        log.file <- "log.changes"
        if ( ! file.exists("log.changes")){
          message("Use 'Highlight' firstly!")
        }
        myChanges <<- readLines(log.file)
        context <<- rstudioapi::getActiveDocumentContext()
        original <<- context$contents
      }
      if (length(myChanges) == 1){
        print("NO CHANGES DETECTED")
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
          candidate <- Find(patern = patern, original = original)
          res.candidate <- Find(patern = res.patern, original = original)
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

#RMDupdaterAddin()



