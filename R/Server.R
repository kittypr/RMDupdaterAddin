library(rstudioapi)
library(shiny)
library(rjson)

#' Shiny server.
#'
#' @param input A shiny input
#' @param output A shiny output
#' @param session A shiny session
#' @return -
server <- function(input, output, session) {
  json.tables.changes <- NULL
  json.text.changes <- NULL

  iter <- 1
  text.iter <- 1

  draft.id <- NULL
  fair.id <- NULL
  current.report <- NULL
  context <- NULL
  report.path <- NULL
  report.name <- NULL
  report.name.draft <- NULL
  result <- NULL
  odt.report <- NULL
  sync.path <- NULL
  sync.info <- NULL
  name <- NULL

  was.found <- FALSE
  information.extracted <- FALSE

  #' Gets information about cerrunt project and current opened .rmd file.
  #'
  #' @return -
  GetInformation <- function(){
    # Extracts report name
    current.report <<- rstudioapi::getActiveDocumentContext()
    report.path <<- current.report$path
    name <<- ExtractName(report.path)

    if (current.report$contents[1] == "" & length(current.report$contents) == 1){
      cat("Set your cursor to *.rmd document and try again.\n")
      return(2)
    }
    title <- ExtractTitle(current.report$contents)
    if (is.null(title)){
      return(NULL)
    }
    report.name <<- paste0("\"", title, "\"")
    report.name.draft <<- gsub("\"$", ". Draft\"", report.name)
    project.path <- rstudioapi::getActiveProject() # path for sync_reports
    sync.path <<- paste0(project.path, "/sync_reports.sh")
    sync.info <<- readLines(sync.path)

    # looks for report information in sync_reports.sh
    regular.exp <- paste0("^# ", report.name)
    result <<- grep(regular.exp, sync.info)
    if (length(result) > 0){
      was.found <<- TRUE
      fair.string <- sync.info[result[1]]
      fair.id <<- strsplit(fair.string, split="/", fixed=TRUE)[[c(1,6)]]
      draft.string <- sync.info[result[1] + 1]
      draft.id <<- strsplit(draft.string, split=" ", fixed=TRUE)[[c(1,3)]]
    }

    # building path to odt
    find.odt.report <- gsub("\\.Rmd$", ".odt", report.path) # CHANGE BEFORE RELEASE to .rmd
    normalized.path <- normalizePath(find.odt.report)
    normalized.project.path <- normalizePath(project.path)
    new.odt.report <- gsub(paste0(normalized.project.path, "\\"), "", normalized.path, fixed=TRUE)
    odt.report <<- gsub("\\", "/", new.odt.report, fixed=TRUE)
    if ( ! file.exists(odt.report)){
      message(paste0("File ", odt.report, " was not found."))
      if (utils::menu(c("Yes"), title="Select it manually?") == 1){
        odt.report <<- rstudioapi::selectFile(caption="Select knitted report:", label="Select", path=NULL,
                                              filter="*.odt", existing=TRUE)
      }
      else {
        shiny::stopApp()
      }
    }
    information.extracted <<- TRUE
    return(1)
  }

  #' Selects part of the current report.
  #'
  #' @param start.line A number of the first line of selection
  #' @param end.line A number of the ;ast line of selection
  #' @param end.length A number - the length of last line of the selection
  #'
  #' @return -
  Highlight <- function(start.line, end.line, end.length){
    rstudioapi::setSelectionRanges(rstudioapi::document_range(rstudioapi::document_position(start.line, 1),
                                                              rstudioapi::document_position(end.line, end.length+1)), id=NULL)
  }

  #' Parses tables changes: founds blocks in current report and highlights them.
  #'
  #' @return -
  ParseChanges <- function(){
    context <<- rstudioapi::getActiveDocumentContext()
    content <- context$contents
    if (content[1] == "" & length(content) == 1){
      cat("Set your cursor to *.rmd document and try again\n")
      return()
    }

    # deletes all empty lines and saves mapping
    indexes <- grep("^$", content, value=FALSE, invert=TRUE)
    original.without.empty <- grep("^$", content, value=TRUE, invert=TRUE)
    shift.content <- list(index=indexes, content=original.without.empty)
    original <- shift.content$content


    if (iter <= length(json.tables.changes$ancestor)) {
      ancestor <- json.tables.changes$ancestor[iter]
      ancestor.context <- json.tables.changes$context[iter]

      pattern <- strsplit(paste0(ancestor.context, "\n", ancestor), "\n")[[1]]
      res.pattern <- strsplit(ancestor, "\n")[[1]]
      pattern.length <- length(pattern)
      res.pattern.length <- length(res.pattern)
      context.length <- pattern.length - res.pattern.length


      # finds exactly the same code strings in open *.rmd file
      candidate <- Find(pattern=pattern, original=original)
      res.candidate <- Find(pattern=res.pattern, original=original)

      if (length(candidate) > 0 & ! is.null(candidate)){
        message("founded in context")
        start.line <- shift.content$index[candidate[1]+context.length]
        end.line <- shift.content$index[candidate[1]+pattern.length-1]
        end.length <- nchar(pattern[pattern.length])+1
        Highlight(start.line, end.line, end.length)
      }
      else if (length(res.candidate) > 0 & ! is.null(res.candidate)){
        start.line <- shift.content$index[res.candidate[1]]
        end.line <- shift.content$index[res.candidate[1]+res.pattern.length-1]
        end.length <- nchar(res.pattern[res.pattern.length])+1
        Highlight(start.line, end.line, end.length)
      }
      else {
        message("- - - - - NOT FOUND. BLOCK: - - - - -")
        cat(res.pattern)
        cat("\n")
      }
    }
    iter <<- iter + 1
  }

  #' Parses text changes: founds blocks in current report and highlights them.
  #'
  #' @return -
  ParseTchanges <- function(){
    raw.text <- c("")
    context <<- rstudioapi::getActiveDocumentContext()
    content <- context$contents
    if (content[1] == "" & length(content) == 1){
      cat("Set your cursor to *.rmd document and try again\n")
      return()
    }

    indexes <- grep("^$", content, value=FALSE, invert=TRUE)
    original.without.empty <- grep("^$", content, value=TRUE, invert=TRUE)
    shift.content <- list(index=indexes, content=original.without.empty)
    original <- shift.content$content

    if (text.iter <= length(json.text.changes$text)) {
      text <- json.text.changes$text[text.iter]
      ancestor <- json.text.changes$ancestor[text.iter]
      ancestor.context <- json.text.changes$context[text.iter]

      raw.text <- strsplit(text, "\n")[[1]]
      pattern <- strsplit(paste0(ancestor.context, "\n", ancestor), "\n")[[1]]
      res.pattern <- strsplit(ancestor, "\n")[[1]]

      raw.text.length <- length(raw.text)
      pattern.length <- length(pattern)
      res.pattern.length <- length(res.pattern)
      context.length <- pattern.length - res.pattern.length

      raw.pattern <- lapply(raw.text, function(x){
        x <- gsub(" ", "[^[:alnum:]]*", x)  # allows to find text even with some formatting: "hello, **world!**" = "hello, world!"
        x <- paste0("^[^(\"'`)]*", x, "[^(\"'`)]*$") # excepts literals
        x
      })

      raw.candidate <- Find(raw.pattern, original, function(a,b){return(grepl(a,b))})
      candidate <- Find(pattern=pattern, original=original)
      res.candidate <- Find(pattern=res.pattern, original=original)

      if (length(raw.candidate) > 0 & ! is.null(raw.candidate)){
        message("founded raw")
        start.line <- shift.content$index[raw.candidate[1]]
        end.line <- shift.content$index[raw.candidate[1]+raw.text.length-1]
        end.length <- nchar(original[raw.candidate[1]+raw.text.length-1])+1
        Highlight(start.line, end.line, end.length)
      } else{
        if (length(candidate) > 0 & ! is.null(candidate)){
          message("founded in context")
          start.line <- shift.content$index[candidate[1]+context.length]
          end.line <- shift.content$index[candidate[1]+pattern.length-1]
          end.length <- nchar(pattern[pattern.length])+1
          Highlight(start.line, end.line, end.length)
        }
        else if (length(res.candidate) > 0 & ! is.null(res.candidate)){
          start.line <- shift.content$index[res.candidate[1]]
          end.line <- shift.content$index[res.candidate[1]+res.pattern.length-1]
          end.length <- nchar(res.pattern[res.pattern.length])+1
          Highlight(start.line, end.line, end.length)
        }
        else {
          message("- - - - - NOT FOUND. BLOCK: - - - - -")
          cat(raw.text)
          cat("\n")
        }
      }
    }
    text.iter <<- text.iter + 1
    raw.text
  }

  #' Founds and loads text json and tables json files.
  #'
  #' @return -
  ReadChangesFiles <- function(){
    log.file <- paste0(name, "_tables_changes.json")
    log.text.file <- paste0(name, "_text_changes.json")
    if ( ! file.exists(log.file) | ! file.exists(log.text.file)){
      message("Can't find json objects files.")
      message("Use 'Update' button to create it.")
      return(NULL)
    }
    else{
      json.text.changes <<- rjson::fromJSON(file=log.text.file)
      json.tables.changes <<- rjson::fromJSON(file=log.file)
      return(1)
    }
  }

  shiny::observeEvent(input$upd, {
    json.text.changes <<- NULL
    json.tables.changes <<- NULL
    progress <- shiny::Progress$new(session, min=1, max = 100)
    progress$set(message = "Updating in progress", detail = "Searching for URLs . . .")
    info <- GetInformation()
    if (is.null(info)){
      shiny::stopApp()
    }
    else if (info == 2){}
    else if ( ! was.found){  # report info wasnt found
      progress$set(value = 100, message = "Updating complete", detail = "")
      progress$close()
      message(paste0("Information associated with ", report.name, " was not found in sync_reports.sh."))
      choice <- utils::menu(c("Yes"), title="Do you want create new fair and draft?")
      if (choice == 1){
        progress <- shiny::Progress$new(session, min=1, max = 100)
        progress$set(value = 10, message = "Uploading in progress")
        Upload(odt.report, report.name, report.name.draft, sync.path)
        progress$set(value = 100, message = "Uploading complete")
        message("Uploaded successfully")
        progress$close()
      }
      shiny::stopApp()
    }
    else {
      progress$set(value = 10, detail = "Creating copy of report with 'echo=True' option . . .")
      message("Draft info was found. Comparison process . . .")
      echo.true.report <- SetOptions(content=current.report$contents, name=name)
      progress$set(value = 20, detail = "Downloading, knitting and comparing. This may take a while . . .")
      answer <- CopyAndCompare(echo.true.report, fair.id, name)
      progress$set(value = 100, message = "Updating complete", detail = "")
      progress$close()
      if (answer[1] == "OUTDATED BLOCKS WERE FOUND."){
        message("Changes detected. Please use 'Find next', 'Find text next' buttons to see outdated blocks.")
        message("You can ignore changes and use 'Force update' button.")
        html.name <- paste0(name, "_rmdupd.html")
        if (file.exists(html.name)){
          output$diff <- shiny::renderUI(expr=shiny::HTML(readLines(html.name, encoding="UTF-8")))
        }
      }
      else if (answer[1] == "CHANGED OR DELETED BLOCKS WERE NOT FOUND. CHECK DIFF."){
        message("RMDupdater didn't detect any changes.")
        Update(session, draft.id, odt.report)
        shiny::stopApp()
      }
      else {
        message("Some errors occurred:")
        message(answer)
        shiny::stopApp()
      }
    }
  })

  shiny::observeEvent(input$fupd, {
    if (is.null(draft.id)){
      info <- GetInformation()
      if (is.null(info)){
        shiny::stopApp()
        return()
      }
      else if (info == 2){ # cursor out of report
        return()
      }
      else if ( ! was.found){
          # report information was not found
          message("Files were not found in sync_reports.sh.")
          message("Use 'Update' button.")
          return()
      }
    }
    Update(session, draft.id, odt.report)
  })

  shiny::observeEvent(input$bupd, {
    if (is.null(draft.id)){
      info <- GetInformation()
      if (is.null(info)){
        shiny::stopApp()
        return()
      }
      else if (info == 2){ # cursor out of report
        return()
      }
      else if ( ! was.found){
          # report information was not found
          message("Files were not found in sync_reports.sh.")
          message("Use 'Update' button.")
          return()
      }
    }
    Reupload(session, draft.id, fair.id, odt.report)
  })

  shiny::observeEvent(input$prv, {
    if (iter == 1 | iter == 2){
      message("- - - - - YOU ARE IN THE BEGINNING OF THE FILE. - - - - -")
      iter <<- 3
    }
    iter <<- iter - 2
    ParseChanges()
  })

  shiny::observeEvent(input$nxt, {
    if (is.null(json.tables.changes)){
      if (is.null(ReadChangesFiles())){
        shiny::stopApp()
        return()
      }
    }
    if (length(json.tables.changes$ancestor) == 0){
      message("- - - - - NO CHANGES DETECTED - - - - -")
    }
    else if (iter > length(json.tables.changes$ancestor)){
      message("- - - - - END OF TABLES CHANGES. USE 'FIND PREV' OR 'DONE' - - - - -")
    }
    else {
      ParseChanges()
    }
  })

  shiny::observeEvent(input$tprv, {
    if (text.iter == 1 | text.iter == 2){
      message("- - - - - YOU ARE IN THE BEGINNING OF THE FILE. - - - - -")
      text.iter <<- 3
    }
    text.iter <<- text.iter - 2
    text <- ParseTchanges()
    output$changed <- shiny::renderText(expr=text)
  })

  shiny::observeEvent(input$tnxt, {
    if (is.null(json.text.changes)){
      ReadChangesFiles()
    }
    if (length(json.text.changes$text) == 0){
      message("- - - - - NO CHANGES DETECTED - - - - -")
    }
    else if (text.iter > length(json.text.changes$text)){
      message("- - - - - END OF TEXT CHANGES. USE 'FIND PREV' OR 'DONE' - - - - -")
    }
    else {
      text <- ParseTchanges()
      output$changed <- shiny::renderText(expr=text)
    }
  })

  shiny::observeEvent(input$ofc, {
    if (! information.extracted){
      GetInformation()
    }
    if (is.null(fair.id)){
      message("Iformation was not founded.")
      return()
    }
    fair.link <- paste0("https://docs.google.com/document/d/", fair.id, "/")
    utils::browseURL(fair.link)
  })

  shiny::observeEvent(input$odc, {
    if (! information.extracted){
      GetInformation()
    }
    if (is.null(draft.id)){
      message("Iformation was not founded.")
      return()
    }
    draft.link <- paste0("https://docs.google.com/document/d/", draft.id, "/")
    utils::browseURL(draft.link)
  })

  shiny::observeEvent(input$odiff, {
    html.name <- paste0(name, "_rmdupd.html")
    rstudioapi::viewer(html.name)
  })

  shiny::observeEvent(input$done, {
    shiny::stopApp()
  })

}
