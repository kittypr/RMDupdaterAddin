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
  my.changes <- NULL
  my.text.changes <- NULL
  json.tables.changes <- NULL
  json.text.changes <- NULL

  iter <- 1
  outer.iter <- 1
  text.iter <- 1
  text.outer.iter <- 1

  text.memory <- c()
  memory <- c()

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
  information..extracted <- FALSE

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
    find.odt.report <<- gsub("\\.Rmd$", ".odt", report.path) # CHANGE BEFORE RELEASE to .rmd
    normalized.path <- normalizePath(find.odt.report)
    normalized.project.path <- normalizePath(project.path)
    new.odt.report <- gsub(paste0(normalized.project.path, "\\"), "", normalized.path, fixed=TRUE)
    odt.report <<- gsub("\\", "/", new.odt.report, fixed=TRUE)
    if ( ! file.exists(odt.report)){
      message(paste0("File ", odt.report, " was not found."))
      if (menu(c("Yes"), title="Select it manually?") == 1){
        odt.report <<- rstudioapi::selectFile(caption="Select knitted report:", label="Select", path=NULL,
                                              filter="*.odt", existing=TRUE)
      }
      else {
        shiny::stopApp()
      }
    }
    information..extracted <<- TRUE
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

  #' Reads *.changes file
  #'
  #' @return A list - list with 2 character vectors - pattern to select and pattern to select with context
  #'                  and 1 number length of context
  ReadChanges <- function(){
    context.length <- 0
    pattern <- c("")
    res.pattern <- c("")
    memory[outer.iter] <<- iter
    outer.iter <<- outer.iter + 1
    iter <<- iter + 1
    inner.iter <- 0
    while (my.changes[iter] != "~~ CHANGED BLOCK"){
      inner.iter <- inner.iter + 1
      pattern[inner.iter] <- my.changes[iter]
      iter <<- iter + 1
    }
    iter <<- iter + 1
    context.length <- inner.iter
    if (context.length == 1 & pattern[1] == ""){
      context.length <<-0
      inner.iter <<-0
    }
    while (my.changes[iter] != "~~ END"){
      inner.iter <- inner.iter + 1
      pattern[inner.iter] <- my.changes[iter]
      res.pattern[inner.iter - context.length] <- my.changes[iter]
      iter <<- iter + 1
    }
    list(pattern=pattern, res.pattern=res.pattern, context.length=context.length)
  }

  #' Reads *.tchanges file
  #'
  #' @return A list - list with 3 character vectors - pattern to select
  #'                                                  pattern to select with context
  #'                                                  raw text to select
  #'                  and 1 number - length of context
  ReadTchanges <- function(){
    context.length <- 0
    pattern <- c("")
    res.pattern <- c("")
    raw.text <- c("")
    text.memory[text.outer.iter] <<- text.iter
    text.outer.iter <<- text.outer.iter +1
    text.iter <<- text.iter + 1
    inner.iter <- 0
    while (my.text.changes[text.iter] != "~~ CHANGED BLOCK"){
      inner.iter <- inner.iter + 1
      pattern[inner.iter] <- my.text.changes[text.iter]
      text.iter <<- text.iter + 1
    }
    text.iter <<- text.iter + 1
    context.length <- inner.iter
    if (context.length == 1 & pattern[1] == ""){
      context.length <<-0
      inner.iter <<-0
    }
    while (my.text.changes[text.iter] != "~~ TEXT"){
      inner.iter <- inner.iter + 1
      pattern[inner.iter] <- my.text.changes[text.iter]
      res.pattern[inner.iter - context.length] <- my.text.changes[text.iter]
      text.iter <<- text.iter + 1
    }
    text.iter <<- text.iter + 1
    raw.text.iter <- 0
    while (my.text.changes[text.iter] != "~~ END"){
      raw.text.iter <- raw.text.iter + 1
      raw.text[raw.text.iter] <- my.text.changes[text.iter]
      text.iter <<- text.iter + 1
    }
    list(pattern=pattern, res.pattern=res.pattern, raw.text=raw.text, context.length=context.length)
  }

  #' Parses *.changes file: reads it, founds blocks in current report and highlights them.
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


    if (my.changes[iter] == "~~ CONTEXT") {
      readed <- ReadChanges()
      pattern <- readed$pattern
      res.pattern <- readed$res.pattern
      context.length <- readed$context.length
      pattern.length <- length(pattern)
      res.pattern.length <- length(res.pattern)

      # finds exactly the same code strings in open *.rmd file
      candidate <- Find(pattern=pattern, original=original)
      res.candidate <- Find(pattern=res.pattern, original=original)

      if (length(candidate) > 0 & ! is.null(candidate)){
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

  #' Parses *.tchanges file: reads it, founds blocks in current report and highlights them.
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

    if (my.text.changes[text.iter] == "~~ CONTEXT") {
      readed <- ReadTchanges()
      raw.text <- readed$raw.text
      pattern <-readed$pattern
      res.pattern <- readed$res.pattern
      context.length <- readed$context.length

      raw.text.length <- length(raw.text)
      pattern.length <- length(pattern)
      res.pattern.length <- length(res.pattern)

      raw.pattern <- lapply(raw.text, function(x){
        x <- gsub(" ", "[^[:alnum:]]*", x)  # allows to find text even with some formatting: "hello, **world!**" = "hello, world!"
        x <- paste0("^[^(\"'`)]*", x, "[^(\"'`)]*$") # excepts literals
        x
      })

      raw.candidate <- Find(raw.pattern, original, function(a,b){return(grepl(a,b))})
      candidate <- Find(pattern=pattern, original=original)
      res.candidate <- Find(pattern=res.pattern, original=original)

      if (length(raw.candidate) > 0 & ! is.null(raw.candidate)){
        start.line <- shift.content$index[raw.candidate[1]]
        end.line <- shift.content$index[raw.candidate[1]+raw.text.length-1]
        end.length <- nchar(original[raw.candidate[1]+raw.text.length-1])+1
        Highlight(start.line, end.line, end.length)
      } else{
        if (length(candidate) > 0 & ! is.null(candidate)){
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

  #' Founds and loads *.changes and \*.tchanges files.
  #'
  #' @return -
  ReadChangesFiles <- function(){
    log.file <- paste0(name, ".changes")
    log.text.file <- paste0(name, ".tchanges")
    if ( ! file.exists(log.file) | ! file.exists(log.text.file)){
      message("Can't find *.changes/*.tchanges file.")
      message("Use 'Update' button to create it.")
      return(NULL)
    }
    else{
      my.changes <<- readLines(log.file, encoding="UTF-8")
      my.text.changes <<- readLines(log.text.file, encoding="UTF-8")
      json.text.changes <<- rjson::fromJSON(file=paste0(name, "_text_changes.json"))
      json.tables.changes <<- rjson::fromJSON(file=paste0(name, "_tables_changes.json"))
      str(json.tables.changes)
      print("\n")
      str(json.text.changes)
      return(1)
    }
  }

  shiny::observeEvent(input$upd, {
    info <- GetInformation()
    if (is.null(info)){
      shiny::stopApp()
    }
    else if (info == 2){}
    else if ( ! was.found){  # report info wasnt found
      message(paste0("Information associated with ", report.name, " was not found in sync_reports.sh."))
      choice <- menu(c("Yes"), title="Do you want create new fair and draft?")
      if (choice == 1){
        Upload(odt.report, report.name, report.name.draft, sync.path)
        message("Uploaded successfully")
      }
      shiny::stopApp()
    }
    else {
      message("Draft info was found. Comparison process . . .")
      echo.true.report <- Echo(content=current.report$contents)
      answer <- CopyAndCompare(echo.true.report, fair.id, name)
      if (answer[1] == "OUTDATED BLOCKS WERE FOUNDED"){
        message("Changes detected. Please use 'Find next', 'Find text next' buttons to see outdated blocks.")
        message("You can ignore changes and use 'Force update' button.")
        html.name <- paste0(name, "_rmdupd.html")
        if (file.exists(html.name)){
          output$diff <- shiny::renderUI(expr=HTML(readLines(html.name, encoding="UTF-8")))
        }
      }
      else if (answer[1] == "ALL IS UP TO DATE"){
        message("RMDupdater didn't detect any changes.")
        Update(draft.id, odt.report)
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
      else if (info == 2){}
      else {
        if ( ! was.found){
          # report info wasnt found
          message("Files were not found in sync_reports.sh.")
          message("Use 'Update' button.")
        }
        else {
          draft.string <- sync.info[result[1] + 1]
          draft.id <<- strsplit(draft.string, split=" ", fixed=TRUE)[[c(1,3)]]
          Update(draft.id, odt.report)
          shiny::stopApp()
        }
      }}
    else {
      Update(draft.id, odt.report)
      shiny::stopApp()
    }
  })

  shiny::observeEvent(input$prv, {
    if (outer.iter == 1 | outer.iter == 2){
      message("- - - - - YOU ARE IN THE BEGINNING OF THE FILE. - - - - -")
      outer.iter <<- 3
    }
    outer.iter <<- outer.iter - 2
    iter <<- memory[outer.iter]
    ParseChanges()
  })

  shiny::observeEvent(input$nxt, {
    if (is.null(my.changes)){
      if (is.null(ReadChangesFiles())){
        shiny::stopApp()
        return()
      }
    }
    if (length(my.changes) == 1){
      message("- - - - - NO CHANGES DETECTED - - - - -")
    }
    else if (iter > length(my.changes)){
      message("- - - - - END OF CHANGES FILE. USE 'FIND PREV' OR 'DONE' - - - - -")
    }
    else {
      ParseChanges()
    }
  })

  shiny::observeEvent(input$tprv, {
    if (text.outer.iter == 1 | text.outer.iter == 2){
      message("- - - - - YOU ARE IN THE BEGINNING OF THE FILE. - - - - -")
      text.outer.iter <<- 3
    }
    text.outer.iter <<- text.outer.iter - 2
    text.iter <<-text.memory[text.outer.iter]
    text <- ParseTchanges()
    output$changed <- shiny::renderText(expr=text)
  })

  shiny::observeEvent(input$tnxt, {
    if (is.null(my.text.changes)){
      ReadChangesFiles()
    }
    if (length(my.text.changes) == 1){
      message("- - - - - NO CHANGES DETECTED - - - - -")
    }
    else if (text.iter > length(my.text.changes)){
      message("- - - - - END OF TCHANGES FILE. USE 'FIND PREV' OR 'DONE' - - - - -")
    }
    else {
      text <- ParseTchanges()
      output$changed <- shiny::renderText(expr=text)
    }
  })

  shiny::observeEvent(input$ofc, {
    if (! information..extracted){
      GetInformation()
    }
    if (is.null(fair.id)){
      message("Iformation was not founded.")
      return()
    }
    fair.link <- paste0("https://docs.google.com/document/d/", fair.id, "/")
    browseURL(fair.link)
  })

  shiny::observeEvent(input$odc, {
    if (! information..extracted){
      GetInformation()
    }
    if (is.null(draft.id)){
      message("Iformation was not founded.")
      return()
    }
    draft.link <- paste0("https://docs.google.com/document/d/", draft.id, "/")
    browseURL(draft.link)
  })

  shiny::observeEvent(input$odiff, {
    html.name <- paste0(name, "_rmdupd.html")
    rstudioapi::viewer(html.name)
  })

  shiny::observeEvent(input$done, {
    shiny::stopApp()
  })

}
