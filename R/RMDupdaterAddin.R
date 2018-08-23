library(shiny)
library(miniUI)
library(rstudioapi)
library(googledrive)
library(knitr)
library(yaml)

Find <- function(pattern, original){
  # find subarray of string in array of strings, return array index of the beginning of subarray
  pattern.length <- length(pattern)
  original.length <- length(original)
  candidate <- seq.int(length=original.length-pattern.length+1)
  for (i in seq.int(length=pattern.length)) {
    candidate <- candidate[pattern[i] == original[candidate + i - 1]]
  }
  candidate
}

Upload <- function(odt.report, report.name, report.name.draft, sync.path){
  result <- googledrive::drive_upload(odt.report, name = report.name, type = "document")
  fair <- result[[2]] # gdoc id for fair copy
  result <- googledrive::drive_upload(odt.report, name = report.name.draft, type = "document")
  draft <- result[[2]] # gdoc id for draft

  # write info in sync_reports.sh
  fair.link <- paste0(" https://docs.google.com/document/d/", fair, "/")
  fair.string <- paste0("# ", report.name, fair.link)
  draft.string <- paste0("gdrive update ", draft, " ", odt.report, " --name ", report.name.draft)
  cat("\n", fair.string, draft.string,file=sync.path,sep="\n",append=TRUE)
}

Compare <- function(echo.md.path, fair.id, name){
  # run the comparing python script
  answer <- shell(paste0("RMD_updater.py ", echo.md.path, " ", fair.id, " ", name), intern = TRUE) # getting answer from python
}

PerformRefactor <- function(contents, from, to, useWordBoundaries = FALSE) {

  matches <- gregexpr(from, contents, fixed = TRUE)

  changes <- sum(unlist(lapply(matches, function(x) {
    if (x[[1]] == -1) 0 else length(x)
  })))

  refactored <- unlist(lapply(contents, function(x) {
    gsub(from, to, x, fixed = TRUE)
  }))

  list(
    refactored = refactored,
    changes = changes
  )
}

Echo <- function(content, context){
  # copying content of current report and replace ECHO=FALSE to ECHO=TRUE, return  changed content
  file.create("report_copy.rmd")
  spec <- PerformRefactor(content, from = "knitr::opts_chunk$set(echo = FALSE)", to = "knitr::opts_chunk$set(echo = TRUE)")  # CHANGE BEFORE RELIASE
  if (spec$changes == 0){
    spec <<- PerformRefactor(content, from = "echo = FALSE", to = "echo = TRUE")  # TODO: make it regular
  }
  spec$refactored  # return as character vector
  #  transformed <- paste(spec$refactored, collapse = "\n")  # return as string witn \n
}

CopyAndCompare <- function(echo.true.report, fair.id, name){
  copy <- paste0(name, "_report_copy.rmd")
  result<- paste0(name, "_echo_report.rmd")
  Ignore(copy, result)
  file.create(copy)
  out <- file(description=copy, open="w", encoding="UTF-8")
  writeLines(echo.true.report, con=out)
  close(con=out)

  knitr::knit(input = copy, output = result)
  answer <- Compare(echo.md.path = result, fair.id = fair.id)
  file.remove(c(copy, result))
  answer
}

ExtractTitle <- function(content){
  indexes <- grep("^---[[:space:]]*$", content, value = FALSE)
  if (length(indexes) < 2){
    message("TitleError: something wrong with YAML information. Can't find title. Exit.")
    title <- NULL
  }
  else {
    info <- yaml::yaml.load(content[indexes[1]+1:indexes[2]-1])
    title <- info$title
  }
}

ExtractName <- function(path){
  name.ext <- basename(path)
  name <- gsub("\\.*$", "", name.ext)
}

Ignore <- function(copy, echo){
  gitignore <- ".gitignore"
  extension <- ".changes"
  if (file.exists(gitignore)){
    content <- readLines(gitignore)
    gitfile <- file(description=gitignore, open="w", encoding = "UTF-8")
    result <- grep(copy, content, fixed=TRUE)
    if (length(result) == 0){
      write(copy, file=gitfile, append=TRUE)
    }
    result <- grep(echo, content, fixed=TRUE)
    if (length(result) == 0){
      write(echo, file=gitfile, append=TRUE)
    }
    result <- grep(extension, content, fixed=TRUE)
    if (length(result) == 0){
      write(extension, file=gitfile, append=TRUE)
    }
    close(gitfile)
  }
  else{
    file.create(gitignore)
    gitfile <- file(description=gitignore, open="w", encoding="UTF-8")
    write(copy, file=gitfile, append=TRUE)
    write(echo, file=gitfile, append=TRUE)
    write(extension, file=gitfile, append=TRUE)
    close(gitfile)
  }
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
    my.changes <- NaN
    context <- NaN
    iter <- 1
    outer.iter <- 1
    memory <- c()

    draft.id <- NaN
    fair.id <- NaN
    current.report <- NaN
    report.path <- NaN
    report.name <- NaN
    report.name.draft <- NaN
    result <- NaN
    odt.report <- NaN
    sync.path <- NaN
    sync.info <- NaN
    name <- NaN

    GetInformation <- function(){
      # extracting report name
      current.report <<- rstudioapi::getActiveDocumentContext()
      report.path <<- current.report$path
      name <<- ExtractName(report.path)


      title <- ExtractTitle(current.report$contents)
      if (is.null(title)){
        NULL
      }
      else {
        report.name <<- paste0("\"", title, "\"")
        report.name.draft <<- gsub("\"$", ". Draft\"", report.name)
        project.path <- rstudioapi::getActiveProject() # path for sync_reports
        sync.path <<- paste0(project.path, "/sync_reports.sh")
        sync.info <<- readLines(sync.path)

        # looking for report info in sync_reports.sh
        regular.exp <- paste0("^# ", report.name)
        result <<- grep(regular.exp, sync.info)

        # building path to odt
        find.odt.report <<- gsub("\\.Rmd$", ".odt", report.path) # CHANGE BEFORE RELEASE to .rmd
        normalized.path <- normalizePath(find.odt.report)
        normalized.project.path <- normalizePath(project.path)
        new.odt.report <- gsub(paste0(normalized.project.path, "\\"), "", normalized.path, fixed = TRUE)
        odt.report <<- gsub("\\", "/", new.odt.report, fixed = TRUE)
        if ( ! file.exists(odt.report)){
          message(paste0("File ", odt.report, " was not found."))
          if (menu(c("Yes"), title = "Select it manually?") == 1){
            odt.report <<- rstudioapi::selectFile(caption = "Select knitted report:", label = "Select", path = NULL,
                                               filter = "*.odt", existing = TRUE)
          }
          else {
            shiny::stopApp()
          }
        }
        returnValue(1)
      }
    }

    Highlight <- function(){
      context <<- rstudioapi::getActiveDocumentContext()
      content <- context$contents
      if (content[1] == "" & length(content) == 1){
        print("Set your cursor to *.rmd document and try again")
      }
      else {
        indexes <- grep("^$", content, value = FALSE, invert = TRUE)
        original.without.comments <- grep("^$", content, value = TRUE, invert = TRUE)
        shift.content <- list(index = indexes, content = original.without.comments)
        original <- shift.content$content
        context.length <- 0
        pattern <- c("")
        res.pattern <- c("")
        if (my.changes[iter] == "~~ CONTEXT") {
          memory[outer.iter] <<- iter
          outer.iter <<- outer.iter +1
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
          pattern.length <- length(pattern)
          res.pattern.length <- length(res.pattern)
          candidate <- Find(pattern = pattern, original = original)
          res.candidate <- Find(pattern = res.pattern, original = original)
          if (length(candidate) > 0){
            start.line <- shift.content$index[candidate[1]+context.length]
            end.line <- shift.content$index[candidate[1]+pattern.length-1]
            rstudioapi::setSelectionRanges(rstudioapi::document_range(rstudioapi::document_position(start.line, 1),
                                                                      rstudioapi::document_position(end.line, nchar(pattern[pattern.length])+1)),
                                           id = NULL)
          }
          else if (length(res.candidate) > 0){
            start.line <- shift.content$index[res.candidate[1]]
            end.line <- shift.content$index[res.candidate[1]+res.pattern.length-1]
            rstudioapi::setSelectionRanges(rstudioapi::document_range(rstudioapi::document_position(start.line, 1),
                                                                      rstudioapi::document_position(end.line, nchar(res.pattern[res.pattern.length])+1)),
                                           id = NULL)
          }
          else {
            message("- - - - - NOT FOUND. BLOCK: - - - - -")
            print(res.pattern)
          }
        }
        iter <<- iter + 1
      }
    }

    shiny::observeEvent(input$upd, {
      info <- GetInformation()
      if (is.null(info)){
        shiny::stopApp()
      }
      else if (length(result) == 0){  # report info wasnt found
        message(paste0("Information associated with ", report.name, " was not found in sync_reports.sh."))
        choice <- menu(c("Yes"), title = "Do you want create new fair and draft?")
        if (choice == 1){
          Upload(odt.report, report.name, report.name.draft, sync.path)
          message("Uploaded successfully")
        }
        shiny::stopApp()
      }
      else {
        fair.string <- sync.info[result[1]]
        fair.id <<- strsplit(fair.string, split = "/", fixed = TRUE)[[c(1,6)]]
        draft.string <- sync.info[result[1] + 1]
        draft.id <<- strsplit(draft.string, split = " ", fixed = TRUE)[[c(1,3)]]
        message("Draft info was found. Comparison process . . .")
        echo.true.report <- Echo(content = current.report$contents, current.report)
        answer <- CopyAndCompare(echo.true.report, fair.id, name)
        if (answer[1] == "OUTDATED BLOCKS FOUNDED"){
          message("Changes detected. Please use 'Find next' button to see outdated blocks.")
          message("You can ignore changes and use 'Force update' button.")
        }
        else if (answer[1] == "UP TO DATE"){
          message("RMDupdater didn't detect any changes.")
          choice <- menu(c("Yes"), title = "Do you want update draft?")
          if (choice == 1){
            googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
            message("Updated successfully")
          }
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
      if (is.nan(draft.id)){
        GetIformation()
        if (length(result) == 0){  # report info wasnt found
          message("Files were not found in sync_reports.sh.")
          message("Use 'Update' button.")
        }
        else {
          draft.string <- sync.info[result[1] + 1]
          draft.id <<- strsplit(draft.string, split = " ", fixed = TRUE)[[c(1,3)]]
          choice <- menu(c("Yes"), title = "Do you want update draft?")
          if (choice == 1){
            googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
            message("Updated successfully")
          }
          shiny::stopApp()
        }
      }
      else {
        choice <- menu(c("Yes"), title = "Do you want update draft?")
        if (choice == 1){
          googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
          message("Updated successfully")
        }
        shiny::stopApp()
      }
    })

    shiny::observeEvent(input$prv, {
      if (outer.iter == 1 | outer.iter == 2){
        message("- - - - - YOU ARE IN THE BEGINNING OF THE FILE. - - - - -")
      }
      else{
        outer.iter <<- outer.iter - 2
        iter <<- memory[outer.iter]
        Highlight()
      }
    })

    shiny::observeEvent(input$nxt, {
      if (iter == 1){
        log.file <- paste0(name, ".changes")
        if ( ! file.exists(log.file)){
          massage("Can't find *.changes file.")
          message("Use 'Update' button to create it.")
        }
        else{
          my.changes <<- readLines(log.file)
        }
      }
      if (length(my.changes) == 1){
        message("- - - - - NO CHANGES DETECTED - - - - -")
      }
      else if (iter > length(my.changes)){
        message("- - - - - END OF CHANGES FILE. USE 'FIND PREV' OR 'DONE' - - - - -")
      }
      else {
        Highlight()
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



