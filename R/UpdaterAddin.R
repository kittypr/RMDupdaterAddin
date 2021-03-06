library(shiny)
library(miniUI)
library(rstudioapi)
library(googledrive)
library(knitr)
library(yaml)
library(shiny)


#' Finds subvector.
#'
#' @param pattern Any type vector - subvector to look for
#' @param original Any type vector to search in
#' @param comparator Function that compares two elements
#' @return Number Vector with start indexes of entries of subvector, if pattern was founded
#'         Otherwise, zero length vector.
Find <- function(pattern, original, comparator = function(a,b){return(a==b)}){
  pattern.length <- length(pattern)
  original.length <- length(original)

  if (pattern.length > original.length){
    message("Pattern is longer then *.rmd content.")
    return(NULL)
  }

  candidate <- seq.int(length=original.length-pattern.length+1)

  # Finds all entries of the first element of pattern.
  # Compares all next elements of founded entries with all next elements of pattern.
  # Saves only ones that have all next elements equal to the all next pattern elements.
  for (i in seq.int(length=pattern.length)) {
    candidate <- candidate[comparator(pattern[i], original[candidate + i - 1])]
  }
  candidate
}


#' Uploads knitted report in two copies on Gdrive and write Gdoc ids to sync_report.sh.
#'
#' @param odt.report Character vector, knitted report path.
#' @param report.name Character vector, fair copy name on Gdrive
#' @param report.name.draft Character vector, draft copy name on Gdrive
#' @param sync.path Character vector, path to sync_report.sh file
#' @return -
Upload <- function(odt.report, report.name, report.name.draft, sync.path){
  tryCatch({
    result <- googledrive::drive_upload(odt.report, name=report.name, type="document")
    fair <- result[[2]] # gdoc id for fair copy
    result <- googledrive::drive_upload(odt.report, name=report.name.draft, type="document")
    draft <- result[[2]] # gdoc id for draft

    # writes information in sync_reports.sh
    fair.link <- paste0(" https://docs.google.com/document/d/", fair, "/")
    fair.string <- paste0("# ", report.name, fair.link)
    draft.string <- paste0("gdrive update ", draft, " ", odt.report, " --name ", report.name.draft)
    cat("\n", fair.string, draft.string, file=sync.path, sep="\n",append=TRUE)
  },
  error = function(e){
    message("Uploading error:")
    message(e$message)
  })
}


#' Updates draft copy on Gdrive.
#'
#' @param odt.report Character vector, path to knitted report
#' @param draft.id Character vector, draft copy Gdoc id
#' @return -
Update <- function(session, draft.id, odt.report){
  choice <- utils::menu(c("Yes"), title="Do you want update draft?")
  if (choice == 1){
    tryCatch({
      progress <- shiny::Progress$new(session, min=0, max=100)
      progress$set(value = 50, message = "Uploading in progress")
      googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
      message("Updated successfully")
      progress$set(value = 100, message = "Uploading complete")
      progress$close()
    },
    error = function(e) {
      message("Updating error:")
      message(e$message)
    })
  }
}

#' Reupload draft and fair copies on Gdrive with using existing links
#'
#' @param odt.report Character vector, path to knitted report
#' @param draft.id Character vector, draft copy Gdoc id
#' @param fair.id Character vector, fair copy Gdoc id
#' @return -
Reupload <- function(session, draft.id, fair.id, odt.report){
  choice <- utils::menu(c("Yes"), title="Do you want reupload draft and fair copy?")
  if (choice == 1){
    tryCatch({
      progress <- shiny::Progress$new(session, min=0, max=100)
      progress$set(value = 10, message = "Uploading in progress")
      googledrive::drive_update(googledrive::as_id(fair.id), odt.report)
      progress$set(value = 50)
      googledrive::drive_update(googledrive::as_id(draft.id), odt.report)
      message("Updated successfully")
      progress$set(value = 100, message = "Uploading complete")
      progress$close()
    },
    error = function(e) {
      message("Uploading error:")
      message(e$message)
    })
  }
}


#' Calls for comparing python script.
#'
#' @param echo.md.path Character vector, path to document with .md extension that was knitted with echo option
#' @param fair.id Character vector, fair copy Gdoc id
#' @param name Character vector, the name of current .rmd document
#' @param fair Character vector, path to downloaded from Gdrive fair copy with .odt extension
#' @return Character vector, python script's answer
Compare <- function(echo.md.path, fair.id, name, fair){
  message("Starting comparison in python script.")
  path <- system.file("src", "RMD_updater.py", package="RMDupdaterAddin", mustWork=TRUE)
  answer <- NULL
  tryCatch(
    {answer <<- system(paste0("python3 ",path, " ", echo.md.path, " ", fair.id, " ", name, " ", fair, " "), intern=TRUE)},
     error = function(e) {answer <<- system(paste0("python ",path, " ", echo.md.path, " ", fair.id, " ", name, " ", fair, " "), intern=TRUE)})
  answer
}


#' Finds patternt in content and replace it.
#'
#' @param contents Character vector, content to search in
#' @param from Character vector, regular expression, that will be replaced
#' @param to Character vector, that will be replacement
#' @return List with new content and number of changes.
PerformRefactor <- function(contents, from, to, useWordBoundaries=FALSE) {
  matches <- gregexpr(from, contents)

  # counts changes
  changes <- sum(unlist(lapply(matches, function(x) {
    if (x[[1]] == -1) 0 else length(x)
  })))

  # replaces
  refactored <- unlist(lapply(contents, function(x) {
    gsub(from, to, x)
  }))

  list(refactored = refactored, changes = changes)
}


#' Inserts cache option into copy of report.
#'
#' @param contents Character vector, content of report's copy
#' @param name String, name of rem file
#' @return Caracter vector, updated content.
SetCache <- function(contents, name){
  # little function for inserting vectors into another
  insert.at <- function(a, pos, ...){
    dots <- list(...)
    stopifnot(length(dots)==length(pos))
    result <- vector("list",2*length(pos)+1)
    result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
    result[c(FALSE,TRUE)] <- dots
    unlist(result)
  }

  cache.path <- paste0('knitr::opts_chunk$set(cache.path="rmdupd_cache/', name, '_cache/")')
  cache.string <- "knitr::opts_chunk$set(cache = TRUE)"

  reg.cache.string <- "^[^#]*knitr\\s*::\\s*opts_chunk\\s*\\$\\s*set\\s*\\(\\s*cache\\s*=\\s*TRUE\\s*\\)"
  reg.cache.path.string <- "^[^#]*knitr\\s*::\\s*opts_chunk\\s*\\$\\s*set\\s*\\(\\s*cache.path\\s*=\\s*.*\\s*\\)"

  result <- NULL
  # If we haven't cache option, we should insert it and its unique path.
  # If we have cache option
  # we should check if we have path for it and replace it with unique, if it is found, and insert if it is not.
  already.cache <- match(TRUE, grepl(reg.cache.string, contents))
  if ( ! is.na(already.cache)) {
    message("Founded cache option.")
    already.has.path <- match(TRUE, grepl(reg.cache.path.string, contents))
    if ( ! is.na(already.has.path)){
      result <- PerformRefactor(contents, from=reg.cache.path.string, to=cache.path)
      result <- result$refactored
    } else {
      result <- insert.at(contents, already.cache[1]+1, cache.path)
    }
    return(result)
  }
  pattern <- "\\s*knitr\\s*::\\s*opts_chunk\\s*\\$\\s*set"

  place <- match(TRUE, grepl(pattern, contents))
  if ( ! is.na(place)) {
    result <- insert.at(contents, place[1], cache.string)
    already.has.path <- match(TRUE, grepl(reg.cache.path.string, contents))
    if ( ! is.na(already.has.path)){
      result <- PerformRefactor(result, from=reg.cache.path.string, to=cache.path)
      result <- result$refactored
    } else {
      result <- insert.at(result, place[1]+1, cache.path)
    }
    return(result)
  }
  message("Auto cache option for copy unavailable.")
  contents
}


#' Creates copy of current report's content with echo option.
#'
#' @param content Character vector, current report content
#' @return Character vector, new content
SetEcho <- function(content){
  ref.result <- PerformRefactor(content, from="echo\\s*=\\s*FALSE", to="echo = TRUE")
  # return as character vector
  ref.result$refactored
  #  transformed <- paste(ref.result$refactored, collapse="\n")  # return as string witn \n
}

#' Creates copy of current report's content with echo and cache options.
#'
#' @param content Character vector, current report content
#' @param name String, name of rmd file
#' @return Character vector, new content
SetOptions <- function(content, name){
  result <- SetEcho(content)
  SetCache(result, name)
}

#' Creates copy of current report with echo option, knits it to .md file, calls comparation function.
#'
#' @param echo.true.report Character vector, with current report content copy with echo option
#' @param fair.id Character vector, fair copy Gdoc id
#' @param name Character vector, the name of current .rmd document
#' @return Character vector, function's answer
CopyAndCompare <- function(echo.true.report, fair.id, name){
  result <- Ignore()
  if ( ! result){
    message("WARNING: WRITING TO GITIGNORE FAILED")
  }

  copy <- paste0(name, "_copy_rmdupd.rmd")
  result <- paste0(name, "_echo_rmdupd.md")
  output <- paste0(name, "_output_rmdupd.odt")

  # downloads fair copy from google drive
  error = FALSE
  tryCatch({
    googledrive::drive_download(file=googledrive::as_id(fair.id), path=output, overwrite=TRUE)
  },
  error = function(e) {
    error <<- TRUE
    message("Error during downloading fair copy:")
    message(e$message)
  })
  if (error){
    return("Error during downloading, exit application.")
  }


  file.create(copy)
  out <- file(description=copy, open="w", encoding="UTF-8")
  writeLines(echo.true.report, con=out)
  close(con=out)

  answer <- NULL
  tryCatch({
    knitr::knit(input=copy, output=result)
    },
    error = function(e) {
      message("Knitr error:")
      message(e$message)
      answer <<- "Error during knitting, exit application."
    })
  if ( ! is.null(answer)){
    return(answer)
  }
  answer <- Compare(echo.md.path=result, fair.id=fair.id, name=name, fair=output)
  file.remove(c(copy, result, output))
  answer
}


#' Extracts title from YAML information in current report.
#'
#' @param content Character vector, current report content
#' @return Character vector, extracted title or
#'         NULL if failed
ExtractTitle <- function(content){
  # finds YAML edges
  indexes <- grep("^[[:space:]]*---[[:space:]]*$", content, value=FALSE)
  if (length(indexes) < 2){
    message("TitleError: something wrong with YAML information. Can't find title. Exit.")
    title <- NULL
  }
  else {
    info <- yaml::yaml.load(content[indexes[1]+1:indexes[2]-1])
    title <- info$title
  }
}


#' Extracts name of current report.
#'
#' @param path Character vector, current report path
#' @return Character vector, extracted report name
ExtractName <- function(path){
  name.ext <- basename(path)
  name <- gsub("\\..*$", "", name.ext)
}


#' Writes exceptions to .gitignore file.
#'
#' @return  TRUE if success
Ignore <- function(){
  gitignore <- ".gitignore"
  log.files <- "*_changes.json"
  files <- "*_rmdupd.*"
  cache.folder <- "rmdupd_cache/"
  if (file.exists(gitignore)){
    content <- readLines(gitignore)
    gitfile <- file(description=gitignore, open="a+", encoding="UTF-8")
    result <- grep(files, content, fixed=TRUE)
    if (length(result) == 0){
      write("", file=gitfile, append=TRUE)
      write(files, file=gitfile, append=TRUE)
    }
    result <- grep(log.files, content, fixed=TRUE)
    if (length(result) == 0){
      write("", file=gitfile, append=TRUE)
      write(log.files, file=gitfile, append=TRUE)
    }
    result <- grep(cache.folder, content, fixed=TRUE)
    if (length(result) == 0){
      write("", file=gitfile, append=TRUE)
      write(cache.folder, file=gitfile, append=TRUE)
    }
    close(gitfile)
    return(TRUE)
  }
  else{
    file.create(gitignore)
    gitfile <- file(description=gitignore, open="w", encoding="UTF-8")
    write("", file=gitfile, append=TRUE)
    write(files, file=gitfile, append=TRUE)
    write(log.files, file=gitfile, append=TRUE)
    write(cache.folder, file=gitfile, append=TRUE)
    close(gitfile)
    return(TRUE)
  }
}


#' Shiny gadget.
#'
#' Runs the shiny gadget with two tabs in Viewer section of RStudio.
#'
#' RMDupdaterAddin was made for synchronization of .rmd file and human readable version on Gdrive.
#' It uses python script "RMD_updater" for comparing and algorythms based on regular expressions for
#' highlighting changes in .rmd document.
#'
#' @export
RMDupdaterAddin <- function() {

  ui <- interface

  server <- server

  viewer <- shiny::paneViewer()
  shiny::runGadget(ui, server)
}

#RMDupdaterAddin()
