#_____________________________________________________________________
# Filename: saveAllGraphsList4pptx.R
# Contains saveAllGraphsInList2pptx:
# A nifty routine:
# saves all the graphs present given in a list
# (with a Title in another list).

# Created July 11, 2022   by Hervé Abdi from
#  PTCA4CATA:: saveGraph2pptx()
#
# This file contains the functions
# (includes the internal function sauveImage() )
# print.save2pptx  (print function
# for objects of class save2pptx created by
# saveAllGraphsInList2pptx).
#
#*************************************************

#_________________________________________________
#*************************************************
#* Preamble saveAllGraphsInList2pptx ----
#'  saves all the graphics in a list
#'  into a PowerPoint file.
#'
#' \code{saveAllGraphsInList2pptx}:
#' a nifty function that
#' saves into a PowerPoint file
#' all the graphics given in a list
#' (Possibly with titles if given
#' in an additional list).
#' Requires packages \code{rsv}
#' and \code{officer}.
#' @param list2Save A list with graphics
#' (that could have been created by
#' \code{ggplot2} or by
#' \code{Recordplot}).
#' @param titles4list2Save
#' A list (with the same number of items as
#' \code{list2Save}) that will serve as the title
#' of the graphs.
#'  When \code{NULL} (default)
#' the title of the graph will the name of the
#' graph in \code{list2Save}.
#' @param file2Save.pptx
#' (default = "PowerpointFromR")
#'  the name of the PowerPoint
#' file for saving the graphs.
#' If this file already exists,
#' the old file is
#' renamed and a warning message is printed
#' in the console.
#' @param title main title of the PowerPoint file.
#' \code{default:}
#' "\code{Graphics from R}" +
#' \code{date}.
#' @param addGraphNames when \code{TRUE}
#' (default) put titles in the powerpoint slides;
#' if titles have been provided in
#' \code{titles4list2Save} these will be used,
#' if not the name of the graphs is used
#' for the title of the slide.
#' @return a list (of class \code{"save2pptx"})
#' with  \code{listOfsavedGraphs} (the list of the graph objects
#' saved)  and
#' \code{nameOfSavingFile4pptx}
#' (name of the file where
#' the graphics are saved).
#' Note: to print one of the graphs from
#' \code{nameOfSavingFile4pptx}, use
#' \code{print(get())}. For example,
#' to print the first graph of the list
#' saved as \code{listOfGraph} use
#' \code{print(get(listOfGraph$listOfsavedGraphs[[1]]))}.
#' @author Hervé Abdi
#' @examples \dontrun{
#'  # Example from data4PCCAR
#' data("sixBeers12Descriptors10Judges", package = 'data4PCCAR')
#' df <- sixBeers12Descriptors10Judges$ratingsIntensity
#' res4graph <- graph4epPCA(data = df, scale = FALSE)
#' }
#' @import rvg officer
# #' # Below. Old @importFrom with specific imports
# #' # does not work anymore as of 03-03-2020. HA
# ## ' @importFrom officer add_slide ph_with_text read_pptx
# ## ' @importFrom rvg ph_with_vg
# ## ' @importFrom officer ph_with
#' @export


saveAllGraphsInList2pptx <- function(
                           list2Save,
              titles4list2Save = NULL,
                file2Save.pptx = "PowerpointFromR",
                         title = NULL,
                addGraphNames  = TRUE
                           ){
  # First a private function
  # A helper function to save recorded plots and ggplots
  # function in development to save the graphs in officer format
  #*******************************************************************
  #* First a local function: sauveImage
  sauveImage <- function(pptxName,  # the name of the officer object
                         graph,     # the graph to file
                         title = "" # The title of the graph
  ){
    # test what type of graph this is
    typeG  <- class(graph)[1]
    if ( !(typeG %in% c("recordedplot", "gg") )){
      stop("Unknown type of graph. Only recordedplot and gg are supported")
    }
    # A new Slide with text and a graph saved as en editable rvg graph
    pptxName <- officer::add_slide(pptxName,
                                   layout = "Title and Content",
                                   master = "Office Theme")
    pptxName <- officer::ph_with(pptxName,
                                 title,
                                 ph_location_type(type = 'title'))# The title
  # Note old code ph_with_vg is now deprecated
  # pptxName <- rvg::ph_with_vg(pptxName, code = print(graph),
  #                         type = "body") # The ggplot2 picture

  pptxName <- officer::ph_with(pptxName,
                        dml(print(graph)),
                        ph_location_type(type = 'body')) # The ggplot2 picture
  } # End of sauveImage
  #___________________________________________________________________
  laDate = substr(as.POSIXlt(Sys.time()),1,10)
  # Make default title
  if (is.null(title)) {
    title = paste0('Powerpoint From R Graphics. As of: ',
                   laDate)
  }
  # Make default file name
  pptx.type = 'pptx'
  if(tools::file_ext(file2Save.pptx) != pptx.type){
    file2Save.pptx <- paste0(file2Save.pptx,'.',pptx.type)
  }
  if (file.exists(file2Save.pptx)){# if file already exists: rename it
    LaDate = substr(as.POSIXlt(Sys.time()),1,10)
    OldFilename = sub(paste0('[.]', pptx.type),
                      paste0('-',LaDate,'.',pptx.type),file2Save.pptx)
    file.rename(from = file2Save.pptx, to = OldFilename)
    warning(paste0("File: ",file2Save.pptx,' already exists.\n',
                   ' Oldfile has been renamed: ', OldFilename),
            call. = FALSE)
  }

  # create a General Title
  # open the file
  doc <- officer::read_pptx() # Create the pptx file
  # Create title slide
  doc <- add_slide(doc, layout = "Title Only", master = "Office Theme")
  #doc <- ph_with_text(doc, type = 'title',
  #                    str =  title )
  doc <- officer::ph_with(doc,
                          value = title ,
                          location = ph_location_type(type = 'title')
                         )

  #
  #___________________________________________________________________
  #___________________________________________________________________
  # Save  in a powerpoint
  #   all the graphs created by either recordPlot or ggplots
  # Old Code from saveGraph2pptx()
  ignore = TRUE # Horrible trick!
  if (!isTRUE(ignore)){ # Ignore from Here ******
  listOfGraphs <- list()
  k = 0
  alist <- ls(.GlobalEnv, sorted = TRUE)
  # list all the objects in the Global environment
  nObj <- length(alist)
  for (i in 1:nObj){
    isGraph <- class(eval(as.symbol(alist[[i]])))[[1]]
    if ((isGraph == 'gg') | (isGraph == "recordedplot") ){
      anImage <- get(alist[[i]], pos = -1)
      if (addGraphNames) {aTitle <- alist[[i]]} else {aTitle = ""}
      suppressMessages(
        sauveImage(doc, anImage , title = aTitle )
      )
      k = k + 1
      listOfGraphs[[k]] <- alist[[i]]
    }
  }
  } # Ignore up to here *********

 # New stuff here
  # get slide titles from list names if
  # not provided
  if (is.null(titles4list2Save)){
     titles4list2Save <- names(list2Save)
  }

  # Double check that the list to save containts
  # only graphs
    alist <- list2Save
  nObj <- length(alist)
  listOfGraphs <- list()
  k = 0
  for (i in 1:nObj){ #loop on i
#
      isGraph <- class(alist[[i]])[[1]]
    if ((isGraph == 'gg') | (isGraph == "recordedplot") ){
        anImage <- alist[[i]]
      if (isTRUE(addGraphNames)) {
         aTitle <- titles4list2Save [[i]]}} else
        {aTitle <-  ""}
      suppressMessages(
        sauveImage(doc, anImage , title = aTitle )
                      )
      k = k + 1
      listOfGraphs[[k]] <- alist[[i]]
                 } # End of loop on i

  #_______________________________________________
  # Save the powerpoint Presentation
  suppressMessages(
    print(doc, target = file2Save.pptx )
  )
  # et voila
  #
  #_______________________________________________
  return.list <- structure(
    list(
      listOfsavedGraphs = listOfGraphs,
      nameOfSavingFile4pptx = file2Save.pptx
      ),  class = "save2pptx")


  return(return.list) #  Return the name of the file
} # End of function saveAllGraphsList4pptx ----
#*********************************************************************
# ********************************************************************
# ********************************************************************
#' Change the print function for object of class save2pptx
#'
#'  Change the print function for objects of class \code{save2pptx}.
#'
#' @param x a list: object of class \code{save2pptx},
#'   output of function: \code{saveGraph2pptx}.
#' @param ... everything else for the function
#' @author Herve Abdi
#' @keywords internal
#' @export
print.save2pptx <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n List of Saved Graphics by function aveAllGraphsList4pptx \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$listOfsavedGraphs     ", "The list of the names of the saved graphs")
  cat("\n                       ", "  NB To print a given graph use print(get())")
  cat("\n$nameOfSavingFile4pptx ", "The powerpoint file where the graphs were saved")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.save2pptx
#_____________________________________________________________________

#_____________________________________________________________________
# Test the function here
#res.save.pptx <- saveGraph2pptx(file2Save.pptx = 'toto.pptx',
#                                title = 'Distatis Test')
#_____________________________________________________________________
