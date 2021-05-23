#' Determine procedure calls
#'
#' @description Determine procedure calls
#'
#' @param nlogocode vector of netlogo code strings
#' @param reporters logical whether to return reporters
#'
#' @return tibble with procedure names and procedure calls
#'
#' @details
#'
#' The procedure searches netlogo code for procedure definitions and calls.
#' The information is stored within a tibble that can be further processed.
#'
#' @aliases nldoc_find_procedure_calls
#' @rdname nldoc_find_procedure_calls
#' @keywords internal

nldoc_find_procedure_calls <- function(nlogocode, reporters = F)
{
  ## Extract modelcode:
  modelcode <- nlogocode$modelcode
  ## Remove all comment lines:
  modelcode <- modelcode[grep(pattern = '^;', modelcode, invert = TRUE)]

  ## Remove all comments within lines:
  modelcode <- sub('\\;.*', '', modelcode)

  ## Function headings:
  procs <-
    tibble::tibble(procedure = sub(" .*", "",
                                  sub("^\\S+\\s+", "",
                                      modelcode[grep(pattern ='^TO', toupper(modelcode))])),
                   start = grep(pattern = '^TO', toupper(modelcode)),
                   type = dplyr::case_when(grepl(pattern = "^TO ",
                                                      modelcode[start],ignore.case = T)~"command",
                                                grepl(pattern = "^TO-REPORT",
                                                      modelcode[start],ignore.case = T)~"reporter",
                                                TRUE~NA_character_))

  if(reporters == F){
    procs <- procs[which(procs$procedure == "command"),]
  }
  nw <- tibble::tibble()

  ## Find appearances of procedures:
  for (i in procs$procedure)
  {
    i.app <-
      grep(pattern = paste0("\\b", i, "(?!\\S)"),
           modelcode,
           ignore.case = TRUE,
           perl = T)

    # Now check appearances and determinate the context
    for (j in i.app)
    {
      ## Only proceed if we dont have the heading of a procedure:
      if (!(j %in% procs$start))
      {
        ## Find the context:
        context <-
          procs %>% dplyr::filter(start < j) %>% dplyr::filter(dplyr::row_number() ==
                                                                 dplyr::n())

        ## Store result:
        nw.i <- tibble::tibble(from = context$procedure, to = i)
        nw <- rbind(nw, nw.i)
      }
    }
  }
  procs$start = NULL
  return(list(links = nw,nodes = procs))
}
