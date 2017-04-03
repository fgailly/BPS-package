#' Add a stop BPMN-event
#'
#' Creates a list in the global environment storing the information of the stop event.
#' The name of the list will be the name specified in the function call.
#' @param name character variable containing the name of the stop event
#' @param prev_element character variable containing the name of the previous element in the BPMN as a character
#' @export
Add_stop_event <- function(name= 'stop_event1', prev_element='')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  name <- name
  prev_element <- prev_element
  type <- 'stop_event'
  l <- list(name = name, prev_element = prev_element, type = type)
  class(l) <- 'bpmn_element'
  assign(name,l, pos = 1)
}
