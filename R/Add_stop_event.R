#' Add a stop BPMN-event
#'
#' Creates a list in the global environment storing the information of the stop event.
#' The name of the list will be the name argument of the function.
#' The list will have a custom class: bpmn_element
#' @param process the process model
#' @param name character variable containing the name of the stop event
#' @param prev_element character variable containing the name of the previous element in the BPMN
#' @return process model
#' @export
add_stop_event <- function(process, name= 'stop_event1', prev_element='')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  if(length(prev_element) > 1)
  {
    stop("A stop event can have only 1 incoming sequence flow, model AND-gates with AND-gateways")
  }
  name <- name
  prev_element <- prev_element
  type <- 'stop_event'
  l <- list(name = name, prev_element = prev_element, type = type)
  class(l) <- 'bpmn_element'
  process[[name]] <-l
  return(process)
}
