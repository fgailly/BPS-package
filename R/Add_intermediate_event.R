#' Add an Intermediate BPMN-event
#'
#' Creates a list in the global environment storing the information of the intermediate event.
#' The name of the list will be the name argument of the function.
#' The list will have a custom class: bpmn_element
#' @param name character variable containing the name of the intermediate event
#' @param prev_element character variable containing the name of the previous element in the BPMN
#' @export
add_intermediate_event <- function(name= 'int_event1', prev_element='')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  if(length(prev_element) > 1)
  {
    stop("An intermediate event can have only 1 incoming and 1 outgoing sequence flow, model loops with XOR-gateways and AND-gates with AND-gateways")
  }
  name <- name
  prev_element <- prev_element
  task <- 0
  type <- 'inter_event'
  l <- list(name = name, prev_element = prev_element, task = task, type = type)
  assign(name,l, pos = 1)
}
