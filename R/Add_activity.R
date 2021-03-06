#' Add a BPMN-activity element
#'
#' Creates a list in the global environment storing the information of the activity.
#' The name of the list will be the name argument of the function.
#' The list will have a custom class: bpmn_element
#' @param process
#' @param name character variable containing the name of the activity
#' @param prev_element character variable containing the name of the previous element in the BPMN
#' @export
add_activity <- function(process, name= 'activity1', prev_element='')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  if(length(prev_element) > 1)
  {
    stop("An activity can have only 1 incoming and 1 outgoing sequence flow, model loops with XOR-gateways and AND-gates with AND-gateways")
  }
  name <- name
  prev_element <- prev_element
  resource <- 'N/A'
  nmbr_resources <- 0
  task <- 0
  type <- 'activity'
  l <- list(name = name, prev_element = prev_element, resource = resource, nmbr_resources = nmbr_resources, task = task, type = type)
  class(l) <- 'bpmn_element'
  process[[name]] <-l
  return(process)
}

