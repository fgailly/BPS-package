#' Add a BPMN-activity element
#'
#' Creates a list in the global environment storing the information of the activity.
#' The name of the list will be the name specified in the function call.
#' @param name character variable containing the name of the activity
#' @param prev_element character variable containing the name of the previous element in the BPMN as a character
#' @export
Add_activity <- function(name= 'activity1', prev_element='')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  name <- name
  prev_element <- prev_element
  resource <- 'N/A'
  nmbr_resources <- 0
  task <- 0
  type <- 'activity'
  l <- list(name = name, prev_element = prev_element, resource = resource, nmbr_resources = nmbr_resources, task = task, type = type)
  class(l) <- 'bpmn_element'
  assign(name,l, pos = 1)
}

