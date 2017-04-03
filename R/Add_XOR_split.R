#' Add a BPMN XOR-split element
#'
#' Creates a list in the global environment storing the information of the XOR-split.
#' The name of the dataframe will be the name specified in the function call.
#' @param name character variable containing the name of the split
#' @param prev_element character variable containing the name of the previous element in the BPMN as a character
#' @export
add_XOR_split <- function(name= '', prev_element = '')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  name <- name
  type <- 'XOR-split'
  prev_element <- prev_element
  l <- list(name = name, prev_element = prev_element, type = type, number_of_branches = 0, first_activities = c(), probabilities = c(), prob_to_continue = 0)
  class(l) <- 'bpmn_element'
  assign(name,l, pos = 1)
}
