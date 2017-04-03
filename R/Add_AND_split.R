#' Add a BPMN AND-split element
#'
#' Creates a dataframe in the global environment storing the information of the AND-split.
#' The name of the dataframe will be the name specified in the function call.
#' @param name character variable containing the name of the split
#' @param prev_element character variable containing the name of the previous element in the BPMN as a character
#' @export
add_AND_split <- function(name= '', prev_element = '')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  name <- name
  type <- 'AND-split'
  prev_element <- prev_element
  number_of_branches <- 0
  datafr <- data.frame(name , prev_element , type , number_of_branches, stringsAsFactors = FALSE)
  class(datafr) <- 'bpmn_element'
  assign(name,datafr, pos = 1)
}
