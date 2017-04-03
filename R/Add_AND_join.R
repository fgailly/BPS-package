#' Add a BPMN AND-join element
#'
#' Creates a dataframe in the global environment storing the information of the AND-split.
#' The name of the dataframe will be the name specified in the function call.
#' @param name character variable containing the name of the join
#' @param prev_element character variable containing the name of the previous element in the BPMN. A join can have multiple previous elements, you should specify them all in a vector containing their names as character. e.g: prev_element = c('el1','el2')
#' @param of_split character variable specifying to which split this join will belong.
#' @export
Add_AND_join <- function(name = '', prev_element = '', of_split = '')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  if(!is.character(of_split)) stop("of_split is not of the character type")
  name <- name
  type <- 'AND-join'
  prev_element <- prev_element
  datafr <- data.frame(name, prev_element, type, of_split, stringsAsFactors = FALSE)
  class(datafr) <- 'bpmn_element'
  assign(name,datafr, pos = 1)
}
