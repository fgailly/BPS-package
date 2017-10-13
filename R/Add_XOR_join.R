#' Add a BPMN XOR-join element
#'
#' Creates a list in the global environment storing the information of the XOR-split.
#' The name of the list will be the name argument of the function.
#' The list will have a custom class: bpmn_element
#' @param process the process model
#' @param name character variable containing the name of the join
#' @param prev_element character variable containing the name of the previous element in the BPMN. A join can have multiple previous elements, you should specify them all in a character vector. e.g: prev_element = c('element1','element2')
#' @param of_split character variable specifying to which split this join belongs.
#' @return process model
#' @export
add_XOR_join <- function(process, name = '', prev_element = '', of_split = '')
{
  if(!is.character(name)) stop("name is not of the character type")
  if(!is.character(prev_element)) stop("prev_element is not of the character type")
  if(!is.character(of_split)) stop("of_split is not of the character type")
  name <- name
  type <- 'XOR-join'
  prev_element <- prev_element
  l <- list(name = name, prev_element=prev_element, type = type, of_split = of_split)
  class(l) <- 'bpmn_element'
  process[[name]] <-l
  return(process)
}
