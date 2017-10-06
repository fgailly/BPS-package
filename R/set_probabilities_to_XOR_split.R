#' Specify the probabilities of a XOR-split
#'
#' A XOR-split is used in BPMN to model a XOR-GATE structure or to model a LOOP.
#' In the case of a XOR-GATE structure, this function can be used to specify the probability to flow through one of the alternative paths by using the parameters first_elements and probabilities.
#' In the case of a LOOP, this function can be used to specify the probability to continue (the probability that you don't go into the loop) by using the parameter prob_to_continue
#' Note that this information should be added before running the transform_BPMN() function (see ?transform_BPMN())
#'
#' @param process processmodel
#' @param split The split object. Accepts a list object created with the add_XOR_split function OR with the import_BPMN function
#' @param first_elements a character vector containing the name of the first element of each alternative path of the XOR-GATE structure.
#' @param probabilities a numeric vector containing the probability of flowing through each alternative path.the sum should be equal to 1. The sequence of the branches should be identical to the sequence in the first_elements vector
#' @param prob_to_continue a numeric between 0 and 1 indicating the probability of continuing without entering the loop.
#' @export
set_probabilities_to_XOR_split <- function(process, split, first_elements = c(), probabilities = c(), prob_to_continue = 0)
{
  if(!(split %in% names(process)) || process[[split]]$type != 'XOR-split') stop("split was not defined by the Add_XOR-split()-function")
  if(!missing(first_elements))
  {
    if(!is.character(first_elements)) stop("first_elements should be a character vector")
    if(!is.numeric(probabilities)) stop("probabilities should be a numeric vector")
    if(sum(probabilities < 0)) stop("all elements in the probabilities vector should be positive")
    if(sum(probabilities) != 1) stop("sum of the probabilities vector should be 1")
    if(length(first_elements) != length(probabilities)) stop("the first_elements vector and probabilities vector should have the same length")
  }
  else
  {
    if(!is.numeric(prob_to_continue)) stop("prob_to_continue should be a numeric")
    if(prob_to_continue > 1 || prob_to_continue < 0) stop("prob_to_continue should be between 0 and 1")
  }
  process[[split]]$first_activities <- first_elements
  process[[split]]$probabilities <- probabilities
  process[[split]]$prob_to_continue <- prob_to_continue
  return(process)
}

