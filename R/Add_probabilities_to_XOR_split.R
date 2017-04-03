#' Specify the probabilities of a XOR-split
#'
#' A XOR-split is used in BPMN to model a XOR-GATE structure or to model a LOOP.
#' In the case of a XOR-GATE structure, this function can be used to specify the probability to flow through a certain branch by using the parameters first_activities and probabilities.
#' In the case of a LOOP, this function can be used to specify the probability to continue (the probability that you do not go into the loop) by using the parameter prob_to_continue
#' Note that this information should be added before running the create_BPMN() function (see ?create_BPMN())
#'
#' @param split The split object. Accepts a list object created with the function Add_XOR_split
#' @param first_activities a character vector containing the name of the first element of each branch of the XOR-GATE structure.
#' @param probabilities a numeric vector containing the probability of flowing through each branch.the sum should be equal to 1. The sequence of the branches should be identical to the sequence in the first_activities vector
#' @param prob_to_continue a numeric between 0 and 1 indicating the probability of continuing without entering the loop.
#' @export
Add_probabilities_to_XOR_split <- function(split, first_activities = c(), probabilities = c(), prob_to_continue = 0)
{
  if(!is.list(split) || split$type != 'XOR-split') stop("split was not defined by the Add_XOR-split()-function")
  if(!missing(first_activities))
  {
    if(!is.character(first_activities)) stop("first_activities should be a character vector")
    if(!is.numeric(probabilities)) stop("probabilities should be a numeric vector")
    if(sum(probabilities < 0)) stop("all elements in the probabilities vector should be positive")
    if(sum(probabilities) != 1) stop("sum of the probabilities vector should be 1")
    if(length(first_activities) != length(probabilities)) stop("the first_activities vector and probabilities vector should have the same length")
  }
  else
  {
    if(!is.numeric(prob_to_continue)) stop("prob_to_continue should be a numeric")
    if(prob_to_continue > 1 || prob_to_continue < 0) stop("prob_to_continue should be between 0 and 1")
  }
  name <- split$name
  split$first_activities = first_activities
  split$probabilities = probabilities
  split$prob_to_continue = prob_to_continue
  assign(name, split, pos = 1)
}

is.vector('lala')
is.atomic('alal')
length('lala')
length(c('a', 'b'))
