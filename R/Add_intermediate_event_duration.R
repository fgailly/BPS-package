#' Adds a duration to an intermediate event
#'
#' Function that can be used to specify how long you should wait for an intermediate event to happen
#' The function will alter the existing list of the intermediate event
#' Note that this information should be added before running the create_BPMN() function (see ?create_BPMN())
#' @param inter_event The intermediate_event of which the duration will be set. Accepts a list object created with the function Add_intermediate_event
#' @param duration Accepts numerics and all functions returning a numeric. If the duration is negative, the absolute value will be taken
#' @export
Add_intermediate_event_duration <- function(inter_event, duration)
{
  if(!is.list(inter_event) || activity$type != 'inter_event' ) stop("inter_event was not defined by the Add_intermediate_event()-function")
  if(!is.numeric(duration) && !is.function(duration)) stop("duration should be of type numeric or should be a function")
  name <- inter_event$name
  inter_event$task <- duration
  assign(name, inter_event, pos = 1)
}
