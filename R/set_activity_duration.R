#' Adds an activity duration to an activity
#'
#' Function that can be used to specify how long it takes to execute an activity.
#' The function will alter the existing list of the activity
#' Note that this information should be added before running the create_BPMN() function (see ?create_BPMN())
#' @param activity The activity of which the duration will be set. Accepts a list object created with the function Add_activity
#' @param duration Accepts numerics and all functions returning a numeric. If the duration is negative, the absolute value will be taken
#' @export
set_activity_duration <- function(activity, duration)
{
  if(!is.list(activity) || activity$type != 'activity' ) stop("activity was not defined by the Add_activity()-function")
  if(!is.numeric(duration) && !is.function(duration)) stop("duration should be of type numeric or should be a function")
  name <- activity$name
  activity$task <- duration
  assign(name, activity, pos = 1)
}


