#' Specify which resource is responsible for executing an activity
#'
#' Specify which resource is responsible for executing an activity
#' The function will alter the existing list of the activity
#' Note that this information should be added before running the transform_BPMN() function (see ?transform_BPMN())
#'
#' @param process processmodel
#' @param activity The activity a resource should be added to. Accepts a list object created with the add_activity function OR with the import_BPMN function
#' @param resource Specify the name of the resource type as a character variable. The same resource type should be later defined in the simulation environment (see ?create_resource())
#' @param amount Can be used to indicate that more than 1 instance of the resource type is needed to execute an activity.
#' @export
set_resource_to_activity <- function(process, activity, resource = '', amount= 1)
{
  if(!(activity %in% names(process))  || process[[activity]]$type != "activity" ) stop("activity was not defined by the Add_activity()-function")
  if(!is.character(resource)) stop("resource is not of the character type")
  if(!is.numeric(amount) || as.integer(amount) != amount || amount < 0) stop("amount is not a positive integer")
  process[[activity]]$resource <-resource
  process[[activity]]$amount <- amount
  return(process)
}


