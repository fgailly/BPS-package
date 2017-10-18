#' Adding resources to a simulation
#'
#' If the simulation_environment variable does not yet exists in the global environment. The function first creates
#' simulation environment object of the simmer package. Afterwards, the function is adding resources to the simulation environment.
#' The simulation will only run when all resources defined by the function add_resource_to_activity, are also defined in the simulation environment
#' (see ?add_resource_to_activity)
#'
#' @param process_sim_model Process simulation model met simmer traj en simmer simmulation environment
#' @param resource Character variable containing the name of the resource-type.
#' @param capacity Number of resources present in the simulation environment.
#' @param schedule Can be used to define work schedules for resources, when present the capacity parameter will be ignored.Accepts schedules created by the schedule()-function of the simmer-package (see ?schedule())
#' @param max_queue_size Can be used to identify when the queue_size of a resource is limited. Instances arriving at the queue when the queue_size is at its maximal queue size will leave the system without finishing it.
#' @return Process simulation model met simmer traj en simmer simmulation environment
#' @export

create_resource <- function(process_sim_model, resource = '', capacity = 1, max_queue_size = Inf, schedule)
{
  if(!is.character(resource)) stop("resource is not of the character type")
  if(!is.numeric(capacity) || as.integer(capacity) != capacity || capacity < 0) stop("capacity is not a positive integer")
  if(!missing(schedule))
  {
    if (!is.null(process_sim_model[["sim_env"]]))
    {
      add_resource(process_sim_model$sim_env, name = resource, capacity = capacity, queue_size = max_queue_size)
    }
    else
    {
      stop("Simulation environment does not exist")
    }
  }
  else
  {
    stop("schedule parameter should be created by the schedule()-function")
  }
  return(process_sim_model)
}
