#' Adding resources to a simulation
#'
#' If the simulation_environment variable does not yet exists in the global environment. The function first creates
#' simulation environment object of the simmer package. Afterwards, the function is adding resources to the simulation environment.
#' The simulation will only run when all resources defined by the function add_resource_to_activity, are also defined in the simulation environment
#' (see ?add_resource_to_activity)
#'
#' @param resource Character variable containing the name of the resource-type.
#' @param capacity Number of resources present in the simulation environment.
#' @param schedule Can be used to define work schedules for resources, when present the capacity parameter will be ignored.Accepts schedules created by the schedule()-function of the simmer-package (see ?schedule())
#' @param max_queue_size Can be used to identify when the queue_size of a resource is limited. Instances arriving at the queue when the queue_size is at its maximal queue size will leave the system without finishing it.
#' @export

create_resource <- function(resource = '', capacity = 1, max_queue_size = Inf, schedule)
{
  if(!is.character(resource)) stop("resource is not of the character type")
  if(!is.numeric(capacity) || as.integer(capacity) != capacity || capacity < 0) stop("capacity is not a positive integer")
  if(missing(schedule))
  {
    if (exists('simulation_environment'))
    {
      add_resource(simulation_environment, name = resource, capacity = capacity, queue_size = max_queue_size)
    }
    else
    {
      simulation_environment <<- simmer(name = 'simulation_environment') %>%
        add_resource(name = resource, capacity = capacity, queue_size = max_queue_size)
    }
  }
  else
  {
    if(!is.environment(schedule)) stop("schedule parameter should be created by the schedule()-function")
    if (exists('simulation_environment'))
    {
      add_resource(simulation_environment, name = resource, schedule, queue_size = max_queue_size)
    }
    else
    {
      simulation_environment <<- simmer(name = 'simulation_environment') %>%
        add_resource(name = resource, schedule, queue_size = max_queue_size)
    }
  }
}
