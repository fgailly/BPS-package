#' Adding interarrival_times to a trajectory
#'
#' If the simulation_environment variable does not yet exists in the global environment. The function first creates
#' simulation environment object of the simmer package. Afterwards, the function is adding an interarrival_time to a trajectory defined by the Create_BPMN() function
#' (see ?Create_BPMN()).
#'
#'
#' @param to_trajectory Specify to which trajectory the interarrival_time is added.Accepts a trajectory environment created by the Create_BPMN()-function or created with the simmer-package.
#' @param interarrival_time Accepts only functions that return numerics. If the functions returns a negative value the simulation is stopped.
#' @export
Add_interarrival_time <- function(to_trajectory, interarrival_time)
{
  if(!is.function(interarrival_time)) stop("interarrival_time should be a function")
  if (exists('simulation_environment'))
  {
    add_generator(simulation_environment, name_prefix = deparse(substitute(to_trajectory)), trajectory = to_trajectory, distribution = interarrival_time)
  }
  else
  {
    simulation_environment <<- simmer(name = 'simulation_environment') %>%
      add_generator(name_prefix = deparse(substitute(to_trajectory)), trajectory = to_trajectory, distribution = interarrival_time)
  }
}
