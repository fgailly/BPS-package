#' Adding interarrival_times to a trajectory
#'
#' If the simulation_environment variable does not yet exists in the global environment. The function first creates
#' simulation environment object of the simmer package. Afterwards, the function is adding an interarrival_time to a trajectory defined by the transform_BPMN() function
#' (see ?transform_BPMN()).
#'
#'
#' @param processsimmodel Specify to which trajectory the interarrival_time is added.Accepts a trajectory environment created by the transform_BPMN()-function or created with the simmer-package.
#' @param interarrival_time Accepts only functions that return numerics. If the functions returns a negative value the simulation is stopped.
#' @return a process simulation model
#' @export
create_arrivals <- function(processsimmodel, interarrival_time)
{
  if(!is.function(interarrival_time)) stop("interarrival_time should be a function")
  name_pr <- deparse(substitute(processsimmodel$traj))
  if (!is.null(processsimmodel[["sim_env"]]))
  {
    add_generator(processsimmodel$sim_env, name_prefix = name_pr, trajectory = processsimmodel$traj, distribution = interarrival_time)
    return(processsimmodel)
  }
  else
  {
    stop("Simulation environment does not exist")
  }
}


