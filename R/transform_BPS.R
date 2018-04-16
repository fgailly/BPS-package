#' Creating a trajectory environment of the simmer package
#'
#' The function returns a trajectory environment of the simmer package.
#' Before running this function, all additional information should be added first.(see ?set_activity_duration, ?set_resource_to_activity, ?set_probabilities_to_XOR_split)
#' All arguments passed to this function will be removed from the global environment.
#'
#' @param ... include as seperate arguments all lists describing the elements of your BPMN (they can be specified in a random order). Accepts lists created by the functions: add_activity, add_XOR_split, add_AND_split, add_XOR_join, add_AND_Join, add_intermediate_event
#' @export
#' @return Process Simulationmodel which containt a simmer trajectory object and a corresponding simulation environment
#' @import simmer
#' @import methods
#'
transform_BPS <- function(...)
{
  elements <- list(...)
  #check whether all arguments provided to the function are created by our package functions
  #when they are checked, romve arguments from global environment, the ensure that these functions are rerunned before running Create_BPMN
  #for(i in 1:length(elements))
  #{
  #  if(!methods::is(elements[[i]], "bpmn_element") )
  #  {stop("Not all arguments were created by the Add_activity(), Add_XOR_split(), Add_AND_split(), Add_XOR_join() or Add_AND_join() functions")}
  #  rm(list = elements[[i]]$name, envir = globalenv())
  #}
  #Sort the elements based on the prev_element variable
  i <- 1
  while(i <= length(elements))
  {
    init_el <- elements[[i]]
    switched <- FALSE
    for(j in 1:length(elements))
    {
      compare_el <- elements[[j]]
      if (sum(init_el$name == compare_el$prev_element) >= 1)  #work with sum because multiple observations with join
      {
        if(i>j)
        {
          elements[[j]] <- init_el
          elements[[i]] <- compare_el
          switched <- TRUE
          break
        }
      }
    }
    if(switched == FALSE)
    {
      i <- i+1
    }
    if(switched==TRUE)
    {
      i <- 1
    }
  }
  i <- length(elements)
  #Sorting: Deal with gates
  checked <- character()
  while(i >= 1)
  {
    if((elements[[i]]$type == "AND-split" || elements[[i]]$type == "XOR-split") && !(elements[[i]]$name %in% checked) )
    {
      pos_split <- i
      j <- i+1
      checked <- c(checked, elements[[i]]$name)
      while(j <= length(elements))
      {
        check <- elements[[j]]
        bool <- FALSE
        for(k in i:length(elements))
        {
          if(sum(check$prev_element == elements[[k]]$name) >= 1)
          {
            bool <- TRUE
            j <- j+1
          }
        }
        if(bool == FALSE)
        {
          elements <- c(elements[1:(i-1)],'empty', elements[i:length(elements)])
          elements[[i]] <- check
          elements <- elements[-(j+1)]
          i <- i+1
          j <- i+1
        }
      }
    }
    i <- i-1
  }
  #Deal with loops
  i <- length(elements)
  while(i > 0)
  {
    #Loop can be identified when xor-join is in front of xor-split
    #determine start and stop index
    if(elements[[i]]$type == 'XOR-split')
    {
      split_ind <- i
      join_ind <- 0
      #Loop can be identified when xor-join is in front of xor-split
      #determine start and stop index
      for (j in 1:i)
      {
        if(elements[[j]]$type== 'XOR-join')
        {
          if(elements[[j]]$of_split == elements[[i]]$name)
          {
            join_ind <- j
            break
          }
        }
      }
      if(join_ind != 0 && join_ind < split_ind) #we have a loop
      {
        #check wether some activities should be executed first before returning
        loop_handeled <- FALSE
        for(j in i:length(elements))
        {
          if(elements[[j]]$type != 'stop_event')
          {
            continue <- TRUE
            for(k in i:length(elements))
            {
              if(sum(elements[[j]]$name %in% elements[[k]]$prev_element) >= 1)
              {
                continue <- FALSE
              }
            }
            if(continue)
            {
              k <- j
              while(k >= i)
              {
                if(elements[[k]]$type == 'XOR-split')
                {
                  if(elements[[k]]$name == elements[[i]]$name)
                  {
                    #create a dataframe 'loop element' with a probability to continue = 0
                    loop_element <- list(name = paste0('loop_', elements[[j]]$name), prev_element = elements[[j]]$name, type = 'loop', loop_to = join_ind, prob_to_continue = 0)
                    #amount parameter of the rollback simmer function should still be initialized
                    #place loop_element after the last activity that should be executed
                    if(j == length(elements))
                    {
                      elements <- c(elements[1:j], 'empty')
                      elements[[length(elements)]] <- loop_element
                    }
                    else
                    {
                      elements <- c(elements[1:j], empty, elements[(j+1),length(elements)])
                      elements[[(j+1)]] <- loop_element
                    }
                    #remove join element
                    elements <- elements[-(join_ind)]
                    loop_handeled <- TRUE
                  }
                  break
                }
                k <- k-1
              }
            }
          }
        }
        if(loop_handeled == FALSE)
        {
          #create a dataframe 'loop element' with same name as the XOR-split element
          loop_element <- list(name = elements[[split_ind]]$name, prev_element = elements[[split_ind]]$prev_element, type = 'loop', loop_to = join_ind, prob_to_continue = elements[[split_ind]]$prob_to_continue)
          #amount parameter of the rollback simmer function should still be initialized
          #place loop_element in place of the current split element & remove join element
          elements[[split_ind]] <- loop_element
          elements <- elements[-(join_ind)]
        }
      }
    }
    i <- i-1
  }
  #Deal with AND-gatestructures and XOR-gatestructures
  i <- length(elements)
  while(i > 0)
  {
    if (elements[[i]]$type == "AND-split")
    {
      #determine start and stop index of the gate structure
      start_ind <- i

      for (j in i:length(elements))
      {
        if(elements[[j]]$type== "AND-join")
        {
          if(elements[[j]]$of_split == elements[[i]]$name)
          {
            stop_ind <- j
            break
          }
        }
      }
      #create df object to store information about AND-structure
      df <- data.frame(name ='', prev_element='', nmbr_of_branches =0, type ='AND-structure')
      df$name <- elements[[start_ind]]$name
      df$prev_element <- elements[[start_ind]]$prev_element
      df$nmbr_of_branches <- length(elements[[stop_ind]]$prev_element)
      branches <- vector('list', df$nmbr_of_branches)
      #AND-structure object will replace split, all objects between split and join, join
      #as a consequence the element after the join his previous element variable should be renamed
      for(j in start_ind:length(elements))
      {
        if(sum(elements[[j]]$prev_element == elements[[stop_ind]]$name) >= 1)
        {
            for(k in 1:length(elements[[j]]$prev_element))
            {
              if(elements[[j]]$prev_element[k] == elements[[stop_ind]]$name) {
                elements[[j]]$prev_element[k] <- df$name
              }
            }
        }
      }
      ##determine the right sequence in the branches
      counter <- 1
      #put start elements of the branch in a seperate branch
      for(z in (start_ind+1):(stop_ind-1))
      {
        if(sum(elements[[z]]$prev_element == df$name) ==1)
        {
          branches[[counter]] <- elements[[z]]
          counter <- counter + 1
        }
      }
      #create branches
      counter <- counter - 1
      for(j in 1:counter)
      {
        l <- branches[j]
        place <- 2
        for(k in (start_ind+1):(stop_ind-1))
        {
          if(elements[[k]]$prev_element == l[[length(l)]]$name)
          {
            l[[place]] <- elements[[k]]
            place <- place+1
          }
        }
        branches[[j]] <- l
      }
      #create simmer trajectory object storing the trajectory of the AND-structure
      t0 <- trajectory()
      #define branches in simmer package
      #loop through number of branches
      arguments <- list()
      arguments[[1]] <- t0
      arguments[[2]] <- df$nmbr_of_branches
      for(j in 1:length(branches))
      {
        br <- trajectory()
        #loop through selected branch
        for(k in 1:length(branches[[j]]))
        {
          #ADDED
          if(branches[[j]][[k]]$type == "inter_event")
          {
            timeout(br, task = branches[[j]][[k]]$task)
          }
          if(branches[[j]][[k]]$type == "activity")
          {
            if(branches[[j]][[k]]$resource == "N/A")
            {
              timeout(br, task = branches[[j]][[k]]$task)

            }
            else
            {

              seize(br, resource = branches[[j]][[k]]$resource, amount = as.integer(branches[[j]][[k]]$nmbr_resources))
              timeout(br, task = branches[[j]][[k]]$task)
              release(br, resource = branches[[j]][[k]]$resource, amount = as.integer(branches[[j]][[k]]$nmbr_resources))
            }
          }
          if(branches[[j]][[k]]$type == 'AND-structure' || branches[[j]][[k]]$type == 'XOR-structure')
          {
            br <- join(br, branches[[j]][[k]]$traj)
          }
        }
        arguments[[(j+2)]] <- br
      }
      do.call(clone, args=arguments)
      synchronize(t0, mon_all = TRUE)
      #create list object storing all the information of the andstructure
      andstr <- list(name =df$name, prev_element = df$prev_element, type = df$type, traj = t0)
      name <- df$name
      assign(name, andstr)

      #delete all elements from start_ind until stop_ind and insert new list element
      elements[[start_ind]] <- andstr
      elements <- elements[-((start_ind+1):stop_ind)]
    }
    if (elements[[i]]$type == "XOR-split")
    {
      #determine start and stop index
      #XOR-gate structure can have no join element and as consequence no stop_index, will then be zero
      #to have XOR-gate structure the join element should be non-existent or after the split element
      start_ind <- i
      stop_ind <- 0
      for (j in i:length(elements))
      {
        if(elements[[j]]$type== 'XOR-join')
        {
          if(elements[[j]]$of_split == elements[[i]]$name)
          {
            stop_ind <- j
            break
          }
        }
      }
      #create df object to store information about XOR-structure
      df <- data.frame(name ='', prev_element='', nmbr_of_branches =0, type ='XOR-structure')
      df$name <- elements[[start_ind]]$name
      df$prev_element <- elements[[start_ind]]$prev_element
      #number of branches: different as witch AND-structure because not all branches have to go to split
      #we can't use number of prev_element of join.
      for(j in (start_ind+1):length(elements))
      {
        if(sum(elements[[j]]$prev_element == df$name) ==1)
        {
          df$nmbr_of_branches <- df$nmbr_of_branches+1
        }
      }
      branches <- vector('list', df$nmbr_of_branches)
      ##determine the right sequence in the branches
      counter <- 1
      #put start elements of the branch in a seperate branch
      for(z in (start_ind+1):length(elements))
      {
        if(sum(elements[[z]]$prev_element == df$name) ==1)
        {
          branches[[counter]] <- elements[[z]]
          counter <- counter + 1
        }
      }
      #create option-parameter for branch()-function
      first_activities <- elements[[i]]$first_activities
      probabilities <- elements[[i]]$probabilities
      if(length(first_activities) ==0)
      {
        #if probabilities are not defined, take equal chance for each branch
        opt <- function() {sample(1:df$nmbr_of_branches, size = 1)}
      }
      else
      {
        #the branches list should be in the same sequence as the first_activities vector
        #then the sequence of the probabilities will be the correct sequence
        for(j in 1:df$nmbr_of_branches)
        {
          for(k in 1:length(first_activities))
          {
            if(branches[[j]]$name == first_activities[k])
            {
              switch <- branches[[j]]
              branches[[j]] <- branches[[k]]
              branches[[k]] <- switch
            }
          }
        }
        opt <- function() {sample(1:df$nmbr_of_branches, size = 1, prob = probabilities)}
      }
      ##create branches
      #create continue parameter for branch() function
      continue <- rep(FALSE, df$nmbr_of_branches)
      for(j in 1:df$nmbr_of_branches)
      {
        l <- branches[j]
        place <- 2
        #if the first element is the join-element, you should continue
        if(l[[1]]$type == 'XOR-join' && l[[1]]$of_split == df$name)
        {
          continue[j] <- TRUE
          branches[[j]] <- l
          break
        }
        for(k in (start_ind+1):length(elements))
        {
          if(sum(elements[[k]]$prev_element == l[[length(l)]]$name) == 1)
          {
            if(elements[[k]]$type == 'XOR-join' || elements[[k]]$type == 'AND-join')
            {
              continue[j] <- TRUE
              #if join is a join of another split, the prev_element of this join should be renamed to the split name.
              if(elements[[k]]$of_split != df$name)
              {
                for(z in 1:length(elements[[k]]$prev_element))
                {
                  if(elements[[k]]$prev_element[z] == l[[length(l)]]$name) {
                    elements[[k]]$prev_element[z] <- df$name
                  }
                }
              }
              break
            }
            else
            {
              l[[place]] <- elements[[k]]
              place <- place+1
            }
          }
        }
        branches[[j]] <- l
      }
      #Now that the branches are created, we can rename the prev_element
      #XOR-structure object will replace split, all objects between split and join, join
      #as a consequence the element after the join his prev_element variable should be renamed
      if(stop_ind != 0){
        #XOR-structure object will replace split, all objects between split and join, join
        #as a consequence the element after the join his previous element variable should be renamed
        for(j in start_ind:length(elements))
        {
          if(sum(elements[[j]]$prev_element == elements[[stop_ind]]$name) >= 1)
          {
            for(k in 1:length(elements[[j]]$prev_element)) #rename the correct prev_element if you have a join
            {
              if(elements[[j]]$prev_element[k] == elements[[stop_ind]]$name) {
                elements[[j]]$prev_element[k] <- df$name
              }
            }
          }
        }
      }
      #create simmer trajectory object storing the trajectory of the XOR-structure
      t0 <- trajectory()
      #define branches in simmer package
      #loop through number of branches
      #store in the remove list the elements that were put on a branch to remove from elements list later
      remove <- list()
      remove_ind <- 1
      arguments <- list()
      arguments[[1]] <- t0
      arguments[[2]] <- opt
      arguments[[3]] <- continue
      for(j in 1:length(branches))
      {
        br <- trajectory()
        #If branch goes directly to stop_event, Add pseudo-task that has duration of 0, simmer does not accept otherwise
        if(branches[[j]][[1]]$type == 'stop_event')
        {
          timeout(br, task = 0)
          branches[[j]][[1]]$type <- 'stop_event_pseudo'
          remove[[remove_ind]] <- branches[[j]][[1]]
          remove_ind <- remove_ind +1
        }
        #If branch goes directly to the XOR-join, Add pseudo-task that has duration of 0, simmer does not accept otherwise
        if(branches[[j]][[1]]$type == 'XOR-join' && branches[[j]][[1]]$of_split == df$name)
        {
          ##Add pseudo-task that has duration of 0, simmer does not accept otherwise
          timeout(br, task = 0)
          branches[[j]][[1]]$type <- 'XOR-join_pseudo'
          remove[[remove_ind]] <- branches[[j]][[1]]
          remove_ind <- remove_ind +1
        }
        #loop through selected branch
        for(k in 1:length(branches[[j]]))
        {
          remove[[remove_ind]] <- branches[[j]][[k]]
          remove_ind <- remove_ind +1
          #ADDED
          if(branches[[j]][[k]]$type == 'inter_event')
          {
            timeout(br, task = branches[[j]][[k]]$task)
          }
          if(branches[[j]][[k]]$type == 'activity')
          {
            if(branches[[j]][[k]]$resource == 'N/A')
            {
              timeout(br, task = branches[[j]][[k]]$task)
            }
            else
            {
              seize(br, resource = branches[[j]][[k]]$resource, amount = as.integer(branches[[j]][[k]]$nmbr_resources))
              timeout(br, task = branches[[j]][[k]]$task)
              release(br, resource = branches[[j]][[k]]$resource, amount = as.integer(branches[[j]][[k]]$nmbr_resources))
            }
          }
          else if(branches[[j]][[k]]$type == 'AND-structure' || branches[[j]][[k]]$type == 'XOR-structure' )
          {
            br <- join(br, branches[[j]][[k]]$traj)
          }
          else if(branches[[j]][[k]]$type == 'loop')
          {
            #determine the amount-parameter of the rollback() simmer function
            amount <- 0
            for(z in 1:length(remove))
            {
              if(remove[[z]]$type == 'inter_event')
              {
                amount <- amount + 1
              }
              #ADDED
              if(remove[[z]]$type == 'stop_event_pseudo')
              {
                amount <- amount + 1
              }
              #ADDED
              if(remove[[z]]$type == 'XOR-join_pseudo')
              {
                amount <- amount + 1
              }
              if(remove[[z]]$type == 'activity')
              {
                #if the activity element has a resource it is translated into 3 r-activities
                if(remove[[z]]$resource != 'N/A')
                {
                  amount <- amount + 3
                }
                else
                {
                  amount <- amount + 1
                }
              }

              #AND-gate structure is seen as 2 r-activities by the rollback function
              else if(remove[[z]]$type == 'AND-structure')
              {
                amount <- amount + 2
              }
              #XOR-gate structure is seen as 1 r-activities by the rollback function
              else if(remove[[z]]$type == 'XOR-structure')
              {
                amount <- amount + 1
              }
            }
            #also include the branch function itself
            amount <- amount + 1
            for(z in branches[[j]][[k]]$loop_to:(i-1))
            {
              #ADDED
              if(elements[[z]]$type == 'inter_event')
              {
                amount <- amount + 1
              }
              if(elements[[z]]$type == 'activity')
              {
                #if the activity element has a resource it is translated into 3 r-activities
                if(elements[[z]]$resource != 'N/A')
                {
                  amount <- amount + 3
                }
                else
                {
                  amount <- amount + 1
                }
              }
              #AND-gate structure is seen as 2 r-activities by the rollback function
              else if(elements[[z]]$type == 'AND-structure')
              {
                amount <- amount + 2
              }
              #XOR-gate structure is seen as 1 r-activities by the rollback function
              else if(elements[[z]]$type == 'XOR-structure')
              {
                amount <- amount + 1
              }
            }
            prob_return <- 1-elements[[i]]$prob_to_continue
            prob_continue <- elements[[i]]$prob_to_continue
            rollback(br, amount = amount, check = function() {sample(c(TRUE, FALSE), 1, prob = c(prob_return, prob_continue))})
          }
        }
        arguments[[(j+3)]] <- br
      }
      do.call(branch, args=arguments)
      #create list object storing all the information of the xorstructure
      xorstr <- list(name =df$name, prev_element = df$prev_element, type = df$type, traj = t0)
      name <- df$name
      assign(name, xorstr)
      #delete all elements that are in the branches of the XOR-structure and put the XOR-structure on the right place
      elements[[start_ind]] <- xorstr
      #remove contains all the elements that were placed on branches
      j <- 1
      while(j <= length(remove))
      {
        if(remove[[j]]$type == 'XOR-join_pseudo')
        {
          #first delete XOR-join_pseudo from the branches because you will remove the XOR-join in a next step
          remove <- remove[(-j)]
        }
        if(remove[[j]]$type == 'stop_event_pseudo')
        {
          #rename stop_event_pseudo in remove list because otherwise it won't match with the elements list
          remove[[j]]$type <- 'stop_event'
        }
        j <- j+1
      }
      elements <- elements[!(elements %in% remove)]
      #the join object will now be right after the start_ind and can be removed as well if there is a join-index
      if(stop_ind != 0)
      {
        elements <- elements[-(start_ind+1)]
      }
    }
    i <- i-1
  }
  #Create full trajectory
  t1 <- trajectory(name='final_traj')
  for(i in 1:length(elements))
  {
    #ADDED
    if(elements[[i]]$type == 'inter_event')
    {
      timeout(t1, task = elements[[i]]$task)
    }
    if(elements[[i]]$type == 'activity')
    {
      if(elements[[i]]$resource == 'N/A')
      {
        timeout(t1, task = elements[[i]]$task)
        set_attribute(t1, elements[[i]]$name, 000)
      }
      else
      {
        #log_(t1, elements[[i]]$name)
        #cat(log_(t1, elements[[i]]$name),file="outfile.txt",sep="\n", append = TRUE)
        seize(t1, resource = elements[[i]]$resource, amount = as.integer(elements[[i]]$nmbr_resources))
        timeout(t1, task = elements[[i]]$task)
        set_attribute(t1, elements[[i]]$name, 000)
        set_attribute(t1, elements[[i]]$resource, 000)
        release(t1, resource = elements[[i]]$resource, amount = as.integer(elements[[i]]$nmbr_resources))
      }
    }
    else if(elements[[i]]$type == 'loop')
    {
      #still need to initialize the amount paramter of the rollback function
      amount <- 0
      for(j in elements[[i]]$loop_to:(i-1))
      {
        #ADDED
        if(elements[[i]]$type == 'inter_event')
        {
          amount <- amount + 1
        }
        if(elements[[j]]$type == 'activity')
        {
          #if the activity element has a resource it is translated into 3 r-activities
          if(elements[[j]]$resource != 'N/A')
          {
            amount <- amount + 3
          }
          else
          {
            amount <- amount + 1
          }
        }
        #AND-gate structure is seen as 2 r-activities by the rollback function
        else if(elements[[j]]$type == 'AND-structure')
        {
          amount <- amount + 2
        }
        #XOR-gate structure is seen as 1 r-activities by the rollback function
        else if(elements[[j]]$type == 'XOR-structure')
        {
          amount <- amount + 1
        }
      }
      prob_return <- 1-elements[[i]]$prob_to_continue
      prob_continue <- elements[[i]]$prob_to_continue
      rollback(t1, amount = amount, check = function() {sample(c(TRUE, FALSE), 1, prob = c(prob_return, prob_continue))})
    }
    else if(elements[[i]]$type == 'AND-structure' || elements[[i]]$type == 'XOR-structure')
    {
      t1 <- join(t1, elements[[i]]$traj)
    }
  }
  simulation_environment <- simmer(name = 'simulation_environment')
  process_sim_model <- list(traj = t1, sim_env = simulation_environment)
  class(process_sim_model) <- "process_sim_model"

  return (process_sim_model)
}
