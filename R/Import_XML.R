#' Reading in a bpmn 2.0 xml file
#'
#' Function is automatically initializing the bpmn-structure based on his xml file.
#' You still need to specify the additional information: set_activity_duration(), set_resource_to_activity(), set_intermediate_event_duration(), ....
#' As well as the simulation environment: create_arrivals() & create_resource()
#' [tested for xml files retrieved by the signavio platform & bizagi modeller]
#' Requirements for the BPMN:
#' - BPMN loops should be modelled using XOR-splits & XOR-joins
#' - AND-structures should be modelled using AND-splits & AND-joins
#' - All elements (activities, splits, joins, intermediate events & stop events) should have unique names
#' - BPMN can only contain 1 start event
#'
#' @param filepath Specify the filepath of the xml_file
#' @export
#' @import xml2
import_XML <- function(filepath)
{
  #clean xml
  test <- xml2::as_list(xml2::read_xml(filepath))
  test <- test$process
  elements <- list()
  unique_names <- c()
  #Create list object storing critical information about the trajectory flow
  #delete BPMN elements that do not provide information for simulation (e.g. data Objects, pools/lanes)
  #throw error when BPMN elements are used that are not supported by our package (OR-GATES, event Based gates)
  number_startevents <- 0
  for(i in 1:length(test))
  {
    if(attr(test[i], which = 'name') == 'task')
    {
      name <- attr(test[[i]], which = 'name')
      if(name == "")
      {
        stop('Not all activities are named in your BPMN')
      }
      if(name %in% unique_names)
      {
        stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
      }
      #testing wether activity has only 1 incoming & 1 outgoing arrow
      if((length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1) > 1)
      {
        stop(paste('All activities can have only 1 incoming and 1 outgoing sequence flow, AND-gates should be modelled using parallel gateway elements &/or loops should be modelled using exclusive gateway elements, error occured at:', name))
      }
      unique_names <- c(unique_names, name)
      incoming <- as.character(test[[i]]$incoming)
      outgoing <- as.character(test[[i]]$outgoing)
      type = 'activity'
      l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
    }
    else if(attr(test[i], which = 'name') == 'intermediateCatchEvent')
    {

      name <- attr(test[[i]], which = 'name')
      if(name == "")
      {
        stop('Not all intermediate events are named in your BPMN')
      }
      if(name %in% unique_names)
      {
        stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
      }
      #testing wether inter_event has only 1 incoming & 1 outgoing arrow
      if((length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1) > 1)
      {
        stop(paste('All intermediate events can have only 1 incoming and 1 outgoing sequence flow, AND-gates should be modelled using parallel gateway elements &/or loops should be modelled using exclusive gateway elements, error occured at:', name))
      }
      unique_names <- c(unique_names, name)
      incoming <- as.character(test[[i]]$incoming)
      outgoing <- as.character(test[[i]]$outgoing)
      type = 'inter_event'
      l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
    }
    else if(attr(test[i], which = 'name') == 'intermediateThrowEvent')
    {
      name <- attr(test[[i]], which = 'name')
      if(name == "")
      {
        stop('Not all intermediate events are named in your BPMN')
      }
      if(name %in% unique_names)
      {
        stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
      }
      #testing wether inter_event has only 1 incoming & 1 outgoing arrow
      if((length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1) > 1)
      {
        stop(paste('All intermediate events can have only 1 incoming and 1 outgoing sequence flow, AND-gates should be modelled using parallel gateway elements &/or loops should be modelled using exclusive gateway elements, error occured at:', name))
      }
      unique_names <- c(unique_names, name)
      incoming <- as.character(test[[i]]$incoming)
      outgoing <- as.character(test[[i]]$outgoing)
      type = 'inter_event'
      l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
    }
    else if(attr(test[i], which = 'name') == 'exclusiveGateway')
    {
      name <- attr(test[[i]], which = 'name')
      if(name == "")
      {
        stop('Not all gateways are named in your BPMN')
      }
      if(name %in% unique_names)
      {
        stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
      }
      unique_names <- c(unique_names, name)
      if(attr(test[[i]], which = 'gatewayDirection') == 'Diverging')
      {
        incoming <- as.character(test[[i]]$incoming)
        type <- 'XOR-split'
        number_of_branches <- (length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1)
        names(test[[i]]) <- make.unique(names(test[[i]]))
        outgoing <- vector('list', number_of_branches)
        outgoing[[1]] <- as.character(test[[i]]$outgoing)
        for(j in 1:(number_of_branches-1))
        {
          outgoing[[j+1]] <- as.character(eval(parse(text = paste('test[[', i, ']]$outgoing.', j, sep =""))))
        }
        l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
      }
      if(attr(test[[i]], which = 'gatewayDirection') == 'Converging')
      {
        outgoing <- as.character(test[[i]]$outgoing)
        type <- 'XOR-join'
        number_of_branches <- (length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1)
        names(test[[i]]) <- make.unique(names(test[[i]]))
        incoming <- vector('list', number_of_branches)
        incoming[[1]] <- as.character(test[[i]]$incoming)
        for(j in 1:(number_of_branches-1))
        {
          incoming[[j+1]] <- as.character(eval(parse(text = paste('test[[', i, ']]$incoming.', j, sep =""))))
        }
        l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
      }
    }
    else if(attr(test[i], which = 'name') == 'parallelGateway')
    {
      name <- attr(test[[i]], which = 'name')
      if(name == "")
      {
        stop('Not all gateways are named in your BPMN')
      }
      if(name %in% unique_names)
      {
        stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
      }
      unique_names <- c(unique_names, name)
      if(attr(test[[i]], which = 'gatewayDirection') == 'Diverging')
      {
        incoming <- as.character(test[[i]]$incoming)
        type <- 'AND-split'
        number_of_branches <- (length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1)
        names(test[[i]]) <- make.unique(names(test[[i]]))
        outgoing <- vector('list', number_of_branches)
        outgoing[[1]] <- as.character(test[[i]]$outgoing)
        for(j in 1:(number_of_branches-1))
        {
          outgoing[[j+1]] <- as.character(eval(parse(text = paste('test[[', i, ']]$outgoing.', j, sep =""))))
        }
        l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
      }
      if(attr(test[[i]], which = 'gatewayDirection') == 'Converging')
      {
        outgoing <- as.character(test[[i]]$outgoing)
        type <- 'AND-join'
        number_of_branches <- (length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1)
        names(test[[i]]) <- make.unique(names(test[[i]]))
        incoming <- vector('list', number_of_branches)
        incoming[[1]] <- as.character(test[[i]]$incoming)
        for(j in 1:(number_of_branches-1))
        {
          incoming[[j+1]] <- as.character(eval(parse(text = paste('test[[', i, ']]$incoming.', j, sep =""))))
        }
        l <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
      }
    }
    else if((attr(test[i], which = 'name') == 'endEvent'))
    {
      name <- attr(test[[i]], which = 'name')
      if(name == "")
      {
        stop('Not all stop events are named in your BPMN')
      }
      if(name %in% unique_names)
      {
        stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
      }
      #testing wether stop event has only 1 incoming & 1 outgoing arrow
      if((length(names(test[[i]])) - length(unique(names(test[[i]]))) + 1) > 1)
      {
        stop(paste('All stop events can have only 1 incoming sequence flow, AND-gates should be modelled using parallel gateway elements, error occured at:', name))
      }
      unique_names <- c(unique_names, name)
      incoming <- as.character(test[[i]]$incoming)
      type = 'stop_event'
      l <- list(name = name, incoming = incoming, outgoing = 'no-outgoing-because-stop-event', type = type)
    }
    else if((attr(test[i], which = 'name') == 'startEvent'))
    {
      l <- list(name = '', type = 'remove')
      number_startevents <- number_startevents + 1
      if(number_startevents > 1)
      {
        stop('BPMN contains more than 1 startevent, BPMNs with multiple startevents are not supported by the package')
      }
    }
    else if((attr(test[i], which = 'name') == 'sequenceFlow'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'extensionElements'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'laneSet'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'association'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'group'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'textAnnotation'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'documentation'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'dataObject' || attr(test[i], which = 'name') == 'dataObjectReference'))
    {
      l <- list(name = '', type = 'remove')
    }
    else if((attr(test[i], which = 'name') == 'eventBasedGateway') || (attr(test[i], which = 'name') == 'inclusiveGateway'))
    {
      stop('Event-based Gateways & Inclusive Gateways are not supported by the BPMNsimulation package, try to remodel using XOR-gateways & parallel gateways')
    }
    else
    {
      l <- list(name = '', type = 'remove')
      warning(paste('the following BPMN structure is not supported by the BPMNsimulation package: ', attr(test[i], which = 'name')))
    }
    elements[[i]] <- l
  }
  #remove startEvent, endEvents, sequenceFlows, extensionElements, ...
  i <- 1
  while(i <= length(elements))
  {
    if(elements[[i]]$type == "remove")
    {
      elements[[i]] <- NULL
    }
    i <- i+1
  }
  #clean elements name
  for(i in 1:length(elements))
  {
    elements[[i]]$name <- gsub('\n','',elements[[i]]$name)
  }
  new_elements <- vector('list', length(elements))
  #initialize the prev_elements
  for(i in 1:length(elements))
  {
    prev_element <- c()
    for(j in 1:length(elements))
    {
      if(sum( unlist(elements[[i]]$incoming) %in% unlist(elements[[j]]$outgoing)) == 1)
      {
        prev_element <- c(prev_element, elements[[j]]$name)
      }
    }
    if(is.null(prev_element))
    {
      prev_element <- ''
    }
    of_split = ''
    #when dealing with a loop, we don't want the split in the previous element of the join
    if(elements[[i]]$type == 'XOR-join')
    {
      for(j in i:length(elements))
      {
        for(k in 1:length(prev_element))
        {
          if(elements[[j]]$name == prev_element[k] & elements[[j]]$type == 'XOR-split')
          {
            #remove it from prev_element of the join
            prev_element <- prev_element[-k]
            of_split <- elements[[j]]$name
            #change type of XOR-split element
            elements[[j]]$type <- 'loop-split'
            elements[[i]]$type <- 'loop-join'
          }
        }
      }
    }
    l <- list(name = elements[[i]]$name, prev_element= prev_element, of_split = of_split, type= elements[[i]]$type)
    new_elements[[i]] <- l
  }
  #initialize of_split of XOR-joins and AND-joins
  #Rebuild branches starting from the prev_elements in join & the first common split is the split of the join
  #We start from the previous elements of the join
  for(i in 1:length(new_elements))
  {
    if(new_elements[[i]]$type == 'AND-join' || new_elements[[i]]$type == 'XOR-join')
    {
      branches <- vector('list', length(new_elements[[i]]$prev_element))
      for(j in 1:length(new_elements[[i]]$prev_element))
      {
        branches[[j]][1] <- new_elements[[i]]$prev_element[j]
        for(z in 1:length(new_elements))
        {
          if(new_elements[[z]]$name == branches[[j]][length(branches[[j]])])
          {
            checker <- new_elements[[z]]
          }
        }
        f <- 0
        while(f == 0)
        {
          for(k in 1:length(new_elements))
          {
            if(new_elements[[k]]$name %in% checker$prev_element)
            {
              branches[[j]][length(branches[[j]])+1] <- new_elements[[k]]$name
              checker <- new_elements[[k]]
            }
            if(checker$prev_element == '')
            {
              f <- 1
              break
            }
          }
        }
      }
      common_elements <- Reduce(intersect, branches)
      new_elements[[i]]$of_split <- common_elements[1]
    }
  }
  for(i in 1:length(new_elements))
  {
    if(new_elements[[i]]$type == 'activity')
    {
      add_activity(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'inter_event')
    {
      add_intermediate_event(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'XOR-split' || new_elements[[i]]$type == 'loop-split')
    {
      add_XOR_split(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'AND-split')
    {
      add_AND_split(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'XOR-join' || new_elements[[i]]$type == 'loop-join')
    {
      add_XOR_join(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element,  of_split = new_elements[[i]]$of_split)
    }
    else if(new_elements[[i]]$type == 'AND-join')
    {
      add_AND_join(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element,  of_split = new_elements[[i]]$of_split)
    }
    else if(new_elements[[i]]$type =='stop_event')
    {
      add_stop_event(name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
  }
}



