#' Reading in a bpmn 2.0 xml file
#'
#' Function is automatically initializing the bpmn-structure based on his xml file.
#' You still need to specify the additional information: add_activity_duration(), add_resource_to_activity(), add_intermediate_event_duration().
#' As well as the simulation environment: add_interarrival_time() & add_resources_to_simulation()
#' [tested for xml files retrieved by the signavio platform & bizagi modeller]
#' Requirements for the BPMN:
#' - BPMN loops should be modelled using XOR-splits & XOR-Splits
#' - AND-structures should be modelled using AND-splits & AND-joins
#' - XOR-split and his accompanying XOR-join (if any) should be named split_xxx & join_xxx respectively, where xxx is the unique name of the gatestructure (e.g split_qualitycheck & join_qualitycheck)
#' - AND-split and his accompanying AND-join should be named split_xxx & join_xxx respectively, where xxx is the unique name of the gatestructure (e.g split_andstr1 & join_andstr1)
#' - All elements (activities, splits, joins, intermediate events & stop events) should have unique names
#'
#' @param filepath Specify the filepath of the xml_file
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
        stop('Not all gateways are named in your BPMN, also check the naming convention for gateways: see ?Import_XML')
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
        stop('Not all gateways are named in your BPMN, also check the naming convention for gateways: see ?Import_XML')
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
      elements[i] <- NULL
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
  #Naming convention: split & his accompanying join are named split_NAMEx & join_NAMEx
  for(i in 1:length(new_elements))
  {
    if(new_elements[[i]]$type == 'XOR-join' || new_elements[[i]]$type == 'AND-join')
    {
      new_elements[[i]]$of_split = paste("split", substring(new_elements[[i]]$name,6), sep="_")
    }
    #check wether this split is present (wether naming convention is followed)
    bool <- FALSE
    for(j in 1:length(new_elements))
    {

      if(new_elements[[j]]$name == new_elements[[i]]$of_split)
      {
        bool <- TRUE
      }
    }
    if(bool == FALSE)
    {
      stop("Naming convention of gate-structures is not followed. the split and join of the same gate-structure should be named split_NameOfGateStructure & join_NameOfGateStructure, were NameOfGateStructure should be unique for each gatestructure")
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



