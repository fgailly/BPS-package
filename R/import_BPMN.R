#' Reading in a bpmn 2.0 xml file
#'
#' Function is automatically initializing the bpmn-structure of the BPMNsimulator package based on his xml file.
#' You still need to specify the additional information: set_activity_duration(), set_resource_to_activity(), set_intermediate_event_duration(), ....
#' As well as the simulation environment: create_arrivals() & create_resource()
#'
#' [tested for BPMN files retrieved by the signavio platform & bizagi modeller]
#'
#' Requirements for the BPMN:
#' - loops should be modelled using XOR-splits & XOR-joins
#' - AND-structures should be modelled using AND-splits & AND-joins
#' - All elements (activities, splits, joins, intermediate events & stop events) should have unique names
#' - BPMN can only contain 1 start event
#'
#' @param filepath Specify the filepath of the xml_file
#' @param subprocesses_included True if the linked subprocesses are included in the BPMN-xml file. False otherwise
#' @export
#' @import xml2
import_BPMN <- function(filepath, subprocesses_included = FALSE)
{
  #clean xml
  test <- xml2::as_list(xml2::read_xml(filepath))
  test <- test$process
  elements <- list()
  unique_names <- c()
  process <-list()
  class(process)<-append(class(process), "ProcessModel")
  #Create list object storing critical information about the trajectory flow
  #delete BPMN elements that do not provide information for simulation (e.g. data Objects, pools/lanes)
  #throw error when BPMN elements are used that are not supported by our package (OR-GATES, event Based gates)
  number_startevents <- 0
  counter <- 1
  counter_stop <- 1
  counter_elements <- 1
  for(i in 1:length(test))
  {
    if(attr(test[i], which = 'name') == 'subProcess')
    {
      if(subprocesses_included == FALSE) #process subProcess as a normal activity
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
      if(subprocesses_included == TRUE) #Include all BPMN-elements of the subprocess
      {
        subprocess <- test[i]$subProcess
        subpr <- list() #empty list to store the processed subprocess's elements
        number_startevents_sub <- 0
        for(j in 1:length(subprocess))
        {
          if(attr(subprocess[j], which = 'name') == 'task')
          {
            name <- attr(subprocess[[j]], which = 'name')
            if(name == "")
            {
              stop('Not all activities are named in your BPMN')
            }
            if(name %in% unique_names)
            {
              stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
            }
            #testing wether activity has only 1 incoming & 1 outgoing arrow
            if((length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1) > 1)
            {
              stop(paste('All activities can have only 1 incoming and 1 outgoing sequence flow, AND-gates should be modelled using parallel gateway elements &/or loops should be modelled using exclusive gateway elements, error occured at:', name))
            }
            unique_names <- c(unique_names, name)
            incoming <- as.character(subprocess[[j]]$incoming)
            outgoing <- as.character(subprocess[[j]]$outgoing)
            type = 'activity'
            l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
          }
          else if(attr(subprocess[j], which = 'name') == 'intermediateCatchEvent')
          {

            name <- attr(subprocess[[j]], which = 'name')
            if(name == "")
            {
              stop('Not all intermediate events are named in your BPMN')
            }
            if(name %in% unique_names)
            {
              stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
            }
            #testing wether inter_event has only 1 incoming & 1 outgoing arrow
            if((length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1) > 1)
            {
              stop(paste('All intermediate events can have only 1 incoming and 1 outgoing sequence flow, AND-gates should be modelled using parallel gateway elements &/or loops should be modelled using exclusive gateway elements, error occured at:', name))
            }
            unique_names <- c(unique_names, name)
            incoming <- as.character(subprocess[[j]]$incoming)
            outgoing <- as.character(subprocess[[j]]$outgoing)
            type = 'inter_event'
            l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
          }
          else if(attr(subprocess[j], which = 'name') == 'intermediateThrowEvent')
          {
            name <- attr(subprocess[[j]], which = 'name')
            if(name == "")
            {
              stop('Not all intermediate events are named in your BPMN')
            }
            if(name %in% unique_names)
            {
              stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
            }
            #testing wether inter_event has only 1 incoming & 1 outgoing arrow
            if((length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1) > 1)
            {
              stop(paste('All intermediate events can have only 1 incoming and 1 outgoing sequence flow, AND-gates should be modelled using parallel gateway elements &/or loops should be modelled using exclusive gateway elements, error occured at:', name))
            }
            unique_names <- c(unique_names, name)
            incoming <- as.character(subprocess[[j]]$incoming)
            outgoing <- as.character(subprocess[[j]]$outgoing)
            type = 'inter_event'
            l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
          }
          else if(attr(subprocess[j], which = 'name') == 'exclusiveGateway')
          {
            name <- attr(subprocess[[j]], which = 'name')
            if(name == "" && attr(subprocess[[j]], which = 'gatewayDirection') == 'Diverging')
            {
              stop('Not all XOR-splits are named in your BPMN')
            }
            if(name == "" && attr(subprocess[[j]], which = 'gatewayDirection') == 'Converging')
            {
              warning(paste0('Not all XOR-joins are named in your BPMN, a default name is given for an unnamed XOR-join: default_gateway_', as.character(counter)))
              name <- paste0('default_gateway_', as.character(counter))
              counter <- counter + 1
            }
            if(name %in% unique_names)
            {
              stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
            }
            unique_names <- c(unique_names, name)
            if(attr(subprocess[[j]], which = 'gatewayDirection') == 'Diverging')
            {
              incoming <- as.character(subprocess[[j]]$incoming)
              type <- 'XOR-split'
              number_of_branches <- (length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1)
              names(subprocess[[j]]) <- make.unique(names(subprocess[[j]]))
              outgoing <- vector('list', number_of_branches)
              outgoing[[1]] <- as.character(subprocess[[j]]$outgoing)
              for(k in 1:(number_of_branches-1))
              {
                outgoing[[k+1]] <- as.character(eval(parse(text = paste('subprocess[[', j, ']]$outgoing.', k, sep =""))))
              }
              l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
            }
            if(attr(subprocess[[j]], which = 'gatewayDirection') == 'Converging')
            {
              outgoing <- as.character(subprocess[[j]]$outgoing)
              type <- 'XOR-join'
              number_of_branches <- (length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1)
              names(subprocess[[j]]) <- make.unique(names(subprocess[[j]]))
              incoming <- vector('list', number_of_branches)
              incoming[[1]] <- as.character(subprocess[[j]]$incoming)
              for(k in 1:(number_of_branches-1))
              {
                incoming[[k+1]] <- as.character(eval(parse(text = paste('subprocess[[', j, ']]$incoming.', k, sep =""))))
              }
              l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
            }
          }
          else if(attr(subprocess[j], which = 'name') == 'parallelGateway')
          {
            name <- attr(subprocess[[j]], which = 'name')
            if(name == "")
            {
              warning(paste0('Not all AND-gateways are named in your BPMN, a default name is given for an unnamed AND-gateway: default_gateway_', as.character(counter)))
              name <- paste0('default_gateway_', as.character(counter))
              counter <- counter + 1
            }
            if(name %in% unique_names)
            {
              stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
            }
            unique_names <- c(unique_names, name)
            if(attr(subprocess[[j]], which = 'gatewayDirection') == 'Diverging')
            {
              incoming <- as.character(subprocess[[j]]$incoming)
              type <- 'AND-split'
              number_of_branches <- (length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1)
              names(subprocess[[j]]) <- make.unique(names(subprocess[[j]]))
              outgoing <- vector('list', number_of_branches)
              outgoing[[1]] <- as.character(subprocess[[j]]$outgoing)
              for(k in 1:(number_of_branches-1))
              {
                outgoing[[k+1]] <- as.character(eval(parse(text = paste('subprocess[[', j, ']]$outgoing.', k, sep =""))))
              }
              l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
            }
            if(attr(subprocess[[j]], which = 'gatewayDirection') == 'Converging')
            {
              outgoing <- as.character(subprocess[[j]]$outgoing)
              type <- 'AND-join'
              number_of_branches <- (length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1)
              names(subprocess[[j]]) <- make.unique(names(subprocess[[j]]))
              incoming <- vector('list', number_of_branches)
              incoming[[1]] <- as.character(subprocess[[j]]$incoming)
              for(k in 1:(number_of_branches-1))
              {
                incoming[[k+1]] <- as.character(eval(parse(text = paste('subprocess[[', j, ']]$incoming.', k, sep =""))))
              }
              l_sub <- list(name = name, incoming = incoming, outgoing = outgoing, type = type)
            }
          }
          else if((attr(subprocess[j], which = 'name') == 'endEvent'))
          {
            name <- attr(subprocess[[j]], which = 'name')
            if(name == "")
            {
              name <- paste0('default_end_', as.character(counter_stop))
              counter_stop <- counter_stop + 1
            }
            if(name %in% unique_names)
            {
              stop(paste('Not all the BPMN-elements (activities, intermediate events, stop events & gateways) have a unique name, the following name is used by multiple elements:', name))
            }
            #testing wether stop event has only 1 incoming & 1 outgoing arrow
            if((length(names(subprocess[[j]])) - length(unique(names(subprocess[[j]]))) + 1) > 1)
            {
              stop(paste('All stop events can have only 1 incoming sequence flow, AND-gates should be modelled using parallel gateway elements, error occured at:', name))
            }
            unique_names <- c(unique_names, name)
            incoming <- as.character(subprocess[[j]]$incoming)
            type = 'stop_event'
            l_sub <- list(name = name, incoming = incoming, outgoing = 'no-outgoing-because-stop-event', type = type)
          }
          else if((attr(subprocess[j], which = 'name') == 'startEvent'))
          {
            number_startevents_sub <- number_startevents_sub + 1
            if(number_startevents_sub > 1)
            {
              stop('subprocess contains more than 1 startevent, subprocesses with multiple startevents are not supported by the package')
            }
            name <- 'start_event_default'
            type <- 'start_event'
            outgoing = as.character(subprocess[[j]]$outgoing)
            l_sub <- list(name = name, incoming = 'no-incoming-because-stop-event', outgoing = outgoing, type = type)
          }
          else if((attr(subprocess[j], which = 'name') == 'incoming') || (attr(subprocess[j], which = 'name') == 'outgoing'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'sequenceFlow'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'extensionElements'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'laneSet'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'association'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'group'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'textAnnotation'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'documentation'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'dataObject' || attr(subprocess[j], which = 'name') == 'dataObjectReference'))
          {
            l_sub <- list(name = '', type = 'remove')
          }
          else if((attr(subprocess[j], which = 'name') == 'eventBasedGateway') || (attr(subprocess[j], which = 'name') == 'inclusiveGateway'))
          {
            stop('Event-based Gateways & Inclusive Gateways are not supported by the BPMNsimulation package, try to remodel using XOR-gateways & parallel gateways')
          }
          else
          {
            l_sub <- list(name = '', type = 'remove')
            warning(paste('the following BPMN structure is not supported by the BPMNsimulation package: ', attr(subprocess[j], which = 'name')))
          }
          #put the elements of the subprocess on a list
          subpr[[j]] <- l_sub
        }
        #remove sequenceFlows, extensionElements, ...
        j <- 1
        while(j <= length(subpr))
        {
          if(subpr[[j]]$type == "remove")
          {
            subpr[[j]] <- NULL
          }
          j <- j+1
        }
        #Make link between overall process and subprocess
        #element with start_event as 'incoming': 'incoming' should be replaced witht the incoming of the subprocess
        #element with stop_event as 'outgoing': 'outgoing' should be replaced witht the outgoing of the subprocess
        for(j in 1:length(subpr))
        {
          for(k in 1:length(subpr))
          {
            if(sum( unlist(subpr[[j]]$incoming) %in% unlist(subpr[[k]]$outgoing)) == 1 && subpr[[k]]$type == 'start_event')
            {
              subpr[[j]]$incoming <- as.character(test[[i]]$incoming)
              subpr[[k]]$type <- 'remove'
            }
            if(sum( unlist(subpr[[j]]$outgoing) %in% unlist(subpr[[k]]$incoming)) == 1 && subpr[[k]]$type == 'stop_event')
            {
              subpr[[j]]$outgoing <- as.character(test[[i]]$outgoing)
              subpr[[k]]$type <- 'remove'
            }
          }
        }
        #put the elements of subpr on the elements list
        for(j in 1:length(subpr))
        {
          if(subpr[[j]]$type != 'remove') #do not end start and stop events
          {
            elements[[counter_elements]] <- subpr[[j]]
            counter_elements <- counter_elements + 1
          }
        }
      }
    }
    else if(attr(test[i], which = 'name') == 'task')
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
      if(name == "" && attr(test[[i]], which = 'gatewayDirection') == 'Diverging')
      {
        stop('Not all XOR-splits are named in your BPMN')
      }
      if(name == "" && attr(test[[i]], which = 'gatewayDirection') == 'Converging')
      {
        warning(paste0('Not all XOR-joins are named in your BPMN, a default name is given for an unnamed XOR-join: default_gateway_', as.character(counter)))
        name <- paste0('default_gateway_', as.character(counter))
        counter <- counter + 1
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
        warning(paste0('Not all AND-gateways are named in your BPMN, a default name is given for an unnamed AND-gateway: default_gateway_', as.character(counter)))
        name <- paste0('default_gateway_', as.character(counter))
        counter <- counter + 1
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
        warning(paste0('Not all end events are named in your BPMN, a default name is given for an unnamed end event: default_end_', as.character(counter_stop)))
        name <- paste0('default_end_', as.character(counter_stop))
        counter_stop <- counter_stop + 1
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
      number_startevents <- number_startevents + 1
      if(number_startevents > 1)
      {
        stop('BPMN contains more than 1 startevent, BPMNs with multiple startevents are not supported by the package')
      }
      name <- 'start_event_default'
      type <- 'start_event'
      outgoing = as.character(test[[i]]$outgoing)
      l <- list(name = name, incoming = 'no-incoming-because-stop-event', outgoing = outgoing, type = type)
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
    elements[[counter_elements]] <- l
    counter_elements <- counter_elements + 1
  }
  #remove sequenceFlows, extensionElements, ...
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
            if(checker$name == "start_event_default")
            {
              f <- 1
              break
            }
            if(new_elements[[k]]$name %in% checker$prev_element)
            {
              if(sum(checker$prev_element %in% branches[[j]]) >= 1)
              {
                if(!(new_elements[[k]]$name %in% branches[[j]]))
                {
                  branches[[j]][length(branches[[j]])+1] <- new_elements[[k]]$name
                  checker <- new_elements[[k]]
                }
              }
              else
              {
                branches[[j]][length(branches[[j]])+1] <- new_elements[[k]]$name
                checker <- new_elements[[k]]
              }
            }
          }
        }
      }
      common_elements <- Reduce(intersect, branches)
      new_elements[[i]]$of_split <- common_elements[1]
      #Deal with loops where previous element is not a XOR-split
      if(new_elements[[i]]$type == 'XOR-join')
      {
        for(j in 1:length(new_elements))
        {
          #check whether of_split variable is of the type XOR-split ==> if NOT, we have a loop
          if(common_elements[1] == new_elements[[j]]$name && new_elements[[j]]$type != 'XOR-split')
          {
            for(k in 1:length(branches))
            {
              #the branch of which the first element is not the common_element is the loop-branch
              if(branches[[k]][1] != common_elements[1])
              {
                #delete that element from the prev_elements of the join!
                new_elements[[i]]$prev_element <- new_elements[[i]]$prev_element[!new_elements[[i]]$prev_element %in% branches[[k]][1]]
                #search for the correct split_element of the join in this loop-branch
                #loop branch is backwards rebuild from the join, this means the first split you will encounter will be the split of the join
                for(z in 1:length(branches[[k]]))
                {
                  for(a in 1:length(new_elements))
                  {
                    if(branches[[k]][z] == new_elements[[a]]$name && new_elements[[a]]$type == 'XOR-split')
                    {
                      new_elements[[i]]$of_split <- branches[[k]][z]
                      break
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  for(i in 1:length(new_elements))
  {

    if(new_elements[[i]]$type == 'activity')
    {
      process<- add_activity(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)

    }
    else if(new_elements[[i]]$type == 'inter_event')
    {
      process <- add_intermediate_event(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'XOR-split' || new_elements[[i]]$type == 'loop-split')
    {
      process <- add_XOR_split(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'AND-split')
    {
      process <- add_AND_split(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }
    else if(new_elements[[i]]$type == 'XOR-join' || new_elements[[i]]$type == 'loop-join')
    {
      process <- add_XOR_join(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element,  of_split = new_elements[[i]]$of_split)
    }
    else if(new_elements[[i]]$type == 'AND-join')
    {
      process <- add_AND_join(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element,  of_split = new_elements[[i]]$of_split)
    }
    else if(new_elements[[i]]$type =='stop_event')
    {
      process <- add_stop_event(process, name = new_elements[[i]]$name, prev_element = new_elements[[i]]$prev_element)
    }

  }
  return(process)

}
