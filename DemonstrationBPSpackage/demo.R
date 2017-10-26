rm(list = ls())
library('BPS')
###step 1: Import BPMN model
processmodel <- import_BPMN(filepath = 'DemonstrationBPSpackage/processmodel/bps_demo_2.bpmn.xml')
###step 2: Add additional information not available in a BPMN
#ACTIVITY DURATIONS
processmodel <- set_activity_duration(processmodel,"CheckCust1", duration = function() rexp(1, rate= 1/1))
processmodel <- set_activity_duration(processmodel,"CheckCust2", duration = function() rexp(1, rate= 1/1))
processmodel <- set_activity_duration(processmodel,"RegCla1", duration = function() rexp(1, rate = 1/9))
processmodel <- set_activity_duration(processmodel,"RegCla2", duration = function() rexp(1, rate = 1/9))
processmodel <- set_activity_duration(processmodel,"DetLikeCla", duration = function() rexp(1, rate = 1/2))
processmodel <- set_activity_duration(processmodel,"AssCla", duration = function() rexp(1, rate = 1/20))
processmodel <- set_activity_duration(processmodel,"InPay", duration = function() rexp(1, rate = 1/2))
processmodel <- set_activity_duration(processmodel,"AdvClaimant", duration = function() rexp(1,rate = 1/4))
processmodel <- set_activity_duration(processmodel,"CloCla", duration = function() rexp(1,rate = 1/1))

#PROBABILITIES TO XOR-SPLIT
#Comment: no need to set probabilities if probability is equal between branches (this is the default)
processmodel <- set_probabilities_to_XOR_split(processmodel, "InfoCompl1GW", first_elements = c("CallEnd1", "RegCla1"), probabilities = c(0.1,0.9))
processmodel <- set_probabilities_to_XOR_split(processmodel, "InfoCompl2GW", first_elements = c("CallEnd2", "RegCla2"), probabilities = c(0.1,0.9))
processmodel <- set_probabilities_to_XOR_split(processmodel, "LiableGW", first_elements = c("CaClo", "AssCla"), probabilities = c(0.15,0.85))
processmodel <- set_probabilities_to_XOR_split(processmodel, "ClaRejGW", first_elements = c("ClaRej", "default_gateway_3"), probabilities = c(0.2,0.8))

#RESOURCES RESPONSIBLE FOR ACTIVITY
processmodel <- set_resource_to_activity(processmodel, "CheckCust1", resource = "op_CC1", amount = 1)
processmodel <- set_resource_to_activity(processmodel, "CheckCust2", resource = "op_CC2", amount = 1)
processmodel <- set_resource_to_activity(processmodel, "RegCla1", resource = "op_CC1", amount = 1)
processmodel <- set_resource_to_activity(processmodel, "RegCla2", resource = "op_CC2", amount = 1)
processmodel <- set_resource_to_activity(processmodel, "DetLikeCla", resource = "claHandler", amount = 1)
processmodel <- set_resource_to_activity(processmodel, "AssCla", resource = "claHandler", amount = 1)

###step 3: Create the process model simulation object

#Now we can use the do.call function (of base R)
processimulationmodel <- do.call(transform_BPS, args = processmodel)

##Arrival rate

processimulationmodel <- create_arrivals(processimulationmodel, function() sample(x=c(0.1,0.3,0.8), size = 1))

##RESOURCES AVAILABLE

sched_CallCentre <- schedule(c(0, 240, 480), c(40, 55, 25), period=720)
sched_Claimhandler <- schedule(c(0, 480), c(150,0), period=720)

processimulationmodel <- create_resource(processimulationmodel, resource = "op_CC1", schedule = sched_CallCentre)
processimulationmodel <- create_resource(processimulationmodel, resource = "op_CC2", schedule = sched_CallCentre)
processimulationmodel <- create_resource(processimulationmodel, resource = "claHandler", schedule = sched_Claimhandler)

###step 4: run the simulation (using simmer package functions)
#run for 1 week: 5 days of 12 hours ==> run for 3600 minutes
run(processimulationmodel$sim_env, until = 3601)
###step 5: retrieve results (using simmer package functions)
##dataframe containing information about arrivals
arrivals <- get_mon_arrivals(processimulationmodel$sim_env)
transf <- transform_get_mon_arrivals(arrivals)
#calculate waiting time
#waiting_time = (end_time - start_time) - activity_time
transf$waiting_time = (transf$end_time - transf$start_time) - transf$activity_time
#Calculate average waiting time and activity time of instance
mean(transf$waiting_time) #62.73 minutes
mean(transf$activity_time) #23.61 minutes
##dataframe containing information about resources
resources <- get_mon_resources(processimulationmodel$sim_env)
##plots containing information
plot(processimulationmodel$sim_env, what="resources", metric="usage", "op_CC1")
plot(processimulationmodel$sim_env, what="resources", metric="utilization", "op_CC1")
plot(processimulationmodel$sim_env, what="arrivals", metric="flow_time")
