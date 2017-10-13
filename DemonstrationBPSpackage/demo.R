rm(list = ls())
library('BPS')
###step 1: Import BPMN model
process <- import_BPMN(filepath = 'DemonstrationBPSpackage/processmodel/bps_demo_2.bpmn.xml')
###step 2: Add additional information not available in a BPMN
#ACTIVITY DURATIONS
process <- set_activity_duration(process,"CheckCust1", duration = function() rexp(1, rate= 1/1))
process <- set_activity_duration(process,"CheckCust2", duration = function() rexp(1, rate= 1/1))
process <- set_activity_duration(process,"RegCla1", duration = function() rexp(1, rate = 1/9))
process <- set_activity_duration(process,"RegCla2", duration = function() rexp(1, rate = 1/9))
process <- set_activity_duration(process,"DetLikeCla", duration = function() rexp(1, rate = 1/2))
process <- set_activity_duration(process,"AssCla", duration = function() rexp(1, rate = 1/20))
process <- set_activity_duration(process,"InPay", duration = function() rexp(1, rate = 1/2))
process <- set_activity_duration(process,"AdvClaimant", duration = function() rexp(1,rate = 1/4))
process <- set_activity_duration(process,"CloCla", duration = function() rexp(1,rate = 1/1))

#PROBABILITIES TO XOR-SPLIT
#Comment: no need to set probabilities if probability is equal between branches (this is the default)
process <- set_probabilities_to_XOR_split(process, "InfoCompl1GW", first_elements = c("CallEnd1", "RegCla1"), probabilities = c(0.1,0.9))
process <- set_probabilities_to_XOR_split(process, "InfoCompl2GW", first_elements = c("CallEnd2", "RegCla2"), probabilities = c(0.1,0.9))
process <- set_probabilities_to_XOR_split(process, "LiableGW", first_elements = c("CaClo", "AssCla"), probabilities = c(0.15,0.85))
process <- set_probabilities_to_XOR_split(process, "ClaRejGW", first_elements = c("ClaRej", "default_gateway_3"), probabilities = c(0.2,0.8))

#RESOURCES RESPONSIBLE FOR ACTIVITY
process <- set_resource_to_activity(process, "CheckCust1", resource = "op_CC1", amount = 1)
process <- set_resource_to_activity(process, "CheckCust2", resource = "op_CC2", amount = 1)
process <- set_resource_to_activity(process, "RegCla1", resource = "op_CC1", amount = 1)
process <- set_resource_to_activity(process, "RegCla2", resource = "op_CC2", amount = 1)
process <- set_resource_to_activity(process, "DetLikeCla", resource = "claHandler", amount = 1)
process <- set_resource_to_activity(process, "AssCla", resource = "claHandler", amount = 1)
###step 3: Create the simmer-trajectory object
#Put all elements created by the import_BPMN function on a list
bpmn_elements <- Filter(function(x) is(x, "bpmn_element"), mget(ls()))
#Now we can use the do.call function (of base R)
traj_acquirer <- do.call(transform_BPS, args = process)

###step 4: Define the simulation environment
#first delete simulation_environment if it already exists from a previous simulation
rm(simulation_environment)
create_arrivals(traj_acquirer, function() sample(x=c(0.1,0.3,0.8), size = 1))

##RESOURCES AVAILABLE

sched_CallCentre <- schedule(c(0, 240, 480), c(40, 55, 25), period=720)
sched_Claimhandler <- schedule(c(0, 480), c(150,0), period=720)

create_resource(resource = "op_CC1", schedule = sched_CallCentre)
create_resource(resource = "op_CC2", schedule = sched_CallCentre)
create_resource(resource = "claHandler", schedule = sched_Claimhandler)

###step 5: run the simulation (using simmer package functions)
#run for 1 week: 5 days of 12 hours ==> run for 3600 minutes
run(simulation_environment, until = 3601)
###step 6: retrieve results (using simmer package functions)
##dataframe containing information about arrivals
arrivals <- get_mon_arrivals(simulation_environment)
transf <- transform_get_mon_arrivals(arrivals)
#calculate waiting time
#waiting_time = (end_time - start_time) - activity_time
transf$waiting_time = (transf$end_time - transf$start_time) - transf$activity_time
#Calculate average waiting time and activity time of instance
mean(transf$waiting_time) #62.73 minutes
mean(transf$activity_time) #23.61 minutes
##dataframe containing information about resources
resources <- get_mon_resources(simulation_environment)
##plots containing information
plot(simulation_environment, what="resources", metric="usage", "op_CC1")
plot(simulation_environment, what="resources", metric="utilization", "op_CC1")
plot(simulation_environment, what="arrivals", metric="flow_time")
