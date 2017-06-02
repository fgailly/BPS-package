rm(list = ls())
library('BPS')
###step 1: Import BPMN model
import_BPMN(filepath = 'processmodel/bps_demo_2.bpmn.xml')
###step 2: Add additional information not available in a BPMN
#ACTIVITY DURATIONS
set_activity_duration(`CheckCust1`, duration = function() rexp(1, rate= 1/1))
set_activity_duration(`CheckCust2`, duration = function() rexp(1, rate= 1/1))
set_activity_duration(`RegCla1`, duration = function() rexp(1, rate = 1/9))
set_activity_duration(`RegCla2`, duration = function() rexp(1, rate = 1/9))
set_activity_duration(`DetLikeCla`, duration = function() rexp(1, rate = 1/2))
set_activity_duration(`AssCla`, duration = function() rexp(1, rate = 1/20))
set_activity_duration(`InPay`, duration = function() rexp(1, rate = 1/2))
set_activity_duration(`AdvClaimant`, duration = function() rexp(1,rate = 1/4))
set_activity_duration(`CloCla`, duration = function() rexp(1,rate = 1/1))

#PROBABILITIES TO XOR-SPLIT
#Comment: no need to set probabilities if probability is equal between branches (this is the default)
set_probabilities_to_XOR_split(`InfoCompl1GW`, first_elements = c("CallEnd1", "RegCla1"), probabilities = c(0.1,0.9))
set_probabilities_to_XOR_split(`InfoCompl2GW`, first_elements = c("CallEnd2", "RegCla2"), probabilities = c(0.1,0.9))
set_probabilities_to_XOR_split(`LiableGW`, first_elements = c("CaClo", "AssCla"), probabilities = c(0.15,0.85))
set_probabilities_to_XOR_split(`ClaRejGW`, first_elements = c("ClaRej", "default_gateway_3"), probabilities = c(0.2,0.8))

#RESOURCES RESPONSIBLE FOR ACTIVITY
set_resource_to_activity(`CheckCust1`, resource = "op_CC1", amount = 1)
set_resource_to_activity(`CheckCust2`, resource = "op_CC2", amount = 1)
set_resource_to_activity(`RegCla1`, resource = "op_CC1", amount = 1)
set_resource_to_activity(`RegCla2`, resource = "op_CC2", amount = 1)
set_resource_to_activity(`DetLikeCla`, resource = "claHandler", amount = 1)
set_resource_to_activity(`AssCla`, resource = "claHandler", amount = 1)
###step 3: Create the simmer-trajectory object
#Put all elements created by the import_BPMN function on a list
bpmn_elements <- Filter(function(x) is(x, "bpmn_element"), mget(ls()))
#Now we can use the do.call function (of base R)
traj_acquirer <- do.call(transform_BPS, args = bpmn_elements)

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
