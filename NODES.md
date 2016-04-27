# Epoxide node documentation

The following nodes are currently available:
[Arp](#arp),
[Clock](#clock),
[Command](#command),
[Decision-summary](#decision-summary),
[Decision](#decision),
[Doubledecker](#doubledecker),
[Dpids-fl](#dpids-fl),
[Dpids-odl](#dpids-odl),
[Dpids-pox](#dpids-pox),
[Emacs-buffer](#emacs-buffer),
[Escape](#escape),
[Filter](#filter),
[Flow-space-filter](#flow-space-filter),
[Flow-stat-fl](#flow-stat-fl),
[Flow-stat-odl](#flow-stat-odl),
[Flow-stat-ofctl](#flow-stat-ofctl),
[Flow-stat-pox](#flow-stat-pox),
[Format](#format),
[Function](#function),
[Gdb](#gdb),
[Graph](#graph),
[Host](#host),
[Ifconfig](#ifconfig),
[Iperf](#iperf),
[Iptables](#iptables),
[Json-filter](#json-filter),
[Json-list-filter](#json-list-filter),
[Ping](#ping),
[Rest-api](#rest-api),
[Route](#route),
[Table-view](#table-view),
[Topology-fl](#topology-fl),
[Topology-pox](#topology-pox),
[Traceroute](#traceroute).

## Arp

The node is able to query the ARP cache by evoking the `arp`
command.  It has one input that serves as a clock input, each time
it changes a query is performed.  The node's first configuration
argument specifies the host where the query should be run at and
the second one specifies those interfaces that are to be kept
(while all the others are discarded) when relaying data to the
node's only output.  The list of these interfaces should appear as
a semicolon (;) separated list.  When the second argument is not
present or set to nil, data is displayed exactly the same as
returned by the `arp` command. All subsequent configuration
arguments are passed to the `arp` command as command line
arguments.

## Clock

A node that takes one configuration argument that specifies a time
interval.  The node constantly updates its output with an increasing
number after waiting for the specified time interval.

## Command

This node is able to wrap any shell command and call it as an Emacs
subprocess.

The optional input serves as an enabling input, when it is not
connected, the node runs the specified command only once.  When it
is connected to an output of another node, the command is evoked
when the input changes (unless the previous call has not finished
yet).

The first configuration argument specifies the host where the
command should be run at.  The second defines the command to be
evoked and all subsequent configuration arguments are passed to the
command as arguments.

The only output of the node displays the results returned by the
command.

When `epoxide-command-verbose` customization variable (in group
`epoxide-command`) is non-nil the default process sentinel gets
associated with the process evoking the specified command thus
Emacs adds a message to the result of the command execution stating
the the process has finished running.  This message is removed by
assigning a dummy sentinel (this is the default setting) to the
Emacs subprocess.

## Decision-summary

Node is able to collect information from Decision nodes' status
outputs and display them in a table format.  The node takes an
indefinite number of inputs that should be the status outputs of
Decision nodes.  Its configuration arguments are optional.  It takes
as many configuration arguments as many inputs the node has, these
should be nil or non-nil values.  When configuration arguments are
present, inputs are checked against these.  The node does not has
any outputs.

Results are displayed using the following columns: decision node
name, result, timestamp, reason.  The first row is always the
evaluation of the current TSG scenario that is the decision nodes'
statuses connected to the summary node.  When no configuration
argument is present, this test passes only when all inputs have
passed as well.  If configuration arguments are present, an input
passes the test iif its value (nil if failed, non-nil if passed)
matches the one specified in the respective configuration
argument.  Other rows in the table list each inputs and their
results.

## Decision

Node provides options for selecting among different inputs.  An
unspecified number of inputs can be connected to the node, and
after performing evaluation on these inputs a decision is
communicated on one of its outputs.  Output #0 serves as a positive
output (i.e. there was at least one input that satisfied initial
criteria) and output #1 is a negative output that displays a
timestamp whenever there were no inputs satisfying initial
criteria.  The number of the node configuration arguments depend on
the number of inputs:

* First argument decides whether decision should be made only when
data is available on every inputs.  If some inputs lag behind or
need more time to produce data this option can be used to wait for
them.  nil value of this argument specifies not to wait, non-nil
values make the node wait for every inputs.

* Second argument specifies a timeout.  When using it together with
a non-nil value of the first argument, a decision is always made at
the timeout even if not every input is present.  In case of the
first argument having a nil value and a timeout is set, when no
input has any data at the timeout, a negative output is given.
Giving nil as the timeout argument switches the function off
whereas giving it a value greater than 0 turns it on.  The Decision
node itself does not have a timer, so in case a timeout is to be
used a Clock node should be connected to the Decision node.  It is
advised that the Clock node is connected to the last input of the
Decision node (this way node configuration arguments should be
given corresponding to this input). Connecting more than one Clock
nodes to the Decision node results in the Decision node selecting
the first Clock node for the timeout calculation.  The second
config argument of the decision node should be set on the tick
count when we would like to have the timeout to occur (since the
Clock node only supplies tick counts). E.g. if we want to have a
timeout of 5 seconds, Clock(1) (that is a Clock giving out a tick
in every second) can be specified in conjunction with a Decision
node having its second argument set to 5 (or a Clock(0.1) and a
timeout value of 50 would have almost the same effect).

* Third argument is used for selecting the proper input to be
dispatched to the output.  The elisp `and` and `or` or any user
defined function can also be used here.  E.g. using the `or` macro
when there is at least one input satisfying criteria, the one
having the lowest index is going to be displayed on the positive
output.  In case no input satisfies its corresponding criteria, the
Decision node's negative output will be triggered.

* Following arguments should be defined on a per input basis.  Let's
assume we are dealing with the 1st input:

  * Argument 5 defines whether input 1 should be processed
line-by-line.  When giving a nil value, everything read in the
current iteration is processed in one bulk, whereas giving a
non-nil value results in splitting the read data to lines and
making an individual decision on each line.

  * Argument 6 specifies whether the result displayed on the
Decision node's positive output should be the same as on the input
(a non-nil value activates this option).  Passing nil to this argument
would mean that the result displayed on the positive output should
be taken from the output of the input's decision function.  Using
this case, processing can be made on the data within the Decision
node.

  * Argument 7 is the decision function assigned to input 1. A
decision function can be any elisp function that takes at least one
argument, and its output is either nil or non-nil.  The decision
function serves as the way to examine whether input 1 satisfies
some criteria or not.

  * Argument 8 defines on which argument of the decision function
should the data of input 1 be passed.

  * Argument 9 defines the number of arguments of the decision
function additional to the data of input 1.

  * Arguments 10+ should be the additional arguments of the decision
function.  When specifying the arguments, they should be listed in
the order in which the elisp function requires them but the
argument requiring the data from input 1 should be left out.  Here
other inputs can be passed to the decision function using the form
'input-x where x denotes the index of the input.

## Doubledecker

This node is able to connect to a DoubleDecker broker, receive and
send messages to a partner or to a topic.

The node has no inputs, when it is instantiated it creates a
subprocess that continuously updates its only output buffer.  The
node has one mandatory and four optional configuration
arguments.  The mandatory first argument specifies the host where
the DoubleDecker client should run at.  The second argument
specifies the topic where the node should subscribe in the form of
<topic>/<scope> (see the reference DoubleDecker client for
available options of scope). In this implementation when more than
one topic is listed, they should appear as a semi-colon (;)
separated list.  The following arguments have default values, that
can be seen and set in the `epoxide` customization group's
`doubledecker` subgroup.  The third argument is defines the location
of the key file, the fourth the tenant and the fifth specifies the
name that the node should have in a DoubleDecker connection.  When
these arguments have non-nil values, those overrule the default
ones.

At startup the node calls a special DoubleDecker client that is
able to display and receive messages in JSON format.  The location
of this python file needs to be given to the node by setting the
`epoxide-doubledecker` customization group's
`epoxide-doubledecker-jsonclient` variable.

The only output of the node relays every message as received from
the jsonclient.  These messages are usually JSON formatted but in
case of startup they are unformatted strings.

## Dpids-fl

Node collects DPID information from a Floodlight controller.
It has one input that should be an output of a Clock node.
It takes one configuration parameter: the IP address of the host where the
POX controller is running at.
The node has one output that lists the available DPIDs.  DPIDs are passed to
the EPOXIDE framework's aggregation variables even when the output is
disabled.
DPID queries are done by using Floodlight's REST API.

## Dpids-odl

Node collects DPID information from an OpenDaylight controller.
It has one input that should be an output of a Clock node.
It takes one configuration parameter: the IP address of the host where the
controller is running at.
The node has one output that lists the available DPIDs.  DPIDs are also
passed to the EPOXIDE framework's aggregation variables even when the
output is disabled.

## Dpids-pox

Node collects DPID and switch name information from a POX controller.
It has one input that should be an output of a Clock node.
It takes one configuration parameter: the IP address of the host where the
POX controller is running at.
The node has two optional outputs.  Output 0 lists the available DPIDs
while output 1 lists switch names.  DPIDs and switch names are passed to
the EPOXIDE frameworks aggregation variables even when outputs are
disabled.
DPID and switch name queries are done by accessing POX's web service.

## Emacs-buffer

The node is able to connect to any Emacs buffer and relay the
changes occurring to these on its outputs.

The node has no inputs and takes and indefinite number of
configuration arguments and provides the same number of outputs as
the number of its configuration arguments. The configuration
arguments specify which Emacs buffer the node has to look for
changes and the changes in these buffers are copied to the node's
outputs in the same order as they were given by the configuration
arguments (i.e. the first specified buffer (in the configuration
list) is connected the to first output, the second to the second
and so on).

The node walks through each of the buffers listed in its
configuration list and creates the `epoxide-emacs-buffer-outputs`
buffer local variable in them and appends the respective outputs to
the variable. It adds the function
`epoxide-emacs-buffer-copy-buffer-content` as an
`after-change-function` to the buffer in order for the outputs to
get notified when a change occurs.

## Escape

Node is able to query NFFG topology information from Escape.  The
node has one input that should be the output of a Clock node which
supplies enable signal to the node.  It has three configuration
arguments.  The first specifies the host where Escape runs at.  The
second specifies the port where the REST API is accessible.  For
ease of use not port but the REST API name should be provided here.
By customizing the `epoxide-escape-apis` variable assignments can
be added or changed in the `epoxide-escape` customization group.
The third configuration argument specifies the currently used data
format.  This can be either JSON or XML.  Both are supported by
Escape and only its configuration decides which format it will use
when returning the answer for the REST API call.  The node has two
outputs where it transfers the topology information.  The output
format is such that it can be processed by a Graph node (see the
documentation of the graph node for the format). The first output
conveys the basic node and connection informations while the second
forwards detailed node information.  Information from Escape is
retrieved by way of REST API calls.

## Filter

This node is able to mark inputs matching the regular expression
defined by its sole configuration argument and convey them to its
output.

## Flow-space-filter

Node provides support to select only that flow space that is wished to be
seen.
It has one input that should contain the data to be filtered.
The node takes one configuration parameter that specifies the flow space.
Flow spaces can be specified be using the format:
<source parameter name 1>=<source parameter value 1>;<source parameter
name 2>=<source parameter value 2>-><destination parameter name 1>=
<destination parameter value 1>
Parameter names are those that appear in the data to be filtered.
Node has one output buffer that contains the data from the input buffer
that satisfies the filter condition.

## Flow-stat-fl

This node provides functionality to query a Floodlight controller for flow
statistics of a specific switch given with its DPID.
The node has one input that should be an output buffer of a Clock node.
It takes three configuration parameters.  #0 specifies the IP address of the
host where the controller is running at, #1 is the DPID of interest and
optional #3 is the requested stat type.  This last argument defaults to
'flow'.  Other possible values are: port, aggregate, desc, features and
queue.
The node has one output that lists queried flow statistics.
The node connects to the specified Floodlight controller via its REST API.
Received data is then processed by giving standard names to the parameters.

## Flow-stat-odl

This node provides functionality to query an OpenDaylight controller for
flow statistics of a specific switch given with its DPID.
The node has one input that should be an output buffer of a Clock node.
It takes four configuration parameters.  #0 specifies the IP address of the
where the controller is running at, #1 is the DPID of interest and #3 is the
username and #4 is the password that enables a connection to be set up with
the controller.
The node has one output that lists queried flow statistics.
The node connects to the specified OpenDaylight controller via its REST API.
Received data is then processed by giving standard names to the parameters.

## Flow-stat-ofctl

The node queries flow statistics for a specific switch name by way of using
`ovs-ofctl dump-flows` shell command.  After data is received it goes through
some modifications that replace proprietary parameter naming to EPOXIDE
standard.
It has one input that should be the output of a Clock node.
It takes two configuration parameters: #0 is the connection inforation, #1 is
the name of the switch to query.  Connection information could be 127.0.0.1
when asking for a local query and <username>@<IP address> or whatever alias
there is in your SSH configuration file for remote connections.
The node has one output that lists the received flow statistics.

## Flow-stat-pox

This node provides functionality to query a POX controller for flow
statistics of a specific switch given with its DPID.
The node has one input that should be an output buffer of a Clock node.
It takes two configuration parameters.  #0 specifies the IP address of the
where the controller is running at and #1 is the DPID of interest.
The node has one output that lists queried flow statistics.
The node connects to the specified POX controller via its web service.
Received data is then processed by giving standard names to the parameters.

## Format

The node is able to format strings coming in on its inputs.  The
node works like the `format` elisp function but it only accepts
string control symbols (since everything arriving on the inputs is
string).

The node accepts indefinite number of inputs that is processes
according to its configuration.  The node has an indefinite number
of configuration arguments, the first of which should be the format
string the rest should define the order of the inputs to pass to
the format function (e.g. listing 0, 2, 1 would pass inputs #0, #2
and #1 to the format function in this order). If no special input
order is given, the standard input order is used (i.e. input #0 is
passed as the first argument, input #1 as second and so on). If
less input is connected then required by the format string then the
unspecified inputs are initialized to empty strings.  If applying the
format function signals an error, it is written into the messages
buffer.  The node's only output displays the formatted text.

## Function

The node is able to call elisp functions or lambda expressions.

The node has one input that serves as an enabling input, when it
changes the function is called.  The node has an indefinite number
of configuration arguments the first of which should be the name of
an elisp function or an elisp lambda expression.  The rest of the
config arguments are passed to the function when it is called.  By
default all configuration arguments are processed as string, in
case of a different type is required it has to be directly
specified by appending the type name and a colon (:) before the
argument.  In case the argument is dynamic, the type inscription is
still taken into account.  Possible types are: number, symbol and
sequence.  The configuration argument can also specify an input,
and how to process it.  Specifying the input can be done by using
the 'input-<x> formula, where <x> is the 0 based number of the node
input.  The processing method can be added to the end of the
configuration argument with a colon.  The default method is bulk
processing (where everything is used that changed from the last
reading cycle). The other method takes only the last line of the
input.  In this latter case the anything (except "bulk") can be
written after the colon to enable the processing method.  The node
has one output where it displays the result of the function call
converted to string.  If the function call fails, an error message
is written to the messages buffer.

## Gdb

The node is able to attach GDB to a running process.  The node does
not have any inputs or outputs and it has three configuration
parameters.  #0 is the host where the process to be debugged is
running on.  This should be specified by writing '/ssh:<user
name>@<IP address>:/'.  #1 is a process name filter: it should
specify part of the process' name.  Optional #2 specifies how to
start the debugger: when it is present, GDB is started as gud-gdb
and this way more then one programs can be debugged otherwise gdb
is called and then only one program can be debugged at a time.  In
the latter case the gdb-many-window configuration can be used, in
the former case only the simple GDB CLI is operational.  When more
than process matches the filter string, all of them are listed and
the user can choose from these options.  When there is only one
possiblity GDB is attached to it without further ado.

## Graph

This node provides graph visualization support.  The node has two
inputs.  On the first input it receives the graph description.  This
description should contain two lines: the first specifying the
nodes, the second the edges of the graph.  Node specification
should follow this form: 'nodes:
node-1[type-1](attribute-1=value-1;
attribute-2=subattribute-2-1=value2-1,subattribute-2-2=value-2-1...),
node-2[type-1]...'.  Edge definition should follow this formula:
'edges: start-node-1 <-> end-node-1 (attribute-1=value-1),
start-node-2 <-> end-node-2...'.  Attributes can be any other
parameter that is needed to be displayed beside the node's name or
the edge association, or they can be left out entirely.  When
defining an edge the following operators can be used: <->, -> and
-.  The optional second input should convey node details: data that
might be interesting but is too long to be displayed with the graph
visualization.  It should use node descriptions like with the first
input.  The graph can be displayed in two ways:

* Text mode: nodes and edges and their attributes are listed in
text.  Here attributes for nodes and edges can also be displayed by
setting the `epoxide-graph-show-attributes-in-text-view` epoxide
group custom variable to t.

* visualization: using COGRE and Graphviz a graph is drawn.  Here
only node attributes can be displayed by setting the
`epoxide-graph-show-attributes-in-graph-view` epoxide group
custom variable to t.

Switching between the two modes can be achieved by using C-c
C-c.  The default behavior can be customized via the
`epoxide-graph-default-view` epoxide group custom variable.

Node names appear as buttons.  Selecting these activate a
semantic-popup-menu that is able to display node details received
on the second input.

The node takes no configuration parameters and has no outputs.

## Host

A node to perform DNS lookup using the calling the 'host' command.
The node takes one input that is used as an enable signal.  When
new data is present on the input a new DNS lookup is executed.  The
node has two configuration parameters.  The first is the host where
the query should be executed, the second the name or IP address
that should be looked up.  The node has one output that conveys the
result of the query.

When `epoxide-host-run-as-process` is non-nil (default setting), in
case when the query takes a long time to complete it will not block
other actions performed in Emacs. Otherwise the node calls the
'host' command as a (possibly blocking) shell call.

## Ifconfig

Node wraps the ifconfig shell command.  It has an enable input: when
there is any data on it, a new ifconfig call is executed.  The node
has indefinit number of configuration arguments.  The first
specifies the host where the call should be executed, when the
second is non-nil the results are checked and the rest of the
arguments denote the interfaces that should be left out of the
check.  The node has two outputs: the first holds the results of the
ifconfig call while the second displays those interfaces that do
not satisfy the check conditions.
When checking the interfaces IP addresses and packet counts are
checked.

## Iperf

This node wraps around the iperf command.

## Iptables

This node is a wrapper for the `iptables` shell command.

It has one input that serves as an enabling input.  Its first
configuration argument is mandatory that specifies the host where
the command should be run at.  When the second configuration argument
is present it should be a `;` separated list of those IP
addresses.  These are checked against the ACLs and those ACLs that
cover the specified IP addresses are kept in the output the rest
will not be displayed.  When this argument is non-nil, the
`iptables` command is always called with the `-L -n` options.  The
remaining configuration arguments are extra arguments to be passed
to the `iptables` command.  The node has one output where it
displays the (filtered) results of the `iptables` call.

## Json-filter

The node is able to search through a JSON formatted expression and
find all values associated with a given key.

The node has one input that supplies the JSON expressions to be
processed.  It has one configuration argument that specifies the key
to be looked up and one output where the result is displayed.  Only
exact matches with the specified key result in successful
matches.  Results are relayed to the output as JSON formatted
objects without the keys.  When multiple occurences of the same key
is found, a JSON array is created.

## Json-list-filter

The node is able to search through a JSON formatted expression and
find lists that contain a specific key-value pair.

The node has one input that supplies the JSON expressions to be
processed.  It has two configuration arguments and one output where
the result is displayed.  The first configuration argument
specifies a key the second a value, the node looks up those lists
that have a dictionary of {<config argument 1>:<config argument 2>}
as an item.  Results are relayed to the output as JSON formatted
objects.  When multiple occurrences of the same key-value pair is
found, a JSON array is created.

## Ping

This node is a wrapper for the ping command.

## Rest-api

The node is able to make a REST API call and display its results on
its output.

The node has one input that serves as an enabling input: each time
this input is updated, the API call is performed.  The node has six
configuration arguments: the first and second define respectively
the host and port where the REST API accessible at; the third
specifies the API call to be made (that appears as part of the
URL); the optional fourth argument is a message.  If the fourth
configuration argument is present an HTTP POST message is sent
relaying the message with the POST request.  If this argument is
not present an HTTP GET message is sent.  The fifth argument
specifies the Content-Type extra header.  If the sixth argument is
non-nil, an HTTP PUT message is sent.  The node has one output
argument where it displays the result of the query as JSON
formatted string.

## Route

This node is a wrapper for the `route` shell command.

It has one input that serves as an enabling input.  The first of its
configuration arguments is the host where the shell command should
be run.  All other arguments are optional.  The second argument is a
(`;` separated) list of IP addresses.  When they are present only
those routes are listed that cover these IP addresses (in this case
the `route` shell command is always called with the `-n`
option). All further configuration arguments are considered as
extra arguments that are passed to the `route` command when calling
it.  The node has one output that lists the appropriate routes.

## Table-view

A node for displaying data received on its inputs in a table form.  Table
header is automatically determined from the data.  It provides options to
reorganize columns and enable/disable them.
The node takes undetermined number of inputs that should be outputs of other
nodes.  Data is expected to have <key>=<value> format.  Header fields
come from keys.
Node takes an indefinite number of configuration arguments.  These will
become the initial header fields.  Every name written here will be dispalyed
initially.  Later modification of these aruments will not have any effect
on the current table instance.  For modifying the actual header the
`epoxide-table-view-header` variable should be edited.
The node does not use any of its outputs.
Data is displayed in the node buffer, table header is displayed in the
window header.

## Topology-fl

Node is used for querying topology information from a Floodlight
controller.  On its first and only input it takes an enable
signal.  Each time this input receives new data a new query is
performed.  The node takes one configuration argument that should be
the IP address of the host where the controller is running at.  The
node has one output where is displays the description of the graph
it read from Floodlight.  The format of the graph description is in
accordance with what a Graph node is to receive.

Queries are preformed by accessing the Floodlight controllers REST
API on port 8080. Hosts, switches and links are queried each time
the node receives an enable signal using the respective Floodlight
REST API calls.  These data are then reformatted and written on the
node output.

## Topology-pox

Node is used for querying topology information from a POX
controller.  On its first and only input it takes an enable
signal.  Each time this input receives new data a new query is
performed.  The node takes one configuration argument that should be
the IP address of the host where the controller is running at.  The
node has one output where is displays the description of the graph
it read from POX.  The format of the graph description is in
accordance with what a Graph node is to receive.

Query is preformed in the following way.  A process is started that
listens on port 8282 for topology information from POX.  (POX should
be started with modules web.webcore, openflow.webservice,
openflow.discovery, epoxide_topo, epoxide_host_tracker in order to
get proper topology information.  epoxide_host_tracker and
epoxide_topo are more stable versions of the original POX
modules.  The topo module opens sends topology information to port
8282 as a stream.) When new information is received the nodes inner
model is refreshed.  Each time the node receives an enable signal
all topology information contained currently in the node's model is
relayed to its output.

## Traceroute

This node is a wrapper for the `traceroute` process.  It has one
optional input, an indefinite number of configuration arguments,
one mandatory and one optional output.

The optional input serves as an enabling input, when it is not
connected the node runs the `traceroute` process only once.  When it
is connected to an output if receives an enabling signal it runs
the `traceroute` process once and waits for another enabling signal
to run it again.  Enabling signals received during the run of the
process are ignored.

The first configuration argument specifies the host where the
`traceroute` process should be run at.  The second defines the
target IP address or name for the `traceroute`.  All subsequent
arguments are passed to the `traceroute` process as extra
arguments.

The first output of the node is always present, it relays the
information received from the `traceroute` process.  The second
output is only present when it is connected to an input.  It
displays information about the evaluation of the current iteration
of the `traceroute` process.  The output is updated only when the
`traceroute` process is finished.  It can display two types of
messages:

* Success: when the process finishes without error.  When information
about the last hop is available that is displayed also.

* Failure: when the `traceroute` runs into and error or it reveals a
routing failure.  When information about the last hop is available
that is displayed also.  When a routing failure has occurred
(e.g. host/network/protocol unreachable) the resolved failure
message is also displayed for the last hop.
