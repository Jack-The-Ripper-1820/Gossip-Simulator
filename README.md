# Gossip and Push Sum Simulator
DOSP Project 2.

**Group members** -

1) Mayur Reddy Junnuthula (UFID - 36921238)
2) Akhil Srivatsa (UFID - 80826297)

**What is working**

We managed to make every topology work with some implementation changes to solve the anti convergence problem which occurs for some parameters.

**What is the largest network you managed to deal with for each type of topology and algorithm?**

After extensive testing, we inferred that the largest network we were able to reach convergence for is for the Full Network topology implementing the Gossip algorithm, although it also Full Network topology also had the highest memory usage. 
The traditional Memory-Performance tradeoff seems to also be applicable here. The details for each topology is as follows

Gossip Algorithm:
Full - 5000,
Imperfect 3D - 4000,
2D - 4000,
Line - 3000

Push Sum Algorithm: 
Full - 3000,
Imperfect 3D - 2000,
2D - 2000,
Line - 1500


**Project Description**

The following algorithms are implemented in the project.

Gossip Algorithm - https://en.wikipedia.org/wiki/Gossip_protocol

Push Sum Algorithm - https://www.cs.cornell.edu/johannes/papers/2003/focs2003-gossip.pdf

**Steps to Run the Project**
1)  Build the project and run it using the erlang console run configuration in IntelliJ IDEA with just default (zero) arguments and default settings.
2) Start the application with the command main:start() in the erlang console, and follow the input prompts.
3) Enter the number of actors to be spawned by the algorithm.
4) Enter the type of topology (Full Network/Line/2D/Imperfect 3D) in string format.
5) Enter the type of algorithm (Gossip Algorithm/Push Sum Algorithm) in string format.
6) Then select if you want to execute the bonus version of the project by typing "Yes"  or "NO".
7) This will start the execution of the selected algorithm and the user will be notified in the terminal of the time it took to reach convergence for the given parameters.

**Input Format**

{Number of actors (integer), Topology (string), Algorithm (string)}.

Ex - {484, "2D", "Gossip Algorithm"}.

**Note** - 
It is mandatory that the input number of nodes be perfect squares for the 2D and Imperfect 3D topologies.

**Implementation**

In our implementation of the gossip algorithm, a node terminates after hearing a rumor 10 times when
it stops passing the rumor to a random neighbor. The convergence of the gossip implementation is measured when 
all the nodes in the network have terminated. For 2D grid and Imperfect 2D grid topologies, the number of nodes to be given as the input should be the nearest
perfect square.


In our implementation of Push-Sum algorithm, every node is initialized with the values suggested in the 
project handout of s = i and w = 1. Similar to our gossip implementation, the number of nodes is rounded to the nearest perfect square for 2D grid and Imperfect 2D grid topologies. The main process asks a random node to start which then passes a message consisting of a tuple of (s/2, w/2) to a rrandom neighbor while keeping values of s/2 and w/2 as its state. When an actor receives a message tuple, it adds the tuple to its state and keeps half its value while passing on another half to a random node. This process continues until an actor’s s/w ratio does not change more than 1.0e-10 for three consecutive iterations after which the actor terminates i.e. it stops passing
a tuple to a random neighbor and the algorithm converges when the sum estimates i.e. s/w converges to the average of the sum.

**Note** - All the performance metrics are subject to change depending on the system it is running on and it's specifications.
The following readings were calculated and averaged based on readings from two computers, we are confident that the global tendency of the metrics should be the same throughout but there might be local variations between systems. 


**Performance Metrics (Convergence Time vs Number of actors/nodes)**


**Gossip Algorithm**

![img.png](img.png)

**Line Topology**

![](line gossip.png)

**Full Topology**

![](full gossip.png)

**2D Topology**

![](2D gossip.png)

**Imperfect 3D Topology**

![](3D gossip.png)

**Comparison of Gossip Topologies**

![](gossip combined.png)

**Push Sum Algorithm**

![img_1.png](img_1.png)

**Line Topology**

![](line PS.png)

**Full Topology**

![](full PS.png)

**2D Topology**

![](2D PS.png)

**Imperfect 3D Topology**

![](3D PS.png)

**Comparison of Push Sum Algorithm**

![](PS combined.png)










