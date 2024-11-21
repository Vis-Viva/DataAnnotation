# For-DataAnnotation
Public repository created to display a snapshot of code for purposes of (hopefully) demonstrating competence.
The code shown here is a very small piece of a large system I have been working on for several years. It is a set of functions for a class called CFRCollector which traverses a poker game tree to collect data samples to be used as training targets for a neural network whose purpose is to estimate advantages (a quantity analogous to expected value) for player actions. Advantage has to be calculated in a specific way by tracing paths travelled through the game tree during data collection. This calculation phase is done immediately after collection of raw data samples has been done, and before using the calculated advantages as training targets for the neural network. The functions contained herein implement the operations necessary for these calculations and save the results to a file which can then later be read from during the training process. Specifically, what is being implemented is ultimately computation of α( I,a ) in the equations in the attached image, where:
* I is an Infoset, which is a representation of the information about a gamestate that can be observed by a player
* a is an action available to the player at some game position
* h is a game history, i.e. a sequence of actions taken by players and random chance up to some game position
* t is iteration, where one iteration is a full cycle of: collect data → calculate α from collected data → train neural net to estimate α more accurately
* σ is a stochastic player strategy mapping state observation I to a probability distribution over actions a, where σ(I,a) ∝ α(I,a)
* π(h) is a reach probability, i.e. the probability of history h happening, given iteration strategy σᵗ
* v(I,a) quantities, then, basically represent expected values, from which advantage targets are calculated
  
![AdvAndVals](https://github.com/user-attachments/assets/44ec0e5e-9e87-429d-b431-1ff84224728a)
