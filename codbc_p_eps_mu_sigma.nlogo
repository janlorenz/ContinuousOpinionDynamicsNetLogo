turtles-own [opinion eps opinion-list initial-opinion mixed-color focus?]
;; eps is the bound of confidence, opinion-list is to hold the list of the last max-pxcor opinions
globals [ alpha beta colored_turtles]

;; BUTTON PROCEDURES

to setup
  clear-all
  ask patches [set pcolor white]
  create-turtles N
  reset-ticks
  update-globals
  ask turtles [
    set initial-opinion new-opinion
    set opinion initial-opinion 
    set opinion-list (list opinion)
    setxy 0 (opinion * max-pycor) 
    set mixed-color one-of base-colors
    set focus? false
    ]
  ask one-of turtles [set focus? true] 
  new_confidence_bounds
  update-plots
end

to go
  if (count turtles = 0) [setup]
  update-globals
  repeat skip_ticks_draw [
    repeat iterations_per_tick [repeat count turtles [ask one-of turtles [ update-opinion ]]]
    ask turtles [ set opinion-list lput opinion opinion-list ] ;; update the opinion-list
    ask turtles [ if (length opinion-list = max-pxcor + 1) [ set opinion-list butfirst opinion-list ] ] ;; cut oldest values for "rolling" opinion list
    update-globals  
    tick
  ] 
  draw-trajectories ;; see the procedure
end

to new_confidence_bounds
  if (count turtles = 0) [setup]
  update-globals
  ask turtles [
    ifelse (sigma > 0) [ set eps random-beta alpha beta ]
                    [ set eps mu ]
    set color colorcode eps 0.5 ;; see reporter colorcode
    ] 
  update-plots
end

;; INTERNAL PROCEDURES

to update-opinion
  update-globals
  ifelse (random-float 1 < independent_probability) [
    if (independent_opinion_is = "New random initial") [set opinion new-opinion]
    if (independent_opinion_is = "Old initial") [set opinion initial-opinion] 
  ][
    let opinion-other [opinion] of one-of turtles
    if (abs (opinion - opinion-other) < eps) [ set opinion (opinion + opinion-other) / 2 ]
  ]
end

to draw-trajectories
  ;; let turtles move with their opinion trajectories from left to right across the world drawing trajectories or coloring patches
  clear-drawing
  if (visualization = "Agents' trajectories") [
    ask turtles [set color colorcode eps 0.5]
    if (agent_pencolors = "mixed") [ask turtles [set color mixed-color]]
  ]
  ask turtles [
    pen-up
    setxy 0 (item 0 opinion-list) * max-pycor
  ]
  let t-counter 1
  while [ t-counter < (length ( [opinion-list] of turtle 1 )) ] [ 
    ifelse (visualization = "Heatmap timeline") 
      [ ask turtles [ pen-up ] ] 
      [ ask turtles with [item t-counter opinion-list = (item t-counter opinion-list)] [ pen-down ] ]
    ask turtles [setxy t-counter ( (item t-counter opinion-list) * max-pycor)]
    ifelse (visualization = "Heatmap timeline") 
      [ ask patches with [pxcor = t-counter ] [ set pcolor colorcode ((count turtles-here with [item t-counter opinion-list =  (item t-counter opinion-list)] ) / (count turtles)) 0.2 ] ] ;; see reporter colorcode
      [ ask patches [ set pcolor white ] ]
    set t-counter t-counter + 1 
  ]
  if (focus_one) [
    ask turtles with [focus?] [
      pen-up
      setxy 0 (item 0 opinion-list) * max-pycor
      pen-down
      set pen-size 3
      set color white     
      set t-counter 1
      while [ t-counter < (length ( [opinion-list] of turtle 1 )) ] [ 
        setxy t-counter ( (item t-counter opinion-list) * max-pycor)
        set t-counter t-counter + 1 
      ]
      pen-up
      setxy 0 (item 0 opinion-list) * max-pycor
      pen-down
      set pen-size 1
      set color colorcode eps 0.5
      set t-counter 1
      while [ t-counter < (length ( [opinion-list] of turtle 1 )) ] [ 
        setxy t-counter ( (item t-counter opinion-list) * max-pycor)
        set t-counter t-counter + 1 
      ]
    ]
  ]
end

to update-globals
  if (sigma > 0) [
    let nu mu * (1 - mu) / sigma ^ 2 - 1
    set alpha max list 0.01 mu * nu
    set beta max list 0.01 (1 - mu) * nu
    set sigma precision (min list sigma (sqrt (mu * (1 - mu) * 0.95))) 5
  ]
end

;; REPORTERS

to-report new-opinion
  update-globals
  report random-float 1
end

to-report random-beta [a b]
   let x random-gamma a 1
   report ( x / ( x + random-gamma b 1) )
end

to-report colorcode [x max_x]
  ;; report a color as "x=0 --> violet", "x=max_x --> red" on the color axis violet,blue,cyan,green,yellow,orange,red 
  report hsb (190 - 190 * (x / max_x)) 255 255
end


;; 2015 by Jan Lorenz
;; See Info tab for license
@#$#@#$#@
GRAPHICS-WINDOW
370
121
1061
409
-1
-1
3.39
1
10
1
1
1
0
0
0
1
0
200
0
75
1
1
1
ticks
30.0

BUTTON
367
35
428
68
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
430
35
491
68
Run!
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
15
276
266
309
N
N
5
1000
540
1
1
NIL
HORIZONTAL

PLOT
377
450
537
570
Histogram eps
eps
NIL
0.0
1.0
0.0
30.0
false
false
"" "set-plot-y-range 0 round(count turtles / 8)"
PENS
"default" 0.02 1 -16777216 true "" "histogram [eps] of turtles"

TEXTBOX
17
320
342
365
2. Mean and dispersion of confidence bounds (eps)\nBeta distribution with parameters:
12
0.0
1

CHOOSER
367
70
517
115
visualization
visualization
"Heatmap timeline" "Agents' trajectories"
0

PLOT
542
450
707
570
Histogram Opinion
Current Opinion
NIL
0.0
1.0
0.0
1.0
false
false
"" "set-plot-y-range 0 round(count turtles / 8)\nset-plot-x-range 0 1"
PENS
"default" 0.0909 1 -13345367 true "" "ifelse (Histogram-style = \"11 bins\")\n  [set-histogram-num-bars 11]\n  [set-plot-pen-interval 0.04]\nhistogram [opinion] of turtles"

PLOT
712
449
1055
569
Opinion
NIL
NIL
0.0
10.0
0.0
1.0
false
true
"" "ifelse rolling [\n  ifelse ticks > (max-pxcor)\n    [set-plot-x-range (ticks - max-pxcor) ticks]\n    [set-plot-x-range 0 max-pxcor] \n  ] [\n  ifelse ticks > (max-pxcor)\n    [set-plot-x-range 0 ticks]\n    [set-plot-x-range 0 max-pxcor] \n  ]"
PENS
"median" 1.0 0 -2674135 true "" "plot median [opinion] of turtles"
"mean" 1.0 0 -16777216 true "" "plot mean [opinion] of turtles"

TEXTBOX
132
596
161
614
violet
9
115.0
1

TEXTBOX
134
586
159
604
blue
9
105.0
1

TEXTBOX
132
574
161
592
cyan
9
85.0
1

TEXTBOX
130
564
159
582
green
9
55.0
1

TEXTBOX
129
554
162
572
yellow
9
45.0
1

TEXTBOX
127
544
163
562
orange
9
25.0
1

TEXTBOX
133
534
150
552
red
9
15.0
1

TEXTBOX
76
522
126
540
heatmap\n
9
0.0
1

TEXTBOX
74
536
120
554
> 0.2 * N 
9
0.0
1

TEXTBOX
77
596
124
614
0 agents
9
0.0
1

TEXTBOX
43
559
123
583
Fraction of agents in patch
9
0.0
1

TEXTBOX
172
534
229
553
eps >= 0.5
9
0.0
1

TEXTBOX
172
596
210
615
eps=0
9
0.0
1

TEXTBOX
152
521
245
539
eps in trajectories
9
0.0
1

TEXTBOX
93
509
212
539
Info: Color axis
12
0.0
1

TEXTBOX
172
564
220
582
eps=0.25
9
0.0
1

SLIDER
18
414
235
447
independent_probability
independent_probability
0
0.5
0.01
0.002
1
NIL
HORIZONTAL

TEXTBOX
17
396
315
414
3. Independent opinion formation
12
0.0
1

SLIDER
783
70
929
103
iterations_per_tick
iterations_per_tick
1
50
7
1
1
NIL
HORIZONTAL

SLIDER
930
70
1061
103
skip_ticks_draw
skip_ticks_draw
1
20
1
1
1
NIL
HORIZONTAL

CHOOSER
18
451
213
496
Independent_opinion_is
Independent_opinion_is
"New random initial" "Old initial"
0

SLIDER
14
354
178
387
mu
mu
0.01
0.99
0.09
0.01
1
NIL
HORIZONTAL

SLIDER
182
354
344
387
sigma
sigma
0
0.499
0
0.001
1
NIL
HORIZONTAL

CHOOSER
549
574
691
619
Histogram-style
Histogram-style
"many bins" "11 bins"
1

TEXTBOX
18
259
168
277
1. Number of agents
12
0.0
1

SWITCH
965
573
1055
606
rolling
rolling
1
1
-1000

TEXTBOX
370
10
965
33
B. Time evoultion of opinion landscapes / Trajectories of opinions
18
0.0
1

TEXTBOX
361
420
1072
463
C. Monitors of confidence bound, opinions and aggregate opinions
18
0.0
1

TEXTBOX
11
10
381
93
Continuous opinion dynamics with\ndyadic interaction,\nheterogeneous bounds of confidence,\nand independent opinion formation\n
18
0.0
1

TEXTBOX
12
102
355
222
Author: Jan Lorenz 2015\n\nTo do:\nA. Setup N agents (1.) with 1-dimensional continuous opinions between 0 and 1, fix the mean and the dispersion of their bounds of confidence (2.). and\nfix the probability of independent opinion formation (3.).\nB. Let them interact and observe the evolution \nC. Observe aggregate outcomes \nD. Use Example buttons for interesting configurations
9
0.0
1

CHOOSER
634
70
781
115
agent_pencolors
agent_pencolors
"confidence bound" "mixed"
0

TEXTBOX
10
234
160
256
A. Parameters
18
0.0
1

TEXTBOX
796
106
909
124
Show longer trajectory
9
0.0
1

SWITCH
520
70
632
103
focus_one
focus_one
1
1
-1000

TEXTBOX
937
105
1042
123
reduce update time
9
0.0
1

@#$#@#$#@
# Continuous Opinion Dynamics under Bounded Confidence v2

## WHAT IS IT?

A model of **continuous opinion dynamics under bounded confidence**, which includes 

  * different initial distributions of opinions based on Beta and Normal distributions
  * heterogeneous bounds of confidence coming from a four parameter [beta distribution](http://en.wikipedia.org/wiki/Beta_distribution), which can reproduce similar cases of heterogeneous bounds of confidence as in [Lorenz 2010](http://dx.doi.org/10.1002/cplx.20295)
  * its two main variants of communication regimes [Deffuant et al 2000](http://dx.doi.org/10.1142/S0219525900000078), [Hegselmann and Krause 2002](http://jasss.soc.surrey.ac.uk/5/3/2.html)) and the model including both of [Urbig et al 2008](http://jasss.soc.surrey.ac.uk/11/2/4.html)
  * independent opinion formation as a new draw from the intial distribution with a small probability as introduced in [Pineda et al 2009](http://dx.doi.org/10.1088/1742-5468/2009/08/P08001))
  * additive noise to the current opinion
  * alternative aggregation of opinions by the median instead of the mean
  * one-sided and two-sided extremism (similar to [Deffuant et al 2002](http://jasss.soc.surrey.ac.uk/5/4/1.html))

Visualizations:

  * a rolling colored histogram of opinion density over time
  * rolling trajectories of opinions over time colored by the bound of confidence 
  * a bar plot histogram of current opinions 
  * trajectories of the mean and the median opinion over time.  


## HOW IT WORKS

### In a nutshell

Agents adjust their opinion gradually towards the opinions of others when the distance in opinion is within their bound of confidence. Sometimes agents change their opinion to a new one at random. When agents hold extremal opinions they might get extremists which never adjust opinions. 

### Variables

Each of N agent has its **opinion** between 0.0 and 1.0 as a dynamic variable and its **bound of confidence** (eps) as a static variable. Other static variables are global and regard the type of communication and aggregation, the probability of random reset of opinion and the type and size of extremism zones. 

### Setup

Each agent is assigned its initial opinion as a random number between 0.0 and 1.0 from the uniform distribution. Each agent is assigned its bound of confidence as a random number form a [beta-distribution](http://en.wikipedia.org/wiki/Beta_distribution) between min_eps and max_eps. 

### Dynamics

Each tick agents are asked to adjust their opinions with respect to the opinions of others and their bound of confidence (eps).

**1. Communication and aggregation:**

  * "DW (select one)": Each tick each agent is asked to select a random partner and changes its opinion to the average of the two opinions but only when the opinion of the partner is closer than eps to the own opinion.
When original=On, N randomly selected agents are chosen each tick (so some agents possibly more than once) and both agents (the selected one and the randomly selected partner) both adjust opinions. (This is the version of Deffuant et al 2000, differences to original=Off are mostly minor.)
  * "HK (select all)": Each tick each agent is asked to change its opinion to the aggregated opinion of all opinions which are closer than eps to its opinion. The aggregate opinion can be the mean or the median. When original=On is checked all agents do the change simultaneously. (This is the version of Hegselmann and Krause 2002, differences are minor.)  With original=Off an agent's change of opinion can immediately takes place and the new opinion might already effect the adjustments of the next agent even in the same tick. 

**2. Probability of random opinion reset**

After the update of opinion each agent is asked to select a new opinion (in the same way as its initialization) but only with probability given by the variable entry_exit_rate. 

**3. Heterogeneous bounds of confidence**

Notice that under heterogeneous bounds of confidence it might be that one agent has confidence in another but not vice versa! 

**4. Extremists**

Extremism zones are intervals in the opinion space from 0.0 to 1.0 which are determined by the parameter extremism_range. The extremism intervals are from 0.0 to extremism_range for the selection "one side" and additionally from 1.0-extremism_range to 1.0 for the selection "two side". Agents which are located in the extermism zone never change their opinion. But still other non-extremist agents might adjust their opinion with respect to the opinions of extremists. Extremists still undergo change of opinion due to random reset of opinions. 

## HOW TO USE IT

Click "setup" to inititialize agents with opinions random and uniformly distributed between 0.0 and 1.0. Agents are located at the left border of the world with their opinions spreading over the vertical axis. Further, on confidence bounds are initialized for each agent as random draws from a beta distribution under the current choice of the four parameters. 

Click "go" to start the simulation. Agents move with ticks from left to right, displaying their opinion with the position on the vertical axis. This goes over into a "rolling" graphic in the world, where the last 120 ticks are shown (respectively the last max-pxcor ticks). Visualization can be chosen as trajectories of agents or as color-coded histograms. In colored histograms each patch's color is associated to the number of agents at this patch. 

A change of the variables communication regime, original, aggregation_in_HK, entry_exit_rate, extremism_range, and extremism_type is immediately effective in a running simulation. 

A change of the variable number_of_agents immediately changes the color axis in the "Colored histogram over time"-visualization and the vertical axis in the histogram of current opinions.

A change of the four parameters of the distribution of eps (min_eps, max_esp, alpha, beta) immediately effect only the plot of the pdf of the distribution, but not the eps in the running simulation (click "new_confidence_bounds" for this).  

Click "new_confidence_bounds" to make new random draws for eps for each agent. Draws come from the beta distribution with the current parameters. (The same procedure is called when clicking "setup".)


## THINGS TO NOTICE

Agents move towards the right-hand side of the world with one step each tick. This goes over into a "rolling" plot. 

Notice how agents form **clusters** in the opinion space. See how these clusters **evolve**, **drift** and **unite** in the "**Colored histograms over time**"-visualization. 

Look at the role of agents with different bounds of confidence in the "**Agents' trajectories**"-visualization. 

Look at the current distribution of opinions in the **bar plot histogram** on the right hand side and compare it to the colored histogram (the most recent colored vertical line in the world at the right hand side).

Look how the **mean and the median opinion** evolve over time. The mean represents the center of mass of the distribution (cf. the current histogram). The median represents an unbeatable opinion under pairwise majority decisions. (This holds when agents have single-peaked preferences with peaks at their opinion, cf. [median voter theorem](http://en.wikipedia.org/wiki/Median_voter_theorem)).

Look how the histogram of bounds of confidence matches the probability density function of the beta distribution when you click "new_confidence_bounds". 


## THINGS TO TRY

Try the **original models of homogeneous DW and HK communication without noise**: Set entry_exit_rate to zero, min_eps and max_eps to the same value, and extremism_range to zero. See how a stable configuration of clusters evolves. Check how the **number**, **sizes** and **locations** of clusters depend on the bound of confidence. Try to determine critical bounds of confidence where the evolving cluster pattern changes significantly. Use the setup button to rerun with a new initial configuration. 

Try to understand the impact of **noise** (entry_exit_rate) under homogeneous bounds of confidence: Set up a homogeneous eps (min_eps=max_eps), run the simulation and play with the entry_exit_rate slider (reasonable values are low, e.g. between 0.01 and 0.1). There are bounds of confidence, where the cluster pattern oscillates between two numbers of clusters. Under different entry_exit_rates the two patterns become differently likely. 

Play with **hetergeneous bounds of confidence** (min_eps<max_eps) without noise (entry_exit_rate=0). Try different shapes of the distribution by manipulation of alpha and beta. Try to understand the **drifting** phenomena under the two communication regimes. Try to reproduce the phenomenon of "**more chance for consensus under lower but heterogeneous bounds of confidence**" (see [Lorenz 2010](http://dx.doi.org/10.1002/cplx.20295)): Set N=500, DW communication, original=off, entry_exit_rate=extremism_range=0, alpha=beta=0.1, min_eps=0.24, max_eps=0.24. See by several runs (repeatedly click "setup") that consensus is not (or very rarely) possible. Now set min_eps=0.1 and see that the bimodal distribution (half of the population with eps=0.1 and half with eps=0.24) makes it possible that almost all agents unite in a single cluster. Beware, this does not happen all the time and might takes a lot of time (rerun and wait!). The phenomenon is better understood in the "Agents' trajectories"-visualization. 

Play with **extremism** under homogeneous bounds of confidence and no noise (entry_exit_rate=0). Observe the pattern of convergence under different bounds of confidence. Under what bounds of confidence does global extremism (at one or both extremes) occur? Use small extremism ranges (between 0.01 and 0.05) and play with one or two-sided extremism. Compare to [Deffuant et al 2002](http://jasss.soc.surrey.ac.uk/5/4/1.html).

Try to find differences between the original versions of the DW and HK models and the one implemented when original=off. 

Try to calibrate parameters which represent an evolution of the political landscape of opinions which looks to you as a realistic evolution of a party system (regarding location and size of opinion clusters in the left--right continuum). 


## RELATED MODELS AND PAPERS

**Original HK and DW models**
Hegselmann, R. & Krause, U. [Opinion Dynamics and Bounded Confidence, Models, Analysis and Simulation](http://jasss.soc.surrey.ac.uk/5/3/2.html) Journal of Artificial Societies and Social Simulation, 2002, 5, 2
Deffuant, G.; Neau, D.; Amblard, F. & Weisbuch, G. [Mixing Beliefs among Interacting Agents](http://dx.doi.org/10.1142/S0219525900000078) Advances in Complex Systems, 2000, 3, 87-98
Weisbuch, G.; Deffuant, G.; Amblard, F. & Nadal, J.-P. [Meet, discuss, and segregate!](http://dx.doi.org/10.1002/cplx.10031) Complexity, 2002, 7, 55-63

**General model including HK and DW**
Urbig, D.; Lorenz, J. & Herzberg, H. [Opinion dynamics: The effect of the number of peers met at once](http://jasss.soc.surrey.ac.uk/11/2/4.html) Journal of Artificial Societies and Social Simulation, 2008, 11, 4

**On noise:** 
Pineda, M.; Toral, R. & Hernandez-Garcia, E. [Noisy continuous-opinion dynamics](http://stacks.iop.org/1742-5468/2009/P08001) Journal of Statistical Mechanics: Theory and Experiment, 2009, 2009, P08001 (18pp)
Mäs, M.; Flache, A. & Helbing, D. [Individualization as Driving Force of Clustering Phenomena in Humans](http://dx.doi.org/10.1371/journal.pcbi.1000959) PLoS Comput Biol, Public Library of Science, 2010, 6, e1000959

**On heterogeneous bounds of confidence**
Lorenz, J. [Heterogeneous bounds of confidence: Meet, Discuss and Find Consensus!](http://dx.doi.org/10.1002/cplx.20295) Complexity, 2010, 15, 43-52

**On extremism**
Deffuant, G.; Neau, D.; Amblard, F. & Weisbuch, G. [How Can Extremism Prevail? A Study Based on the Relative Agreement Interaction Model](http://jasss.soc.surrey.ac.uk/5/5/1.html) Journal of Artificial Societies and Social Simulation, 2002, 5, 1
Deffuant, G. [Comparing Extremism Propagation Patterns in Continuous Opinion Models](http://jasss.soc.surrey.ac.uk/9/3/8.html) Journal of Artificial Societies and Social Simulation, 2006, 9, 8

**Survey, Motivation and Variation**
Lorenz, J. [Continuous Opinion Dynamics under bounded confidence: A Survey](http://dx.doi.org/10.1142/S0129183107011789) Int. Journal of Modern Physics C, 2007, 18, 1819-1838
Urbig, D. [Attitude Dynamics with Limited Verbalisation Capabilities](http://www.jasss.surrey.ac.uk/6/1/2.html) Journal of Artificial Societies and Social Simulation, 2003, 6, 2
Lorenz, J. & Urbig, D. [About the Power to Enforce and Prevent Consensus by Manipulating Communication Rules](http://dx.doi.org/10.1142/S0219525907000982) Advances in Complex Systems, 2007, 10, 251
Amblard, F. & Deffuant, G. [The role of network topology on extremism propagation with the relative agreement opinion dynamics](http://dx.doi.org/10.1016/j.physa.2004.06.102) Physica A: Statistical Mechanics and its Applications, 2004, 343, 725-738 
Groeber, P.; Schweitzer, F. & Press, K. [How Groups Can Foster Consensus: The Case of Local Cultures](http://jasss.soc.surrey.ac.uk/12/2/4.html) Journal of Artificial Societies and Social Simulation, 2009, 12, 4




## CREDITS AND REFERENCES
This is the extended version of "Continuous Opinion Dynamics under Bounded Confidence" from 2012

Copyright 2015 Jan Lorenz. http://janlo.de, post@janlo.de

![Creative Commons Attribution-ShareAlike 3.0 Unported License](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)
 
This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ .
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
