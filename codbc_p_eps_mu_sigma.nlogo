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


;; INTERNAL PROCEDURES

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

to update-opinion
  update-globals
  ifelse (random-float 1 < p) [
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
      [ ask patches with [pxcor = t-counter ] [ set pcolor colorcode ((count turtles-here with [item t-counter opinion-list =  (item t-counter opinion-list)] ) / (count turtles)) color_axis_max ] ] ;; see reporter colorcode
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

to change_focus
  ask turtles [set focus? false]
  ask one-of turtles [set focus? true]
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
345
309
N
N
5
1000
500
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
514
116
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
177
601
206
619
violet
9
115.0
1

TEXTBOX
179
591
204
609
blue
9
105.0
1

TEXTBOX
177
579
206
597
cyan
9
85.0
1

TEXTBOX
175
569
204
587
green
9
55.0
1

TEXTBOX
174
559
207
577
yellow
9
45.0
1

TEXTBOX
172
549
208
567
orange
9
25.0
1

TEXTBOX
178
539
195
557
red
9
15.0
1

TEXTBOX
120
527
170
545
heatmap\n
9
0.0
1

TEXTBOX
63
541
166
588
> color_axis_max * N 
9
0.0
1

TEXTBOX
122
601
169
619
0 agents
9
0.0
1

TEXTBOX
88
564
168
588
Fraction of agents in patch
9
0.0
1

TEXTBOX
217
539
274
558
eps >= 0.5
9
0.0
1

TEXTBOX
217
601
255
620
eps=0
9
0.0
1

TEXTBOX
197
526
290
544
eps in trajectories
9
0.0
1

TEXTBOX
137
510
255
528
Info: Color axis
12
0.0
1

TEXTBOX
217
569
265
587
eps=0.25
9
0.0
1

SLIDER
16
414
346
447
p
p
0
0.5
0.002
0.002
1
NIL
HORIZONTAL

TEXTBOX
17
396
315
414
3. Probability of independent opinion formation
12
0.0
1

SLIDER
777
70
934
103
iterations_per_tick
iterations_per_tick
1
50
10
1
1
NIL
HORIZONTAL

SLIDER
936
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
16
450
178
495
Independent_opinion_is
Independent_opinion_is
"New random initial" "Old initial"
1

SLIDER
16
352
180
385
mu
mu
0.01
0.99
0.23
0.005
1
NIL
HORIZONTAL

SLIDER
184
352
346
385
sigma
sigma
0
0.499
0.15
0.001
1
NIL
HORIZONTAL

CHOOSER
553
574
695
619
Histogram-style
Histogram-style
"many bins" "11 bins"
0

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
0
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
378
122
Continuous opinion dynamics with\ndyadic interaction,\nheterogeneous bounds of confidence,\nand independent opinion formation\n
18
0.0
1

TEXTBOX
12
103
356
224
Author: Jan Lorenz 2015\n\nTo do:\nA. Setup N agents (1.) with 1-dimensional continuous opinions between 0 and 1, fix the mean and the dispersion of their bounds of confidence (2.). and fix the probability of independent opinion formation (3.).\nB. Let them interact and observe the evolution \nC. Observe aggregate outcomes \nD. Use Example buttons for interesting configurations
9
0.0
1

CHOOSER
517
35
659
80
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
661
35
773
68
focus_one
focus_one
0
1
-1000

TEXTBOX
937
105
1061
129
Reduce grapic updates
9
0.0
1

TEXTBOX
1086
10
1236
32
D. Examples
18
0.0
1

BUTTON
1084
75
1170
108
mu = 0.35
set N 500\nset mu 0.35\nset sigma 0\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1085
39
1353
71
Homogeneous bounds of confidence\nNo independent opinion formation
12
0.0
1

BUTTON
1084
110
1170
143
mu = 0.27
set N 500\nset mu 0.27\nset sigma 0\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1084
145
1170
178
mu = 0.23
set N 500\nset mu 0.23\nset sigma 0\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1084
180
1170
213
mu = 0.19
set N 500\nset mu 0.19\nset sigma 0\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1084
215
1170
248
mu = 0.15
set N 500\nset mu 0.15\nset sigma 0\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1178
84
1386
102
Consensus evolves
12
0.0
1

TEXTBOX
1178
119
1386
137
Critical mu Consensus/2 Clusters
12
0.0
1

TEXTBOX
1178
154
1330
172
2 Clusters evolve
12
0.0
1

TEXTBOX
1178
189
1330
207
Critical mu 2 Clusters/3 Clusters
12
0.0
1

TEXTBOX
1178
224
1330
242
3 Clusters evolve
12
0.0
1

BUTTON
1084
293
1170
326
Indep.1
set N 500\nset mu 0.23\nset sigma 0\nset p 0.01\nset visualization \"Agents' trajectories\"\nset focus_one true\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1085
257
1340
289
Homogeneous bounds of confidence\nwith also independent opinion formation
12
0.0
1

BUTTON
1084
328
1170
361
Indep.2
set N 500\nset mu 0.23\nset sigma 0\nset p 0.05\nset visualization \"Heatmap timeline\"\nset focus_one true\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1084
363
1170
396
Indep.3
set N 500\nset mu 0.23\nset sigma 0\nset p 0.15\nset visualization \"Heatmap timeline\"\nset focus_one true\nset agent_pencolors \"mixed\"\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1084
404
1170
437
Indep.4
set N 500\nset mu 0.285\nset sigma 0\nset p 0.05\nset visualization \"Heatmap timeline\"\nset focus_one false\nset agent_pencolors \"mixed\"\nset iterations_per_tick 5\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1177
302
1385
320
2 Clusters, 1% independent
12
0.0
1

TEXTBOX
1178
337
1386
355
2 Clusters, 5% independent
12
0.0
1

TEXTBOX
1178
372
1369
402
2 Clusters, 15% independent
12
0.0
1

TEXTBOX
1178
413
1379
443
At critical mu, 5% indep. *
12
0.0
1

TEXTBOX
1083
447
1383
481
Heterogeneous bounds of confidence\nNo independent opinion formation
12
0.0
1

BUTTON
1084
481
1170
514
Het.1
set N 500\nset mu 0.23\nset sigma 0.05\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"confidence bound\"\nset iterations_per_tick 10\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1178
490
1522
520
mu=0.23, sigma=0.05 -> Drifts to center or extreme *
12
0.0
1

BUTTON
1084
516
1170
549
Het.2
set N 500\nset mu 0.23\nset sigma 0.1\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"confidence bound\"\nset iterations_per_tick 10\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1178
525
1516
543
mu=0.23, sigma=0.1 -> Drifts to extreme *
12
0.0
1

BUTTON
1084
551
1170
584
Het.3
set N 500\nset mu 0.23\nset sigma 0.15\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"confidence bound\"\nset iterations_per_tick 10\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1178
560
1549
590
mu=0.23, sigma=0.15 -> Extremal drift, direction change *
12
0.0
1

BUTTON
1084
586
1170
619
Het.4
set N 500\nset mu 0.23\nset sigma 0.25\nset p 0\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"confidence bound\"\nset iterations_per_tick 10\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1178
594
1543
613
mu=0.23, sigma=0.25 -> No drift *
12
0.0
1

BUTTON
1080
647
1199
680
Oscillations!
set N 500\nset mu 0.23\nset sigma 0.15\nset p 0.002\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"confidence bound\"\nset iterations_per_tick 10\nset skip_ticks_draw 1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1207
655
1526
685
mu = 0.23, sigma = 0.15, 0.2% independent *
12
0.0
1

TEXTBOX
1082
627
1369
645
All together can trigger regular oscillations:
12
0.0
1

TEXTBOX
1208
15
1392
33
(Don't forget to click \"Run!\")
12
15.0
1

TEXTBOX
1086
693
1488
711
* Note, that these trajectories are longer then the others. See iterations_per_tick.
9
0.0
1

SLIDER
517
83
661
117
color_axis_max
color_axis_max
0.01
0.4
0.08
0.01
1
NIL
HORIZONTAL

BUTTON
663
70
773
104
Change focus
change_focus
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
# Continuous Opinion Dynamics under Bounded Confidence with Dyadic Interaction, Heterogeneous Bounds of Confidence and Independent Opinion Formation

## WHAT IS IT?

This is model of **continuous opinion dynamics under bounded confidence**, which focusses on 

  * heterogeneous bounds of confidence coming from a two parameter [beta distribution](http://en.wikipedia.org/wiki/Beta_distribution) in a mean/dispresion paramterization
  * a dyadic communication regime very similar to [Deffuant et al 2000](http://dx.doi.org/10.1142/S0219525900000078)
  * independent opinion formation as a new draw from the intial distribution with a small probability as introduced in [Pineda et al 2009](http://dx.doi.org/10.1088/1742-5468/2009/08/P08001)), additionally independent opinion formation can also be set to be a switch back to the original initial opinion

It has visualizations for:

  * a bar plot histogram of the different static bounds of confidence (eps)
  * a rolling colored histogram (heatmap) of opinion density over time
  * rolling trajectories of opinions over time colored by the bound of confidence 
  * it can additionally focus on the trajectory of a randomly chosen agent
  * a bar plot histogram of current opinions either with many or with 11 bins, the latter is analog to left-right ideological positions in survey questionaires as the European Social Survey
  * trajectories of the mean and the median opinion over time
 
## HOW IT WORKS

### In a nutshell

Agents are selected randomly to adjust their opinion which are numbers between zero and one. With a certain (typically small) probability a selected agent chooses independent opinion formation which is a random draw. Otherwise agents choose interdependent opinion formation where the agent adjusts its opinion gradually towards the opinions of another randomly selected agent but only when the distance in opinion is within its bound of confidence. 

### Variables

Each of N agent has its **opinion** between 0.0 and 1.0 as a dynamic variable and its **bound of confidence** (eps) as a static variable. The global static variables are 
  * mu - the mean of the beta distribution where confidence bounds are drawn from
  * sigma - the standard deviaition  of the beta distribution where confidence bounds are drawn from
  * p - the probability if an agent is doing independent insteadt of interdependent opinion formation 

### Initialization of agent variables

Each agent is assigned its initial opinion as a random number between 0.0 and 1.0 from the uniform distribution. Each agent is assigned its bound of confidence as a random number form a [beta-distribution](http://en.wikipedia.org/wiki/Beta_distribution) with mean mu and standard deviation sigma. 

## HOW TO USE IT

Click "Setup" to inititialize agents with opinions random and uniformly distributed between 0.0 and 1.0. Agents are located at the left border of the world with their opinions spreading over the vertical axis. Further, on confidence bounds (eps) are initialized for each agent as random draws from a beta distribution under the current choice of mu and sigma. 

Click "Go" to start the simulation. With each tick, agents move from left to right displaying their opinion with the position on the vertical axis. This goes over into a "rolling" graphic in the world, where the last ticks are shown. 

Visualization can be chosen as trajectories of agents or as heatmaps of the density (color-coded histograms). In heatmaps each patch's color is associated to the number of agents at this patch. Further switches, choosers, and sliders can be used to change the visualization. 

A change of N, mu and sigma is only effective at setup. A change of p is effective at runtime. 


## THINGS TO NOTICE

Agents move towards the right-hand side of the world with one step each tick. This goes over into a "rolling" plot. 

When setup see how the distribution of the bounds of confidence (eps) looks like under different mu and sigma in the **bar plot histogram of eps**. See what happens when sigma is small and large. 

Notice how agents form **clusters** in the opinion space. See how under what conditions how many clusters **evolve**, when they **drift**, and **unite** in the "**Heatmap**"-visualization. 

Look at the role of agents with different bounds of confidence in the "**Agents' trajectories**"-visualization. 

In either visualization, focus additionally on the **trajectory of an individual agent** and see how it jumps between clusters when the p is not zero. 

Increase iterations_per_tick to see what happens in the really long run. q

Look at the current distribution of opinions in the **bar plot histogram** on the right hand side and compare it to the colored histogram (the most recent colored vertical line in the world at the right hand side).

Look how the **mean and the median opinion** evolve over time. The mean represents the center of mass of the distribution. The median represents an unbeatable opinion under pairwise majority decisions. (This holds when agents have single-peaked preferences with peaks at their opinion, cf. [median voter theorem](http://en.wikipedia.org/wiki/Median_voter_theorem)).


## THINGS TO TRY

Try the example buttons to explore characteristic behavior and try to systematically understand the role of average open-mindedness (mu), dispresion of open- and closed-mindedness, and the degree of independent opinion formation (p) the 


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
