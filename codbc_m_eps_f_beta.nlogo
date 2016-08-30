turtles-own [opinion opinion-list initial-opinion mixed-color focus? accept?]
;; eps is the bound of confidence, opinion-list is to hold the list of the last max-pxcor opinions
globals [colored_turtles f]

;; BUTTON PROCEDURES

to setup
  clear-all
  ask patches [set pcolor white]
  create-turtles N
  reset-ticks
  ask turtles [
    if (initial_opinions = "random uniform") [set initial-opinion new-opinion]
    if (initial_opinions = "consensus at 0.5") [set initial-opinion 0.5]
    set opinion initial-opinion 
    set opinion-list (list opinion)
    setxy 0 (opinion * max-pycor) 
    set mixed-color one-of base-colors
    set focus? false
    ]
  ask one-of turtles [set focus? true] 
  update-plots
end

to go
  if (count turtles = 0) [setup]
  repeat skip_ticks_draw [
    repeat iterations_per_tick [
      ;; if (communication_regime = "Deffuant et al") [repeat (count turtles) / 2 [ask one-of turtles [ update-opinion ]]] ;; repeat N/2 times because for each interaction there are two updates
      if (communication_regime = "Deffuant et al") [repeat (count turtles) [ask one-of turtles [ update-opinion ]]] ;; repeat N/2 times because for each interaction there are two updates
      if (communication_regime = "Hegselmann-Krause") [ask turtles [ update-opinion ]]
      ]
    ask turtles [ set opinion-list lput opinion opinion-list ] ;; update the opinion-list
    ask turtles [ if (length opinion-list = max-pxcor + 1) [ set opinion-list butfirst opinion-list ] ] ;; cut oldest values for "rolling" opinion list  
    tick
  ] 
  draw-trajectories ;; see the procedure
end

;; TODO Opinion-List mix up with HK
;; Why does it work for Old initial but not for new random??????


;; INTERNAL PROCEDURES

to update-opinion
  ifelse (random-float 1 < m) 
   [set opinion new-opinion]
   [
    if (communication_regime = "Deffuant et al") [
      let partner one-of turtles
      if (random-float 1 < probability_acceptance abs ([opinion] of partner - opinion) eps tune_f beta) [
        set opinion (opinion + [opinion] of partner) / 2
        ;;ask partner [set opinion (opinion + [opinion] of myself) / 2]
      ]]
    if (communication_regime = "Hegselmann-Krause") [
        set accept? true
        ask other turtles [
          set accept? (probability_acceptance abs(last opinion-list - [last opinion-list] of myself) eps tune_f beta) > random-float 1
          ]
        set opinion mean [last opinion-list] of turtles with [ accept? ] 
      ]
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

to change_focus
  ask turtles [set focus? false]
  ask one-of turtles [set focus? true]
end

;; REPORTERS

to-report new-opinion
  report random-float 1
end

to-report probability_acceptance [discrepancy local_eps local_tune_f local_beta]
  ifelse (local_tune_f < 1) [
    let local_f (1 / (1 - local_tune_f ^ 2) - 1)
    ifelse (discrepancy <= eps) [
      ifelse (tune_f > 0) 
        [report max list local_beta ( (1 - discrepancy / local_eps) ^ (1 / local_f) )]
        [report beta]
    ]
    [report beta]
  ]
  [ ifelse (discrepancy <= eps) [report 1] [report beta] ]
end

to-report random-beta [a b]
   let x random-gamma a 1
   report ( x / ( x + random-gamma b 1) )
end

to-report colorcode [x max_x]
  ;; report a color as "x=0 --> violet", "x=max_x --> red" on the color axis violet,blue,cyan,green,yellow,orange,red 
  report hsb (190 - 190 * (x / max_x)) 255 255
end
@#$#@#$#@
GRAPHICS-WINDOW
383
121
1074
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
380
35
441
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
443
35
504
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
17
331
186
364
N
N
5
1000
500
1
1
NIL
HORIZONTAL

TEXTBOX
19
373
344
418
2. Confidence bound (eps), facilitation (f), default influence probability (beta)
12
0.0
1

CHOOSER
380
70
527
115
visualization
visualization
"Heatmap timeline" "Agents' trajectories"
0

PLOT
555
450
720
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
725
449
1068
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
1239
707
1268
725
violet
9
115.0
1

TEXTBOX
1241
697
1266
715
blue
9
105.0
1

TEXTBOX
1239
685
1268
703
cyan
9
85.0
1

TEXTBOX
1237
675
1266
693
green
9
55.0
1

TEXTBOX
1236
665
1269
683
yellow
9
45.0
1

TEXTBOX
1234
655
1270
673
orange
9
25.0
1

TEXTBOX
1240
645
1257
663
red
9
15.0
1

TEXTBOX
1182
633
1232
651
heatmap\n
9
0.0
1

TEXTBOX
1125
645
1233
663
> color_axis_max * N 
9
0.0
1

TEXTBOX
1184
707
1231
725
0 agents
9
0.0
1

TEXTBOX
1156
668
1236
692
Fraction of agents in patch
9
0.0
1

TEXTBOX
1279
645
1336
664
eps >= 0.5
9
0.0
1

TEXTBOX
1279
707
1317
726
eps=0
9
0.0
1

TEXTBOX
1259
632
1352
650
eps in trajectories
9
0.0
1

TEXTBOX
1199
616
1317
634
Info: Color axis
12
0.0
1

TEXTBOX
1279
675
1327
693
eps=0.25
9
0.0
1

SLIDER
18
568
316
601
m
m
0
0.5
0.01
0.002
1
NIL
HORIZONTAL

TEXTBOX
17
535
315
565
3. Probability of independent random opinion replacement
12
0.0
1

SLIDER
790
70
947
103
iterations_per_tick
iterations_per_tick
1
50
1
1
1
NIL
HORIZONTAL

SLIDER
949
70
1074
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

SLIDER
138
409
300
442
eps
eps
0.01
1
1
0.005
1
NIL
HORIZONTAL

SLIDER
138
446
300
479
tune_f
tune_f
0
1
0.998
0.002
1
NIL
HORIZONTAL

CHOOSER
566
574
708
619
Histogram-style
Histogram-style
"many bins" "11 bins"
0

TEXTBOX
21
298
171
316
1. Number of agents
12
0.0
1

SWITCH
978
573
1068
606
rolling
rolling
0
1
-1000

TEXTBOX
383
10
978
33
B. Time evoultion of opinion landscapes / Trajectories of opinions
18
0.0
1

TEXTBOX
374
420
1085
464
C. Monitors of probability of acceptance, opinions and aggregate opinions
18
0.0
1

TEXTBOX
11
10
378
164
Continuous opinion dynamics with probability of acceptance based on \na confidence bounded, \nsmooth facilitation, and \na default influence probability \nincluding independent opinion formation\n\n
18
0.0
1

TEXTBOX
12
148
356
269
\nTo do:\nA. Setup N agents with 1-dimensional continuous opinions between 0 and 1and decide for a communication regime (1.), a bounds of confidence, facilitation, and default interaction probability (2.), and fix the probability of random opinion replacement (3.).\nB. Let them interact and observe the evolution \nC. Observe aggregate outcomes \nD. Use Example buttons for interesting configurations
9
0.0
1

CHOOSER
530
35
672
80
agent_pencolors
agent_pencolors
"confidence bound" "mixed"
1

TEXTBOX
13
273
163
295
A. Parameters
18
0.0
1

TEXTBOX
809
106
922
124
Show longer trajectory
9
0.0
1

SWITCH
674
35
786
68
focus_one
focus_one
1
1
-1000

TEXTBOX
950
105
1074
129
Reduce grapic updates
9
0.0
1

TEXTBOX
1096
10
1246
32
D. Examples
18
0.0
1

BUTTON
1094
89
1182
122
m = 0
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.218\nset beta 0.001\nset m 0\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1095
39
1417
84
1. Hegselmann-Krause communcation, no bound of confidence (eps=1), intermediate facilitation (f=0.05): impact of m
12
0.0
1

BUTTON
1094
124
1182
157
m = 0.1
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.218\nset beta 0.001\nset m 0.1\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1094
159
1182
192
m = 0.5
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.218\nset beta 0.001\nset m 0.5\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1188
98
1410
128
Consensus evolves (but very slow)
12
0.0
1

TEXTBOX
1188
133
1396
151
Two stable clusters evolve
12
0.0
1

TEXTBOX
1188
168
1340
186
No clusters evolve
12
0.0
1

BUTTON
1101
325
1156
358
H.-K.
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.238\nset beta 0\nset m 0.1\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1105
250
1231
293
No bound of confidence and low facilitation
12
0.0
1

TEXTBOX
1223
323
1294
341
2 Clusters
12
0.0
1

TEXTBOX
1224
396
1290
414
3 Clusters
12
0.0
1

BUTTON
1101
390
1156
423
H.-K.
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.196\nset beta 0\nset m 0.1\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1108
371
1203
389
eps=1, f=0.04
12
0.0
1

BUTTON
1160
390
1215
423
D.ea
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f  0.196\nset beta 0\nset m 0.01\nset communication_regime \"Deffuant et al\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 5\nset skip_ticks_draw 1\nsetup
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
1301
370
1405
388
eps=0.13, f=Inf
12
0.0
1

BUTTON
1295
390
1350
423
H.-K.
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 0.13\nset tune_f 1\nset beta 0\nset m 0.1\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1355
390
1410
423
D.ea
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 0.13\nset tune_f  1\nset beta 0\nset m 0.01\nset communication_regime \"Deffuant et al\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 5\nset skip_ticks_draw 1\nsetup
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
1218
15
1402
33
(Don't forget to click \"Run!\")
12
15.0
1

SLIDER
530
83
674
116
color_axis_max
color_axis_max
0.01
0.4
0.11
0.01
1
NIL
HORIZONTAL

BUTTON
676
70
786
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

SLIDER
138
483
300
516
beta
beta
0
0.01
0
0.0005
1
NIL
HORIZONTAL

PLOT
384
450
550
570
acceptance
discrepancy
prob.
0.0
1.0
0.0
1.0
false
false
"" "clear-plot"
PENS
"default" 1.0 0 -16777216 true "" "foreach ( n-values 100 [? / 100] ) [plotxy ? ( probability_acceptance ? eps tune_f beta) ]"

CHOOSER
192
319
359
364
communication_regime
communication_regime
"Deffuant et al" "Hegselmann-Krause"
0

MONITOR
307
443
362
488
f
1 / (1 - tune_f ^ 2) - 1
3
1
11

TEXTBOX
1297
251
1447
296
Small bound of confidence and full facilitation
12
0.0
1

CHOOSER
192
273
359
318
initial_opinions
initial_opinions
"random uniform" "consensus at 0.5"
0

TEXTBOX
17
418
132
436
Confidence bound
12
0.0
1

TEXTBOX
16
451
109
469
Facilitation
12
0.0
1

TEXTBOX
17
484
133
514
Default influence probability
12
0.0
1

TEXTBOX
16
468
124
486
Use tune_f to tune f!
9
0.0
1

TEXTBOX
1099
211
1410
244
2. Similarity under influx of independent opinions\nH.-K.: m=0.1      D.ea: m=0.01       (beta=0)
12
0.0
1

BUTTON
1161
325
1216
358
D.ea
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.238\nset beta 0\nset m 0.01\nset communication_regime \"Deffuant et al\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 5\nset skip_ticks_draw 1\nsetup
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
1296
323
1351
356
H.-K.
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 0.2\nset tune_f 1\nset beta 0\nset m 0.1\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
1356
323
1411
356
D.ea
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 0.2\nset tune_f 1\nset beta 0\nset m 0.01\nset communication_regime \"Deffuant et al\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 5\nset skip_ticks_draw 1\nsetup
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
1110
305
1208
323
eps=1, f=0.06
12
0.0
1

TEXTBOX
1310
304
1403
322
eps=0.2, f=Inf
12
0.0
1

TEXTBOX
1093
441
1426
486
3. Slow (!!!) convergence to consensus for no bound of confidence and low facilitation without influx of new opinions and default influence probability 
12
0.0
1

BUTTON
1101
490
1386
523
D.ea, eps=1, f=0.06, m=0, beta=0
set focus_one false\nset initial_opinions \"random uniform\"\nset N 500\nset eps 1\nset tune_f 0.238\nset beta 0\nset m 0\nset communication_regime \"Deffuant et al\"\nset visualization \"Agents' trajectories\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 5\nset skip_ticks_draw 1\nsetup
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
1091
537
1428
555
4. Emergence of clusters from initial consensus
12
0.0
1

BUTTON
1103
560
1385
593
H.-K., eps=1, f=0.06, m=0.1, beta=0
set focus_one false\nset initial_opinions \"consensus at 0.5\"\nset N 500\nset eps 1\nset tune_f 0.238\nset beta 0\nset m 0.1\nset communication_regime \"Hegselmann-Krause\"\nset visualization \"Heatmap timeline\"\nset agent_pencolors \"mixed\"\nset color_axis_max 0.1\nset iterations_per_tick 1\nset skip_ticks_draw 1\nsetup
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
900
51
985
69
For speed-up
12
0.0
1

@#$#@#$#@
# Continuous Opinion Dynamics under Generalized Bounded Confidence and Independent Opinion Formation

## WHAT IS IT?

This model of **continuous opinion dynamics** accompanies the paper
_"Robust clustering in generalized bounded confidence models"_


## HOW IT WORKS

### In a nutshell

Agents repeatedly update their opinions which are numbers between zero and one. Agents adopt the average opinion of those agents which they accept. They accept other opinions more likely when these are close to their own opinion. With a small probability an agent chooses independent opinion formation which mean that he replaces its opinion with new one drawn at random. 

### Variables

Each of N agent has its **opinion** between 0.0 and 1.0 as a dynamic variable. 

Bounded confidence is generalzed by a **probability of acceptance function**, which is a function which maps opinion discrepancy (meaning the distance between two opinions) to a probability that the opinion is accepted by the agent. The probability of acceptance is one when discrepancy is zero. The function is monotonically decreasing. The probability of acceptance function has 3 parameters which are global variables:

  * The **default influence probability** (beta) is the minimal acceptance probability which is always effective. It is thought to be very small. It is zero in the original bounded confidencde models.
  * The **bound of confidence** (eps) is the maximal discrepancy for which the probability of acceptance can be larger than the default influence probability.
  * The **facilitation** (f) models how slow the probability of acceptance declines when discrepancy rises towards the bound of confidence. Facilitation is maximal in the original bounded confidence models.

For two opinions x_i and x_j  with discrepancy D(i,j) = |x_i - x_j| the probability of acceptence is 
P(i,j) = max( beta , (1 - D(i,j)/eps)^(1/f) ) when D(i,j) <= eps and beta otherwise.

We implement the two **communication regimes** from the original bounded confidence models:

  * Hegselmann-Krause (2002), where all agents update synchronously and each agent selects a set of accepeted opinions (including its own) and moves to the average opinion 
  * Deffuant et al. (2000), where two random agents are selected and interact with a probability of acceptance based on their opinion discrepancy. When they interact they both move to the average of their opinions. (With respect to the original model, we only treat the most used case mu=0.5.)

The process of independent **random opinion replacement** is implemented as introduced by Pineda et al (2009). With probability m an agent does not interact with others but draws a new opinion form the opinion space with a uniform distribution. 
 
### Initialization of agent variable

Each agent is assigned its initial opinion as a random number between 0.0 and 1.0 from a uniform distribution. Alternatively, all initial opinions can be initialized with the consensual opinion of 0.5. 

### Visualization ##

  * The functional form of the probability of acceptance function.
  * A rolling colored histogram (heatmap) of opinion density over time.
  * Rolling trajectories of opinions over time (multicolored or colored by the bound of confidence).
  * It can additionally focus on the trajectory of a randomly chosen agent.
  * A bar plot histogram of current opinions either with many or with 11 bins, the latter is analog to left-right ideological positions in survey questionaires as the European Social Survey.
  * Trajectories of the mean and the median opinion over time.


## HOW TO USE IT

How to make decisions to setup and run a simulation is documented in the interface. 

## THINGS TO NOTICE

Follow the arguments of the paper by playing with the examples in section D. of the interface. 


## REFERENCES

**Original Hegselmann-Krause and Deffuant et al. models**
Hegselmann, R. & Krause, U. [Opinion Dynamics and Bounded Confidence, Models, Analysis and Simulation](http://jasss.soc.surrey.ac.uk/5/3/2.html) Journal of Artificial Societies and Social Simulation, 2002, 5, 2
Deffuant, G.; Neau, D.; Amblard, F. & Weisbuch, G. [Mixing Beliefs among Interacting Agents](http://dx.doi.org/10.1142/S0219525900000078) Advances in Complex Systems, 2000, 3, 87-98
Weisbuch, G.; Deffuant, G.; Amblard, F. & Nadal, J.-P. [Meet, discuss, and segregate!](http://dx.doi.org/10.1002/cplx.10031) Complexity, 2002, 7, 55-63

**On noise:** 
Pineda, M.; Toral, R. & Hernandez-Garcia, E. [Noisy continuous-opinion dynamics](http://stacks.iop.org/1742-5468/2009/P08001) Journal of Statistical Mechanics: Theory and Experiment, 2009, P08001 (18pp)



## LICENSE
 
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
