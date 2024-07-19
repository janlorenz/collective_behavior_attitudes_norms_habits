extensions [csv]
globals [stat_fraction-behavior-list]
turtles-own [behavior? compliance-attitude pastbehavior ; core variables
             stat_visible-others stat_frac-behavior-others ; variablse for statistics
]

to setup
  clear-all
  setup-network
  ask turtles [set compliance-attitude random-normal 0 1] ; In the following the effective attitude is: attitude-mean + attitude-sd * compliance-attitude
  initialize-run
end

to initialize-run
  ask turtles [
    set pastbehavior n-values memory-capacity [0.5]
    set behavior? ifelse-value (attitude-mean + attitude-sd * compliance-attitude + random-normal 0 1 > 0) [1] [0]
    set stat_frac-behavior-others 0.5
  ]
  set stat_fraction-behavior-list [0.5 0.5]
  visualize
  clear-all-plots
  reset-ticks
end

to setup-network
  create-turtles N [setxy random-xcor random-ycor]
  let r ifelse-value (frac-random-contacts < 1)
    [r-from-N-avg-degree N (avg-contacts * (1 - frac-random-contacts))]
    [0]
  ask turtles [
    create-links-with other turtles with [distance myself < r * (2 * max-pxcor + 1)] [set hidden? not show-links]
  ] ;; We use (max-pxcor + 1) because this is the length of the square of the world
    ;; That way r * (max-pxcor + 1) matches distances of r if the world was a unit square as in
    ;; the standard random geometric graph
end

to go
  if (timeline = "day-night") [turtles-go-home]
  turtles-come-and-decide
  set stat_fraction-behavior-list lput (fraction-behavior turtles) stat_fraction-behavior-list
  if length stat_fraction-behavior-list > 50 [set stat_fraction-behavior-list butfirst stat_fraction-behavior-list]
  visualize
  tick
end

to turtles-go-home
  ask turtles [
    set hidden? true
  ]
end

to turtles-come-and-decide
  ask turtles [
    let others relevant-others
    set stat_frac-behavior-others fraction-behavior others
    set stat_visible-others count relevant-others ; mainly for statisitcal purpose
    set behavior? ifelse-value (utility others stat_frac-behavior-others > 0) [1] [0]
    set hidden? false
    set pastbehavior fput behavior? pastbehavior
    if length pastbehavior > memory-capacity [set pastbehavior butlast pastbehavior]
  ]
end

to visualize
  if (visualization) [
  ask turtles [
    set shape ifelse-value (behavior? = 1) ["square"] ["x"]
    set size ifelse-value (behavior? = 1) [0.5] [0.7]
    set color (ifelse-value
      (agent-color = "attitude") [scale-color-diverging (attitude-mean + attitude-sd * compliance-attitude) color-max]
      (agent-color = "behavior? R/G") [ifelse-value (behavior? = 1) [green + 1] [red]]
      [ifelse-value (behavior? = 1) [blue + 2] [blue - 1]])
    set hidden? not show-agents
  ]
  ask patches [
    set pcolor (ifelse-value
      (patch-color = "average-attitude") [scale-color-diverging mean [attitude-mean + attitude-sd * compliance-attitude] of (turtle-set turtles-here turtles-on neighbors) color-max]
      (patch-color = "white") [white]
      [black])
  ]
  ask links [set hidden? not show-links]
  ]
end

to set-baseline-other-parameters
  set timeline "day-night"
  set memory-capacity 5
  set unobserved-utility true
end

to scenario [name]
  set N 2000
  set avg-contacts 15
  set frac-random-contacts 0.2
  set timeline "day-night"
  set memory-capacity 5
  set beta-conformity 0
  set beta-pastbehavior 0
  set attitude-sd 0
  set attitude-mean 0

  if (name = "baseline") [set beta-conformity 2 set beta-pastbehavior 1 set attitude-sd 0.5 set attitude-mean 0.05]
  ; Baseline: Small bias 0.1 in favor of compliance.
  if (name = "0") [set attitude-mean 0.1] ;  Implies expected 0.54 fraction of behavior.
  if (name = "0 div") [set attitude-mean 0.1 set attitude-sd 0.5] ; Implies expected 0.536 fraction of behavior.
  ; Section: Impact of past behavior
  if (name = "A-1") [set attitude-mean 0.1 set beta-pastbehavior 1.5] ; boosts!
  if (name = "A-2") [set attitude-mean 0.1 set beta-pastbehavior 3] ; still boosts (even more) but takes much longer!
  if (name = "B") [set attitude-mean 0.1 set attitude-sd 0.5 set beta-pastbehavior 1.5] ; boosts!
  ; Section: Impact of conformity
  if (name = "C") [set beta-conformity 1] ; still boosts (even more) but takes much longer!
  if (name = "E") [set attitude-mean 0.1 set beta-conformity 1.5] ; boosts, strong difference in timeline "day-night"
  if (name = "F") [set attitude-mean 0.1 set beta-conformity 3] ; boosts, strong difference in timeline "day-night"
  setup
end

; REPORTERS

to-report utility [others fractionbehaviorothers]
  report beta-conformity * 2 * (fractionbehaviorothers - 0.5) + ; utility of conformity of mask-wearing
         beta-pastbehavior * 2 * (fraction-behavior-past - 0.5) + ; utility of consistency with pastbehavior of mask wearing
         attitude-mean + attitude-sd * compliance-attitude +    ; utility of consistency with attitude
  ifelse-value (unobserved-utility) [random-normal 0 1] [0] ; unobserved utility
end

to-report fraction-behavior [turtleset]
  report ifelse-value (any? turtleset)
    [mean [behavior?] of turtleset]
    [0.5]
end

to-report fraction-behavior-past
  report ifelse-value (empty? pastbehavior) [0.5] [mean pastbehavior]
end

to-report relevant-others
;  report turtles with [hidden? = false and distance myself < view]
  let mean-random-contacts (avg-contacts * frac-random-contacts)
  let max-random-contacts round (2 * mean-random-contacts)
  let random-others ifelse-value (max-random-contacts > 0)
    [n-of (random-binom max-random-contacts (mean-random-contacts / max-random-contacts)) other turtles]
    [nobody]
  report (turtle-set random-others link-neighbors) with [hidden? = false]
end

to-report random-binom [num success-prob] report sum n-values num [ifelse-value (random-float 1 < success-prob) [1] [0]] end

to-report expected-fraction-behavior
  ; under the assumption of only attitude bias and beta-attitude being non-zero
  ; The computation uses the sample of compliance attitudes which are from standard normal distribution
  report count (turtles with [sqrt(1 + attitude-sd ^ 2) * compliance-attitude + attitude-mean > 0 ]) / N
end


to-report scale-color-diverging [number max-range]
  report ifelse-value (number < 0) [scale-color red number (- max-range) 0 ] [scale-color green number max-range 0 ]
end

; For random geometric network
to-report r-from-N-avg-degree [NN avdeg]
  ; This implements the Newton method for finding the root of f
  ; which represent how to compute the radius r from N and the desired average degree
  let x sqrt (avdeg / (pi * NN))
  let xn x - (f x avdeg) / (df x)
  while [xn - x > 10 ^ -10] [
    set x xn
    set xn x - (f x avdeg) / (df x)
  ]
  report xn
end

to-report f [x avdeg]
  ; https://mathworld.wolfram.com/SquareLinePicking.html
  ; provides an equation for the cumulative distribution function of the
  ; length of a line between two random points l:
  ; Prob(l < r) = 1/2 * l^4 - 8/3 * l^3 + pi * l^2
  ; Form that we can deduce an equation for the average degree given N and radius r as
  ; avg-degree = (N - 1) * Prob(l < r)
  ; This function can be used to iterative compute r from avg-degree and N
  report x ^ 4 - 16 / 3 * x ^ 3 + 2 * pi * x ^ 2 - 2 * avdeg / (N - 1)
end

to-report df [x]
  ; The derivative of f to be used for the Newton method in r-from-N-avg-degree
  report 4 * x ^ 3 - 16 * x ^ 2 + 4 * pi * x
end

;;; For Publication Example Figure and Data Output

to make-pub-example-runs
  make-figs-data-exports-one-run "example1_run-" false -1712090258 -1019159744
  make-figs-data-exports-one-run "example1_rerun-" true 804910256 "no need"
end

to make-figs-data-exports-one-run [filename is-rerun? seed-setup seed-go]
  set-baseline-other-parameters
  set N 2000
  set avg-contacts 15
  set frac-random-contacts 0.2
  set timeline "day-night"
  set memory-capacity 5
  set beta-conformity 6
  set beta-pastbehavior 3
  set attitude-sd 1
  set attitude-mean 0.1
  set visualization true
  set show-agents true
  set show-links true
  set agent-color "attitude"
  set patch-color "black"
  set color-max 4
  ifelse is-rerun?  [
    random-seed seed-setup
    initialize-run
  ] [
    random-seed seed-setup setup
    ;random-seed  -1712090258 setup
    export-view (word filename "0-network.png")
    set show-agents false set show-links false set patch-color "average-attitude" set agent-color "behavior? Blue" visualize
    export-view (word filename "0-patches-att-avg.png")
    random-seed seed-go
  ]
  set show-agents true set show-links false set patch-color "average-attitude" set agent-color "behavior? R/G"
  visualize export-view-csv (word filename ticks)
  repeat 10 [go visualize export-view-csv (word filename ticks)] ; first ten steps every tick a png
  repeat 10 [go] visualize export-view-csv (word filename ticks) ; now one png after ten ticks
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks)
  repeat 10 [go] visualize export-view-csv (word filename ticks) ; results in a total of 19 png
end

to write-csv [filename]
  csv:to-file filename
  sentence (list
    ["Parameters"]
    ["attitude-mean" "beta-attitude" "beta-pastbehavior" "beta-conformity" "frac-random-contacts" "avg-contacts" "timeline" "N" "memory-capacity" "ticks"]
    (list attitude-mean attitude-sd beta-pastbehavior beta-conformity frac-random-contacts avg-contacts timeline N memory-capacity ticks)
    ["Behavior and Dynamic Utility Function Terms"]
    ["compliant" "u_contacts" "u_past" "u_attitude"])
    [(list behavior? (2 * stat_frac-behavior-others - 1) (2 * fraction-behavior-past - 1) compliance-attitude)] of turtles
end

to export-view-csv [filename]
  export-view (word filename ".png")
  write-csv (word filename ".csv")
end




;;; Not Needed
to-report logit [p] report ln (p / (1 - p)) end
to-report random-gumbel report (- ln (- ln random-float 1)) end
to-report random-logistic [mu sigma] report mu + sigma * logit random-float 1 end
@#$#@#$#@
GRAPHICS-WINDOW
400
10
843
454
-1
-1
13.2
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

SLIDER
10
35
200
68
N
N
1
2000
2000.0
1
1
NIL
HORIZONTAL

BUTTON
10
225
65
258
NIL
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
70
225
125
258
NIL
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
10
395
200
428
beta-conformity
beta-conformity
0
10
6.0
0.1
1
NIL
HORIZONTAL

SLIDER
10
430
201
463
beta-pastbehavior
beta-pastbehavior
0
10
3.0
0.1
1
NIL
HORIZONTAL

PLOT
605
465
840
585
fraction behavior population
time
frac
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot fraction-behavior turtles"

PLOT
400
500
605
635
fraction behavior past
fraction
freq
0.0
1.05
0.0
1.0
true
false
"" ""
PENS
"default" 0.05 1 -16777216 true "" "set-histogram-num-bars memory-capacity + 1\nhistogram [fraction-behavior-past] of turtles"

SLIDER
855
85
1020
118
memory-capacity
memory-capacity
1
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
10
285
200
318
attitude-sd
attitude-sd
0
3
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
607
590
716
635
fraction behavior
fraction-behavior turtles
3
1
11

SLIDER
10
320
200
353
attitude-mean
attitude-mean
-0.5
2
0.1
0.01
1
NIL
HORIZONTAL

SWITCH
10
570
150
603
show-links
show-links
1
1
-1000

MONITOR
220
180
305
225
mean neigh.
mean [count link-neighbors] of turtles
2
1
11

PLOT
220
230
380
350
attitudes
NIL
NIL
-4.5
4.5
0.0
10.0
true
false
"" ""
PENS
"default" 0.25 1 -16777216 true "" "histogram [attitude-mean + attitude-sd * compliance-attitude] of turtles"
"pen-1" 1.0 0 -2674135 false "" "plotxy 0 0 plotxy 0 plot-y-max"

MONITOR
300
350
380
395
attitude sd
standard-deviation [attitude-sd * compliance-attitude] of turtles
3
1
11

MONITOR
220
350
302
395
attit. mean
mean [attitude-mean + attitude-sd * compliance-attitude] of turtles
3
1
11

SLIDER
10
100
200
133
avg-contacts
avg-contacts
0
50
15.0
1
1
NIL
HORIZONTAL

PLOT
220
60
380
180
num neighbors
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-plot-x-range 0 (1 + max [count link-neighbors] of turtles)\nhistogram [count link-neighbors] of turtles"

SLIDER
10
135
200
168
frac-random-contacts
frac-random-contacts
0
1
0.2
0.01
1
NIL
HORIZONTAL

TEXTBOX
15
80
165
98
Contacts parameters
12
0.0
1

MONITOR
10
170
200
215
avg fixed / random contacts
(word \nprecision (avg-contacts * (1 - frac-random-contacts)) 3\n\" / \" \nprecision (avg-contacts * frac-random-contacts) 3 )
3
1
11

CHOOSER
855
35
1020
80
timeline
timeline
"day-night" "updates-in-place"
0

TEXTBOX
225
10
375
55
Static agent properties
18
105.0
1

TEXTBOX
860
10
1020
35
Other parameters
18
105.0
1

TEXTBOX
15
475
290
500
Visualization parameters
18
105.0
1

TEXTBOX
1205
10
1480
40
Scenarios - Work-in-Progress
18
0.0
1

BUTTON
1200
110
1255
143
A-1
scenario \"A-1\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
220
395
380
440
Expec. fraction behavior
expected-fraction-behavior
3
1
11

TEXTBOX
227
443
377
469
without conformity and pastbehavior
10
0.0
1

MONITOR
710
590
765
635
boost
fraction-behavior turtles - expected-fraction-behavior
3
1
11

TEXTBOX
770
590
840
651
Difference to expected without norm and habit
10
0.0
1

BUTTON
1320
110
1385
143
B
scenario \"B\"
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
1260
110
1315
143
A-2
scenario \"A-2\"
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
1210
205
1265
238
D
scenario \"D\"
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
1310
195
1372
228
E
scenario \"E\"
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
1385
195
1447
228
F
scenario \"F\"
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
1200
75
1315
108
base
scenario \"0\"
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
1320
75
1385
108
base div
scenario \"0 div\"
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
1390
110
1560
146
small bias with or without beta-attitude for attitude diversity
9
0.0
1

TEXTBOX
1205
150
1355
168
Conformity
12
0.0
1

BUTTON
1210
170
1265
203
C
scenario \"C\"
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
855
440
1015
473
Write Individuals CSV
let filename user-new-file \nwrite-csv filename
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
860
480
1015
580
Write a csv with\nLine 2-3 : Table with all current parameters \nLine 5-end: Table with all individuals and the last\nbehavior and dynamic utility function terms
9
0.0
1

MONITOR
305
180
380
225
sd neigh.
standard-deviation [count link-neighbors] of turtles
2
1
11

CHOOSER
155
500
295
545
agent-color
agent-color
"behavior? R/G" "behavior? Blue" "attitude"
0

BUTTON
10
605
150
639
visualize
visualize\nupdate-plots
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
155
600
295
633
color-max
color-max
0
4
4.0
0.1
1
NIL
HORIZONTAL

BUTTON
130
225
200
258
restart
initialize-run
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
855
120
1020
153
unobserved-utility
unobserved-utility
0
1
-1000

CHOOSER
155
550
295
595
patch-color
patch-color
"white" "black" "average-attitude"
2

SWITCH
10
535
150
568
show-agents
show-agents
0
1
-1000

SWITCH
10
500
150
533
visualization
visualization
0
1
-1000

TEXTBOX
15
265
190
283
Attitude parameters
12
0.0
1

TEXTBOX
15
360
195
395
Importance conformity and importance past behavior
12
0.0
1

TEXTBOX
15
10
165
31
Main Parameters
18
105.0
1

TEXTBOX
405
470
560
495
Output Measures
18
105.0
1

BUTTON
855
155
1020
188
set baseline values
set-baseline-other-parameters
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
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-larger" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <steppedValueSet variable="attitude-mean" first="0" step="0.05" last="0.1"/>
    <steppedValueSet variable="attitude-sd" first="0" step="0.5" last="1"/>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-overview" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <steppedValueSet variable="attitude-mean" first="0" step="0.05" last="0.1"/>
    <steppedValueSet variable="attitude-sd" first="0" step="0.5" last="1"/>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="3"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
      <value value="&quot;updates-in-place&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-larger-fixed-contacts" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <steppedValueSet variable="attitude-mean" first="0" step="0.05" last="0.1"/>
    <steppedValueSet variable="attitude-sd" first="0" step="0.5" last="1"/>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-larger-detail" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3" step="0.2" last="6"/>
    <steppedValueSet variable="beta-conformity" first="3" step="0.2" last="6"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="5"/>
      <value value="15"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill1" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3" step="0.5" last="6"/>
    <steppedValueSet variable="beta-conformity" first="3.5" step="1" last="5.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill4" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill2" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3.5" step="1" last="5.5"/>
    <steppedValueSet variable="beta-conformity" first="3" step="1" last="6"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill3" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="4" step="1" last="6"/>
    <steppedValueSet variable="beta-conformity" first="3" step="1" last="6"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill5" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="2.5"/>
    <steppedValueSet variable="beta-conformity" first="3.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill6" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="6.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill7" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="2.5"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill8" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="2.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill9" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill10" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="2.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill11" repetitions="60" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="6.5" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="3" step="0.5" last="6"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill12" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="2.5"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill13" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill14" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-conformity" first="3.5" step="1" last="5.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill15" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="4.5"/>
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill16" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="4"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-conformity" first="3.5" step="0.5" last="5.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill17" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3.5"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill18" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="5"/>
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="3.5"/>
      <value value="4.5"/>
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill19" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="2.5"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill20" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="6" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill21" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="6" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="3.5" step="0.5" last="5.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill22" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3.5" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill23_aborted_but_used" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="3" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill24" repetitions="26" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill25" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="7.5"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill26" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3.5" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill27" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="2.5"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="2.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-surrounding_fill28" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill1" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="6.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill2" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="2.5"/>
    <enumeratedValueSet variable="beta-conformity">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill3" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="beta-conformity">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill4" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="5.5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="3"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill6" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3.5" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill7" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="3.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill8" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="beta-conformity">
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-norandom_fill9" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="3.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-updates-in-place" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;updates-in-place&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-updates-in-place-fill1" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="3.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;updates-in-place&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-updates-in-place-fill2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="3.5" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="3"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;updates-in-place&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-baseline-50contacts" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-behavior turtles</metric>
    <enumeratedValueSet variable="attitude-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attitude-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-pastbehavior" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="beta-conformity" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="unobserved-utility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
