extensions [csv]
globals [stat_fraction-masked-list]
turtles-own [masked? compliance-attitude pastbehavior ; core variables
             stat_visible-others stat_frac-masked-others ; variablse for statistics
]

to setup
  clear-all
  setup-network
  ask turtles [
    set shape "dot"
    set pastbehavior []
    set compliance-attitude random-normal 0 1 ; random number between -1 and +1
    ; set compliance-attitude (random-float 2) - 1 ; random number between -1 and +1
    set masked? compliance-attitude > 0
  ]
  set stat_fraction-masked-list [0.5 0.5]
  visualize
  reset-ticks
end

to setup-network
  create-turtles N [setxy random-xcor random-ycor]
  let r ifelse-value (frac-random-contacts < 1)
    [r-from-N-avg-degree N (avg-contacts * (1 - frac-random-contacts))]
    [0]
  ask turtles [
    create-links-with other turtles with [distance myself < r * (2 * max-pxcor + 1)] [set hidden? hide-links]
  ] ;; We use (max-pxcor + 1) because this is the length of the square of the world
    ;; That way r * (max-pxcor + 1) matches distances of r if the world was a unit square as in
    ;; the standard random geometric graph
end

to go
  if (timeline = "day-night") [turtles-go-home]
  turtles-come-and-decide
  set stat_fraction-masked-list lput fraction-masked stat_fraction-masked-list
  if length stat_fraction-masked-list > 50 [set stat_fraction-masked-list butfirst stat_fraction-masked-list]
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
    set stat_frac-masked-others fraction-masked-others others
    set stat_visible-others count relevant-others
    set masked? utility others stat_frac-masked-others > 0
    set hidden? false
    set pastbehavior fput masked? pastbehavior
    if length pastbehavior > memory-capacity [set pastbehavior butlast pastbehavior]
  ]
end

to visualize
  ask turtles [ set color ifelse-value masked? [white] [red] ]
  ask links [ set hidden? hide-links ]
end

to scenario [name]
  set N 2000
  set avg-contacts 15
  set frac-random-contacts 0
  set beta-conformity 0
  set beta-pastbehavior 0
  set beta-attitude 0
  set attitude-bias 0
  ; Baseline: Small bias 0.1 in favor of compliance.
  if (name = "0") [set attitude-bias 0.1] ;  Implies expected 0.54 fraction of masked.
  if (name = "0 div") [set attitude-bias 0.1 set beta-attitude 0.5] ; Implies expected 0.536 fraction of masked.
  ; Section: Impact of past behavior
  if (name = "A-1") [set attitude-bias 0.1 set beta-pastbehavior 1.5] ; boosts!
  if (name = "A-2") [set attitude-bias 0.1 set beta-pastbehavior 3] ; still boosts (even more) but takes much longer!
  if (name = "B") [set attitude-bias 0.1 set beta-attitude 0.5 set beta-pastbehavior 1.5] ; boosts!
  ; Section: Impact of conformity
  if (name = "C") [set beta-conformity 1] ; still boosts (even more) but takes much longer!


  if (name = "E") [set attitude-bias 0.1 set beta-conformity 1.5] ; boosts, strong difference in timeline "day-night"
  if (name = "F") [set attitude-bias 0.1 set beta-conformity 3] ; boosts, strong difference in timeline "day-night"
  setup
end

to write-csv

  csv:to-file "turtles.csv" fput ["compliant" "u_contacts" "u_past" "u_attitude"] [(list masked?
    (2 * stat_frac-masked-others - 1) (2 * fraction-masked-past - 1) compliance-attitude)] of turtles
end

; REPORTERS

to-report utility [others fractionmaskedothers]
  report beta-conformity * 2 * (fractionmaskedothers - 0.5) + ; utility of conformity of mask-wearing
         beta-pastbehavior * 2 * (fraction-masked-past - 0.5) + ; utility of consistency with pastbehavior of mask wearing
         attitude-bias + beta-attitude * compliance-attitude +    ; utility of consistency with attitude
         random-normal 0 1                         ; unknown utility
end

to-report fraction-masked-others [others]
  report ifelse-value (any? others)
    [count others with [masked?] / count others ]
    [0.5]
end

to-report fraction-masked-past
  report ifelse-value (empty? pastbehavior)
    [0.5]
    [length filter [i -> i = true] pastbehavior / length pastbehavior ]
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

to-report fraction-masked report count turtles with [masked?] / N end
to-report random-binom [num success-prob] report sum n-values num [ifelse-value (random-float 1 < success-prob) [1] [0]] end

to-report expected-fraction-masked
  ; under the assumption of only attitude bias and beta-attitude being non-zero
  ; The computation uses the sample of compliance attitudes which are from standard normal distribution
  report count (turtles with [sqrt(1 + beta-attitude ^ 2) * compliance-attitude + attitude-bias > 0 ]) / N
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




;;; Not Needed
to-report logit [p] report ln (p / (1 - p)) end
to-report random-gumbel report (- ln (- ln random-float 1)) end
to-report random-logistic [mu sigma] report mu + sigma * logit random-float 1 end
@#$#@#$#@
GRAPHICS-WINDOW
205
10
519
325
-1
-1
9.3
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
10
200
43
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
190
65
224
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
190
125
224
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
5
230
195
263
beta-conformity
beta-conformity
0
10
1.5
0.1
1
NIL
HORIZONTAL

SLIDER
5
265
195
298
beta-pastbehavior
beta-pastbehavior
0
10
1.5
0.1
1
NIL
HORIZONTAL

PLOT
530
10
760
130
fraction masked pop
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
"default" 1.0 0 -16777216 true "" "plot fraction-masked"

PLOT
530
130
760
250
fraction-masked-past
fraction-masked-past
freq
0.0
1.05
0.0
1.0
true
false
"" ""
PENS
"default" 0.05 1 -16777216 true "" "set-histogram-num-bars memory-capacity + 1\nhistogram [fraction-masked-past] of turtles"

SLIDER
5
455
195
488
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
5
305
145
338
beta-attitude
beta-attitude
0
3
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
760
10
865
55
fraction masked
fraction-masked
3
1
11

SLIDER
5
340
145
373
attitude-bias
attitude-bias
-0.5
2
0.05
0.01
1
NIL
HORIZONTAL

SWITCH
920
300
1042
333
hide-links
hide-links
0
1
-1000

MONITOR
370
415
447
460
avg neigh
mean [count link-neighbors] of turtles
2
1
11

PLOT
455
415
615
535
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
"default" 0.2 1 -16777216 true "" "histogram [compliance-attitude] of turtles"

MONITOR
615
490
680
535
att sd
standard-deviation [compliance-attitude] of turtles
3
1
11

MONITOR
615
445
680
490
att mean
mean [compliance-attitude] of turtles
3
1
11

SLIDER
10
70
200
103
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
210
415
370
535
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
105
200
138
frac-random-contacts
frac-random-contacts
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
10
50
160
68
Contacts parameters
12
0.0
1

MONITOR
10
140
180
185
avg fixed / random contacts
(word \nprecision (avg-contacts * (1 - frac-random-contacts)) 3\n\" / \" \nprecision (avg-contacts * frac-random-contacts) 3 )
3
1
11

CHOOSER
5
490
195
535
timeline
timeline
"day-night" "updates-in-place"
0

TEXTBOX
215
395
365
413
Static agent properties
12
0.0
1

TEXTBOX
10
435
160
453
Other parameters
12
0.0
1

TEXTBOX
925
265
1075
295
Visualization parameters
12
0.0
1

TEXTBOX
935
10
1210
40
Scenarios - Work-in-Progress
18
0.0
1

BUTTON
930
110
985
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
135
375
197
420
exp-frac
expected-fraction-masked
3
1
11

TEXTBOX
10
375
120
430
Expected fraction of masked with only beta-attitude and attitude-bias
10
0.0
1

MONITOR
760
55
815
100
boost
fraction-masked - expected-fraction-masked
3
1
11

TEXTBOX
820
60
910
105
Difference fraction masked to expected
10
0.0
1

BUTTON
1050
110
1115
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
990
110
1045
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
940
205
995
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
1040
195
1102
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
1115
195
1177
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
930
75
1045
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
1050
75
1115
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
1120
110
1290
146
small bias with or without beta-attitude for attitude diversity
9
0.0
1

TEXTBOX
935
35
1230
81
Without conformity - no network effects\nBias is boosted by past behavior when importance not too strong and individual attitudes not too important
9
0.0
1

TEXTBOX
935
150
1085
168
Conformity
12
0.0
1

BUTTON
940
170
995
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
130
190
195
223
go 100
setup\nrepeat 100 [go]
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
205
330
297
363
NIL
write-csv
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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-overview" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>fraction-masked</metric>
    <metric>mean stat_fraction-masked-list</metric>
    <metric>standard-deviation stat_fraction-masked-list</metric>
    <enumeratedValueSet variable="attitude-bias">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-attitude">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-pastbehavior">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
      <value value="2.5"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-conformity">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
      <value value="2.5"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frac-random-contacts">
      <value value="0"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-contacts">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="timeline">
      <value value="&quot;updates-in-place&quot;"/>
      <value value="&quot;day-night&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-capacity">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-links">
      <value value="true"/>
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
