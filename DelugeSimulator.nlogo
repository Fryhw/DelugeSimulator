patches-own [
  elevation
]

breed [populations population]
breed[lighthouses lighthouse]
breed[edges edge]
breed [bateaux bateau]
breed [contours contour]


populations-own [
  radius
  target
  stationned?
]

bateaux-own [
  speed
  radius
  radius_color
  pops
]

links-own [
  dist
  weight
]


lighthouses-own [
  linked?
  pops
]

globals [
  water-height    ;; how high the floods are currently
  raise-water?    ;; true or false, are we ready for the water to go higher?
  ;; The rest of the variables are used only for coloring.
  saved-count
  flood-1-color
  ocean-color divide-color
  initial-ground-color flooded-ground-colors
  total-elevation
]

;;;
;;; SETUP PROCEDURES
;;;



to setup
  clear-all
  set saved-count 0
  set-default-shape turtles "circle"

  setup-colors
  setup-elevations
  setup-floods
  color-world
  reset-ticks

   create-lighthouses nb-lighthouses [
    set color red
    set size 1
    move-to one-of contours
    set linked? False
    set pops []
    set label 0
    set label-color brown
  ]

  create-populations nb-population [
    let target-patch one-of patches with [elevation > -3]

    if target-patch != nobody [
      move-to target-patch
      set shape "person graduate"
      set size 2
      set color red
      set radius 15
    ]

  ]
create-bateaux nb-boat [
  ;; Chercher un patch avec elevation = -3
  let target-patch one-of patches with [elevation = -3]


  ;; Si un patch avec elevation = -3 est trouvé, place le bateau dessus
  if target-patch != nobody [
    move-to target-patch  ;; Déplace la tortue sur ce patch spécifique
    set shape "sailboat side"
    set size 7
    set color blue
    set radius 15
    set radius_color orange
  ]
]
  set total-elevation count patches with [elevation > 0]
;  ask n-of 15 contours [
;      set color red
;    set size 3
;
;   ]



  ask bateaux [
    ;;create-links-with lighthouses
    ask patches in-radius radius [
      set pcolor [radius_color] of myself
    ]
  ]
  ;;show one-of links

end

to setup-elevations
  ;; Here we're using FOREACH to walk down two lists simultaneously:
  ;; the list of patches, and the list of elevations.
  (foreach (sort patches) load-map [ [the-patch the-elevation] ->
    ask the-patch [ set elevation the-elevation ]
  ])
end

to setup-floods
  ask patches [
    if elevation = -1 [
      sprout-contours 1 [ set color flood-1-color ]
    ]
  ]
end

;;;
;;; MAIN PROCEDURES
;;;

to go
  if not any? turtles [ stop ]
  set raise-water? true
  ask turtles with [breed = contours][ flood-contours ]
  ask turtles with [breed = populations][ flood-people ]
  ask turtles with [breed = bateaux] [
    check-and-save
  ]
  ask turtles with [breed = bateaux][ bat ]
  ask turtles with [breed = bateaux][ tofar ]

  if raise-water? [
    ;; raising by 5 is less accurate than raising by 1, but it's faster
    set water-height water-height + 1
  ]
  replace_lighthouse
  radius_detection
  pop_move

  tick
end


to check-and-save
  ;; Demander à l'observer de vérifier les tortues proches du bateau
  ask turtles with [distance myself <= distance-boat AND breed = populations ] [  ;; Vérifier les tortues dans un rayon de 2 blocs du bateau
    set saved-count saved-count + 1
    die  ;; Les tuer
      ;; Ajouter au compteur de gens sauvés
  ]
end

to tofar  ;; turtle procedure
  ;; Si la tortue est près du bord gauche
if xcor < 2 [

 setxy 237 ycor  ;; Déplacer la tortue à la coordonnée (237, ycor)
 fd 2
]
  ;; Si la tortue est près du bord droit
  if xcor > 236 [
    setxy 1 ycor
    fd 2
  ]
  ;; Si la tortue est près du bord inférieur
  if ycor < 2 [
    setxy xcor 116
    fd 2
  ]
  ;; Si la tortue est près du bord supérieur
  if ycor > 117 [
    setxy xcor 2
    fd 2
  ]
end




to bat
  ask turtles with [breed = bateaux] [dep]

end

to dep
  ;; Chercher des tortues "contours" dans un rayon de 9 cases
  let danger-neighbors turtles with [breed != populations and distance myself < 2 and self != myself]


  ;; Si des tortues contours sont proches, on fait une rotation pour les éviter
  if any? danger-neighbors [
    let escape-heading (heading + 180 + random 60 - 30)  ;; Tourner dans la direction opposée + un petit random pour varier
    set heading escape-heading
  ]

  ;; Rotation aléatoire pour un déplacement naturel
  rt random 35
  lt random 35

  ;; Avancer d'une unité
  fd boat-travel-distance
end

to flood-people  ;; turtle procedure
  let my-color color
  let unflooded-neighbors neighbors4 with [shade-of? pcolor initial-ground-color and
                                           not any? turtles-here with [color = my-color]]
    if not any? unflooded-neighbors [
      die
    ]
end

to avoid  ;; turtle procedure
  let my-color color
  let unflooded-neighbors neighbors4 with [shade-of? pcolor initial-ground-color and
                                           not any? turtles-here with [color = my-color]]
    if not any? unflooded-neighbors [
      die
    ]
end

to flood-contours  ;; turtle procedure
  let my-color color
  let unflooded-neighbors neighbors4 with [shade-of? pcolor initial-ground-color and
                                           not any? turtles-here with [color = my-color]]
    if not any? unflooded-neighbors [
       recolor-patch
      set total-elevation total-elevation - 1
      die

    ]

  ask unflooded-neighbors with [elevation < water-height] [
    sprout-contours 1 [
      set color my-color
      set raise-water? false
    ]
  ]
end



;;;
;;; COLOR PROCEDURES
;;;

to setup-colors
  set flood-1-color yellow + 1
  set ocean-color black
  set divide-color red
  set initial-ground-color brown
  set flooded-ground-colors [magenta blue sky turquoise lime green]
end

to color-world
  ask patches [
    ifelse elevation < 0 [    ;; check: is ocean?
      set pcolor ocean-color
    ] [
      set pcolor scale-color initial-ground-color elevation -150 max-elevation
    ]
  ]
end

to recolor-patch  ;; turtle procedure
  ;; while staying within the bounds of the list,
  ;; find the (water-height / 150)th element of the flooded-ground-colors
  let current-flooded-ground-color item (max list 0
                                                 (min list (length flooded-ground-colors - 1)
                                                           (floor (water-height / 150))))
                                        flooded-ground-colors
  set pcolor scale-color current-flooded-ground-color elevation -150 max-elevation ; -150 makes things lighter than the ocean
end

;;;
;;; THE MAP
;;;
to radius_detection
   ask bateaux [
    let target_lighthouses lighthouses in-radius radius with [not linked?]
    if target_lighthouses != nobody [
      create-links-with target_lighthouses [
        set dist end1
        set color red
        ;;show dist
      ]
      ask target_lighthouses [
        set linked? True
      ]
    ]



;    if any? other lighthouses in-radius radius [
;      create-link-with one-of other lighthouses
;      ;;set pcolor [radius_color] of myself
;
;    ]
  ]



  ask populations [
    let target_lighthouses lighthouses in-radius radius
    if any? target_lighthouses [
      create-links-with target_lighthouses [
        set dist end1
        set color blue
        ;show dist
      ]
      ;face one-of target_lighthouses
      ;;set target target_lighthouses
      ;;show target_lighthouses
      show my-links
    ]
  ]
end



to pop_station [lh]
  ask lh [
    set pops lput myself pops
  ]
  ;;hide-turtle
  ask my-links [
    hide-link
  ]

end

to pop_move
  ask populations [
    ;;show target

;    show nearest-lighthouse target
    ifelse not any? my-links
    [fd random 5
     right random 180
    ]
    [
    ;; move towards target.  once the distance is less than 1,
    ;; use move-to to land exactly on the target.

      if any? my-links[
        let nearest nearest-lighthouse my-links
        ifelse  distance nearest > 5
        [ face nearest
        fd 1 ]
        [ifelse not member? self [pops] of nearest[
          pop_station nearest
          ask nearest [show pops]

        ]
        [update_lighthouse_size nearest]]
      ]
    ]
  ]



end

;; replace lighthouse to new position near current pos
to replace_lighthouse
  ask lighthouses [
;    if pcolor = ocean-color or pcolor = flood-1-color or pcolor = flooded-ground-colors or pcolor = divide-color [
;      move-to one-of contours in-radius 10
;    ]
    if not any? turtles-here with [breed = contours][
      if any? contours in-radius 5 [
        move-to one-of contours in-radius 5
      ]
    ]
  ]

end

to update_lighthouse_size [lh]
  ask lh [
    set size length pops * 0.5
    set label length pops
  ]

end


;to delete_radius
;  ask patches with [pcolor = radius_color ] [
;
;  ]
;
;end
;
to-report load-map
  file-open "maps/usa.txt"
  ;;print file-read

  let worldmap []
  while [not file-at-end?]
  [
    set worldmap file-read


    ;;show worldm

  ]
  file-close
  report worldmap
  ;show worldmap
  ;file-close


end

;; report from agentset, the nearest lighthouse in distance
to-report nearest-lighthouse [l]
  ;;let my-agentset  my-list
  let lh turtle-set [other-end] of l
  let nearest min-one-of lh [distance myself]
  ;;let nearest min-one-of [other-end] of l [distance [other-end] of l]
  report nearest

end




to-report max-elevation
  report 3000
end



; Copyright 2007 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
583
10
2019
739
-1
-1
6.0
1
20
1
1
1
0
0
0
1
0
237
0
119
1
1
1
ticks
30.0

BUTTON
10
38
81
80
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
95
38
166
80
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
0

MONITOR
26
97
151
142
water height
word water-height \" meters\"
3
1
11

MONITOR
165
97
236
142
Sauvés
saved-count
17
1
11

PLOT
336
113
536
263
Elevation
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
"Elevation" 1.0 0 -16777216 true "" "plot total-elevation"

SLIDER
31
162
203
195
nb-boat
nb-boat
1
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
31
196
203
229
nb-population
nb-population
50
500
125.0
25
1
NIL
HORIZONTAL

SLIDER
26
330
198
363
distance-boat
distance-boat
2
10
5.0
1
1
NIL
HORIZONTAL

MONITOR
183
36
311
81
nombre de morts
nb-population - count turtles with [breed = populations] - saved-count
17
1
11

PLOT
337
266
537
416
Evolution du nombre de morts
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
"default" 1.0 0 -16777216 true "" "plot nb-population - count turtles with [breed = populations] - saved-count"

SLIDER
26
366
198
399
boat-travel-distance
boat-travel-distance
2
10
3.0
1
1
NIL
HORIZONTAL

PLOT
337
425
537
575
niveau de l'eau
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
"default" 1.0 0 -16777216 true "" "plot water-height"

PLOT
338
587
538
737
Evolution du nombre de personnes sauvés
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
"default" 1.0 0 -16777216 true "" "plot saved-count"

SLIDER
31
229
203
262
nb-lighthouses
nb-lighthouses
0
25
7.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model demonstrates one way to locate a continental divide.  A continental divide separates a continent into two regions based on two bodies of water.  Rain in one region flows into one body of water and rain in the other region flows into the other.

In the example data, the continent is North America and the two bodies of water used to calculate the divide are the Pacific and Atlantic oceans.

## HOW IT WORKS

The model is initialized with an elevation map.  Then both oceans systematically rise, bit by bit.  The two floods run towards each other over the continent and eventually crash.  The continental divide is precisely where the two floods collide.

## HOW TO USE IT

SETUP initializes the model.  Elevations are stored in the patches and they are colored appropriately. Also, the two floods are started off on the coasts.

GO runs the model.  When the floods cannot advance any more with the given height of the water, the water level is raised a little bit.  Eventually, when the whole continent has been flooded and the continental divide has been found, the model stops automatically.

## THINGS TO NOTICE

The two floods move at different rates.

The first 100 meters of flood covers more land than the last 100 meters.  What about in between?

Land that's flooded later isn't necessarily higher elevation. (Why?)

## THINGS TO TRY

Use the speed slider to slow the model down and watch what happens in more detail.

Increase the patch-size to get a better view of the action.  (Because the elevation data assumes specific dimensions, you can't change the number of patches in the model.)

## EXTENDING THE MODEL

Make a slider to control how much water-height changes when the flooding at a given water-height has stopped.

Make a slider for controlling how many colors from FLOODED-GROUND-COLOR-LIST get used.  With a smaller number, the flooded land's elevation is easier to see.  With a larger number, the progression of flooding is easier to see.

Is there a difference if `neighbors` is used instead of `neighbors4`? Make a switch to toggle between the two options and compare them.

Try the model with a more detailed dataset.

Allow the user of the model to specify different bodies of water than the Atlantic and Pacific oceans.  For example, it'd be interesting to see which water flows into the Gulf of Mexico and which flows into the Atlantic.

Allow the user to import maps of other parts of the world.

## NETLOGO FEATURES

Note the use of turtles to represent the edges of the flood.  Instead of asking all the patches to find the ones on each edge, we only need to ask the turtles to act.  Since at any given moment only a few patches are at a flood edge, this is much faster.

Note the used of `foreach` on multiple lists to initialize the elevation data in the patches.

## RELATED MODELS

Grand Canyon

## CREDITS AND REFERENCES

This model was inspired by Brian Hayes' article "Dividing the Continent" in American Scientist, Volume 88, Number 6, page 481.  An online version can be found here: https://www.jstor.org/stable/27858114

Thanks to Josh Unterman for his work on this model.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (2007).  NetLogo Continental Divide model.  http://ccl.northwestern.edu/netlogo/models/ContinentalDivide.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2007 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2007 -->
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

person graduate
false
0
Circle -16777216 false false 39 183 20
Polygon -1 true false 50 203 85 213 118 227 119 207 89 204 52 185
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -8630108 true false 90 19 150 37 210 19 195 4 105 4
Polygon -8630108 true false 120 90 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90
Polygon -1184463 true false 135 90 120 90 150 135 180 90 165 90 150 105
Line -2674135 false 195 90 150 135
Line -2674135 false 105 90 150 135
Polygon -1 true false 135 90 150 105 165 90
Circle -1 true false 104 205 20
Circle -1 true false 41 184 20
Circle -16777216 false false 106 206 18
Line -2674135 false 208 22 208 57

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

sailboat side
false
0
Line -16777216 false 0 240 120 210
Polygon -7500403 true true 0 239 270 254 270 269 240 284 225 299 60 299 15 254
Polygon -1 true false 15 240 30 195 75 120 105 90 105 225
Polygon -1 true false 135 75 165 180 150 240 255 240 285 225 255 150 210 105
Line -16777216 false 105 90 120 60
Line -16777216 false 120 45 120 240
Line -16777216 false 150 240 120 240
Line -16777216 false 135 75 120 60
Polygon -7500403 true true 120 60 75 45 120 30
Polygon -16777216 false false 105 90 75 120 30 195 15 240 105 225
Polygon -16777216 false false 135 75 165 180 150 240 255 240 285 225 255 150 210 105
Polygon -16777216 false false 0 239 60 299 225 299 240 284 270 269 270 254

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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
0
@#$#@#$#@
