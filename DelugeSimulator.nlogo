patches-own [
  elevation
  waterlvl
  n
  step
  recovered
  isexit
  road
  path
  savedvalue

]

globals [
  water-height
  raise-water?
  saved-count
  flood-1-color
  ocean-color divide-color
  initial-ground-color flooded-ground-colors
  total-elevation
  notVisited    ;; Ensemble des patches non visités pour Dijkstra
  solved
  search
]



breed [populations population]
breed[lighthouses lighthouse]
breed[edges edge]
breed [bateaux bateau]
breed [contours contour]


;;;
;;; SETUP PROCEDURES
;;;

;; Setup ajusté
to setup
  clear-all
  set saved-count 0
  set-default-shape turtles "circle"
  reset
  setup-colors
  setup-elevations
  setup-floods
  color-world
  reset-ticks
  set search false

  ;if search = FALSE [
  ;  reset
  ;  buildmaze
  ;]
  ask patches [

    set waterlvl elevation
  ]

  ;; Création des populations
  create-populations nb-population [
    let target-patch one-of patches with [elevation > -3]
    if target-patch != nobody [
      move-to target-patch
      set shape "person graduate"
      set size 2
      set color red
    ]
  ]

  ;; Création des bateaux
  create-bateaux nb-boat [
    let target-patch one-of patches with [elevation = -3]
    if target-patch != nobody [
      move-to target-patch
      set shape "sailboat side"
      set size 7
      set color blue
    ]
  ]

  set total-elevation count patches with [elevation > 0]
end




to move-to-nearest-population
  ask bateaux [
    let near-neighbors turtles with [breed = populations and distance myself < 500 and self != myself]
    if any? near-neighbors [
      ;; Sélectionner le voisin le plus proche
      let nearest-one min-one-of near-neighbors [distance myself]
      ;; Récupérer le patch où se trouve ce voisin
      let target-patch [patch-here] of nearest-one

    ]
  ]
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
  ask contours [ flood-contours ]
  ask populations [ flood-people ]

  if not any? patches with [pcolor = orange or pcolor = red or pcolor = lime][
  reset
  buildmaze
]
  while [search = TRUE ] [solve]

  ask bateaux [
    orienter-et-avancer-vers-blocs
    check-and-save
    ;tofar

  ]
  ;move-to-nearest-population
    ;; check-and-save
    ;; bat
  ;;

  if raise-water? [
    set water-height water-height + 1
    ask patches with [ waterlvl > 0 ] [set waterlvl waterlvl - 1]
  ]
  tick
end


to bringback
  ask patches with [pcolor = black]
  [set pcolor savedvalue]
end


to buildmaze
  set search TRUE
  ;ask [patch-here] of one-of contours [set road 3
  ;set pcolor red]
  ask one-of patches with [road = 1] [set road 3
  set pcolor red]
  ask turtles with [breed = bateaux][

  ask patch-here [set road 4
      ;set pcolor lime
    ]
  ]
end

to reset
  set solved FALSE
  ask patches [set n 0
    set savedvalue pcolor
               set road 1
               set step 0
               set plabel ""
               set recovered FALSE
              ]
  ask patches with [waterlvl > 0][set road 2]
  ask patches with [road = 1][
    if any? patches in-radius 1 with [road = 2][
      set road 2]]
end


to orienter-et-avancer-vers-blocs
  let bloc-cible 0
  ;; Si un bloc orange est à proximité (dans un rayon de 10 patches)
  if any? patches in-radius 4 with [pcolor = orange or pcolor = red] [
    set bloc-cible min-one-of patches in-radius 4 with [pcolor = orange or pcolor = red] [distance myself]

    ;; La tortue s'oriente vers le bloc orange le plus proche
    face bloc-cible

    ;; Elle avance vers le bloc
    fd 1

    ;; Supprime les blocs trop proches (distance < 1)
    ask patches in-radius 2 with [pcolor = orange or pcolor = lime or pcolor = red] [
      set pcolor savedvalue
    ]
  ]
end


to recover-path
  ifelse any? patches with [road = 4 and any? neighbors4 with [road = 3] ][
    set search FALSE
    ask patches with [pcolor = red][set pcolor savedvalue]
    stop
  ] [
   ask patches with [road = 4 and recovered = FALSE] [
     if any? neighbors4 with [road = 5] [
      let tset neighbors4 with [road = 5]
      ask min-one-of tset [step] [set road 4
        set path 1
        set pcolor orange]
      set recovered TRUE

     ]
   ]

  ]
end

to solve
  ifelse solved [
    recover-path
  ] [
      ask patches with [road = 1][
        if any? neighbors4 with [road = 3]  or
           any? neighbors4 with [road = 5] [
             set road 5
             let laststep [step] of one-of neighbors4 with [road = 5 or
                                                            road = 3]
             set step laststep + 1
           ]
      ]
      ask patches with [road = 4] [
        if any? neighbors4 with [road = 5 ] [
          set solved TRUE
        ]
      ]
  ]
end




to check-and-save;;Check if people arround the boat (with an distnace) if yes kill them and add point to saved
  ask turtles with [distance myself <= distance-boat AND breed = populations ] [
    set saved-count saved-count + 1
    die
  ]
end

to tofar ;; If the boat go to far make it travel across the map
if xcor < 2 [
 setxy 237 ycor
    fd 2
]
  if xcor > 236 [
    setxy 1 ycor
    fd 2
  ]
  if ycor < 2 [
    setxy xcor 116
    fd 1
  ]
  if ycor > 117 [
    setxy xcor 2
    fd 1
  ]
end




to bat
  ask turtles with [breed = bateaux] [dep]
end

to dep
  ;; Chercher des tortues "contours" dans un rayon de 9 cases
  let danger-neighbors turtles with [breed != populations and distance myself < 4 and self != myself]
  let save-neighbors turtles with [breed = populations and distance myself < 6 and self != myself and distance myself > 3]

  ;; Si des tortues contours sont proches, on fait une rotation pour les éviter
  if any? danger-neighbors [
    let escape-heading (heading + 180 + random 30 - 20)  ;; Tourner dans la direction opposée + un petit random pour varier
    set heading escape-heading
    fd boat-travel-distance + 2
  ]
  if (not any? danger-neighbors) and any? save-neighbors [
    let target one-of save-neighbors  ;; Choisir un voisin de sauvetage au hasard
    face target  ;; Pointer vers ce voisin
  ]
  if (not any? danger-neighbors) and (not any? save-neighbors) [
  ;; Rotation aléatoire pour un déplacement naturel
  rt random 35
  lt random 35
  ]
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

to avoid  ;; If people touch water they died
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
  set ocean-color sky
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
1.0
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
2.0
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

@#$#@#$#@
## Résumé / Summary

Dans le cadre de notre projet concernant le module Systeme multi-Agent et Ce document vise à définir le projet Déluge, de définir les limites de ce projet et de proposer un type de modélisation multi-agents pertinentes à un scénario donnée.
 

This document aims to define the Deluge project, to define the limits of this
project and to propose a type of multi-agent modeling relevant to a given
scenario.


## Introduction

Le projet Déluge a comme thématique l’élévation du niveau de la mer. Ce thème
sera abordé sous forme d’un scénario spécifique. Il se déroule de la façon suivante :
La simulation démarre à partir d’une map basée sur le globe terrestre. Cette
map représente ainsi les continents avec leurs reliefs et les différents océans. Des
populations sont présentes sur ces différentes zones terrestre. Elles sont capables de
se déplacer afin de se rassembler et échapper aux inondations. Un bateau est placé
sur cette map et a comme objectif de sauver un maximum de personnes. Le
bateau se déplacera selon une stratégie de déplacement (heuristique).

## But du projet

Nous voulons proposé une manière d’évaluer différentes stratégies de déplace-
ment du bateau (basé sur différentes heuristiques) et un mode de communication
idéale pour le sauvetage du plus grand nombre de populations dans notre scénario
catastrophe.

## Modélisation du terrain

Le terrain est modélisé par une carte d’élévation qui représente les différents
reliefs pouvant exister. Le terrain basique actuel est la carte des États-Unis. Ces
cartes sont des ressources en ligne qui ont été importé dans le projet. Le terrain
est composé de 2 types différents : eau ou terre. L’élévation du terrain détermine
le type de terrain : une élévation inférieure à 0 correspond à de l’eau. La carte est
inondée de façon progessive et uniforme changeant donc la nature du terrain au fur
et à mesure que le temps progresse dans la simulation.

## Modélisation des agents

On dénombre 2 types d’agents principaux dans notre modélisation : les bateaux
et populations.
Un bateau est un véhicule se déplaçant sur l’eau caractérisé par sa position sur
la carte, sa vitesse, et la stratégie de parcours qu’il utilise. Il possède un champ de
détection (d’un rayon de x pixels). Ce champ de détection est utilisé pour obtenir
la localisation des populations, connaître l’état de l’environnement proche et pour
permettre la communication avec un autre agent (bateau ou population) et notam-
ment éviter les collisions avec d’autres navires.

Une population est une personne habitant sur la carte. Elle est caractérisée par
une position, une vitesse de déplacement. Elle possède différents types de compor-
tements comme l’action de se regrouper, de fuir les inondations. Ces actions sont
contraintes au champ de détection qui permet à la population de s’informer sur
l’état de l’environnement proche, le nombre et la position des populations proches.

## Pathfinding

Plusieurs stratégie de création d’itinéraires pour les bateaux sont envisagées.
Nous avons pensé à une heuristique locale qui privilégie les foyers de populations les
plus peuplés à l’instantée.
Pour modéliser la carte sous forme de graphe simplifié, nous allons assigner à la
carte, lors de l’initialisation des phares qui feront office de sommets. Des stratégies
d’itinéraires globales sont proposés comme l’algorithme de Dijkstra ou A*.

## Statistiques et mesures

Nous pouvons grâce à cette simulation mesurer le pourcentage de la popula-
tion qui s’est noyé, le pourcentage de la carte immmergée. Ces statistiques peuvent
être relevées pour différentes stratégies de pathfinding qui ont été utilisés et ainsi
comparer leurs résultats. Il est intéressant de comparer des heuristiques plus lo-
cales effectuées par les bateaux et les comparer à des stratégies qui nécessitent une
connaissance globale de la carte comme Dijkstra et déterminer si ces dernières sont
adéquates pour notre tâche.
Paramètres globaux envisagés :
— nombre de bateaux
— vitesse montée des eaux
— densité de population
Les agents, leurs caractéristiques et leurs paramètres :
— bateau (se déplace sur l’eau et effectue l’embarquement des populations ) :
— vitesse du bateau
— stratégie de parcours
— position
— population (disséminés sur la map, possèdent la capacité de se regrouper et
de se déplacer)
— vitesse de déplacement
— position
Environnement :
— sol
— eau
Données : carte géographique (map), sous forme de matrices d’élévations
Statistiques
— nombre de noyés
— pourcentage de la carte immergée
Evaluation
— Stratégie de parcours du bateau (heuristique)
— Mode de communication populations / bateau


## utilisation 



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
