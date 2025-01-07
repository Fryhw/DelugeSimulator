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
  bringback

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
    fd 2
    
    ;; Supprime les blocs trop proches (distance < 1)
    ask patches in-radius 2 with [pcolor = orange or pcolor = lime or pcolor = red] [
      set pcolor savedvalue
    ]
  ]
end


to recover-path
  ifelse any? patches with [road = 4 and any? neighbors4 with [road = 3] ][
    set search FALSE
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
