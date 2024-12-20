patches-own [
  elevation
  dist        ;; Distance minimale calculée par Dijkstra
  previous    ;; Patch précédent dans le chemin optimal
  visited?    ;; Si le patch a été visité ou non
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

  setup-colors
  setup-elevations
  setup-floods
  color-world
  reset-ticks

  ask patches [
    set dist 99999
    set visited? false
    set previous nobody
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


to calculate-dijkstra [source]
  ;; Cette partie doit être exécutée par l'observateur uniquement, donc on déplace l'action ask patches ici.
  ask patches [
    set dist 99999
    set visited? false
    set previous nobody
  ]
  ask source [
    set dist 0
  ]
  set notVisited patches

  while [any? notVisited] [
    let current min-one-of notVisited [dist]
    if current = nobody [ stop ]

    ask current [
      set visited? true
      ask neighbors4 with [not visited?] [
        let elevation-cost ifelse-value ([elevation] of myself - elevation < 0) [0] [[elevation] of myself - elevation]
        let newDist ([dist] of myself + 1 + elevation-cost)
        if newDist < dist [
          set dist newDist
          set previous myself
        ]
      ]
    ]
    set notVisited notVisited with [self != current]
  ]
end



to move-to-nearest-population
  ask bateaux [
    let near-neighbors turtles with [breed = populations and distance myself < 500 and self != myself]
    if any? near-neighbors [
      ;; Sélectionner le voisin le plus proche
      let nearest-one min-one-of near-neighbors [distance myself]
      ;; Récupérer le patch où se trouve ce voisin
      let target-patch [patch-here] of nearest-one
      ;; Calculer le chemin avec Dijkstra
      calculate-dijkstra target-patch
      ;; Suivre le chemin
      follow-dijkstra-path
    ]
  ]
end




to follow-dijkstra-path  ;; Utilisé par les bateaux
  let current patch-here
  while [current != nobody and [previous] of current != nobody] [
    let next-patch [previous] of current
    move-to next-patch
    set current next-patch
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
  ask bateaux [
    ;;move-to-nearest-population
    check-and-save
    bat
    tofar
  ]

  if raise-water? [
    set water-height water-height + 1
  ]
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
  let save-neighbors turtles with [breed = populations and distance myself < 5 and self != myself]

  ;; Si des tortues contours sont proches, on fait une rotation pour les éviter
  if any? danger-neighbors [
    let escape-heading (heading + 180 + random 60 - 30)  ;; Tourner dans la direction opposée + un petit random pour varier
    set heading escape-heading
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
