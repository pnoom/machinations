;;;; recipes.lisp - representation and database

(in-package #:machinations)

;; As yet, the source slot of recipes, and the abbreviations, serve no purpose.

(defparameter *source-abbrevs*
  '((am assembling-machine)
    (chem chemical-plant)
    (basic basic-oil-processing)
    (adv advanced-oil-processing)
    (pump offshore-pump)))

(defun un-abbrev (abbrev)
  (second (assoc abbrev *source-abbrevs*)))

(defstruct (recipe (:conc-name nil)
		   (:constructor make-recipe (source duration products
						     ingredients)))
  source
  duration
  products
  ingredients)

(defun build-recipe (spec)
  "Given a Lisp form that represents a recipe, return a recipe object."
  (destructuring-bind (source duration (&rest products) (&rest ingredients))
      spec
    (apply #'make-recipe source duration products (list ingredients))))

(defun make-recipes (specs)
  (mapcar #'build-recipe specs))

;; The vast majority of recipes are 'simple', i.e. only have one output. For
;; brevity, describe those using alternative syntax.

(defun build-simple-recipe (source spec)
  (destructuring-bind (product n d &rest ingredients) spec
    (apply #'make-recipe source d (list (list product n)) (list ingredients))))

(defun make-simple-recipes (source specs)
  (mapcar (lambda (spec)
	    (build-simple-recipe source spec))
	  specs))

(defparameter *simple-am-recipes*
  (make-simple-recipes
   'am
   '((iron-chest       1   0.5  (iron-plate 8))
     (steel-chest      1   0.5  (steel-plate 8))
     (storage-tank     1   3    (iron-plate 20) (steel-plate 5))
     (belt             2   0.5  (iron-plate 1) (iron-gear 1))
     (fast-belt        1   0.5  (belt 1) (iron-gear 5))
     ;;express-belt
     (ug-belt          2   1    (iron-plate 10) (belt 5))
     (fast-ug-belt     2   0.5  (iron-gear 40) (ug-belt 2))
     ;;express-ug-belt
     (splitter         1   1    (green-circuit 5) (iron-plate 5) (belt 4))
     (fast-splitter    1   2    (splitter 1) (iron-gear 10) (green-circuit 10))
     ;;express-splitter
     (burner-inserter  1   0.5  (iron-plate 1) (iron-gear 1))
     (inserter         1   0.5  (iron-plate 1) (iron-gear 1) (green-circuit 1))
     (long-inserter    1   0.5  (iron-plate 1) (iron-gear 1) (inserter 1))
     (fast-inserter    1   0.5  (inserter 1) (iron-plate 2) (green-circuit 2))
     (filter-inserter  1   0.5  (fast-inserter 1) (green-circuit 4))
     (stack-inserter   1   0.5  (iron-gear 15) (green-circuit 15) (red-circuit 1) (fast-inserter 1))
     (stack-filter     1   0.5  (stack-inserter 1) (green-circuit 5))
     (medium-pole      1   0.5  (steel-plate 2) (copper-plate 2))
     (big-pole         1   0.5  (steel-plate 2) (copper-plate 2))
     (substation       1   0.5  (steel-plate 10) (red-circuit 5) (copper-plate 5))
     (pipe             1   0.5  (iron-plate 1))
     (ug-pipe          2   0.5  (iron-plate 5) (pipe 10))
     (pump             1   2    (engine-unit 1) (steel-plate 1) (pipe 1))
     ;;train stuff
     (logistic-bot     1   0.5  (robot-frame 1) (red-circuit 2))
     (construction-bot 1   0.5  (robot-frame 1) (green-circuit 2))
     (passive-chest    1   0.5  (steel-chest 1) (green-circuit 3) (red-circuit 1))
     (storage-chest    1   0.5  (steel-chest 1) (green-circuit 3) (red-circuit 1))
     (roboport         1   10   (steel-plate 45) (iron-gear 45) (red-circuit 45))
     (lamp             1   0.5  (green-circuit 1) (iron-stick 3) (iron-plate 1))
     ;;circuit stuff
     ;;stone-brick
     ;;concrete
     ;;hazard-concrete
     ;;landfill

     ;;pickaxe
     (repair-pack      1   0.5  (green-circuit 2) (iron-gear 2))
     (boiler           1   0.5  (stone-furnace 1) (pipe 4))
     (steam-engine     1   0.5  (iron-plate 10) (iron-gear 8) (pipe 5))
     (solar-panel      1   10   (steel-plate 5) (green-circuit 15) (copper-plate 5))
     (accumulator      1   10   (iron-plate 2) (battery 5))
     (mining-drill     1   2    (green-circuit 3) (iron-gear 5) (iron-plate 10))
     (offshore-pump    1   0.5  (green-circuit 2) (pipe 1) (iron-gear 1))
     (pumpjack         1   5    (steel-plate 5) (iron-gear 10) (green-circuit 5) (pipe 10))
     (stone-furnace    1   0.5  (stone 5))
     (steel-furnace    1   3    (steel 6) (stone-brick 10))
     (electric-furnace 1   5    (steel-plate 10) (stone-brick 10) (red-circuit 5))
     (assembling-1     1   0.5  (green-circuit 3) (iron-gear 5) (iron-plate 9))
     (assembling-2     1   0.5  (green-circuit 3) (iron-gear 5) (iron-plate 9) (assembling-1 1))
     (oil-refinery     1   10   (steel-plate 15) (iron-gear 10) (green-circuit 10) (pipe 10)
      (stone-brick 10))
     (chemical-plant   1   5    (steel-plate 5) (iron-gear 5) (green-circuit 5) (pipe 5))
     (lab              1   3    (green-circuit 10) (iron-gear 10) (belt 4))
     ;;upgrade modules
     (speed-module-1   1   15   (green-circuit 5) (red-circuit 5))

     
     (iron-gear        1   0.5  (iron-plate 2))
     (copper-cable     2   0.1  (copper-plate 1))
     (green-circuit    1   0.5  (iron-plate 1) (copper-cable 3))
     (red-circuit      1   6    (green-circuit 2) (plastic-bar 2) (copper-cable 4))
     (blue-circuit     1   10   (green-circuit 20) (red-circuit 2) (sulfuric-acid 5))
     (engine-unit      1   10   (steel-plate 1) (iron-gear 1) (pipe 2))
     (electric-engine  1   10   (engine-unit 1) (green-circuit 2) (lubricant 15))
     (robot-frame      1   20   (electric-engine 1) (battery 2) (steel-plate 1) (green-circuit 3))
     (red-science      1   5    (copper-plate 1) (iron-gear 1))
     (green-science    1   6    (inserter 1) (belt 1))
     (blue-science     1   12   (engine-unit 1) (red-circuit 1) (mining-drill 1))
     (military-science 2   10   (piercing-rounds 1) (grenade 1) (turret 1))
     (production-science 2 14   (electric-engine 1) (assembling-1 1) (electric-furnace 1))
     (high-tech-science 2  14   (battery 1) (blue-circuit 3) (speed-module-1 1) (copper-cable 30))

     (rounds           1   1    (iron-plate 4))
     (piercing-rounds  1   3    (rounds 1) (steel-plate 1) (copper-plate 5))
     (grenade          1   8    (iron-plate 5) (coal 10))
     (turret           1   8    (iron-plate 20) (iron-gear 10) (copper-plate 10))
     (laser-turret     1   20   (steel-plate 20) (green-circuit 20) (battery 12))

     )))

(defparameter *simple-chem-recipes*
  (make-simple-recipes
   'chem
   '((sulfur           2   1    (water 3) (petroleum-gas 3))
     (sulfuric-acid    50  1    (sulfur 5) (iron-plate 1) (water 100))
     (battery          1   5    (iron-plate 1) (copper-plate 1) (sulfuric-acid 20))
     (plastic-bar      2   1    (coal 1) (petroleum-gas 3))
     (lubricant        10  1    (heavy-oil 10))
     ;;cracking and solid fuel
     )))

(defparameter *complex-recipes*
  (make-recipes
   '(;;(pump
     ;; 1
     ;; ((water 1200))
     ;; (nil))
     (basic
      5
      ((heavy-oil 30) (light-oil 30) (petroleum-gas 40))
      ((crude-oil 100)))
     ;;(adv
     ;; 5
     ;; ((heavy-oil 10) (light-oil 45) (petroleum-gas 55))
     ;; ((water 50) (crude-oil 100)))

     )))

(defparameter *recipes*
  (append *simple-am-recipes* *simple-chem-recipes* *complex-recipes*))

;; For now, treat smelted and mined stuff as raw, until I've checked that the
;; timings from my pre-0.15 calculations haven't changed.

(defparameter *raws*
  '(crude-oil stone coal iron-ore copper-ore stone-brick iron-plate
    copper-plate steel-plate water)
  "Anything you don't want to recurse on. Overrides default behaviour.")

(defparameter *rate* 1)
