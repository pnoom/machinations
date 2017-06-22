# Machinations

Machinations is intended to be a useful group of tools related to [Factorio](https://www.factorio.com/).

As yet, it provides only one tool, which is similar in vein to [Factorio Planner](http://doomeer.com/factorio).
The basic idea is to help you calculate the number of machines you'll need
crafting each component in order to craft the end product at a desired rate.
This knowledge can be used to inform the design of your factories, and give
a ball-park idea as to their size and layout.

Since this is a (working) prototype, it ignores considerations such as upgrade
modules and beacons, and treats certain materials (such as metal plates) as
raw materials.

Eventually I would like to add tools relating to belts, circuit networks,
power generation, and so on.

## Installation

Clone the respository to a location that ASDF/Quicklisp can find, eg.
`~/common-lisp/`, then start up your CL implementation and run:

```
CL-USER> (ql:quickload :machinations)
```

## Usage

The API is fluid, so for now just "enter" (or "use") the `machinations`
package (nickname `mach`).

The main interface is provided by the function `breakdown`, which takes
two arguments: a symbol denoting a product, and the rate at which you'd
like to craft said product. (If no rate is provided, the default of
1 product per second is used).

```
MACH> (breakdown 'fast-splitter)

  Total number of machines required for each component:

  FAST-SPLITTER 2 
  SPLITTER 1 
  BELT 1 
  IRON-GEAR 7 
  GREEN-CIRCUIT 8 
  COPPER-CABLE 3 

  Full breakdown:

  FAST-SPLITTER 2 
    SPLITTER 1 
      GREEN-CIRCUIT 3 
        COPPER-CABLE 1 
      BELT 1 
        IRON-GEAR 2 
    IRON-GEAR 5 
    GREEN-CIRCUIT 5 
      COPPER-CABLE 2 
NIL
```
The brains of `breakdown` lies in the function `calculate`, which can be used
directly if you want to further manipulate the data that it outputs.

For now, to toggle between basic and advanced oil processing, simply comment
out the undesired recipe in `recipes.lisp`. You may also want to experiment
with using the oil cracking recipes as your source for the various fluids.

For complex recipes, it is often useful to treat certain components as raw
materials, if only to reduce the amount of text that `breakdown` spits out.
For example: 
```
MACH> (breakdown 'high-tech-science 0.5)

  Total number of machines required for each component:

  HIGH-TECH-SCIENCE 4 
  BATTERY 3 
  BLUE-CIRCUIT 15 
  SULFURIC-ACID 2 
  SULFUR 44 
  SPEED-MODULE-1 8 
  RED-CIRCUIT 33 
  GREEN-CIRCUIT 23 
  PLASTIC-BAR 6 
  PETROLEUM-GAS 39 
  COPPER-CABLE 11 

  Full breakdown:

  HIGH-TECH-SCIENCE 4 
    BATTERY 3 
      SULFURIC-ACID 1 
        SULFUR 25 
          PETROLEUM-GAS 19 
    BLUE-CIRCUIT 15 
      GREEN-CIRCUIT 15 
        COPPER-CABLE 5 
      RED-CIRCUIT 18 
        GREEN-CIRCUIT 3 
          COPPER-CABLE 1 
        PLASTIC-BAR 3 
          PETROLEUM-GAS 3 
        COPPER-CABLE 1 
      SULFURIC-ACID 1 
        SULFUR 19 
          PETROLEUM-GAS 15 
    SPEED-MODULE-1 8 
      GREEN-CIRCUIT 2 
        COPPER-CABLE 1 
      RED-CIRCUIT 15 
        GREEN-CIRCUIT 3 
          COPPER-CABLE 1 
        PLASTIC-BAR 3 
          PETROLEUM-GAS 2 
        COPPER-CABLE 1 
    COPPER-CABLE 1 
NIL
```
Now treat red and green circuits as raw and see the collapsed output:
```
MACH> (setf *raws* (append *raws* '(red-circuit green-circuit)))
(CRUDE-OIL STONE COAL IRON-ORE COPPER-ORE STONE-BRICK IRON-PLATE COPPER-PLATE
 STEEL-PLATE WATER RED-CIRCUIT GREEN-CIRCUIT)
MACH> (breakdown 'high-tech-science 0.5)

  Total number of machines required for each component:

  HIGH-TECH-SCIENCE 4 
  BATTERY 3 
  BLUE-CIRCUIT 15 
  SULFURIC-ACID 2 
  SULFUR 44 
  PETROLEUM-GAS 34 
  SPEED-MODULE-1 8 
  COPPER-CABLE 1 

  Full breakdown:

  HIGH-TECH-SCIENCE 4 
    BATTERY 3 
      SULFURIC-ACID 1 
        SULFUR 25 
          PETROLEUM-GAS 19 
    BLUE-CIRCUIT 15 
      SULFURIC-ACID 1 
        SULFUR 19 
          PETROLEUM-GAS 15 
    SPEED-MODULE-1 8 
    COPPER-CABLE 1 
NIL
```
The interface will remain purely REPL-based until the backend provides enough
useful functionality that it's worth adding a GUI.
