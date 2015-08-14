# Packages of texonomy

## texonomy-util
### util
Utility function of back-end, generic helper functions will be here, mostly
about arithmetics of matrices, vectors and helper structure generating
functions. Components will be utilized by `texonomy-cs`, `texonomy-graph` and
`texonomy-recognition`.

### sparse
Sparse vector utilities and functions, designed to save space (maybe time)
when meet with compressed sensing problems. Components will be utilized
by `texonomy-cs`.

## texonomy-cs
### solver
General compressed sensing solver and helper functions, here I choose
stagewise orthogonal matching pursuit by David Donoho. Components will be
utilized by `texonomy-classify`.

## texonomy-graphic
### graphic
Graphic manipulation routines, in order to generate distorted samples if
needed, or resize our samples in back-end. Components will be utilized by
`texonomy-recognition`.

## texonomy-recognition
### classify
Classification and recognition system, core of recognition process,
will have a score system and corresponding statistics. Components will be
utilized by `texonomy-server`.

## texonomy-server
### server
Server module, used to communicate with front-end.
