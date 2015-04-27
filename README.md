# Coriolis

alter a set of values by a given amount maintaining their relation to each other, i.e. rotate the underlying param spaces.

While the core of this library is mathematic, one application is a bridge between the functional pattern DSL tidal and its sampler dirt.

coriolis sits in between both and allows rotating a set of params by a given angle possibly drastically changing the resulting sounds. As an analogy, Tidal allows for fine grained control over each param of each channel in each time slice like working with pliers, coriolis is a wrench.

## Disclaimer

still __experimental__ software

## Usage with tidal

add

```emacs
(tidal-send-string "import Math.Coriolis.Coriolis")
```

within `tidal-start-haskell` of your `tidal.el` file, then start up tidal and try out the following.

```haskell
b <- dirtBridge 7777
r <- rotatorStream "127.0.0.1" 7777


b $ every 2 ((within (0.5, 1.0) ((|+| delayfeedback "0.8") . (|+| speed "0.3") . (|+| coarse "8" ) . (|+| delay "0.4")))) $
  every 3 (within (0.0, 0.5) (|+| crush "-3")) $
  sound "bd sn"
  |+| delayfeedback "0.5"
  |+| delay "0.9"
  |+| crush "6"
  |+| speed "0.2"
  |+| cutoff "0.001"
  |+| resonance "0.9"

b silence

r 245 ["crush", "delay", "delayfeedback"]

r 130 ["speed", "delay", "crush"]

r 130 ["speed", "delayfeedback", "crush"]

```

# Current Limitations

- only R^3 is implemented as a rotational space but n-dimensional rotation might be possible.
- running rotation within a `clockedTick` to change rotation over time, fails after short time