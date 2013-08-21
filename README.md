hFlyCapture
===========

Haskell bindings to FlyCapture2 SDK from Point Grey

Tested with flycapture 2.5.2.3 from www.ptgrey.com/support/downloads


Usage Notes
===========

See examples/ for usage demos.

ghc-mod: To help ghc-mod see through 'missing modules' in the imports list, we manually turn FlyCapBase.hsc into FlyCapBase.hs (hsc2hs FlyCapBase.hsc -I /usr/include/flycapture/C).  The manual conversion might fall behind changes we make to FlyCapBase.hsc, and that might interfere with code hilighting.