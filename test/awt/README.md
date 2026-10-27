Run `python3 test/awt/check.py` with a JDK 25 and a built OpenLDK.
The same probes check integer RGB/ARGB/BGR pixels, bulk transfers, shared
subimages, and Java2D drawing on both runtimes. No display is required. Font family discovery uses the system Fontconfig
library (`libfontconfig.so.1`).

The rendering probe covers opaque rectangles, antialiased curves, gradients,
transforms, rectangular clips, all twelve Porter-Duff rules, and image
transfers. Rendered pixels are compared with HotSpot, allowing two levels
of premultiplied color rounding and one alpha level. OpenLDK uses the JDK's
Java drawing pipelines with CL alpha compositing; it does not load HotSpot
Java2D libraries. Font family enumeration is checked; glyph rendering remains under development.
Native span iterators for arbitrary clips,
and display-backed AWT surfaces still require support.

`FontProbe discover` checks enumeration, rather than equality of all physical
font families. Physical font/scaler compatibility remains incomplete. Running
`FontProbe` without that argument also checks glyph advances and outlines;
that part remains an explicit unfinished target.
