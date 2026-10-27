Run `python3 test/awt/check.py` with a JDK 25 and a built OpenLDK.
The same probes check integer RGB/ARGB/BGR pixels, bulk transfers, shared
subimages, and Java2D drawing on both runtimes. No display is required.

The rendering probe covers opaque rectangles, antialiased curves, gradients,
transforms, rectangular clips, all twelve Porter-Duff rules, and image
transfers. Rendered pixels are compared with HotSpot, allowing two levels
of premultiplied color rounding and one alpha level. OpenLDK uses the JDK's
Java drawing pipelines with CL alpha compositing; it does not load HotSpot
Java2D libraries. Font rendering, native span iterators for arbitrary clips,
and display-backed AWT surfaces still require support.
