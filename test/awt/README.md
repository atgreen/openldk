Run `python3 test/awt/check.py` with a JDK 25 and a built OpenLDK.
The same probes check integer RGB/ARGB/BGR pixels, bulk transfers, shared
subimages, and Java2D drawing on both runtimes. No display is required.
Font family discovery uses the system Fontconfig library (`libfontconfig.so.1`).

The rendering probe covers opaque rectangles, antialiased curves, gradients,
transforms, rectangular clips, all twelve Porter-Duff rules, and image
transfers. Rendered pixels are compared with HotSpot, allowing two levels
of premultiplied color rounding and one alpha level. OpenLDK uses the JDK's
Java drawing pipelines with CL alpha compositing; it does not load HotSpot
Java2D libraries. Font glyph advances, outlines, and bounds are checked
against the JDK.
Convolution checks compare 11,970 samples from RGB, straight/premultiplied
alpha, byte-gray, and ushort-gray images and grayscale rasters. They cover
asymmetric, even-sized, sharpening, and blur kernels with both edge modes,
allowing two sample levels for native integer-kernel rounding. The CL loops
retain JDK 25's signed 16-bit convolution behavior for ushort storage.
Filtered image production, pixel grabbing, and Toolkit image materialization
are checked separately; these are used by SVG filter pipelines.
Complex clips use CL scanline spans over flattened Java paths, with normalized
coordinates, both winding rules, and clipped alpha tiles. Eight image frames
check curves, holes, polygons, transforms, and antialiased painting against the
same JDK pixel tolerances. Complex text shaping and display-backed AWT surfaces
still require support.

`FontProbe discover` checks only enumeration. The default probe also checks
glyph advances and outline bounds; these are compared to the JDK within half
a pixel. The scaler uses the system Cairo FreeType backend (`libcairo.so.2`),
with Java font files and CL-owned glyph-cache memory.

`make check-font-image` checks cached glyph bounds and new glyph rendering
after saving and restarting an application image.
