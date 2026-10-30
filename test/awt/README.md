Run `python3 test/awt/check.py` with a JDK 25 and a built OpenLDK.
The same probes check integer RGB/ARGB/BGR pixels, bulk transfers, shared
subimages, and Java2D drawing on both runtimes. No display is required.
Font family discovery uses the system Fontconfig library (`libfontconfig.so.1`).

`ComponentProbe` checks copied geometry, event sources, containment, and
BorderLayout calculations in both headless modes. Its components have no
native peers, so the graphical-mode class initializers can be checked without
connecting to a display server. The same suite still requires no display.
The component and widget `initIDs` entry points only cache field/method IDs
for native peers (many are empty in OpenJDK's Unix implementation). OpenLDK's
CLOS storage accesses fields directly, so these initializers need no native
cache. The source duties are visible in OpenJDK's
[XToolkit initializers](https://github.com/openjdk/jdk25u/blob/master/src/java.desktop/unix/native/libawt_xawt/xawt/XToolkit.c)
and [shared Unix initializers](https://github.com/openjdk/jdk25u/blob/master/src/java.desktop/unix/native/libawt/awt/initIDs.c).

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

For display-backed checks, start an isolated X server such as Xvfb and run:

```sh
python3 test/awt/check-x11.py --display :4 --runtime ./openldk
python3 test/awt/check-x11.py --display :5 --runtime ./openldk --all-visuals
```

The first command checks actual server pixels, window and Robot input events,
disposal, primary-selection ownership and local Unicode string contents,
image-save preflight, callback recovery, and screen capture. The selection
probe exercises the real X11 selection initialization rather than a private
Java clipboard. The second paints and captures each advertised visual, comparing color-model types
and primary-color samples with JDK 25 by physical visual ID. A full StaticGray
ramp additionally checks custom colors, copying, curves, alpha, and image
uploads exactly. Other indexed palettes can allocate different legal colors;
those extra samples are logged for inspection. Enumeration order can differ
between runtimes. An 8-bit server exercises private colormaps, grayscale and
byte TrueColor; a 24-bit server checks ordinary TrueColor. Testing every visual
can take longer on servers advertising many configurations.

Run the foreign-client capture fixture separately with:

```sh
python3 test/awt/check-capture.py --display :5 --runtime ./openldk
```

It checks all 16,800 pixels of overlapping windows with private colormaps,
borders, children clipped by their parent, and an L-shaped bounding region.
Expected pixels come from the independently defined fixture geometry. Both
runtimes must match rectangular-window expectations. For shaped windows,
OpenLDK must match the geometry; JDK reference differences are reported because
its [multiVis capture implementation](https://github.com/openjdk/jdk/blob/jdk-25-ga/src/java.desktop/unix/native/libawt_xawt/awt/multiVis.c)
uses rectangular window regions without consulting XShape. The fixture uses
ordinary system Xlib/Xext, and does not load JDK native libraries into OpenLDK.
