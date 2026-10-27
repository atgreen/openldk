Run `python3 test/awt/check.py` with a JDK 25 and a built OpenLDK.
The same probe checks integer RGB/ARGB/BGR buffered-image pixel access,
bulk pixel transfers, and shared subimage rasters on both runtimes.
No display is required.

This covers Java image storage, not Java2D drawing. Graphics2D rendering
still needs runtime support before Eclipse's JSVG rasterizer can draw SVG
icons without HotSpot native libraries.
