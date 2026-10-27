// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.svg.JSVGRasterizer;

public class SVGProbe {
    public static void main(String[] args) {
        String svg = "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='20'>"
            + "<defs><linearGradient id='g'><stop stop-color='#0000ff'/>"
            + "<stop offset='1' stop-color='#00ff00'/></linearGradient></defs>"
            + "<rect x='2' y='2' width='8' height='8' fill='#ff0000'/>"
            + "<path d='M12 2 C22 0 25 15 12 18 Z' fill='url(#g)' opacity='.6'/></svg>";
        ImageData image = new JSVGRasterizer().rasterizeSVG(
                new ByteArrayInputStream(svg.getBytes(StandardCharsets.UTF_8)), 100);
        RGB red = image.palette.getRGB(image.getPixel(5, 5));
        if (image.width != 24 || image.height != 20 || red.red != 255
                || red.green != 0 || red.blue != 0 || image.getAlpha(5, 5) != 255
                || image.getAlpha(0, 0) != 0 || image.getAlpha(15, 8) < 100)
            throw new AssertionError("SVG pixels");
        ImageData scaled = new JSVGRasterizer().rasterizeSVG(
                new ByteArrayInputStream(svg.getBytes(StandardCharsets.UTF_8)), 48, 40);
        if (scaled.width != 48 || scaled.height != 40 || scaled.getAlpha(10, 10) != 255)
            throw new AssertionError("scaled SVG");
        String text = "<svg xmlns='http://www.w3.org/2000/svg' width='32' height='20'>"
            + "<text x='1' y='15' font-family='sans-serif' font-size='14'>Ab</text></svg>";
        ImageData letters = new JSVGRasterizer().rasterizeSVG(
                new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8)), 100);
        int ink = 0;
        for (int y = 0; y < letters.height; y++) for (int x = 0; x < letters.width; x++)
            if (letters.getAlpha(x, y) > 0) ink++;
        if (ink < 20) throw new AssertionError("SVG text glyphs were not rendered");
        String blur = "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24'>"
            + "<defs><filter id='b' x='0' y='0' width='24' height='24' filterUnits='userSpaceOnUse'>"
            + "<feGaussianBlur stdDeviation='1'/></filter></defs>"
            + "<rect x='8' y='8' width='8' height='8' fill='red' filter='url(#b)'/></svg>";
        ImageData blurred = new JSVGRasterizer().rasterizeSVG(
                new ByteArrayInputStream(blur.getBytes(StandardCharsets.UTF_8)), 100);
        if (blurred.getAlpha(7, 12) < 10 || blurred.getAlpha(12, 12) < 180
                || blurred.getAlpha(0, 0) != 0 || blurred.getAlpha(8, 12) >= 250)
            throw new AssertionError("SVG Gaussian blur was not rendered");
        System.out.println("SWT JSVG paths, alpha, gradient, scaling, text, and blur passed");
    }
}
