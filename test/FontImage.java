// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;

public class FontImage {
    static Font font;
    static FontRenderContext context;
    static GlyphVector saved;
    static double advance;
    static Rectangle pixels;
    public static void main(String[] args) {
        System.setProperty("java.awt.headless", "true");
        if (saved == null) {
            font = new Font("SansSerif", Font.PLAIN, 16);
            context = new FontRenderContext(new AffineTransform(), true, true);
            saved = font.createGlyphVector(context, "Ab");
            advance = saved.getGlyphPosition(2).getX();
            pixels = saved.getPixelBounds(context, 0, 0);
            if (advance <= 0 || pixels.width <= 0 || saved.getOutline().getBounds2D().getWidth() <= 0)
                throw new AssertionError("Font warmup");
            System.out.println("Font image prepared");
        } else {
            if (saved.getGlyphPosition(2).getX() != advance
                    || !saved.getPixelBounds(context, 0, 0).equals(pixels))
                throw new AssertionError("Saved glyph cache changed");
            GlyphVector fresh = font.createGlyphVector(context, "Cd");
            if (fresh.getGlyphPosition(2).getX() <= 0 || fresh.getOutline().getBounds2D().getWidth() <= 0
                    || fresh.getPixelBounds(context, 0, 0).width <= 0)
                throw new AssertionError("Restored font scaler could not render new glyphs");
            System.out.println("Font image passed");
        }
    }
}
