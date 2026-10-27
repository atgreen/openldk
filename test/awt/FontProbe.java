// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;

public class FontProbe {
    public static void main(String[] args) {
        String[] families = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
        if (families.length < 5) throw new AssertionError("font families");
        System.out.println("Font family discovery passed: " + families.length);
        if (args.length > 1 && args[1].equals("list"))
            for (String family : families) System.out.println("FAMILY " + family);
        if (args.length > 0 && args[0].equals("discover")) return;
        Font font = new Font("SansSerif", Font.PLAIN, 16);
        FontRenderContext context = new FontRenderContext(new AffineTransform(), true, true);
        GlyphVector glyphs = font.createGlyphVector(context, "Ab");
        if (glyphs.getNumGlyphs() != 2 || glyphs.getGlyphPosition(2).getX() <= 0
                || glyphs.getOutline().getBounds2D().getWidth() <= 0)
            throw new AssertionError("glyph advances or outlines");
        var bounds = glyphs.getOutline().getBounds2D();
        System.out.println("FONT_METRICS " + glyphs.getGlyphPosition(2).getX() + " "
                + bounds.getX() + " " + bounds.getY() + " " + bounds.getWidth() + " " + bounds.getHeight());
        System.out.println("Font glyph advances and outlines passed");
    }
}
