// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.image.BufferedImage;

public class GlyphMaskProbe {
    public static void main(String[] args) {
        BufferedImage image = new BufferedImage(80, 60, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(Color.BLUE); graphics.fillRect(0, 0, 80, 60);
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
            graphics.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_OFF);
            graphics.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 24));
            graphics.setColor(Color.BLACK); graphics.drawString("|", 10, 35);
        } finally { graphics.dispose(); }
        int top = 60, bottom = -1, left = 80, right = -1;
        for (int y = 0; y < 60; y++) for (int x = 0; x < 80; x++) {
            if (image.getRGB(x, y) != Color.BLUE.getRGB()) {
                top = Math.min(top, y); bottom = Math.max(bottom, y);
                left = Math.min(left, x); right = Math.max(right, x);
            }
        }
        if (bottom - top < 8 || right < left) throw new AssertionError("Missing vertical glyph");
        // Interior rows of a straight vertical stroke must agree. Padding
        // interpreted as coverage breaks the stroke into shifting fragments.
        for (int y = top + 3; y <= bottom - 3; y++)
            for (int x = left; x <= right; x++)
                if (image.getRGB(x, y) != image.getRGB(x, top + 2))
                    throw new AssertionError("Corrupted glyph row " + y + " at column " + x);
        System.out.println("Glyph bitmap rows render continuous vertical strokes passed");
    }
}
