// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.image.*;

public class IndexedGraphicsProbe {
    static void fill(IndexColorModel model, int color) {
        BufferedImage image = new BufferedImage(model, model.createCompatibleWritableRaster(8, 4), false, null);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setComposite(AlphaComposite.Src);
            graphics.setColor(new Color(color, true)); graphics.fillRect(0, 0, 8, 4);
        } finally { graphics.dispose(); }
        for (int y = 0; y < 4; y++) for (int x = 0; x < 8; x++)
            if (image.getRGB(x, y) != color)
                throw new AssertionError("Indexed pixel: " + Integer.toHexString(image.getRGB(x, y))
                                         + " expected " + Integer.toHexString(color));
    }
    public static void main(String[] args) {
        byte[] ramp = new byte[256];
        for (int i = 0; i < 256; i++) ramp[i] = (byte)i;
        IndexColorModel gray = new IndexColorModel(8, 256, ramp, ramp, ramp);
        fill(gray, 0xff808080); fill(gray, 0xffffffff);
        byte[] zero = { 0, 0 }, extremes = { 0, (byte)255 };
        fill(new IndexColorModel(8, 2, extremes, zero, zero), 0xffff0000);
        IndexColorModel transparent = new IndexColorModel(8, 2, extremes, extremes, extremes, extremes);
        fill(transparent, 0xffffffff); fill(transparent, 0);
        System.out.println("Indexed grayscale, colored, and transparent rendering passed");
    }
}
