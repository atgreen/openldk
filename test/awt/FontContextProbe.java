// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.image.BufferedImage;

public class FontContextProbe {
    public static void main(String[] args) {
        BufferedImage image = new BufferedImage(80, 40, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            // Read defaults before touching RenderingHints constants, which
            // would otherwise initialize the table and conceal the regression.
            FontRenderContext initial = graphics.getFontRenderContext();
            if (initial.getAntiAliasingHint() == null || initial.getFractionalMetricsHint() == null)
                throw new AssertionError("Missing default font hints");
            if (initial.isAntiAliased() || initial.usesFractionalMetrics())
                throw new AssertionError("Unexpected default font hints");
            if (graphics.getFontMetrics().stringWidth("OpenLDK") <= 0)
                throw new AssertionError("Missing default font metrics");
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            if (!graphics.getFontRenderContext().isAntiAliased() || initial.isAntiAliased())
                throw new AssertionError("Font context cache was not refreshed");
        } finally { graphics.dispose(); }
        System.out.println("Default font contexts, metrics, and hint updates passed");
    }
}
