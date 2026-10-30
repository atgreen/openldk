// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.image.BufferedImage;

public class X11CaptureProbe {
    public static void main(String[] args) throws Exception {
        int x = Integer.parseInt(args[0]), y = Integer.parseInt(args[1]);
        int width = Integer.parseInt(args[2]), height = Integer.parseInt(args[3]);
        BufferedImage image = new Robot().createScreenCapture(new Rectangle(x, y, width, height));
        for (int row = 0; row < height; row++) {
            StringBuilder line = new StringBuilder();
            for (int column = 0; column < width; column++) {
                if (column != 0) line.append(',');
                line.append(Integer.toHexString(image.getRGB(column, row) & 0xffffff));
            }
            System.out.println(line);
        }
    }
}
