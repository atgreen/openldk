// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.geom.*;
import java.awt.image.BufferedImage;
import java.io.*;

public class ClipProbe {
    public static void main(String[] args) throws Exception {
        Path2D hole = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        hole.append(new Rectangle2D.Double(2, 2, 55, 40), false);
        hole.append(new Ellipse2D.Double(12, 7, 30, 28), false);
        Path2D curves = new Path2D.Float();
        curves.moveTo(3, 6);
        curves.curveTo(18, -7, 60, 5, 47, 34);
        curves.quadTo(15, 50, 3, 6);
        Shape[] shapes = {new Ellipse2D.Double(3.25, 2.5, 45.5, 32.25), hole, curves,
                new Polygon(new int[]{-8, 54, 44, 8}, new int[]{8, 3, 42, 35}, 4)};
        try (DataOutputStream output = new DataOutputStream(new FileOutputStream(args[0]))) {
            for (Shape shape : shapes) for (boolean transformed : new boolean[]{false, true}) {
                BufferedImage image = new BufferedImage(64, 48, BufferedImage.TYPE_INT_ARGB);
                Graphics2D g = image.createGraphics();
                g.setComposite(AlphaComposite.Src);
                if (transformed) {
                    g.translate(5, 2);
                    g.rotate(.13, 25, 20);
                }
                g.setClip(shape);
                g.clip(new Rectangle2D.Double(1, 1, 59, 44));
                g.setTransform(new AffineTransform());
                g.setColor(new Color(20, 160, 70));
                g.fillRect(0, 0, 64, 48);
                g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g.setComposite(AlphaComposite.SrcOver);
                g.setColor(new Color(170, 40, 210, 140));
                g.fill(new Ellipse2D.Double(0.5, 1.5, 57, 39));
                g.dispose();
                int ink = 0;
                for (int y = 0; y < 48; y++) for (int x = 0; x < 64; x++) {
                    int pixel = image.getRGB(x, y);
                    if ((pixel >>> 24) != 0) ink++;
                    output.writeInt(pixel);
                }
                if (ink < 100 || image.getRGB(63, 47) != 0)
                    throw new AssertionError("Complex clip coverage");
            }
        }
        System.out.println("Curved, holed, polygonal, transformed, and antialiased clips passed");
    }
}
