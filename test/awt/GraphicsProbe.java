// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;

public class GraphicsProbe {
    public static void main(String[] args) throws Exception {
        BufferedImage image = new BufferedImage(80, 64, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = image.createGraphics();
        g.setColor(Color.RED);
        g.fillRect(2, 3, 4, 5);
        if (image.getRGB(3, 4) != 0xffff0000 || image.getRGB(0, 0) != 0)
            throw new AssertionError("opaque rectangle");
        System.out.println("Graphics2D rectangle passed");
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(new Color(0, 0, 255, 128));
        g.fill(new Ellipse2D.Double(1.5, 1.5, 15, 12));
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, .6f));
        g.setColor(new Color(0, 255, 0, 190));
        g.fillRect(6, 6, 10, 10);
        g.setComposite(AlphaComposite.SrcOver);
        g.translate(23, 5);
        g.scale(1.5, 1.25);
        g.setClip(0, 0, 20, 20);
        g.setPaint(new GradientPaint(0, 0, Color.RED, 15, 15, Color.BLUE));
        Path2D path = new Path2D.Double();
        path.moveTo(-5, 2); path.curveTo(10, -8, 28, 4, 18, 19);
        path.lineTo(0, 24); path.closePath();
        g.fill(path);
        g.dispose();
        System.out.println("Graphics2D alpha, antialiasing, transform, clip, and gradient passed");
        Graphics2D rules = image.createGraphics();
        rules.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        for (int rule = 1; rule <= 12; rule++) {
            int x = (rule - 1) * 6 + 1;
            rules.setComposite(AlphaComposite.Src);
            rules.setColor(new Color(40, 180, 90, 130));
            rules.fillRect(x, 40, 5, 16);
            rules.setComposite(AlphaComposite.getInstance(rule, .65f));
            rules.setColor(new Color(210, 60, 120, 170));
            rules.fill(new Ellipse2D.Double(x + .25, 40.5, 4.5, 15));
        }
        rules.dispose();
        BufferedImage copy = new BufferedImage(80, 64, BufferedImage.TYPE_INT_ARGB);
        Graphics2D target = copy.createGraphics();
        target.drawImage(image, 0, 0, null);
        target.dispose();
        if (copy.getRGB(3, 4) != image.getRGB(3, 4)) throw new AssertionError("image transfer");
        ByteBuffer pixels = ByteBuffer.allocate(80 * 64 * 4);
        for (int y = 0; y < 64; y++) for (int x = 0; x < 80; x++) pixels.putInt(copy.getRGB(x, y));
        Files.write(Path.of(args[0]), pixels.array());
        System.out.println("Graphics2D image transfer passed");
    }
}
