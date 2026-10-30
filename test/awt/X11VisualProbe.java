// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.image.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

public class X11VisualProbe {
    static final Color[] COLORS = { Color.BLACK, Color.WHITE, Color.RED, Color.GREEN, Color.BLUE };
    static final AtomicReference<Throwable> background = new AtomicReference<>();
    static final CountDownLatch painted = new CountDownLatch(1);
    static Frame frame;
    static Canvas canvas;
    public static void main(String[] args) throws Throwable {
        Thread.setDefaultUncaughtExceptionHandler((thread, error) -> {
            background.compareAndSet(null, error); error.printStackTrace();
        });
        GraphicsDevice device = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        GraphicsConfiguration[] configs = device.getConfigurations();
        if (configs.length == 0 || configs[0] != device.getDefaultConfiguration())
            throw new AssertionError("Missing default configuration");
        int choice = args.length > 1 && args[1].startsWith("config:")
            ? Integer.parseInt(args[1].substring(7)) : 0;
        GraphicsConfiguration selected = configs[choice];
        if (args.length > 1 && args[1].startsWith("config:"))
            System.out.println("X11_SELECTED " + selected + " " + selected.getColorModel().getClass().getSimpleName());
        int indexed = 0;
        for (GraphicsConfiguration config : configs) {
            ColorModel model = config.getColorModel();
            if (args.length > 0 && args[0].equals("list"))
                System.out.println("X11_VISUAL " + java.util.Arrays.asList(configs).indexOf(config)
                                   + " " + config + " " + model.getClass().getSimpleName());
            if (model == null || model.getPixelSize() <= 0) throw new AssertionError("Missing color model");
            if (model instanceof IndexColorModel) indexed++;
            for (Color color : COLORS) {
                int rgb = model.getRGB(model.getDataElements(color.getRGB(), null));
                int again = model.getRGB(model.getDataElements(rgb, null));
                if (rgb != again || (rgb >>> 24) != 255) throw new AssertionError("Unstable palette entry");
            }
            BufferedImage image = config.createCompatibleImage(8, 8);
            Graphics2D g = image.createGraphics();
            try { g.setColor(Color.WHITE); g.fillRect(0, 0, 8, 8); }
            finally { g.dispose(); }
            if ((image.getRGB(3, 3) & 0xffffff) != 0xffffff)
                throw new AssertionError("Compatible image lost white");
        }
        System.out.println("X11_CONFIGURATIONS " + configs.length + " INDEXED " + indexed);
        if (args.length > 0 && args[0].equals("screen")) {
            try { EventQueue.invokeAndWait(() -> {
                frame = new Frame("OpenLDK X11 palette probe", selected);
                canvas = new Canvas(selected) {
                    public void paint(Graphics g) {
                        for (int i = 0; i < COLORS.length; i++) {
                            g.setColor(COLORS[i]); g.fillRect(i * 40, 0, 40, getHeight());
                        }
                        if (args.length > 2 && args[2].equals("patterns")) {
                            g.setColor(new Color(0x204080)); g.fillRect(0, 40, 40, 40);
                            g.copyArea(0, 40, 40, 40, 40, 0);
                            g.setColor(Color.RED); g.fillOval(80, 40, 40, 40);
                            Graphics2D alpha = (Graphics2D) g.create();
                            try {
                                alpha.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, .5f));
                                alpha.setColor(Color.BLUE); alpha.fillRect(120, 40, 40, 40);
                            } finally { alpha.dispose(); }
                            BufferedImage image = new BufferedImage(40, 40, BufferedImage.TYPE_INT_RGB);
                            for (int y = 0; y < 40; y++) for (int x = 0; x < 40; x++)
                                image.setRGB(x, y, 0x80a0c0);
                            g.drawImage(image, 160, 40, null);
                        }
                        Toolkit.getDefaultToolkit().sync(); painted.countDown();
                    }
                };
                canvas.setPreferredSize(new Dimension(200, 80));
                frame.add(canvas); frame.pack(); frame.setLocation(20, 30); frame.setVisible(true);
            }); } catch (java.lang.reflect.InvocationTargetException error) { throw error.getCause(); }
            try {
                Robot robot = new Robot();
                // Drain native configure/expose events as well as the Java
                // queue before using getLocationOnScreen on a new window.
                Class.forName("sun.awt.SunToolkit").getMethod("realSync").invoke(Toolkit.getDefaultToolkit());
                if (!painted.await(30, TimeUnit.SECONDS)) throw new AssertionError("Missing palette paint");
                robot.waitForIdle();
                // Paint the complete canvas after initial expose processing.
                // The first paint callback may have only a partial dirty clip.
                EventQueue.invokeAndWait(() -> {
                    Graphics graphics = canvas.getGraphics();
                    try { canvas.paint(graphics); }
                    finally { graphics.dispose(); }
                    Toolkit.getDefaultToolkit().sync();
                });
                robot.waitForIdle();
                Point p = canvas.getLocationOnScreen();
                if (args.length > 1 && args[1].equals("hold")) {
                    System.out.println("PALETTE_READY " + p.x + " " + p.y); System.out.flush();
                    System.in.read();
                }
                for (int i = 0; i < COLORS.length; i++) {
                    Color actual = robot.getPixelColor(p.x + i * 40 + 20, p.y + 20);
                    if (args.length > 1 && args[1].startsWith("config:")) {
                        System.out.println("PIXEL " + i + " " + (actual.getRGB() & 0xffffff));
                        continue;
                    }
                    if ((actual.getRGB() & 0xffffff) != (COLORS[i].getRGB() & 0xffffff))
                        throw new AssertionError("Screen color " + i + ": " + actual);
                }
                if (args.length > 2 && args[2].equals("patterns")) {
                    for (int i = 0; i < COLORS.length; i++)
                        System.out.println("PATTERN " + i + " "
                            + (robot.getPixelColor(p.x + i * 40 + 20, p.y + 60).getRGB() & 0xffffff));
                }
            } finally { EventQueue.invokeAndWait(() -> frame.dispose()); }
        }
        if (background.get() != null) throw new AssertionError("Background failure", background.get());
        System.out.println("X11 visual models, compatible images, and palette pixels passed");
    }
}
