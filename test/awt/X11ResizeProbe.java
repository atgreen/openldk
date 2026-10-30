// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.util.concurrent.atomic.AtomicReference;

public class X11ResizeProbe {
    static final AtomicReference<Throwable> failure = new AtomicReference<>();
    static Frame frame;
    static ScrollPane scroll;
    static Canvas canvas;
    static volatile Color color;

    static void sync() throws Exception {
        Class.forName("sun.awt.SunToolkit").getMethod("realSync").invoke(Toolkit.getDefaultToolkit());
    }

    public static void main(String[] args) throws Throwable {
        Thread.setDefaultUncaughtExceptionHandler((thread, error) -> {
            failure.compareAndSet(null, error);
            error.printStackTrace();
        });
        EventQueue.invokeAndWait(() -> {
            frame = new Frame("OpenLDK resize probe");
            scroll = new ScrollPane(ScrollPane.SCROLLBARS_ALWAYS);
            canvas = new Canvas() {
                public void paint(Graphics graphics) {
                    graphics.setColor(color == null ? Color.RED : color);
                    graphics.fillRect(0, 0, getWidth(), getHeight());
                }
            };
            scroll.add(canvas);
            frame.add(scroll);
            frame.setSize(120, 100);
            frame.setLocation(20, 30);
            frame.setVisible(true);
        });
        try {
            Robot robot = new Robot();
            int[][] sizes = {{200, 160}, {300, 240}, {120, 100}, {220, 180}};
            Color[] colors = {Color.RED, Color.GREEN, Color.BLUE, Color.CYAN};
            for (int i = 0; i < sizes.length; i++) {
                final int step = i;
                EventQueue.invokeAndWait(() -> {
                    color = colors[step];
                    canvas.setPreferredSize(new Dimension(sizes[step][0], sizes[step][1]));
                    frame.setSize(sizes[step][0], sizes[step][1]);
                    frame.validate();
                });
                sync();
                EventQueue.invokeAndWait(() -> {
                    Graphics graphics = canvas.getGraphics();
                    try { canvas.paint(graphics); }
                    finally { graphics.dispose(); }
                    Toolkit.getDefaultToolkit().sync();
                });
                sync();
                Point origin = canvas.getLocationOnScreen();
                Dimension viewport = scroll.getViewportSize();
                for (Point point : new Point[] {new Point(5, 5),
                        new Point(viewport.width - 6, viewport.height - 6)}) {
                    Color actual = robot.getPixelColor(origin.x + point.x, origin.y + point.y);
                    if (!actual.equals(colors[i]))
                        throw new AssertionError("Resize step " + i + " at " + point + ": " + actual);
                }
                if (failure.get() != null) throw new AssertionError("Background painting failed", failure.get());
                System.out.println("X11_RESIZE_STEP " + i);
                System.out.flush();
            }
        } catch (java.lang.reflect.InvocationTargetException error) {
            throw error.getCause();
        } finally {
            EventQueue.invokeAndWait(() -> frame.dispose());
        }
        if (failure.get() != null) throw new AssertionError("Background failure", failure.get());
        System.out.println("X11 backing rasters follow grow and shrink operations passed");
    }
}
