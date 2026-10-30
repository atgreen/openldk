// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class X11WindowProbe {
    static final AtomicReference<Throwable> backgroundFailure = new AtomicReference<>();
    static final CountDownLatch opened = new CountDownLatch(1), closed = new CountDownLatch(1);
    static Frame window;
    public static void main(String[] args) throws Exception {
        Thread.setDefaultUncaughtExceptionHandler((thread, failure) -> backgroundFailure.compareAndSet(null, failure));
        BufferedReader commands = new BufferedReader(new InputStreamReader(System.in));
        EventQueue.invokeAndWait(() -> {
            window = new Frame("OpenLDK X11 window probe");
            window.setBounds(20, 30, 200, 160);
            window.addWindowListener(new WindowAdapter() {
                public void windowOpened(WindowEvent event) { opened.countDown(); }
                public void windowClosed(WindowEvent event) { closed.countDown(); }
            });
            window.setVisible(true);
        });
        try {
            if (!opened.await(10, TimeUnit.SECONDS)) throw new AssertionError("Missing windowOpened event");
            EventQueue.invokeAndWait(() -> {
                Graphics2D graphics = (Graphics2D) window.getGraphics();
                if (graphics == null) throw new AssertionError("No window graphics");
                try {
                    graphics.setColor(new Color(0x123456)); graphics.fillRect(0, 0, 200, 160);
                    graphics.setColor(new Color(0xe05020)); graphics.fillRect(30, 30, 100, 90);
                    graphics.setColor(new Color(0x2060e0)); graphics.fillRect(70, 60, 50, 40);
                    graphics.copyArea(30, 30, 20, 20, 110, 20);
                    java.awt.image.VolatileImage offscreen = window.createVolatileImage(20, 20);
                    if (offscreen == null) throw new AssertionError("No volatile image");
                    try {
                        if (offscreen.validate(window.getGraphicsConfiguration())
                            == java.awt.image.VolatileImage.IMAGE_INCOMPATIBLE)
                            throw new AssertionError("Incompatible volatile image");
                        Graphics2D imageGraphics = offscreen.createGraphics();
                        try {
                            imageGraphics.setColor(new Color(0x20c040));
                            imageGraphics.fillRect(0, 0, 20, 20);
                        } finally { imageGraphics.dispose(); }
                        if (!graphics.drawImage(offscreen, 145, 20, null) || offscreen.contentsLost())
                            throw new AssertionError("Volatile image transfer failed");
                    } finally { offscreen.flush(); }
                    graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, .5f));
                    graphics.setColor(Color.WHITE); graphics.fillRect(140, 110, 30, 30);
                } finally { graphics.dispose(); }
                Toolkit.getDefaultToolkit().sync();
            });
            if (backgroundFailure.get() != null) throw new AssertionError("Toolkit failure", backgroundFailure.get());
            System.out.println("X11_WINDOW_READY"); System.out.flush();
            String command = commands.readLine();
            if (!"close".equals(command)) throw new AssertionError("Missing close command");
        } finally { EventQueue.invokeAndWait(() -> window.dispose()); }
        if (!closed.await(10, TimeUnit.SECONDS)) throw new AssertionError("Missing windowClosed event");
        if (backgroundFailure.get() != null) throw new AssertionError("Toolkit failure", backgroundFailure.get());
        Toolkit.getDefaultToolkit().sync();
        System.out.println("X11_WINDOW_CLOSED"); System.out.flush();
        if (!"exit".equals(commands.readLine())) throw new AssertionError("Missing exit command");
        System.out.println("X11 window rendering, events, and disposal passed");
    }
}
