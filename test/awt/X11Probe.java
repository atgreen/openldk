// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
public class X11Probe {
    static volatile Throwable backgroundFailure;
    public static void main(String[] args) throws Exception {
        Thread.setDefaultUncaughtExceptionHandler((thread, failure) -> backgroundFailure = failure);
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] screens = ge.getScreenDevices();
        if (screens.length == 0) throw new AssertionError("No screens");
        GraphicsDevice device = ge.getDefaultScreenDevice();
        GraphicsConfiguration gc = device.getDefaultConfiguration();
        Rectangle bounds = gc.getBounds();
        if (bounds.width <= 0 || bounds.height <= 0) throw new AssertionError(bounds);
        if (gc.getDevice() != device) throw new AssertionError("Device identity");
        System.out.println("X11 screen: " + bounds.width + "x" + bounds.height);
        if (args.length > 0 && args[0].equals("colors")) {
            java.awt.image.ColorModel model = gc.getColorModel();
            if (model == null || model.getPixelSize() <= 0)
                throw new AssertionError("Missing screen color model");
            int rgb = model.getRGB(model.getDataElements(0xff123456, null));
            for (int shift : new int[] { 0, 8, 16 })
                if (Math.abs(((rgb >>> shift) & 255) - ((0xff123456 >>> shift) & 255)) > 8)
                    throw new AssertionError("Color round trip: " + Integer.toHexString(rgb));
            System.out.println("X11 color model: " + model.getPixelSize() + " bits, "
                               + model.getClass().getSimpleName());
        }
        if (args.length > 0 && args[0].equals("frame")) {
            Frame frame = new Frame("OpenLDK X11");
            frame.setBounds(10, 20, 120, 90);
            if (frame.getWidth() != 120) throw new AssertionError("Frame geometry");
            if (Boolean.getBoolean("openldk.probe.show")) {
                try {
                    frame.setVisible(true);
                    if (!frame.isShowing()) throw new AssertionError("Frame is not showing");
                    Thread.sleep(100);
                } finally { frame.dispose(); }
            }
            frame.dispose();
            Thread.sleep(100);
            System.out.println("X11 frame construction passed");
        }
        if (backgroundFailure != null) throw new AssertionError("Toolkit background failure", backgroundFailure);
    }
}
