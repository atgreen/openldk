// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class X11RobotProbe {
    static Frame frame;
    static Button button;
    static AtomicInteger pressed = new AtomicInteger(), released = new AtomicInteger(), typed = new AtomicInteger();
    static AtomicReference<Throwable> background = new AtomicReference<>();
    static void checkKey(boolean correct) {
        if (!correct) background.compareAndSet(null, new AssertionError("Incorrect key event"));
    }
    public static void main(String[] args) throws Exception {
        Thread.setDefaultUncaughtExceptionHandler((t, e) -> { background.set(e); e.printStackTrace(); });
        EventQueue.invokeAndWait(() -> {
            frame = new Frame("OpenLDK Robot input probe");
            button = new Button("Keyboard target");
            button.addFocusListener(new FocusAdapter() {
                public void focusGained(FocusEvent e) { System.out.println("FOCUS_GAINED"); }
                public void focusLost(FocusEvent e) { System.out.println("FOCUS_LOST"); }
            });
            button.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent e) { System.out.println("KEY_PRESSED " + e.getKeyCode()); checkKey(e.getKeyCode() == KeyEvent.VK_A); pressed.incrementAndGet(); }
                public void keyReleased(KeyEvent e) { System.out.println("KEY_RELEASED " + e.getKeyCode()); checkKey(e.getKeyCode() == KeyEvent.VK_A); released.incrementAndGet(); }
                public void keyTyped(KeyEvent e) { System.out.println("KEY_TYPED " + (int)e.getKeyChar()); checkKey(e.getKeyChar() == 'a'); typed.incrementAndGet(); }
            });
            frame.add(button); frame.setBounds(30, 40, 200, 120); frame.setVisible(true);
        });
        try {
            Robot robot = new Robot(); robot.waitForIdle(); robot.delay(250);
            Point p = button.getLocationOnScreen();
            robot.mouseMove(p.x + button.getWidth() / 2, p.y + button.getHeight() / 2);
            robot.mousePress(InputEvent.BUTTON1_DOWN_MASK); robot.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
            EventQueue.invokeAndWait(() -> button.requestFocus());
            robot.waitForIdle(); robot.delay(250);
            System.out.println("FOCUS_OWNER " + button.isFocusOwner() + " " + KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner());
            robot.keyPress(KeyEvent.VK_A); robot.keyRelease(KeyEvent.VK_A);
            robot.waitForIdle(); robot.delay(250);
            System.out.println("KEY_COUNTS " + pressed.get() + " " + released.get() + " " + typed.get());
            if (background.get() != null) throw new AssertionError("Background failure", background.get());
            // Lazy compilation may leave the key held long enough for normal
            // server autorepeat. Every event must still identify the right key.
            if (pressed.get() < 1 || released.get() < 1 || typed.get() < 1)
                throw new AssertionError("Missing real keyboard events");
            int pcount = pressed.get(), rcount = released.get(), tcount = typed.get();
            robot.delay(750); robot.waitForIdle();
            if (pressed.get() != pcount || released.get() != rcount || typed.get() != tcount)
                throw new AssertionError("Keyboard events continued after key release");
        } finally { EventQueue.invokeAndWait(() -> frame.dispose()); }
        if (background.get() != null) throw new AssertionError("Disposal failure", background.get());
        System.out.println("X11 Robot input passed");
    }
}
