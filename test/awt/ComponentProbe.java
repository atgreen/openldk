// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.event.*;
public class ComponentProbe {
    static int checks;
    static void check(boolean value) {
        checks++;
        if (!value) throw new AssertionError("check " + checks);
    }
    static class Box extends Component {
        final Dimension preferred;
        Box(int w, int h) { preferred = new Dimension(w, h); }
        public Dimension getPreferredSize() { return new Dimension(preferred); }
    }
    public static void main(String[] args) {
        Dimension d = new Dimension(2, 3);
        d.setSize(3.2, 4.8); check(d.equals(new Dimension(4, 5)));
        Dimension copy = new Dimension(d); copy.width = 9; check(d.width == 4);
        Insets insets = new Insets(1, 2, 3, 4);
        Insets clone = (Insets)insets.clone(); clone.left = 8; check(insets.left == 2);
        Box box = new Box(10, 20);
        box.setBounds(2, 3, 40, 50);
        check(box.getBounds().equals(new Rectangle(2, 3, 40, 50)));
        Rectangle bounds = box.getBounds(); bounds.x = 100; check(box.getX() == 2);
        check(box.getSize().equals(new Dimension(40, 50)));
        check(box.contains(0, 0) && !box.contains(40, 0));
        box.setName("box"); check(box.getName().equals("box"));
        ComponentEvent event = new ComponentEvent(box, ComponentEvent.COMPONENT_MOVED);
        check(event.getComponent() == box && event.getID() == ComponentEvent.COMPONENT_MOVED);
        Object replacement = new Object(); event.setSource(replacement); check(event.getSource() == replacement);
        Panel panel = new Panel(new BorderLayout(3, 5));
        Box center = new Box(10, 20), east = new Box(20, 20);
        panel.add(center, BorderLayout.CENTER); panel.add(east, BorderLayout.EAST);
        panel.setSize(100, 80); panel.doLayout();
        check(center.getBounds().equals(new Rectangle(0, 0, 77, 80)));
        check(east.getBounds().equals(new Rectangle(80, 0, 20, 80)));
        check(center.getParent() == panel && panel.getComponentCount() == 2);
        panel.remove(east); check(east.getParent() == null && panel.getComponentCount() == 1);
        System.out.println("AWT component geometry, events, and layouts passed: " + checks);
    }
}
