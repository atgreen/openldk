// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;

public class X11SelectionProbe {
    public static void main(String[] args) throws Exception {
        Clipboard primary = Toolkit.getDefaultToolkit().getSystemSelection();
        if (primary == null) throw new AssertionError("Missing X11 primary selection");
        if (primary.getAvailableDataFlavors().length != 0)
            throw new AssertionError("PRIMARY is already owned; use an isolated X server");
        String text = "OpenLDK primary selection: \u03bb \u2603";
        primary.setContents(new StringSelection(text), null);
        Object actual = primary.getContents(null).getTransferData(DataFlavor.stringFlavor);
        if (!text.equals(actual)) throw new AssertionError("Selection contents: " + actual);
        System.out.println("X11 primary selection round trip passed");
    }
}
