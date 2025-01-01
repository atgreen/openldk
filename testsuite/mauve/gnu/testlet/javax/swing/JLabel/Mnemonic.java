// Tags: JDK1.2

// Copyright (C) 2004 Thomas Zander <zander@kde.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JLabel;

/**
 * These tests pass with the Sun JDK 1.4.1_03 on GNU/Linux IA-32.
 */
public class Mnemonic implements Testlet {
    public void test(TestHarness harness) {
        JLabel l = new JLabel("lskdjnvmdsklzedfsdmnWK");
        harness.check(l.getDisplayedMnemonic(), 0);
        harness.check(l.getDisplayedMnemonicIndex(), -1);

        l.setDisplayedMnemonic(java.awt.event.KeyEvent.VK_K);
        harness.check(l.getDisplayedMnemonic(), java.awt.event.KeyEvent.VK_K);
        harness.check(l.getDisplayedMnemonicIndex(), 2);

        l.setDisplayedMnemonic(java.awt.event.KeyEvent.VK_Q);
        harness.check(l.getDisplayedMnemonicIndex(), -1);

        l.setDisplayedMnemonic(java.awt.event.KeyEvent.VK_W);
        harness.check(l.getDisplayedMnemonicIndex(), 20);

        l.setText("new text");
        harness.check(l.getDisplayedMnemonicIndex(), 2);

        l.setDisplayedMnemonicIndex(5);
        // the following test is really un-intuitive... Is this a bug in Suns JVM?
        harness.check(l.getDisplayedMnemonic(), java.awt.event.KeyEvent.VK_W); // unchanged
        harness.check(l.getDisplayedMnemonicIndex(), 5);

        l = new JLabel("new text");
        l.setDisplayedMnemonicIndex(5);
        harness.check(l.getDisplayedMnemonic(), 0);
        harness.check(l.getDisplayedMnemonicIndex(), 5);
    }
}
