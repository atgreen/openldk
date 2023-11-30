// Tags: GUI JDK1.3

// Copyright (C) 2006 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>

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

package gnu.testlet.java.awt.Graphics2D;

import java.awt.AWTPermission;
import java.awt.Composite;
import java.awt.CompositeContext;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Window;
import java.awt.RenderingHints;
import java.awt.image.ColorModel;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      Window window = new Frame();
      window.setVisible(true);

      Graphics2D graphics2d = (Graphics2D) window.getGraphics();

      Composite composite = new TestComposite();

      Permission[] readDisplayPixels = new Permission[] {
	new AWTPermission("readDisplayPixels")};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.awt.Graphics2D-setComposite
	harness.checkPoint("setComposite");
	try {
	  sm.prepareChecks(readDisplayPixels);
	  try {
	    graphics2d.setComposite(composite);
	  }
	  catch (UnsupportedOperationException ex) {
	  }
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
      }
      finally {
	sm.uninstall();
      }

      window.setVisible(false);
      window.dispose();
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }

  private static class TestComposite implements Composite
  {
    public CompositeContext createContext(ColorModel srcColorModel,
					  ColorModel dstColorModel,
					  RenderingHints hints)
    {
      return null;
    }
  }
}
