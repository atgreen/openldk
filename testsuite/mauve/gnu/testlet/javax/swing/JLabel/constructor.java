// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>
//               2006 Red Hat

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Tests if the constructor sets the default values for the JLabel's properties
 * correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class constructor implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);  
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    testHorizontalAlignment(harness);
  }
  
  public void test1(TestHarness harness)
  {
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    
    // use this to check that the defaults are those for a Label
    UIManager.put("Label.font", new Font("Dialog", Font.PLAIN, 21));
    UIManager.put("Label.foreground", Color.green);
    UIManager.put("Label.background", Color.yellow);
    
    JLabel l = new JLabel();
    harness.check(l.getFont(), new Font("Dialog", Font.PLAIN, 21));
    harness.check(l.getForeground(), Color.green);
    harness.check(l.getBackground(), Color.yellow);
    harness.check(l.getAlignmentX(), 0.0F, "alignmentX");
    harness.check(l.getAlignmentY(), 0.5F, "alignmentY");
    harness.check(l.getVerticalAlignment(), SwingConstants.CENTER,
                  "verticalAlignment");
    harness.check(l.getDisabledIcon(), null);
    harness.check(l.getDisplayedMnemonic(), KeyEvent.VK_UNDEFINED);
    harness.check(l.getDisplayedMnemonicIndex(), -1);
    harness.check(l.getHorizontalTextPosition(), JLabel.TRAILING);
    harness.check(l.getVerticalTextPosition(), JLabel.CENTER);
    harness.check(l.getLabelFor(), null);
    harness.check(l.getIconTextGap(), 4);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    JLabel label = new JLabel();
    harness.check(label.getIcon(), null);
    harness.check(label.getText(), "");
  }
  
  public void testConstructor2(TestHarness harness)
  {
    JLabel label = new JLabel(new ImageIcon());
    harness.check(label.getIcon() != null);
    harness.check(label.getText(), null);
    
    label = new JLabel(new ImageIcon
                      (new BufferedImage(16, 16, BufferedImage.TYPE_INT_BGR)));
    harness.check(label.getIcon() != null);
    harness.check(label.getText(), null);
    
    label = new JLabel((Icon) null);
    harness.check(label.getIcon() == null);
    harness.check(label.getText(), null);
  }
  
  public void testConstructor3(TestHarness harness)
  {
    JLabel label = new JLabel(new ImageIcon(), 2);
    harness.check(label.getIcon() != null);
    harness.check(label.getText(), null);
    harness.check(label.getHorizontalAlignment(), SwingConstants.LEFT);
    
    label = new JLabel((Icon) null, 2);
    harness.check(label.getIcon() == null);
    harness.check(label.getText(), null);
    harness.check(label.getHorizontalAlignment(), SwingConstants.LEFT);
    
    label = new JLabel(new ImageIcon(), 0);
    harness.check(label.getIcon() != null);
    harness.check(label.getText(), null);
    harness.check(label.getHorizontalAlignment(), SwingConstants.CENTER);
    
    boolean fail = false;
    try
      {
        label = new JLabel(new ImageIcon(), -1);
      }
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);   
  }
  
  public void testConstructor4(TestHarness harness)
  {
    JLabel label = new JLabel("This is some text.");
    harness.check(label.getIcon(), null);
    harness.check(label.getText(), "This is some text.");
    
    label = new JLabel("");
    harness.check(label.getIcon(), null);
    harness.check(label.getText(), "");
    
    String text = null;
    label = new JLabel(text);
    harness.check(label.getIcon(), null);
    harness.check(label.getText(), null);
  }
  
  public void testConstructor5(TestHarness harness)
  {
    JLabel label = new JLabel("This is some text.", 11);
    harness.check(label.getIcon() == null);
    harness.check(label.getText(), "This is some text.");
    harness.check(label.getHorizontalAlignment(), SwingConstants.TRAILING);
    
    label = new JLabel("", 11);
    harness.check(label.getIcon() == null);
    harness.check(label.getText(), "");
    harness.check(label.getHorizontalAlignment(), SwingConstants.TRAILING);
    
    String text = null;
    label = new JLabel(text, 0);
    harness.check(label.getIcon() == null);
    harness.check(label.getText(), null);
    harness.check(label.getHorizontalAlignment(), SwingConstants.CENTER);
    
    boolean fail = false;
    try
      {
        label = new JLabel("This is some text.", -2);
      }
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
  }
  
  public void testConstructor6(TestHarness harness)
  {
    JLabel label = new JLabel("This is some text.", new ImageIcon(), 0);
    harness.check(label.getIcon() != null);
    harness.check(label.getText(), "This is some text.");
    harness.check(label.getHorizontalAlignment(), SwingConstants.CENTER);
    
    label = new JLabel("", (Icon) null, 10);
    harness.check(label.getIcon() == null);
    harness.check(label.getText(), "");
    harness.check(label.getHorizontalAlignment(), SwingConstants.LEADING);
    
    String text = null;
    label = new JLabel(text, new ImageIcon(), 11);
    harness.check(label.getIcon() != null);
    harness.check(label.getText(), null);
    harness.check(label.getHorizontalAlignment(), SwingConstants.TRAILING);
    
    boolean fail = false;
    try
      {
        label = new JLabel("This is some text.", new ImageIcon(), -4);
      }
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
  }
  
  public void testHorizontalAlignment(TestHarness harness)
  {
    JLabel label = null;
    
    boolean fail = false;
    try 
      {
        label = new JLabel("", -1);
      } 
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
    
    boolean pass = false;
    try 
      {
        label = new JLabel("", 0);
        pass = true;
      } 
    catch (IllegalArgumentException e)
      {
        // Do nothing.
      }
    harness.check(pass);
    harness.check(label.getHorizontalAlignment(), SwingConstants.CENTER);
    
    fail = false;
    try 
      {
        label = new JLabel("", 1);
      } 
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
    
    pass = false;
    try 
      {
        label = new JLabel("", 2);
        pass = true;
      } 
    catch (IllegalArgumentException e)
      {
        // Do nothing.
      }
    harness.check(pass);
    harness.check(label.getHorizontalAlignment(), SwingConstants.LEFT);
    
    fail = false;
    try 
      {
        label = new JLabel("", 3);
      } 
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
    
    pass = false;
    try 
      {
        label = new JLabel("", 4);
        pass = true;
      } 
    catch (IllegalArgumentException e)
      {
        // Do nothing.
      }
    harness.check(pass);
    harness.check(label.getHorizontalAlignment(), SwingConstants.RIGHT);
    
    fail = false;
    try 
      {
        label = new JLabel("", 5);
      } 
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
    
    fail = false;
    try 
      {
        label = new JLabel("", 7);
      } 
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
    
    pass = false;
    try 
      {
        label = new JLabel("", 10);
        pass = true;
      } 
    catch (IllegalArgumentException e)
      {
        // Do nothing.
      }
    harness.check(pass);
    harness.check(label.getHorizontalAlignment(), SwingConstants.LEADING);
    
    pass = false;
    try 
      {
        label = new JLabel("", 11);
        pass = true;
      } 
    catch (IllegalArgumentException e)
      {
        // Do nothing.
      }
    harness.check(pass);
    harness.check(label.getHorizontalAlignment(), SwingConstants.TRAILING);
    
    fail = false;
    try 
      {
        label = new JLabel("", 13);
      } 
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
  }
}
