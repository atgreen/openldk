/* testName.java -- 
   Copyright (C) 2006 RedHat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK 1.4E

package gnu.testlet.java.awt;

import java.awt.CheckboxMenuItem;
import java.awt.Choice;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.List;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.ScrollPane;
import java.awt.TextField;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testName implements Testlet
{

  public void test(TestHarness harness)
  {
   Choice a0 = new Choice();
   Choice a1 = new Choice();
   Choice a2 = new Choice();
   harness.check(a0.getName(), "choice0");
   harness.check(a1.getName(), "choice1");
   harness.check(a2.getName(), "choice2");
   Choice a3 = new Choice();
   Choice a4 = new Choice();
   Choice a5 = new Choice();
   harness.check(a5.getName(), "choice3");
   harness.check(a4.getName(), "choice4");
   harness.check(a3.getName(), "choice5");
   
   CheckboxMenuItem b0 = new CheckboxMenuItem();
   CheckboxMenuItem b1 = new CheckboxMenuItem();
   CheckboxMenuItem b2 = new CheckboxMenuItem();
   harness.check(b0.getName(), "chkmenuitem0");
   harness.check(b1.getName(), "chkmenuitem1");
   harness.check(b2.getName(), "chkmenuitem2");
   CheckboxMenuItem b5 = new CheckboxMenuItem();
   CheckboxMenuItem b4 = new CheckboxMenuItem();
   CheckboxMenuItem b3 = new CheckboxMenuItem();
   harness.check(b5.getName(), "chkmenuitem3");
   harness.check(b4.getName(), "chkmenuitem4");
   harness.check(b3.getName(), "chkmenuitem5");
   
   List c0 = new List();
   List c1 = new List();
   List c2 = new List();
   harness.check(c0.getName(), "list0");
   harness.check(c1.getName(), "list1");
   harness.check(c2.getName(), "list2");
   List c3 = new List();
   List c4 = new List();
   List c5 = new List();
   harness.check(c5.getName(), "list3");
   harness.check(c4.getName(), "list4");
   harness.check(c3.getName(), "list5");
   
   MenuBar d0 = new MenuBar();
   MenuBar d1 = new MenuBar();
   MenuBar d2 = new MenuBar();
   harness.check(d0.getName(), "menubar0");
   harness.check(d1.getName(), "menubar1");
   harness.check(d2.getName(), "menubar2");
   MenuBar d3 = new MenuBar();
   MenuBar d4 = new MenuBar();
   MenuBar d5 = new MenuBar();
   harness.check(d5.getName(), "menubar3");
   harness.check(d4.getName(), "menubar4");
   harness.check(d3.getName(), "menubar5");
   
   MenuItem e0 = new MenuItem();
   MenuItem e1 = new MenuItem();
   MenuItem e2 = new MenuItem();
   harness.check(e0.getName(), "menuitem0");
   harness.check(e1.getName(), "menuitem1");
   harness.check(e2.getName(), "menuitem2");
   MenuItem e3 = new MenuItem();
   MenuItem e4 = new MenuItem();
   MenuItem e5 = new MenuItem();
   harness.check(e5.getName(), "menuitem3");
   harness.check(e4.getName(), "menuitem4");
   harness.check(e3.getName(), "menuitem5");
   
   Menu f0 = new Menu();
   Menu f1 = new Menu();
   Menu f2 = new Menu();
   harness.check(f0.getName(), "menu0");
   harness.check(f1.getName(), "menu1");
   harness.check(f2.getName(), "menu2");
   Menu f3 = new Menu();
   Menu f4 = new Menu();
   Menu f5 = new Menu();
   harness.check(f5.getName(), "menu3");
   harness.check(f4.getName(), "menu4");
   harness.check(f3.getName(), "menu5");
    
   PopupMenu g0 = new PopupMenu();
   PopupMenu g1 = new PopupMenu();
   PopupMenu g2 = new PopupMenu();
   harness.check(g0.getName(), "popup0");
   harness.check(g1.getName(), "popup1");
   harness.check(g2.getName(), "popup2");
   PopupMenu g3 = new PopupMenu();
   PopupMenu g4 = new PopupMenu();
   PopupMenu g5 = new PopupMenu();
   harness.check(g5.getName(), "popup3");
   harness.check(g4.getName(), "popup4");
   harness.check(g3.getName(), "popup5");
   
   ScrollPane h0 = new ScrollPane();
   ScrollPane h1 = new ScrollPane();
   ScrollPane h2 = new ScrollPane();
   harness.check(h0.getName(), "scrollpane0");
   harness.check(h1.getName(), "scrollpane1");
   harness.check(h2.getName(), "scrollpane2");
   ScrollPane h3 = new ScrollPane();
   ScrollPane h4 = new ScrollPane();
   ScrollPane h5 = new ScrollPane();
   harness.check(h5.getName(), "scrollpane3");
   harness.check(h4.getName(), "scrollpane4");
   harness.check(h3.getName(), "scrollpane5");

   TextField i0 = new TextField();
   TextField i1 = new TextField();
   TextField i2 = new TextField();
   harness.check(i0.getName(), "textfield0");
   harness.check(i1.getName(), "textfield1");
   harness.check(i2.getName(), "textfield2");
   TextField i3 = new TextField();
   TextField i4 = new TextField();
   TextField i5 = new TextField();
   harness.check(i5.getName(), "textfield3");
   harness.check(i4.getName(), "textfield4");
   harness.check(i3.getName(), "textfield5");
   
   harness.check((new Cursor(0)).getName(), "Default Cursor");
   harness.check((new Cursor(1)).getName(), "Crosshair Cursor");
   harness.check((new Cursor(2)).getName(), "Text Cursor");
   harness.check((new Cursor(3)).getName(), "Wait Cursor");
   harness.check((new Cursor(4)).getName(), "Southwest Resize Cursor");
   harness.check((new Cursor(5)).getName(), "Southeast Resize Cursor");
   harness.check((new Cursor(6)).getName(), "Northwest Resize Cursor");
   harness.check((new Cursor(7)).getName(), "Northeast Resize Cursor");
   harness.check((new Cursor(8)).getName(), "North Resize Cursor");
   harness.check((new Cursor(9)).getName(), "South Resize Cursor");
   harness.check((new Cursor(10)).getName(), "West Resize Cursor");
   harness.check((new Cursor(11)).getName(), "East Resize Cursor");
   harness.check((new Cursor(12)).getName(), "Hand Cursor");
   harness.check((new Cursor(13)).getName(), "Move Cursor");
    
   Font j = new Font(null, 0, 0);
   harness.check(j.getName(), "Default");
   harness.check(j.getFontName(), "Dialog.plain");
  }

}
