/* removeAttributes.java -- Tests for the removeAttributes() method in the 
                            SimpleAttributeSet class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
  
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.text.SimpleAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;

/**
 * Some checks for the removeAttributes() method in the 
 * {@link SimpleAttributeSet} class.
 */
public class removeAttributes implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness) 
  {
    harness.checkPoint("(AttributeSet)");
    SimpleAttributeSet s = new SimpleAttributeSet();
    s.addAttribute("A", "1");
    s.addAttribute("B", "2");
    s.addAttribute("C", "3");
    s.addAttribute("D", "4");
    s.addAttribute("E", "5");
    s.addAttribute("F", "6");
    harness.check(s.getAttributeCount(), 6);
    
    SimpleAttributeSet ss = new SimpleAttributeSet();
    ss.addAttribute("A", "1");
    ss.addAttribute("C", "3");
    ss.addAttribute("E", "5");
    
    s.removeAttributes(ss);
    harness.check(s.getAttributeCount(), 3);
    harness.check(s.getAttribute("B"), "2");
    harness.check(s.getAttribute("D"), "4");
    harness.check(s.getAttribute("F"), "6");
    
    SimpleAttributeSet sss = new SimpleAttributeSet();
    ss.addAttribute("B", "XXX");
    s.removeAttributes(sss);    
    harness.check(s.getAttributeCount(), 3);
    harness.check(s.getAttribute("B"), "2");
    harness.check(s.getAttribute("D"), "4");
    harness.check(s.getAttribute("F"), "6");
    
    // check for remove of the resolve parent
    SimpleAttributeSet s2 = new SimpleAttributeSet();
    SimpleAttributeSet sParent = new SimpleAttributeSet();
    s2.setResolveParent(sParent);
    harness.check(s2.getResolveParent(), sParent);
    SimpleAttributeSet s3 = new SimpleAttributeSet();
    s3.setResolveParent(sParent);
    s2.removeAttributes(s3);
    harness.check(s2.getResolveParent(), null);

    // try null
    boolean pass = false;
    try 
      {
        s.removeAttributes((AttributeSet) null);    
      }
    catch (NullPointerException ex) 
      {
        pass = true;
      }
    harness.check(pass);
  }

  public void test2(TestHarness harness) 
  {
    harness.checkPoint("(Enumeration)");
    SimpleAttributeSet s = new SimpleAttributeSet();
    s.addAttribute("A", "1");
    s.addAttribute("B", "2");
    s.addAttribute("C", "3");
    s.addAttribute("D", "4");
    s.addAttribute("E", "5");
    s.addAttribute("F", "6");
    harness.check(s.getAttributeCount(), 6);
    
    Vector v = new Vector();
    v.add("B");
    v.add("D");
    v.add("F");
    Enumeration e = v.elements();
    s.removeAttributes(e);
    harness.check(s.getAttributeCount(), 3);
    harness.check(s.getAttribute("A"), "1");
    harness.check(s.getAttribute("C"), "3");
    harness.check(s.getAttribute("E"), "5");
    
    // try an enumeration with null in it
    boolean pass = false;
    try 
      {
        v.clear();
        v.add(null);
        e = v.elements();
        s.removeAttributes(e);    
      }
      catch (NullPointerException ex) 
      {
        pass = true;
      }
    harness.check(pass);
    
    // try a null enumeration
    pass = false;
    try 
      {
        s.removeAttributes((Enumeration) null);    
      }
    catch (NullPointerException ex) 
      {
        pass = true;
      }
    harness.check(pass);
  }

}