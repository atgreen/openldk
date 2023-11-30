// Test EventHandler.check14().

// Written by Jerry Quinn <jlquinn@optonline.net>

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

// Tags: JDK1.4

// Test features added by JDK 1.4

package gnu.testlet.java.beans.EventHandler;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.beans.EventHandler;
import java.lang.reflect.Method;

/**
 * <p>Basic tests for the <code>EventHandler</code></p>
 */
public class check14 implements Testlet
{
  // Inner classes because compilation complains it can't find the files
  // otherwise
  public class Target
  {
    public boolean flag;
    public String str;

    public Target() { flag = false; str = null; }
    public void reset() { flag = false; str = null; }

    // Test that null eventPropertyName activates this
    public void action1() { flag = true; }
	
    // Test that moving whole objects works
    public void action(Event e) { str =  e.getD(); flag = e.isB(); }
    // To test that wrapper handling works
    public void setAction(boolean b) { flag = b; }
    // To test that basic properties work
    public void setAction(String s) { str = s; }
	// To test event forwarding.
	public void setEventProperty(Event e) { str =  e.getD(); flag = e.isB(); }
  }

  public class Event
  {
    public Object getA() { return this; }
    public boolean isB() { return true; }
    public Object getC() { return this; }
    public String getD() { return "yes"; }
  }

  public interface Listener
  {
    public void listen1(check14.Event x);
    public void listen2(check14.Event x);
  }
  
  /* A Listener interface without arguments in the methods. */
  public interface Listener2
  {
	  public void listen1();
	  public void listen2();
  }

  public static int x = 0;
  
  /* A variable that has to be set by calling itself. */
  public boolean called;

  public void test (TestHarness harness)
  {
    Target target = new Target();
    Event ev = new Event();

    // Basic event handler value tests
    EventHandler eh = new EventHandler(target, "action", "a.c.b", "listen1");

	// Note: Referential equality works because all String instances where available at compilation time
    harness.check(eh.getAction() == "action", "Check basic settings");
    harness.check(eh.getEventPropertyName() == "a.c.b");
    harness.check(eh.getListenerMethodName() == "listen1");
    harness.check(eh.getTarget() == target);

    // Simple invoke test
    Method listen1 = null;
    Method listen2 = null;
    try
      {
	listen1 = Listener.class.getMethod("listen1",
					   new Class[] {Event.class});
	listen2 = Listener.class.getMethod("listen2",
					   new Class[] {Event.class});
      }
    catch (Exception e)
      {
	harness.fail("No listener methods - test is broken");
      }
    harness.check(target.flag == false, "Test invoke");

    try
      {
	eh.invoke(null, listen1, new Object[] {ev});
      }
    catch (Exception e)
      {
	harness.fail("Invoke listen1 failed " + e.toString());
      }
    harness.check(target.flag == true, "Invoke listen1 test");
    target.reset();

    try
      {
	eh.invoke(null, listen2, new Object[] {ev});
      }
    catch (Exception e)
      {
	harness.fail("Invoke listen2 failed " + e.toString());
      }
    harness.check(target.flag == false, "Invoke listen2 test");
    target.reset();

    // Static create tests
    Listener o1 = (Listener) EventHandler.create(Listener.class,
						 target, "action", "");
	try 
	  {
    o1.listen1(ev);
	  }
	catch(Exception e)
      {
		harness.fail("Invoke listen1 failed " + e.toString());
	  }
    harness.check(target.flag == true, "Test null event property");
    target.reset();

	try 
	  {
    o1.listen2(ev);
	  }
	catch(Exception e)
      {
		harness.fail("Invoke listen1 failed " + e.toString());
	  }
    harness.check(target.flag == true);
    harness.check(target.str == "yes");
    target.reset();
	
	/* Listener methods may have no arguments, too. This tests whether these methods
	 * are properly implemented when calling the static create methods of EventHandler. 
	 */
	Listener2 l2 = (Listener2) EventHandler.create(Listener2.class, target, "action1");
	l2.listen1();
	harness.check(target.flag, true, "no argument listener method");
	target.reset();
	
	l2.listen2();
	harness.check(target.flag, true);
	target.reset();

    Listener o2 = (Listener) EventHandler.create(Listener.class,
						 target, "action1");
    o2.listen1(ev);
    harness.check(target.flag == true, "Test action with no parameter");
    harness.check(target.str == null);
    target.reset();

    o2.listen2(ev);
    harness.check(target.flag == true);
    harness.check(target.str == null);
    target.reset();
	

    Listener o3 = (Listener) EventHandler.create(Listener.class, target,
					       "action", "a.c.d");
    o3.listen1(ev);
    harness.check(target.flag == false, "Test null listener");
    harness.check(target.str == "yes");
    target.reset();

    o3.listen2(ev);
    harness.check(target.flag == false, "Test null listener");
    harness.check(target.str == "yes");
    target.reset();


    Listener o4 = (Listener) EventHandler.create(Listener.class, target,
					       "action", "a.c.d", "listen2");
    o4.listen1(ev);
    harness.check(target.flag == false, "Test full, ignore listen1");
    harness.check(target.str == null);
    target.reset();

    o4.listen2(ev);
    harness.check(target.flag == false, "Test full, invoke listen2");
    harness.check(target.str == "yes");
    target.reset();

    // Make sure created objects actually implement Listener, not just the
    // method.
    Object o5 = EventHandler.create(Listener.class, target, "action");
    Class ifs[] = o5.getClass().getInterfaces();
    boolean found = false;
    for (int i=0; i < ifs.length; i++)
      if (ifs[i] == Listener.class)
	found = true;
    
    harness.check(found == true, "Proxy implements Listener");
	
  }

  public void targetMethod() {
	  called = true;
  }
  
}
