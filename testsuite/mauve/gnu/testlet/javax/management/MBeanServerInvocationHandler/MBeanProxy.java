// Tags: JDK1.5
// Uses: ChildMXBean Colour Test TestX TestC TestMBean TestXMBean TestCMXBean

// Copyright (C) 2007 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

package gnu.testlet.javax.management.MBeanServerInvocationHandler;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.reflect.Proxy;

import javax.management.JMX;
import javax.management.MalformedObjectNameException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Tests {@link MBeanServerInvocationHandler}
 * for MBeans.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class MBeanProxy
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    ObjectName name = null;
    ObjectName namex = null;
    ObjectName namec = null;
    ObjectName namecc = null;
    try
      {
	name = new ObjectName("mauve:test=go");
	namex = new ObjectName("mauve:test=gox");
	namec = new ObjectName("mauve:test=goc");
	namecc = new ObjectName("mauve:test=gocc");
      }
    catch (MalformedObjectNameException e)
      {
	h.debug(e);
      }
    MBeanServer server = MBeanServerFactory.createMBeanServer();
    try
      {
	server.registerMBean(new Test("GNU Classpath"), name);
	server.registerMBean(new TestX("GNU Classpath"), namex);
	server.registerMBean(new TestC(), namec);
      }
    catch (Exception e)
      {
	h.debug(e);
      }
    TestMBean test = JMX.newMBeanProxy(server, name, TestMBean.class);
    h.check(test.getName(), "GNU Classpath", "Name test");
    h.check(test.isEdible(), false, "Edible test");
    h.checkPoint("Mutator test");
    test.setName("Mauve");
    h.check(test.getName(), "Mauve", "Name test after change");
    h.check(test.equals(test), "Proxy equivalence reflection test");
    TestXMBean testx = JMX.newMBeanProxy(server, namex, TestXMBean.class);
    h.checkPoint("Calling equals");
    testx.equals(null);
    h.check(testx.getLastMethodCalled(), "equals");
    h.checkPoint("Calling hashCode");
    testx.hashCode();
    h.check(testx.getLastMethodCalled(), "hashCode");
    h.checkPoint("Calling toString");
    testx.toString();
    h.check(testx.getLastMethodCalled(), "toString");
    final TestCMXBean testc = JMX.newMXBeanProxy(server, namec, TestCMXBean.class);
    h.checkPoint("Setting id");
    testc.setId(42);
    h.check(testc.getId(), 42, "Getting id");
    h.checkPoint("Setting size");
    testc.setSize(5);
    h.check(testc.getSize() == 5, "Getting size");
    h.checkPoint("Setting name");
    testc.setName(namec);
    h.check(testc.getName(), namec, "Getting name");
    h.checkPoint("Setting weights");
    float[] weights = new float[] { 0.5f, -0.7f };
    testc.setWeights(weights);
    h.check(testc.getWeights(), weights, "Getting weights");
    h.checkPoint("Setting names");
    String[] names = new String[] { "Bob", "Jim", "Jake" };
    testc.setNames(names);
    h.check(testc.getNames(), names, "Getting names");
    h.checkPoint("Setting ages");
    Set<Integer> ages = new HashSet();
    ages.add(45);
    ages.add(24);
    testc.setAges(ages);
    h.check(testc.getAges(), ages, "Getting ages");
    h.checkPoint("Setting biscuits");
    SortedSet<String> biscuits = new TreeSet();
    biscuits.add("Chocolate");
    biscuits.add("Ginger");
    biscuits.add("Plain");
    testc.setBiscuits(biscuits);
    h.check(testc.getBiscuits(), biscuits, "Getting biscuits");
    h.checkPoint("Setting colour");
    testc.setColour(Colour.RED);
    h.check(testc.getColour(), Colour.RED, "Getting colour");
    h.checkPoint("Setting phone numbers");
    Map<String,Integer> numbers = new HashMap();
    numbers.put("Bob",999);
    numbers.put("Jim",111);
    numbers.put("Sam",55);
    testc.setPhoneNumbers(numbers);
    h.check(testc.getPhoneNumbers(), numbers, "Getting phone numbers");
    h.checkPoint("Setting sorted phone numbers");
    SortedMap<String,Integer> snumbers = new TreeMap();
    snumbers.put("Bob",999);
    snumbers.put("Jim",111);
    snumbers.put("Sam",55);
    testc.setSortedPhoneNumbers(snumbers);
    h.check(testc.getSortedPhoneNumbers(), numbers, "Getting sorted phone numbers");
    h.checkPoint("Creating and setting child");
    ChildMXBean child = new ChildMXBean() {
	public TestCMXBean getParent() { return testc; }
	public void setParent(TestCMXBean bean) { }
      };
    try
      {
	server.registerMBean(child, namecc);
      }
    catch (Exception e)
      {
	h.debug(e);
      }
    ChildMXBean cproxy = JMX.newMXBeanProxy(server, namecc, ChildMXBean.class);
    testc.setChild(cproxy);
    h.check(testc.getChild(), cproxy, "Getting child"); 
  }

}

