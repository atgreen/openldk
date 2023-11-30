//Tags: JDK1.4

//Copyright (C) 2005 Robert Schuster <thebohemian@gmx.net>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.java.beans.EventHandler;

import java.beans.EventHandler;
import java.math.BigInteger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * <p>This tests more complex behavior of the <code>EventHandler</code></p>
 */
public class check14b implements Testlet {

	public interface Listener {
		
		void listen();
		
		void listen(Object o);
		
		void listen(String m);
	}

	public class Event {
		public boolean isBooleanValue() {
			return true;
		}
		
		public BigInteger getBigValue() {
			return new BigInteger("903281390123809123");
		}
		
		public int getIntValue() {
			return 0xDEADBEEF;
		}
		
		public Integer getIntegerValue() {
			return new Integer(0xDEADBEEF);
		}
	}
	
	public class EventSub extends Event {
		
		boolean method = false;
		boolean property = false;
		
		public EventSub getSelf() {
			method = true;
			
			return this;
		}
		
		public EventSub getGetSelf() {
			property = true;
			
			return this;
		}
		
	}
	
	public interface Listener2 {
		// The check. prefix is a workaround for a Jikes bug.
		public void listen(check14b.Event e);
	}
	
	private boolean noArgMethodCalled;
	private boolean objectArgMethodCalled;
	private boolean stringArgMethodCalled;
	private boolean numberArgMethodCalled;
	
	private boolean calledSetter;
	
	public void test(TestHarness harness) {
		Listener l = (Listener)
			EventHandler.create(Listener.class, this, "targetMethod");
		
		l.listen();
		harness.check(noArgMethodCalled, true, "prefer no arg method");
		harness.check(objectArgMethodCalled, false);
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;
		
		l.listen(new Object());
		harness.check(noArgMethodCalled, true, "prefer no arg method (Object given)");
		harness.check(objectArgMethodCalled, false);
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;
		
		l.listen("GNU!");
		harness.check(noArgMethodCalled, true, "prefer no arg method (String given)");
		harness.check(objectArgMethodCalled, false);
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;
		
		// We try to apply the value from the "eventValue" property of the Event
		// object to the target object (= this) by using its "value" property.
		// This tests whether the implementation properly retrieves the 'setValue'
		// method when it is given as a property name.
		Listener2 l2 = (Listener2)
			EventHandler.create(Listener2.class, this, "value", "booleanValue", null);
		
		l2.listen(new Event());
		harness.check(calledSetter, true, "setter invoked as property");
		calledSetter = false;

		// Now we do the same test but use a subclass of Event. Though the
		// implementation should be able to find the "eventValue" property. 
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;
		
		// This is basically the same test as above but now the target method
		// has to be found via a method name instead of a property name.
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setValue", "booleanValue", null);
	
		l2.listen(new Event());
		harness.check(calledSetter, true, "setter invoked as method");

		// The situation is that we have a BigInteger value but no "targetValue" property
		// of that type (ie. no setTargetValue(BigInteger) method). However the implementation
		// should find the property with Object-type of the same name instead and use the
		// corresponding method. It is important that we do not use a primitive for this test.
		harness.checkPoint("replacement for BigInteger property");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "targetValue", "bigValue", null);
		
		l2.listen(new Event());
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;
		
		// Now the same test with a boolean property. The reason for a special test
		// is that internally booleans are represented by Boolean.TYPE.
		// The funny thing is that Boolean.TYPE.getSuperclass() is null but the
		// EventHandler mechanism should nevertheless find an Object property and
		// its method.
		harness.checkPoint("replacement for boolean property");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "targetValue", "booleanValue", null);
		
		l2.listen(new Event());
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;
		
		// Again the same kind of test but now the property is an int and the expected
		// property method has a 'java.lang.Number' argument. Again the internally used
		// Integer.TYPE superclass is null and not Number.
		harness.checkPoint("replacement for integer property");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "numberValue", "intValue", null);
		
		l2.listen(new Event());
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, false);
		harness.check(numberArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = numberArgMethodCalled = false;
		
		// Repeats the BigInteger test but uses an explicit method name instead of a property.
		harness.checkPoint("replacement for BigInteger method");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setTargetValue", "bigValue", null);
		
		l2.listen(new Event());
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;
		
		// Same as boolean test but uses method name instead of property name.
		harness.checkPoint("replacement for boolean method");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setTargetValue", "booleanValue", null);
		
		l2.listen(new Event());
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = false;

		// Same as integer test but uses method name instead of property name.
		harness.checkPoint("replacement for integer method");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setNumberValue", "intValue", null);
		
		l2.listen(new Event());
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, false);
		harness.check(numberArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = numberArgMethodCalled = false;

		// The EventHandler supports method names in the property string as well.
		// This tests whether the implementation evaluates this properly.
		harness.checkPoint("property as method names");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setNumberValue", "getSelf.getIntValue", null);

		boolean exceptionThrown = false;
		try {
			l2.listen(new EventSub());
		} catch(Exception e) {
			exceptionThrown = true;
		}
		harness.check(exceptionThrown, false);
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, false);
		harness.check(numberArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = numberArgMethodCalled = false;
		
		// The EventHandler can even use a mix of method names and property names. This tests
		// the implementation's support for this.
		harness.checkPoint("property and method names");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setNumberValue", "self.getSelf.self.getSelf.intValue", null);

		exceptionThrown = false;
		try {
			l2.listen(new EventSub());
		} catch(Exception e) {
			exceptionThrown = true;
		}
		harness.check(exceptionThrown, false);
		harness.check(noArgMethodCalled, false);
		harness.check(objectArgMethodCalled, false);
		harness.check(numberArgMethodCalled, true); // -> this one should be called
		harness.check(stringArgMethodCalled, false);
		noArgMethodCalled = objectArgMethodCalled = stringArgMethodCalled = numberArgMethodCalled = false;

		// This simply checks that in case of ambiguity ("getSelf" as a method and as a property available)
		// the property variant is preferred.
		harness.checkPoint("name ambiguity preference");
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setNumberValue", "getSelf.intValue", null);
		EventSub event = new EventSub();
		
		l2.listen(event);
		harness.check(event.property, true);
		harness.check(event.method, false);

		
		harness.checkPoint("primitive to wrapper");
		calledSetter = false;
		// This tests automatic primitive to wrapper conversion. That means the value retrieved from
		// the event object is a primitive one and the target accepts it as an instance of a wrapper
		// class.
		// The 4 subtests check that the target and the source can use the property name as well
		// as the method name.
		
		// source: property name
		// target: property name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "integerValue", "intValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;

		// source: property name
		// target: method name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setIntegerValue", "intValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;
		
		// source: method name
		// target: method name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setIntegerValue", "getIntValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;

		// source: method name
		// target: property name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "integerValue", "getIntValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;
		
		harness.checkPoint("wrapper to primitive");
		// The following 4 tests check whether the property conversion works the other
		// way to. Now the value retrieved from the event object is an instance of a
		// wrapper class and the target accepts a primitive.
		
		// source: property name
		// target: property name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "intValue", "integerValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;

		// source: property name
		// target: method name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setIntValue", "integerValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;
		
		// source: method name
		// target: method name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "setIntValue", "getIntegerValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;

		// source: method name
		// target: property name
		l2 = (Listener2)
		EventHandler.create(Listener2.class, this, "intValue", "getIntegerValue");
		
		l2.listen(new EventSub());
		harness.check(calledSetter, true);
		calledSetter = false;
	}
	
	public void targetMethod() {
		noArgMethodCalled = true;
	}
	
	public void targetMethod(Object o) {
		objectArgMethodCalled = true;
	}
	
	public void targetMethod(String m) {
		stringArgMethodCalled = true;
	}
	
	public void setTargetValue() {
		noArgMethodCalled = true;
	}
	
	public void setTargetValue(Object o) {
		objectArgMethodCalled = true;
	}
	
	public void setTargetValue(String m) {
		stringArgMethodCalled = true;
	}
	
	public void setNumberValue() {
		noArgMethodCalled = true;
	}
	
	public void setNumberValue(Object o) {
		objectArgMethodCalled = true;
	}

	public void setNumberValue(Number n) {
		numberArgMethodCalled = true;
	}
	
	public void setNumberValue(String m) {
		stringArgMethodCalled = true;
	}

	public void setValue(boolean arg) {
		calledSetter = arg;
	}
	
	public void setIntegerValue(Integer value) {
		calledSetter = true;
	}

	public void setIntValue(int value) {
		calledSetter = true;
	}
	
}
