/* Copyright (C) 1999, 2000, 2001, 2002 Hewlett-Packard Company

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.0

package gnu.testlet.java.lang.Class;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;
import java.net.*;

public class ClassTest implements Cloneable, java.io.Serializable, Testlet
{
  final int ACC_PUBLIC =  0x0001; //Marked or implicitly public in source. 
  final int ACC_PRIVATE = 0x0002; // Marked private in source. 
  final int ACC_PROTECTED = 0x0004; // Marked protected in source.
  final int ACC_STATIC = 0x0008; // Marked or implicitly static in source. 
  final int ACC_FINAL = 0x0010; // Marked final in source. 
  final int ACC_INTERFACE = 0x0200; // Was an interface in source. 
  final int ACC_ABSTRACT = 0x0400; // Marked or implicitly abstract in source.
  
  protected static TestHarness harness;
  public void test_toString()
  {
    harness.checkPoint("test_toString");
    harness.check(getClass().toString().equals(getClass().isInterface() ? 
					       "interface " : "class " + 
					       getClass().getName()));
    harness.check((new Object()).getClass().toString().
		  equals("class java.lang.Object"));
  }
  
  public void test_getName()
  {
    harness.checkPoint("test_getName");
    harness.check((new java.util.Vector()).getClass().getName().
		  equals("java.util.Vector"));
    harness.check((new Object[3]).getClass().getName().
		  equals("[Ljava.lang.Object;")) ;
    harness.check((new int[6][7][8]).getClass().getName().equals("[[[I"));

    // Note: the javadoc Class.getName() for JDK 1.3.x, 1.4.0 & 1.4.1 
    // seems to say that getName() returns a one character code for
    // primitive types and void, etcetera.  In fact, this is a bug in
    // the Sun javadoc.  According to Sun's bug database, it is fixed 
    // in JDK 1.4.2 (Merlin) release.
    harness.check(Void.TYPE.getName().equals("void"));
    harness.check(Boolean.TYPE.getName().equals("boolean"));
    harness.check(Byte.TYPE.getName().equals("byte"));
    harness.check(Character.TYPE.getName().equals("char"));
    harness.check(Short.TYPE.getName().equals("short"));
    harness.check(Integer.TYPE.getName().equals("int"));    
    harness.check(Long.TYPE.getName().equals("long"));
    harness.check(Float.TYPE.getName().equals("float"));
    harness.check(Double.TYPE.getName().equals("double"));
  }
  
  public void test_isInterface()
  {
    harness.checkPoint("test_isInterface");
    harness.check(!(new Object()).getClass().isInterface());
    harness.check(!getClass().isInterface());
    try {
      harness.check(Class.forName("java.lang.Cloneable").isInterface());
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
  }
  
  public void test_getSuperclass()
  {
    harness.checkPoint("test_getSuperclass (superclass of Boolean is Object)");
    try {
      harness.check((new Boolean(true)).getClass().getSuperclass() == 
		    Class.forName("java.lang.Object"));
    } catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }


    harness.checkPoint("test_getSuperclass (superclass of java.lang.Boolean.TYPE is null)");
    try {
	harness.check( java.lang.Boolean.TYPE.getSuperclass() == null);
    } catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    
    harness.checkPoint("test_getSuperclass (superclass of Object is null)");
    harness.check((new Object()).getClass().getSuperclass() == null);
    
    harness.checkPoint("test_getSuperclass (superclass of [[I is Object)");
    try {	
      Class clss = Class.forName("[[I");
      harness.check(clss.getSuperclass() == Class.forName("java.lang.Object"));
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    harness.checkPoint("test_getSuperclass (superclass of [D is Object)");
    try {	
      Class clss = Class.forName("[D");
      harness.check(clss.getSuperclass() == Class.forName("java.lang.Object"));
      harness.debug("superclass of " + clss + " is " + clss.getSuperclass());
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    harness.checkPoint("test_getSuperclass (superclass of Cloneable is null)");
    try {	
      Class clss = Class.forName("java.lang.Cloneable");
      harness.check(clss.getSuperclass() == null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    
    try {	
      Class clss = Void.TYPE;
      harness.check(clss.getSuperclass() == null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    
    try {	
      Class clss = Double.TYPE;
      harness.check(clss.getSuperclass() == null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
  }

    public void test_primitiveTypes()
    {
	Class cls;

	harness.checkPoint("test_primitiveTypes java.lang.Boolean.TYPE is primitive");
	cls = java.lang.Boolean.TYPE;
	harness.check(cls.isPrimitive() == true);

	harness.checkPoint("test_primitiveTypes java.lang.Double.TYPE is primitive");
	cls = java.lang.Double.TYPE;
	harness.check(cls.isPrimitive() == true);

	harness.checkPoint("test_primitiveTypes java.lang.Void.TYPE is primitive");
	cls = java.lang.Void.TYPE;
	harness.check(cls.isPrimitive() == true);

	harness.checkPoint("test_primitiveTypes java.lang.Object is not primitive");
	try {
	    cls = Class.forName("java.lang.Object");
	    harness.check(cls.isPrimitive() == false);
	} catch(Exception e)
	    {
		harness.check(false);
	    }

	harness.checkPoint("test_primitiveTypes java.lang.Integer is not primitive");
	try {
	    cls = Class.forName("java.lang.Integer");
	    harness.check(cls.isPrimitive() == false);
	} catch(Exception e)
	    {
		harness.check(false);
	    }

	try {
	    harness.checkPoint("test_primitiveTypes [I is not primitive");
	    cls = Class.forName("[I");
	    harness.check(cls.isPrimitive() == false);
	} catch(Exception e)
	    {
		harness.check(false);
	    }
    }  

    private class PrivateType {
	int foo;
    }

    public void test_Modifiers()
    {
	Class cls;

	harness.checkPoint("test_Modifiers java.lang.Boolean.TYPE modifiers");
	cls = java.lang.Boolean.TYPE;
	harness.check((cls.getModifiers() & 
		       (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE | 
			ACC_FINAL | ACC_INTERFACE)), 
		      (ACC_PUBLIC | ACC_FINAL));

	harness.checkPoint("test_Modifiers java.lang.Boolean modifiers");
	try {
	    cls = Class.forName("java.lang.Boolean");
	    harness.check((cls.getModifiers() & 
			   (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE | 
			    ACC_FINAL | ACC_INTERFACE)),
			  (ACC_PUBLIC | ACC_FINAL));
	} catch(Exception e)
	    {
		harness.check(false);
	    }

	harness.checkPoint("test_Modifiers [I modifiers");
	try {
	    cls = Class.forName("[I");
	    harness.check((cls.getModifiers() & 
			   (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE | 
			    ACC_FINAL | ACC_INTERFACE)), 
			  (ACC_PUBLIC | ACC_FINAL));
	} catch(Exception e)
	    {
		harness.check(false);
	    }

	harness.checkPoint("test_Modifiers private modifier");
	PrivateType foo = new PrivateType(); //new Cloneable() { int d; };
	cls = foo.getClass();
	harness.check((cls.getModifiers() & (ACC_PRIVATE)), (ACC_PRIVATE));

	harness.checkPoint("test_Modifiers array modifiers");
	/*	PrivateType[] array = new PrivateType[2];
	cls = array.getClass();
	harness.check((cls.getModifiers() & (ACC_PRIVATE)) == (ACC_PRIVATE));
	harness.check((cls.getModifiers() & (ACC_FINAL)) == (ACC_FINAL));
	harness.check((cls.getModifiers() & (ACC_INTERFACE)) == 0);
	*/

	harness.checkPoint("test_Modifiers java.lang.Boolean modifiers");
	cls = java.lang.Boolean.TYPE;
	harness.check((cls.getModifiers() & (ACC_PUBLIC | ACC_FINAL)) != 0);

    }  

  public void test_getInterfaces()
  {
    harness.checkPoint("test_getInterfaces");
    Class clss[] = getClass().getInterfaces();
    
    Class clclass = null, clclass1 = null;
    try {
      clclass = Class.forName("java.lang.Cloneable");
      clclass1 = Class.forName("java.io.Serializable");
      harness.check(true);
    } 
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    
    harness.check(clss != null && clss.length == 3 && 
		  clss[0] == clclass && clss[1] == clclass1);
    if (clss != null && clss.length == 3 &&
	!(clss[0] == clclass && clss[1] == clclass1)) {
      for (int i = 0; i < clss.length; i++) {
	harness.debug ("" + clss[i], false);
	harness.debug (" ", false);
      }
      harness.debug("");
    }

    try {	
      Class clsss = Class.forName("[[I");
      harness.check(clsss.getInterfaces().length, 2);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
    
    try {	
      Class clsss = Class.forName("[D");
      harness.check(clsss.getInterfaces().length, 2);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
  }
  
  public void test_newInstance()
  {
    harness.checkPoint("test_newInstance");
    Class clss = getClass();
    Object obj;
    
    try {
      obj = clss.newInstance();
      obj = clss.newInstance();
      obj = clss.newInstance();
      obj = clss.newInstance();
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error: newInstance failed");
      harness.debug(e);
    }
    catch (Error e) {
      harness.fail("Error: newInstance failed with an Error");
      harness.debug(e);
    }
  }
  
  
  public void test_forName()
  {
    harness.checkPoint("test_forName");
    try {
      Object obj = Class.forName("java.lang.Object");
      harness.check(obj != null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    // A non-existing class.
    checkClassNotFoundException("ab.cd.ef");

    // You can't use Class.forName() to get a primitive.
    checkClassNotFoundException("I");
    checkClassNotFoundException("int");

    // Some malformed array types.
    checkClassNotFoundException("[");
    checkClassNotFoundException("[int");
    checkClassNotFoundException("[II");
    checkClassNotFoundException("[L");
    checkClassNotFoundException("[L;");
    checkClassNotFoundException("[L[I;");
    checkClassNotFoundException("[Ljava.lang.Object");
    checkClassNotFoundException("[Ljava.lang.Objectx");
    checkClassNotFoundException("[Ljava.lang.Object;x");

    // Using slashes isn't allowed.
    checkClassNotFoundException("java/lang/Object");
  }

  private void checkClassNotFoundException(String className)
  {
    try {
      Class c = Class.forName(className);
      harness.debug("class: " + c);
      harness.debug("classloader: " + c.getClassLoader());
      if (c.isArray())
	{
	  Class ct = c.getComponentType();
	  harness.debug("component type: " + ct);
	  harness.debug("component type classloader: " + ct.getClassLoader());
	}
      harness.check(false);
    }
    catch (ClassNotFoundException e) {
      harness.check(true);
    }
    catch (Exception x) {
      harness.debug(x);
      harness.check(false);
    }
  }

  public void test_getClassloader()
  {
    harness.checkPoint("test_getClassloader");
    try {
      Class obj2 = Class.forName("gnu.testlet.java.lang.Class.ClassTest");
      ClassLoader ldr1 = obj2.getClassLoader();
      // For compatibility with (at least) JDK 1.3.1 & JDK 1.4.0 ...
      harness.check(ldr1 != null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }	
  }

  public void test_ComponentType()
  {
    harness.checkPoint("test_ComponentType");
    try {
      Class obj1 = Class.forName("java.lang.String");
      harness.check(obj1.getComponentType() == null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {
      Class obj2 = Class.forName("java.lang.Exception");
      harness.check(obj2.getComponentType() == null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {
      Class arrclass = Class.forName("[I");
      harness.check(arrclass.getComponentType() != null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {
      Class arrclass = Class.forName("[[[[I");
      harness.check(arrclass.getComponentType() != null);
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
  }

  public void test_isMethods()
  {
    harness.checkPoint("test_isMethods");
    try {
      Class obj1 = Class.forName("java.lang.String");
      harness.check(obj1.isInstance("babu"));
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {
      Class obj2 = Class.forName("java.lang.Integer");
      harness.check(obj2.isInstance(new Integer(10)));
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {
      int arr[] = new int[3];
      Class arrclass = Class.forName("[I");
      harness.check(arrclass.isInstance(arr));
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {
      Class cls1 = Class.forName("java.lang.String");
      Class supercls = Class.forName("java.lang.Object"); 
      harness.check(supercls.isAssignableFrom(cls1) &&
		    !cls1.isAssignableFrom(supercls));
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {  
      Class cls1 = Class.forName("java.lang.String");
      Class cls2 = Class.forName("java.lang.String");
      harness.check(cls2.isAssignableFrom(cls1));
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }

    try {          
      Class arrclass = Class.forName("[I");
      Class arrclass1 = Class.forName("[[[I");
      Class arrclass2 = Class.forName("[[D");
		    
      harness.check(arrclass.isArray() && arrclass1.isArray() && 
		    arrclass2.isArray());
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false);
    }
  }		

  public void test_getResource()
  {
    harness.checkPoint("test_getResource");
    // this test assume the classpath setting include current directory
    
    try {
      FileInputStream is = new FileInputStream("ClassTest.class");
      URL url = getClass().getResource("ClassTest.class");
      harness.check(url != null);
      if (url == null) {
	// Can't do any more of this test
	return;
      }
      
      InputStream uis = url.openStream();
      byte[] b1 = new byte[100];
      byte[] b2 = new byte[100];
      int ret = is.read(b1);
      harness.check(ret == 100);
      ret = uis.read(b2);
      harness.check(ret == 100);
      for (int i = 0; i < 100; i++) {
	if (b1[i] != b2[i]) {
	  harness.check(false);
	  break;
	}
	if (i == 99) {
	  harness.check(true);
	}
      }
      
      uis = getClass().getResourceAsStream("ClassTest.class");
      harness.check(uis != null);
      if (uis == null) {
	// Can't do any more of this test
	return;
      }
      ret = uis.read(b2);
      harness.check(ret == 100);
      for (int i = 0; i < 100; i++) {
	if (b1[i] != b2[i]) {
	  harness.check(false);
	  break;
	}
	if (i == 99) {
	  harness.check(true);
	}
      }
    }
    catch (IOException ex) {
      harness.debug(ex);
      harness.fail("IOException in test_getResource");
    }
  }

  public void test_getResourceAsStream()
  {
    harness.checkPoint("test_getResourceAsStream");
    // The bootclassloader does this different from most other CLs, so
    // add a test for it.
    InputStream in = Class.class.getResourceAsStream("Class.class");
    harness.check(in != null);
    in = Class.class.getResourceAsStream("/java/lang/Class.class");
    harness.check(in != null);
    // and a last extra check to see if we ever get a null
    in = InputStream.class.getResourceAsStream("Class.class");
    harness.check(in == null);
  }

  public void testall()
  {
    test_toString();
    test_getName();
    test_isInterface();
    test_getSuperclass();
    test_primitiveTypes();
    test_Modifiers();
    test_getInterfaces();
    test_newInstance();
    test_forName();
    test_ComponentType();
    test_getClassloader();
    test_isMethods();
    // This one doesn't work so well in Mauve.
    // test_getResource();
    test_getResourceAsStream();

  }

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall();
  }

}
