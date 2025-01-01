// Tags: JDK1.1
// Uses: sub/OtherPkg sub/Super Other

// Test reflection member accessibility checks.

package gnu.testlet.java.lang.reflect;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.lang.reflect.*;
import gnu.testlet.java.lang.reflect.sub.*;

public class ReflectAccess extends Super implements Testlet
{
  TestHarness harness;
  
  public void test(TestHarness harness)
  {
    this.harness = harness;
    try 
    {
      doTest();
    }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail(x.toString());
    }
  }
  
  void doTest() throws Exception
  {
    Method methodA = ReflectAccess.class.getDeclaredMethod("a", null);
    Method methodB = ReflectAccess.class.getDeclaredMethod("b", null);
    Method methodC = ReflectAccess.class.getDeclaredMethod("c", null);

    Field fieldD = ReflectAccess.class.getDeclaredField("d");
    Field fieldE = ReflectAccess.class.getDeclaredField("e");
    Field fieldF = ReflectAccess.class.getDeclaredField("f");
    
    Method methodG = OtherPkg.class.getDeclaredMethod("g", null);
    Method methodH = OtherPkg.class.getDeclaredMethod("h", null);
    Method methodI = OtherPkg.class.getDeclaredMethod("i", null);

    Field fieldJ = OtherPkg.class.getDeclaredField("j");
    Field fieldK = OtherPkg.class.getDeclaredField("k");
    Field fieldL = OtherPkg.class.getDeclaredField("l");
    
    Method methodM = Other.class.getDeclaredMethod("m", null);
    Method methodN = Other.class.getDeclaredMethod("n", null);
    Method methodO = Other.class.getDeclaredMethod("o", null);

    Field fieldP = Other.class.getDeclaredField("p");
    Field fieldQ = Other.class.getDeclaredField("q");
    Field fieldR = Other.class.getDeclaredField("r");

    try
    {
      Method methodT = ReflectAccess.class.getDeclaredMethod("t", null);
      harness.fail(methodT + " is not declared in class ReflectAccess");
    }
    catch (NoSuchMethodException x)
    {
      // ok
      harness.check(true, "method 't' is declared in class ReflectAccess");
    }
    
    Method methodS = Super.class.getDeclaredMethod("s", null);
    Method methodT = Super.class.getDeclaredMethod("t", null);
    Method methodU = Super.class.getDeclaredMethod("u", null);
    Method methodV = Super.class.getDeclaredMethod("v", null);

    Field fieldW = Super.class.getDeclaredField("w");
    Field fieldX = Super.class.getDeclaredField("x");
    Field fieldY = Super.class.getDeclaredField("y");
    Field fieldZ = Super.class.getDeclaredField("z");

    Object obj = new ReflectAccess();
    
    methodA.invoke(obj, null);
    methodB.invoke(null, null);
    methodC.invoke(obj, null);
    
    harness.check (fieldD.getChar(obj) == 'd', "field d is accessible");
    harness.check (fieldE.getChar(obj) == 'e', "field e is accessible");
    harness.check (fieldF.getChar(obj) == 'f', "field f is accessible");

    obj = new OtherPkg();
    
    try
    {
      methodG.invoke(obj, null);
      harness.fail(methodG + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, methodG + " is inaccessible");
    }
    try
    {
      methodH.invoke(obj, null);
      harness.fail(methodH + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, methodH + " is inaccessible");
    }
    try
    {
      methodI.invoke(obj, null);
      harness.fail(methodI + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, methodI + " is inaccessible");
    }
    try
    {
      fieldJ.getChar(obj);
      harness.fail(fieldJ + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, fieldJ + " is inaccessible");
    }
    try
    {
      fieldK.getChar(obj);
      harness.fail(fieldK + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, fieldK + " is inaccessible");
    }
    try
    {
      fieldL.getChar(obj);
      harness.fail(fieldL + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, fieldL + " is inaccessible");
    }
    
    obj = new Other();

    methodM.invoke(null, null);
    methodN.invoke(obj, null);
    
    try
    {
      methodO.invoke(obj, null);
      harness.fail(methodO + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, methodO + " is inaccessible");
    }
    
    methodO.setAccessible(true);
    methodO.invoke(obj, null);

    harness.check (fieldP.getChar(obj) == 'p');
    harness.check (fieldQ.getChar(obj) == 'q');
    
    try
    {
      fieldR.getChar(obj);
      harness.fail(fieldR + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, fieldR + " is inaccessible");
    }
    
    fieldR.setAccessible(true);
    harness.check(fieldR.getChar(obj) == 'r', fieldR + " is accessible");
    obj = new ReflectAccess();
    
    try 
    {
      methodS.invoke(obj, null);
      harness.fail(methodS + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, methodS + " is inaccessible");
    }
    
    methodT.invoke(obj, null);
    methodU.invoke(obj, null);
    
    try 
    { 
      methodV.invoke(obj, null);
      harness.fail(methodV + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, methodV + " is inaccessible");
    }

    harness.check (fieldW.getChar(obj) == 'w');
    harness.check (fieldX.getChar(obj) == 'x');

    try 
    { 
      fieldY.getChar(obj);
      harness.fail(fieldY + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, fieldY + " is inaccessible");
    }
    
    try
    { 
      fieldZ.getChar(obj); 
      harness.fail(fieldZ + " should not be accessible");
    }
    catch (IllegalAccessException x)
    {
      // ok
      harness.check(true, fieldZ + " is inaccessible");
    }
  }
  
  private void a() 
  { 
  }

  private static void b()
  {
  }
  
  protected void c()
  {
  }
  
  private char d = 'd';
  private static char e = 'e';
  protected char f = 'f';
}
