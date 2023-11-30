/*
 * PreferenceTest.java -- simple test for java.utils.prefs Copyright (C) 2006
 * Lima Software.
 * 
 * This file is part of Mauve.
 * 
 * Mauve is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2, or (at your option) any later version.
 * 
 * Mauve is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * Mauve; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * 
 */

// Tags: JDK1.4
package gnu.testlet.java.util.prefs;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import java.util.prefs.BackingStoreException;
import java.util.prefs.NodeChangeEvent;
import java.util.prefs.NodeChangeListener;
import java.util.prefs.PreferenceChangeEvent;
import java.util.prefs.PreferenceChangeListener;
import java.util.prefs.Preferences;

/**
 * This simple test just read and write a set of preference. Does not assume a
 * particular backend.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class PreferenceTest implements Testlet
{
  /**
   * The expected full name of this preference node. <strong>Note:</strong>
   * change it if you move this class, it always reflects the package name.
   */
  private static final String FULL_PATH = "/gnu/testlet/java/util/prefs";

  /**
   * Default key used for tests.
   */
  private static final String KEY = "AppName";

  /**
   * Default value for the test key.
   */
  private static final String VALUE = "GNU Classpath - Preference API Test Case";

  /**
   * Defines if we want debug information on the default debug log or not.
   * Enable the if you need it or leave disabled for normal tests.
   */
  private static final boolean DEBUG = false;

  /* Test Harness */
  protected TestHarness harness = null;

  /** Preference */
  private Preferences prefs = null;

  public void test(TestHarness harness)
  {
    this.harness = harness;

    // initiliaze tests, call it before any other method of this class
    setup();

    if (DEBUG) printInfo();

    // run tests
    testAbsolutePath();

    // clear the preference tree, to be sure it does not contains
    // keys we will use
    testClear();

    testPut();
    testByte();
    testBoolean();
    testSpecialCharacters();
    
    testListener();
    testChildren();
  }

  /**
   * Setup the preference api backend.
   */
  private void setup()
  {
    // System.setProperty("java.util.prefs.PreferencesFactory",
    // "gnu.java.util.prefs.GConfBasedFactory");

    this.prefs = Preferences.userNodeForPackage(PreferenceTest.class);
  }

  private void testAbsolutePath()
  {
    this.harness.checkPoint("absolutePath()");

    String absolutePath = this.prefs.absolutePath();
    print("Absolute path: " + absolutePath);

    this.harness.check(FULL_PATH.compareTo(absolutePath) == 0);
  }

  private void testClear()
  {
    this.harness.checkPoint("testClear()");

    try
      {
        this.prefs.clear();
      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("testClear()");
      }

    this.harness.check(true);
  }

  /**
   * Put the default key on the preference node, then get it and check the
   * result to see if the preference api is actually capable of storing
   * preferences.
   */
  private void testPut()
  {
    this.harness.checkPoint("testPut()");

    this.prefs.put(KEY, VALUE);

    // suggest a sync to try to avoid memory caching.
    try
      {
        this.prefs.sync();

      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("testPut(), call to sync");
      }

    String value
      = this.prefs.get(KEY,
                       "Wrong value for key: " + KEY + ", expected: " + VALUE);

    print("Key (" + KEY + "): " + value);
    this.harness.check(VALUE.compareTo(value) == 0);
  }

  /**
   * Add a series of children to this node, put some preferences inside them and
   * retrieve everything, checking that the preference backend correctly handles
   * the tree structure of the entries under the current node. <strong>Note</strong>:
   * this test removes the preference node.
   */
  private void testChildren()
  {
    this.harness.checkPoint("testChildren()");

    String absolutePath = null;

    // add 3 new nodes, these will be direct children of the current node

    // node 1
    Preferences pref1 = this.prefs.node("children_1");
    this.harness.check("children_1".compareTo(pref1.name()) == 0);
    absolutePath = pref1.absolutePath();
    this.harness
        .check((FULL_PATH + "/children_1").compareTo(absolutePath) == 0);

    // node 2
    Preferences pref2 = this.prefs.node("children_2");
    this.harness.check("children_2".compareTo(pref2.name()) == 0);
    absolutePath = pref2.absolutePath();
    this.harness
        .check((FULL_PATH + "/children_2").compareTo(absolutePath) == 0);

    // node 3
    Preferences pref3 = this.prefs.node("children_3");
    this.harness.check("children_3".compareTo(pref3.name()) == 0);
    absolutePath = pref3.absolutePath();
    this.harness
        .check((FULL_PATH + "/children_3").compareTo(absolutePath) == 0);

    // now add a preference key to each of these new nodes
    pref1.put("key1", "value1");
    pref2.put("key2", "value2");
    pref3.put("key3", "value3");

    // add a subnode for child #1
    Preferences child1 = pref1.node("subPreference1");
    this.harness.check("subPreference1".compareTo(child1.name()) == 0);
    absolutePath = child1.absolutePath();
    this.harness.check((FULL_PATH + "/children_1/" + "subPreference1")
        .compareTo(absolutePath) == 0);

    child1.put("key1-child1", "some value");

    // retrieve the list of children of the root node
    this.harness.checkPoint("testAddChildren() - check new children");
    String[] expResult = {
        "children_1", "children_2", "children_3"
    };
    if (!listChildren(this.prefs, expResult))
      {
        this.harness.fail("testAddChildren(), children listing error");
      }

    this.harness.checkPoint("testAddChildren() - check subnodes");
    expResult = new String[] {
      "subPreference1"
    };
    if (!listChildren(pref1, expResult))
      {
        this.harness.fail("testAddChildren(), children listing error");
      }

    // clean everything
    try
      {
        this.prefs.removeNode();
        this.prefs.flush();

      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("testAddChildren(), call to clear()");
      }

    // to check that the node is empty we simply call childrenNames on it
    // this operation should fail because the node does not exist anymore
    this.harness.checkPoint("testAddChildren() - checking emptyness");
    try
      {
        this.prefs.childrenNames();
        this.harness.fail("The node should not exist anymore!");
      }
    catch (IllegalStateException e)
      {
        this.harness.check(true);
      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("The node should not exist anymore!");
      }
  }

  private boolean listChildren(Preferences pref, String[] expResult)
  {
    boolean _res = false;

    try
      {
        String[] result = pref.childrenNames();

        print("Resuls length: " + result.length +
              ", expected: " + expResult.length);
      
        this.harness.check(result.length == expResult.length);

        for (int i = 0; i < expResult.length; i++)
          {
            print("result[" + i + "] = " + result[i] +
                  ", expected = " + expResult[i]);
            this.harness.check(result[i], expResult[i]);
          }

        _res = true;

      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("call to childrenNames()");
      }

    return _res;
  }

  private void testByte()
  {
    this.harness.checkPoint("testByte()");

    String string = "an array of bytes value";
    byte[] bytes = null;
    byte[] result = null;

    ByteArrayOutputStream stream = new ByteArrayOutputStream();
    ObjectOutputStream oStream;

    try
      {
        oStream = new ObjectOutputStream(stream);
        oStream.writeObject(string);
        bytes = stream.toByteArray();
        
        this.harness.checkPoint("testByte() - put byte array");
        prefs.putByteArray(KEY, bytes);
        
        this.harness.checkPoint("testByte() - get byte array");
        result = prefs.getByteArray(KEY, null);

        // this fails, but the result is correct when restoring the
        // String
        // this.harness.check(result, bytes);

        ByteArrayInputStream iStream = new ByteArrayInputStream(result);
        ObjectInputStream oiStream = new ObjectInputStream(iStream);
        String rString = (String) oiStream.readObject();

        print("Result: " + rString + ", expected: " + string);
        this.harness.check(rString, string);

      }
    catch (IOException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("call to testByte() - IO Exception");
      }
    catch (ClassNotFoundException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("call to testByte() - ClassNotFoundException");
      }
    }

  private void testBoolean()
  {
    this.harness.checkPoint("testBoolean()");

    String key = "boolean_key";
    String _true = "TrUe";

    // test "normal" booleans
    this.prefs.putBoolean(key, true);
    boolean result = this.prefs.getBoolean(key, false);

    this.harness.check(result, true);

    // test String as boolean
    this.prefs.remove(key);
    try
      {
        this.prefs.flush();
      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("call to testBoolean() - fail to flush");
      }

      this.prefs.put(key, _true);
      result = this.prefs.getBoolean(key, false);

      this.harness.check(result, true);
    }

  /**
   * This test is used by the GConf backend to test if it correctly handles
   * nodes that contains invalid characters.
   */
  private void testSpecialCharacters()
  {
    this.harness.checkPoint("testSpecialCharacters()");
    
    // try an invalid name as path
    Preferences _prefs = this.prefs.node("invalid children node");
    _prefs.put("Invalid Key", "An invalid key, on an invalid subnode");
    _prefs.put("Valid_Key", "A valid key, on an invalid subnode");
    
    String one = _prefs.get("Invalid Key", "unable to get the invalid key");
    String two = _prefs.get("Valid_Key", "unable to get the invalid key");
    
    this.harness.check(one, "An invalid key, on an invalid subnode");
    this.harness.check(two, "A valid key, on an invalid subnode");
    
    try
      {
        _prefs.flush();
        _prefs.removeNode();
      }
    catch (BackingStoreException e)
      {
        print(e.getLocalizedMessage());
        this.harness.fail("call to testSpecialCharacters() fail to removeNode");
      }
  }
  
  private void testListener()
  {
    this.harness.checkPoint("testListener()");

    PreferenceListener listener = new PreferenceListener();

    this.harness.checkPoint("testListener() - adding listeners");
    this.prefs.addNodeChangeListener(listener);
    this.prefs.addPreferenceChangeListener(listener);

    // store this, key, read it then remove it
    this.harness.checkPoint("testListener() - inserting key");

    this.prefs.put("new_key", "some value");
    String key = this.prefs.get("new_key", "Wrong! Wrong! Wrong!");
    this.harness.check(key, "some value");

    this.harness.checkPoint("testListener() - updating key");

    this.prefs.put("new_key", "some other value");
    key = this.prefs.get("new_key", "Wrong! Wrong! Wrong!");
    this.harness.check(key, "some other value");

    this.harness.checkPoint("testListener() - removing listeners");

    this.prefs.removeNodeChangeListener(listener);
    this.prefs.removePreferenceChangeListener(listener);

    this.harness.checkPoint("testListener() - removing key");
    this.prefs.remove("new_key");
  }

  /**
   * Prints on screen some information about the class we are about to test.
   * This can be useful for debugging, ant to check if a particoular backend is
   * enabled.
   */
  private void printInfo()
  {
    String backendName = System.getProperty(
        "java.util.prefs.PreferencesFactory",
        "No backend registered, using default backend");
    String vendor = System.getProperty("java.vendor");

    this.harness.debug(vendor);
    this.harness.debug(backendName);
  }

  private void print(String message)
  {
    if (DEBUG) harness.debug(message);
  }

  private class PreferenceListener implements NodeChangeListener,
      PreferenceChangeListener
  {
    public void childAdded(NodeChangeEvent event)
    {
      PreferenceTest.this.harness.check(true);

      print("Child added!");
      String name = event.getChild().name();
      print("name: " + name);
    }

    public void childRemoved(NodeChangeEvent event)
    {
      PreferenceTest.this.harness.check(true);

      print("Child removed!");
      String name = event.getChild().name();
      print("name: " + name);
    }

    public void preferenceChange(PreferenceChangeEvent event)
    {
      PreferenceTest.this.harness.check(true);

      print("Preference changed!");
      String name = event.getNode().name();
      String value = event.getNewValue();
      print("name: " + name);
      print("value: " + value);
    }
  }
}
