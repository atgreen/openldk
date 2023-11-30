/* EnumerateAndModify.java -- A test for Hashtable
   Copyright (C) 2006 Fridjof Siebert
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

// Tags: JDK1.0

package gnu.testlet.java.util.Hashtable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Hashtable;
import java.util.Enumeration;


/**
 * EnumerateAndModify tests that enumerating a Hashtable that is
 * concurrently modified will not throw an exception.
 *
 * @author Fridtjof Siebert (siebert@aicas.com)
 */
public class EnumerateAndModify implements Testlet
{

  /**
   * test is the main test routine testing enumaration of keys and
   * elements of a concurrently modified hashtable.
   *
   * @param harness the current test harness.
   */
  public void test(TestHarness harness)
  {
    Hashtable allKeys = new Hashtable(); 
    allKeys.put("C","c");
    allKeys.put("D","d");
    allKeys.put("A","a");
    allKeys.put("B","b");
    allKeys.put("E","e");
    allKeys.put("C1","c");
    allKeys.put("D1","d");
    allKeys.put("A1","a");
    allKeys.put("B1","b");
    allKeys.put("E1","e");

    Hashtable allElements = new Hashtable(); 
    allElements.put("c","c");
    allElements.put("d","d");
    allElements.put("a","a");
    allElements.put("b","b");
    allElements.put("e","e");
    allElements.put("c1","c1");
    allElements.put("d1","d1");
    allElements.put("a1","a1");
    allElements.put("b1","b1");
    allElements.put("e1","e1");

    Hashtable ht = new Hashtable(); 
    ht.put("A","a");
    ht.put("B","b");
    ht.put("C","c");
    ht.put("D","d");
    ht.put("E","e");
    
    Throwable thrown;
    boolean returnedOnlyKeysThatWerePut = true; 
    
    try
      {
    	// We walk through the keys while we modify the hashtable. This
    	// is not legal, and the result of the enumaration is undefined,
    	// but we should not get any exception when enumerating and we
    	// should not get null or any key that was never added. 
        for (Enumeration e = ht.keys(); e.hasMoreElements(); )
          {
            String str = (String) e.nextElement();
            if (str != null && !allKeys.containsKey(str))
              {
                returnedOnlyKeysThatWerePut = false;
              }
            ht.put("C","c");
            ht.put("D","d");
            ht.put("A","a");
            ht.put("B","b");
            ht.put("E","e");
            ht.put("C1","c");
            ht.put("D1","d");
            ht.put("A1","a");
            ht.put("B1","b");
            ht.put("E1","e");
          }
        thrown = null; 
    }
    catch (Throwable t)
      {
        t.printStackTrace(); 
        thrown = t; 
      }
    harness.check(thrown == null);
    harness.check(returnedOnlyKeysThatWerePut);

    ht = new Hashtable(); 
    ht.put("A","a");
    ht.put("B","b");
    ht.put("C","c");
    ht.put("D","d");
    ht.put("E","e");

    boolean returnedOnlyElementsThatWerePut = true; 

    try
      {
        // We walk through the keys while we modify the hashtable. This
        // is not legal, and the result of the enumaration is undefined,
        // but we should not get any exception when enumerating and we
        // should not get null or any key that was never added. 
        for (Enumeration e = ht.elements(); e.hasMoreElements(); )
          {
            String str = (String) e.nextElement();
            if (str != null && !allElements.containsKey(str))
              {
                returnedOnlyElementsThatWerePut = false;
              }
            ht.put("C","c");
            ht.put("D","d");
            ht.put("A","a");
            ht.put("B","b");
            ht.put("E","e");
            ht.put("C1","c1");
            ht.put("D1","d1");
            ht.put("A1","a1");
            ht.put("B1","b1");
            ht.put("E1","e1");
          }
        thrown = null; 
    }
    catch (Throwable t)
      {
        thrown = t; 
      }
    harness.check(thrown == null);
    harness.check(returnedOnlyElementsThatWerePut);
  }
}
