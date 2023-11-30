/* CollatorTests.java -- Contain various tests for the Collator and
   RuleBasedCollator class.
   
   Copyright (C) 2006 Mario Torre <neugens@limasoftware.net>
   
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

// Tags: JDK1.4

package gnu.testlet.java.text.RuleBasedCollator;

import java.text.CollationElementIterator;
import java.text.Collator;
import java.text.ParseException;
import java.text.RuleBasedCollator;
import java.util.Locale;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class CollatorTests implements Testlet
{
  private TestHarness harness = null;
  
  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    // TODO Auto-generated method stub
    this.harness = harness;
    
    basicCompare();
    orderComparision();
  }
  
  private void basicCompare()
  {
    // test taken from the JDK Javadoc
    // Compare two strings in the default locale
    Collator myCollator = Collator.getInstance(Locale.US);
    
    this.harness.check((myCollator.compare("abc", "ABC") < 0),
                       "basic comparision");
    
    myCollator.setStrength(Collator.PRIMARY);
    this.harness.check((myCollator.compare("abc", "ABC") == 0),
                       "equivalent strings");
    
    String SimpleRule = "< a< b< c< d";
    
    boolean pass = false;
    try
      {
        RuleBasedCollator simpleRuleCollator =
          new RuleBasedCollator(SimpleRule);
        
        pass = (simpleRuleCollator.compare("abc", "ABC") < 0);
      }
    catch (ParseException e)
      {
        pass = false;
      }
    
    this.harness.check(pass, "simple rule test");
  }
  
  private void orderComparision()
  {
    RuleBasedCollator c = (RuleBasedCollator)Collator.getInstance(Locale.US);
    CollationElementIterator iter = c.getCollationElementIterator("Foo");

    // given by the 1.5.0 jdk
    int [][] results = 
      {
       {5767169, 88, 0, 1},
       {6356992, 97, 0, 0},
       {6356992, 97, 0, 0}
      };
    
    int element;
    int i = 0;
    while ((element = iter.next()) != CollationElementIterator.NULLORDER)
      {
        
        int primary = CollationElementIterator.primaryOrder(element);
        int secondary = CollationElementIterator.secondaryOrder(element);
        int tertiary = CollationElementIterator.tertiaryOrder(element);
        
        this.harness.check((results[i][0] == element), "element #" + i);
        
        this.harness.check((results[i][1] == primary), "primary #" + i);
        this.harness.check((results[i][2] == secondary), "secondary #" + i);
        this.harness.check((results[i][3] == tertiary), "tertiary #" + i);
        
        i++;
      }
  }
}
