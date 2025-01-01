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

// Tags: JDK1.5

package gnu.testlet.javax.management.ObjectName;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class Parsing implements Testlet
{
  public void test(TestHarness h)
  {
    ObjectName name;
    try
      {
	h.checkPoint("Default name");
	name = new ObjectName("*:*");
	h.check(true);
	h.check(name.isDomainPattern(), true);
	h.check(name.isPropertyPattern(), true);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "*");
	h.check(name.getKeyPropertyListString(), "");
	h.checkPoint("Mixed keys and wildcards");
	name =
	  new ObjectName("jboss.management.local:j2eeType=ServiceModule,*,name=jbossmq-httpil.sar");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), true);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "jboss.management.local");
	h.check(name.getKeyPropertyListString(), "j2eeType=ServiceModule,name=jbossmq-httpil.sar");
	h.checkPoint("Match any domain with specific keys");
	name = new ObjectName("*:library=Classpath,project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), true);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "*");
	h.check(name.getCanonicalKeyPropertyListString(), "library=Classpath,project=GNU");
	h.check(name.getKeyPropertyListString(), "library=Classpath,project=GNU");
	h.checkPoint("Match any domain with specific keys and wildcard at end");
	name = new ObjectName("*:library=Classpath,project=GNU,*");
	h.check(true);
	h.check(name.isDomainPattern(), true);
	h.check(name.isPropertyPattern(), true);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "*");
	h.check(name.getKeyPropertyListString(), "library=Classpath,project=GNU");
	h.checkPoint("Match any domain beginning with 'fs' with specific keys");
	name = new ObjectName("fs?:library=Classpath,project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), true);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "fs?");
	h.check(name.getKeyPropertyListString(), "library=Classpath,project=GNU");
	h.checkPoint("Match any domain beginning with 'fs' with specific keys " +
		     "and wildcard at end");
	name = new ObjectName("fs?:library=Classpath,project=GNU,*");
	h.check(true);
	h.check(name.isDomainPattern(), true);
	h.check(name.isPropertyPattern(), true);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "fs?");
	h.check(name.getKeyPropertyListString(), "library=Classpath,project=GNU");
	h.checkPoint("Match the FSF domain with specific keys and wildcard at end");
	name = new ObjectName("fsf:library=Classpath,project=GNU,*");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), true);
	h.check(name.isPattern(), true);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyPropertyListString(), "library=Classpath,project=GNU");
	h.checkPoint("Match the FSF domain with specific keys");
	name = new ObjectName("fsf:library=Classpath,project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyPropertyListString(), "library=Classpath,project=GNU");
	h.checkPoint("Match the FSF domain with specific keys and quoted values");
	name = new ObjectName("fsf:library=\"Classpath\",project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyProperty("library"), "\"Classpath\"");
	h.check(name.getKeyPropertyListString(), "library=\"Classpath\",project=GNU");
	h.checkPoint("Match the FSF domain with specific keys and quoted values with "+
		     "escaped quote");
	name = new ObjectName("fsf:library=\"Class\\\"path\",project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyProperty("library"), "\"Class\\\"path\"");
	h.check(name.getKeyPropertyListString(), "library=\"Class\\\"path\",project=GNU");
	h.checkPoint("Match the FSF domain with specific keys and quoted values with "+
		     "escaped newline");
	name = new ObjectName("fsf:library=\"Class\\npath\",project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyProperty("library"), "\"Class\\npath\"");
	h.check(name.getKeyPropertyListString(), "library=\"Class\\npath\",project=GNU");
	h.checkPoint("Match the FSF domain with specific keys and quoted values with "+
		     "escaped backslash");
	name = new ObjectName("fsf:library=\"Class\\\\path\",project=GNU");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyProperty("library"), "\"Class\\\\path\"");
	h.check(name.getKeyPropertyListString(), "library=\"Class\\\\path\",project=GNU");
	h.checkPoint("Match the FSF domain with space preservation");
	name = new ObjectName("fsf: library = Classpath ");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getKeyProperty(" library "), " Classpath ");
	h.check(name.getKeyPropertyListString(), " library = Classpath ");
	h.checkPoint("Key ordering");
	name = new ObjectName("fsf:project=GNU,library=Classpath");
	h.check(true);
	h.check(name.isDomainPattern(), false);
	h.check(name.isPropertyPattern(), false);
	h.check(name.isPattern(), false);
	h.check(name.getDomain(), "fsf");
	h.check(name.getCanonicalKeyPropertyListString(), "library=Classpath,project=GNU");
	h.check(name.getKeyPropertyListString(), "project=GNU,library=Classpath");
      }
    catch (MalformedObjectNameException e)
      {
	h.debug(e);
	h.check(false);
      }
    try
      {
	name = new ObjectName("fsf:lib,rary=Classpath,project=GNU");
	h.fail("Comma allowed in key name");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Comma in key name caught");
      }
    try
      {
	name = new ObjectName("fsf:lib=rary=Classpath,project=GNU");
	h.fail("Equals allowed in key name");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Equals in key name caught");
      }
    try
      {
	name = new ObjectName("fsf:lib:rary=Classpath,project=GNU");
	h.fail("Colon allowed in key name");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Colon in key name caught");
      }
    try
      {
	name = new ObjectName("fsf:lib*rary=Classpath,project=GNU");
	h.fail("Asterisk allowed in key name");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Asterisk in key name caught");
      }
    try
      {
	name = new ObjectName("fsf:lib?rary=Classpath,project=GNU");
	h.fail("Question mark allowed in key name");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Question mark in key name caught");
      }
    try
      {
	name = new ObjectName("fsf:library=Classpath,library=Sun,project=GNU");
	h.fail("Duplicate key allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Duplicate key caught");
      }
    try
      {
	name = new ObjectName("fsf:library=Clas,path,project=GNU");
	h.fail("Comma allowed in unquoted value");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Comma in unquoted value caught");
      }
    try
      {
	name = new ObjectName("fsf:library=Clas=path,project=GNU");
	h.fail("Equals allowed in unquoted value");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Equals in unquoted value caught");
      }
    try
      {
	name = new ObjectName("fsf:library=Clas:path,project=GNU");
	h.fail("Colon allowed in unquoted value");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Colon in unquoted value caught");
      }
    try
      {
	name = new ObjectName("fsf:library=Clas\"path,project=GNU");
	h.fail("Quote allowed in unquoted value");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Quote in unquoted value caught");
      }
    try
      {
	name = new ObjectName("fsf:library=\"Classpath,project=GNU");
	h.fail("Unclosed quotes allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Unclosed quotes caught");
      }
    try
      {
	name = new ObjectName("fsf:library=\"Class\"path\",project=GNU");
	h.debug(name.getKeyProperty("library"));
	h.fail("Unescaped quote allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Unescaped quote caught");
      }
    try
      {
	name = new ObjectName("fsf:");
	h.fail("Non-pattern with no keys allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Non-pattern with no keys caught");
      }
    try
      {
	name = new ObjectName("fsf:*,*");
	h.fail("Pattern with multiple asterisks in properties allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Pattern with multiple asterisks in properties caught");
      }
    try
      {
	name = new ObjectName("f\nsf:library=Classpath");
	h.fail("Domain with newline allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Domain with newline caught");
      }
    try
      {
	name = new ObjectName("fsf:lib\nrary=Classpath");
	h.fail("Key with newline allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Key with newline caught");
      }
    try
      {
	name = new ObjectName("fsf:library=Class\npath");
	h.fail("Unquoted value with newline allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Unquoted value with newline caught");
      }
    try
      {
	name = new ObjectName("fsf:library=\"Class\npath\"");
	h.fail("Quoted value with newline allowed");
      }
    catch (MalformedObjectNameException e)
      {
	h.check(true, "Quoted value with newline caught");
      }

  }
}

