//Tags: JDK1.2
//Uses: foo/TestSubject foo/info/TestSubjectBeanInfo bar/TestSubject bar/info/TestSubjectBeanInfo

//Copyright (C) 2005  Robert Schuster <theBohemian@gmx.net>

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
//Boston, MA 02111-1307, USA.  */
package gnu.testlet.java.beans.Introspector;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;

/**
 * This class tests whether the Introspector is properly preparing the BeanInfo
 * classes of two Bean classes which live in different packages but share a
 * common base name.
 * 
 * @author Robert Schuster
 * 
 */
public class getBeanInfo4 implements Testlet {

	public void test(TestHarness harness) {
		Introspector.setBeanInfoSearchPath(new String[] {
				"gnu.testlet.java.beans.Introspector.foo.info",
				"gnu.testlet.java.beans.Introspector.bar.info" });

		Class klazz;
		BeanInfo bi = null;
		BeanDescriptor bd;
		boolean failed = false;

		klazz = gnu.testlet.java.beans.Introspector.bar.TestSubject.class;
		try {
			bi = Introspector.getBeanInfo(klazz);
		} catch (IntrospectionException ie) {
			failed = true;
		}

		harness.checkPoint("BAR variant");
		harness.check(failed, false);
		bd = bi.getBeanDescriptor();

		harness.check(bd.getName(), "BAR TestSubject");
		harness.check(bd.getBeanClass(),
				gnu.testlet.java.beans.Introspector.bar.TestSubject.class);

		failed = false;

		klazz = gnu.testlet.java.beans.Introspector.foo.TestSubject.class;
		try {
			bi = Introspector.getBeanInfo(klazz);
		} catch (IntrospectionException ie) {
			failed = true;
		}

		harness.checkPoint("FOO variant");
		harness.check(failed, false);
		bd = bi.getBeanDescriptor();

		harness.check(bd.getName(), "FOO TestSubject");
		harness.check(bd.getBeanClass(),
				gnu.testlet.java.beans.Introspector.foo.TestSubject.class);

	}

}
