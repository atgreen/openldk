// Tags: JDK1.2

// Copyright (C) 2005 Mark J. Wielaard <mark@klomp.org>

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
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.datatransfer.DataFlavor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.datatransfer.*;
import java.io.*;

public class flavor implements Testlet
{
  private TestHarness harness;

  public void test(TestHarness harness)      
  {
    this.harness = harness;

    // The static fields
    DataFlavor df = DataFlavor.javaFileListFlavor;
    String primaryType = "application";
    String subType = "x-java-file-list";
    Class representationClass = java.util.List.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);

    df = DataFlavor.plainTextFlavor;
    primaryType = "text";
    subType = "plain";
    String param = "charset";
    String value = "unicode";
    representationClass = java.io.InputStream.class;
    checkFlavor(df, primaryType, subType, param, value, representationClass);

    df = DataFlavor.stringFlavor;
    primaryType = "application";
    subType = "x-java-serialized-object";
    representationClass = java.lang.String.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);

    try
      {
	df = new DataFlavor("image/jpeg");
	primaryType = "image";
	subType = "jpeg";
	representationClass = java.io.InputStream.class;
	checkFlavor(df, primaryType, subType, null, null, representationClass);
	harness.check(df.getHumanPresentableName(), "image/jpeg");
      }
    catch (ClassNotFoundException cnfe)
      {
	harness.debug(cnfe);
	harness.check(false, cnfe.toString());
      }

    try
      {
	df = new DataFlavor("application/x-java-serialized-object"
			    + "; class=java.awt.Point");
	primaryType = "application";
	subType = "x-java-serialized-object";
	representationClass = java.awt.Point.class;
	checkFlavor(df, primaryType, subType, null, null, representationClass);
	harness.check(df.getHumanPresentableName(),
		      "application/x-java-serialized-object"
		      + "; class=java.awt.Point");
      }
    catch (ClassNotFoundException cnfe)
      {
        harness.debug(cnfe);
        harness.check(false, cnfe.toString());
      }

    df = new DataFlavor("application/x-java-serialized-object"
			+ "; class=java.awt.Point", "Point object");
    primaryType = "application";
    subType = "x-java-serialized-object";
    representationClass = java.awt.Point.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);
    harness.check(df.getHumanPresentableName(), "Point object");

    df = new DataFlavor("application/x-java-serialized-object"
			+ "; class=java.awt.Point", null);
    primaryType = "application";
    subType = "x-java-serialized-object";
    representationClass = java.awt.Point.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);
    harness.check(df.getHumanPresentableName(),
		  "application/x-java-serialized-object"
		  + "; class=java.awt.Point");

    df = new DataFlavor("image/jpeg", "JPEG image");
    primaryType = "image";
    subType = "jpeg";
    representationClass = java.io.InputStream.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);
    harness.check(df.getHumanPresentableName(), "JPEG image");

    df = new DataFlavor("image/jpeg", null);
    primaryType = "image";
    subType = "jpeg";
    representationClass = java.io.InputStream.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);
    harness.check(df.getHumanPresentableName(), "image/jpeg");

    try
      {
	df = new DataFlavor("application/x-java-serialized-object"
			    + "; class=java.awt.Point", "Point object",
			    ClassLoader.getSystemClassLoader());
	primaryType = "application";
	subType = "x-java-serialized-object";
	representationClass = java.awt.Point.class;
	checkFlavor(df, primaryType, subType, null, null, representationClass);
	harness.check(df.getHumanPresentableName(), "Point object");
      }
    catch (ClassNotFoundException cnfe)
      {
        harness.debug(cnfe);
        harness.check(false, cnfe.toString());
      }

    try
      {
	df = new DataFlavor("application/x-java-serialized-object"
			    + "; class=java.awt.Point", null,
			    ClassLoader.getSystemClassLoader());
	primaryType = "application";
	subType = "x-java-serialized-object";
	representationClass = java.awt.Point.class;
	checkFlavor(df, primaryType, subType, null, null, representationClass);
	harness.check(df.getHumanPresentableName(),
		      "application/x-java-serialized-object"
		      + "; class=java.awt.Point");
      }
    catch (ClassNotFoundException cnfe)
      {
        harness.debug(cnfe);
        harness.check(false, cnfe.toString());
      }

    df = new DataFlavor(java.awt.Point.class, "Point object");
    primaryType = "application";
    subType = "x-java-serialized-object";
    representationClass = java.awt.Point.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);
    harness.check(df.getHumanPresentableName(), "Point object");

    df = new DataFlavor(java.awt.Point.class, null);
    primaryType = "application";
    subType = "x-java-serialized-object";
    representationClass = java.awt.Point.class;
    checkFlavor(df, primaryType, subType, null, null, representationClass);
    harness.check(df.getHumanPresentableName(),
		  "application/x-java-serialized-object"
                  + "; class=java.awt.Point");
  }
  
  private void checkFlavor(DataFlavor df, String primaryType,
			   String subType, String param, String value,
			   Class representationClass)
  {
    harness.checkPoint(df.toString());
    harness.check(df.getPrimaryType(), primaryType);
    harness.check(df.getSubType(), subType);
    harness.check(df.getRepresentationClass(), representationClass);

    boolean hasClassParam = subType.startsWith("x-java");
    String mimeType = primaryType + "/" + subType;
    if (hasClassParam)
      mimeType += "; class=" + representationClass.getName();
    if (param != null)
      mimeType += "; " + param + "=" + value;
    harness.check(df.getMimeType(), mimeType);

    harness.check(df.isMimeTypeEqual(primaryType + "/" + subType));
    try
      {
	harness.check(df.isMimeTypeEqual((DataFlavor) df.clone()));

	harness.check(((DataFlavor) df.clone()).equals(df));
	harness.check(df.equals((DataFlavor) df.clone()));
      }
    catch (CloneNotSupportedException cnse)
      {
	harness.debug(cnse);
	harness.check(false, cnse.toString());
      }

    harness.check(df.getParameter("humanPresentableName"),
		  df.getHumanPresentableName());
    if (hasClassParam)
      harness.check(df.getParameter("class"), representationClass.getName());
    else
      harness.check(df.getParameter("class"), null);

    if (param != null)
      harness.check(df.getParameter(param), value);

    harness.check(df.getParameter("NO-SUCH-PARAM"), null);

    harness.check(df.isFlavorJavaFileListType(),
		  subType.equals("x-java-file-list"));
    harness.check(df.isFlavorRemoteObjectType(),
		  subType.equals("x-java-remote-object"));
    harness.check(df.isFlavorSerializedObjectType(),
		  subType.equals("x-java-serialized-object"));
    harness.check(df.isRepresentationClassInputStream(),
		  java.io.InputStream.class.isAssignableFrom
		  (representationClass));
    harness.check(df.isRepresentationClassRemote(),
		  java.rmi.Remote.class.isAssignableFrom
		  (representationClass));
    harness.check(df.isRepresentationClassSerializable(),
		  java.io.Serializable.class.isAssignableFrom
		  (representationClass));
  }
}
