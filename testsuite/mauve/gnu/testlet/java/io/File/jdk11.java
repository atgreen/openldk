/*************************************************************************
/* jdk11.java -- java.io.File 1.1 tests
/*
/* Copyright (c) 2001, 2002, 2003, 2005 Free Software Foundation, Inc.
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.io.File;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.config;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.util.Date;

public class jdk11 implements Testlet, FilenameFilter
{
  
  public void test (TestHarness harness)
  {
    String srcdirstr = harness.getSourceDirectory ();
    String tmpdirstr = harness.getTempDirectory ();

    // File (String)
    File srcdir = new File (srcdirstr);
    File tmpdir = new File (tmpdirstr);
    String THIS_FILE = new String ("gnu" + 
				   File.separator + "testlet" +
				   File.separator + "java" + 
				   File.separator + "io" + 
				   File.separator + "File" + 
				   File.separator + "tmp");

    // File (File, String)
    File cons = new File (srcdir, THIS_FILE);
    // File (String, String)
    File cons2 = new File (srcdirstr, THIS_FILE);


    // mkdir ()
    harness.check (cons.mkdir (), "mkdir ()");

    // canRead ()
    harness.check (cons.canRead (), "canRead ()");
    // equals (Object)
    harness.check (cons.equals (cons2), "equals ()");
    // isDirectory ()
    harness.check (srcdir.isDirectory (), "isDirectory ()");
    harness.check (tmpdir.isDirectory (), "isDirectory ()");

    String TMP_FILENAME = "File.tst";
    String TMP_FILENAME2 = "Good.doc";
    String TMP_FILENAME3 = "File.doc";

    // create empty file
    File tmp = new File (cons, TMP_FILENAME);
    try 
      {
	FileOutputStream fos = new FileOutputStream (tmp);
	fos.close ();
      } 
    catch (FileNotFoundException fne) { }
    catch (IOException ioe) { }

    // create empty file
    File tmp2 = new File (cons, TMP_FILENAME2);
    try 
      {
	FileOutputStream fos = new FileOutputStream (tmp2);
	fos.close ();
      } 
    catch (FileNotFoundException fne) { }
    catch (IOException ioe) { }

    File tmp3 = new File (cons, TMP_FILENAME3);

    // canWrite ()
    harness.check (tmp.canWrite (), "canWrite()");
    // exists ()
    harness.check (tmp.exists (), "exists ()");
    // isFile ()
    harness.check (tmp.isFile (), "isFile ()");
    // length ()
    harness.check (tmp.length (), 0L, "length ()");
    byte[] b = new byte[2001];
    try 
      {
	FileOutputStream fos = new FileOutputStream (tmp);
	fos.write (b);
	fos.close ();
      } 
    catch (FileNotFoundException fne) { }
    catch (IOException ioe) { }
    harness.check (tmp.length (), b.length, "length ()");

    // toString ();
    String tmpstr = new String (srcdirstr + File.separator 
				+ THIS_FILE + File.separator 
				+ TMP_FILENAME);
    harness.debug (tmp.toString () + " =? " + tmpstr);
    harness.check (tmp.toString ().equals (tmpstr), "toString ()");

    // list ();
    String [] tmpdirlist = cons.list ();
    String [] expectedlist = new String[] {TMP_FILENAME, TMP_FILENAME2};
//      for (int ll=0; ll<tmpdirlist.length; ll++)
//        System.err.println (tmpdirlist[ll]);
    harness.check (compareStringArray (tmpdirlist, expectedlist), "list ()");
    
    // list (FilenameFilter);
    tmpdirlist = cons.list (this);
    expectedlist = new String[] {TMP_FILENAME2};
//      for (int ll=0; ll<tmpdirlist.length; ll++)
//        System.err.println (tmpdirlist[ll]);
    harness.check (compareStringArray (tmpdirlist, expectedlist), "list (FilenameFilter)");

    // renameTo (File);
    if (tmp3.exists ())
      tmp3.delete ();
    harness.check (tmp.renameTo (tmp3), "renameTo (File)");
    harness.check (tmp3.exists (), "renameTo (File)");

    // check delete of directory with something in it fails
    if (tmp.exists ())
      harness.check (tmp.delete (), "delete ()");
    if (tmp2.exists ())
      harness.check (tmp2.delete (), "delete ()");
    if (tmp3.exists ())
      harness.check (tmp3.delete (), "delete ()");
    harness.check (!tmp.exists (), "delete ()");
    harness.check (!tmp2.exists (), "delete ()");
    harness.check (!tmp3.exists (), "delete ()");
    
    // mkdir ();
    harness.check (tmp.mkdir (), "mkdir ()");
    harness.check (tmp.exists () && tmp.isDirectory (), "mkdir ()");
    
    // mkdirs ();
    File mkdirstest = new File (tmpdirstr, new String ("one" + File.separator
						       + "two" + File.separator
						       + "three"));
    harness.check (mkdirstest.mkdirs (), "mkdirs ()");
    harness.check (mkdirstest.exists () && mkdirstest.isDirectory (), "mkdirs ()");
    File mkdirstest2 = new File (tmpdirstr, new String ("one" + File.separator 
							+ "two"));
    harness.check (mkdirstest2.exists () && mkdirstest2.isDirectory (), "mkdirs ()");
    File mkdirstest1 = new File (tmpdirstr, new String ("one"));
    harness.check (mkdirstest1.exists () && mkdirstest1.isDirectory (), "mkdirs ()");

    harness.check (mkdirstest.delete (), "delete () of a directory");
    harness.check (!mkdirstest.exists (), "delete () of a directory");

    // negative test case
    harness.check (!mkdirstest1.delete (), "delete () of a directory");

    harness.check (mkdirstest2.delete (), "delete () of a directory");
    harness.check (!mkdirstest2.exists (), "delete () of a directory");

    harness.check (mkdirstest1.delete (), "delete () of a directory");
    harness.check (!mkdirstest1.exists (), "delete () of a directory");
    
    // check delete of an empty directory
    harness.check (tmp.delete (), "delete () of a directory");
    harness.check (!tmp.exists (), "delete () of a directory");

    harness.check (cons.delete (), "delete () of a directory");
    harness.check (!cons.exists (), "delete () of a directory");

    harness.check (File.pathSeparator.equals (config.pathSeparator), "pathSeparator");
    harness.check (new Character (File.pathSeparatorChar).toString ().equals (config.pathSeparator), "pathSeparatorChar");

    harness.check (File.separator.equals (config.separator), "separator");
    harness.check (new Character (File.separatorChar).toString ().equals (config.separator), "separatorChar");
    
    // getAbsolutePath ();
    harness.debug ("tmp3.getAbsolutePath () = " + tmp3.getAbsolutePath ());
    harness.debug ("equals? "  + srcdirstr
		   + File.separator
		   + THIS_FILE
		   + File.separator
		   + TMP_FILENAME3);
    harness.check (tmp3.getAbsolutePath ().equals (srcdirstr
						  + File.separator
						  + THIS_FILE
						  + File.separator
						  + TMP_FILENAME3), "getAbsolutePath ()");

    // getCanonicalPath ();

    try 
      {
	// Make sure that file exists.
	cons.mkdir ();
	FileOutputStream fos = new FileOutputStream (tmp3);
	fos.write (1);
	fos.close ();
	harness.debug ("tmp3.getCanonicalPath () = " + tmp3.getCanonicalPath ());
	harness.debug ("equals? " + srcdirstr + File.separator 
		       + THIS_FILE + File.separator
		       + TMP_FILENAME3);
	harness.check (tmp3.getCanonicalPath ().equals (srcdirstr
						       + File.separator 
						       + THIS_FILE
						       + File.separator
						       + TMP_FILENAME3), "getCanonicalPath ()");
	// Remove again
	tmp3.delete ();
	cons.delete ();
      } 
    catch (IOException ioe)
      {
	harness.check (false, "getCanonicalPath () " + ioe);
      }

    // Another getCanonicalPath() test.
    boolean ok = false;
    try
      {
	File x1 = new File ("").getCanonicalFile ();
	File x2 = new File (".").getCanonicalFile ();
	ok = x1.equals(x2);
      }
    catch (IOException ioe)
      {
	// Nothing.
      }
    harness.check (ok, "getCanonicalFile with empty path");

    // getName ();
    harness.debug ("tmp3.getName () = " + tmp3.getName ());
    harness.check (tmp3.getName ().equals (TMP_FILENAME3), "getName ()");

    // getParent ();
    harness.check (tmp3.getParent ().equals (srcdirstr 
					    + File.separator 
					    + THIS_FILE), "getParent ()");

    // getPath ();
    harness.debug ("tmp3.getPath () = " + tmp.getPath ());
    harness.check (tmp3.getPath ().equals (srcdirstr 
					  + File.separator
					  + THIS_FILE
					  + File.separator
					  + TMP_FILENAME3), "getPath ()");

    // hashCode ();
	int hc1 = tmp3.hashCode();
	int hc2 = tmp3.hashCode();
	harness.check (hc1, hc2, "hashCode()");
    
    // isAbsolute ();
    harness.check (tmp3.isAbsolute (), "isAbsolute ()");
    harness.check (! new File("").isAbsolute());

    // lastModified ();
    File lastmod = new File (tmpdir, "lastmod");
    if (lastmod.exists ())
      lastmod.delete ();
    Date now = new Date ();
    long time = now.getTime ();
    try 
      {
	Thread.sleep (1000);
      }
    catch (InterruptedException ie) { }
    try 
      {
	FileOutputStream fos = new FileOutputStream (lastmod);
	fos.close ();
      } 
    catch (FileNotFoundException fne) { }
    catch (IOException ioe) { }
    harness.debug (lastmod.lastModified () + " >= " + time);
    if (lastmod.lastModified () >= time)
      harness.check (true, "lastModified ()");
    else
      harness.check (false, "lastModified ()");
    if (lastmod.exists ())
      lastmod.delete ();
  }

  /**
   * Compare two String arrays, and if of the same length compare
   * contents for equality, order does not matter.
   */
  private boolean compareStringArray (String[] x, String[] y)
  {
    if (x.length != y.length)
      return false;

    boolean[] test = new boolean[y.length];
    for (int i = 0; i < test.length; i++)
      test[i] = true;

    for (int i = 0; i < x.length; i++)
      {
	boolean nomatch = true;
	for (int j = 0; j < y.length; j++)
	  {
	    if (test[j])
	      if (x[i].equals (y[j]))
		{
		  test[j] = false;
		  nomatch = false;
		  break;
		}
	  }
	if (nomatch)
	  return false;
      }
    return true;
  }

  /**
   * Defined by NameFilter. Only accepts files ending with .doc.
   */
  public boolean accept (File dir, String name)
  {
    if (name.endsWith (".doc"))
      return true;
    return false;
  }

}

