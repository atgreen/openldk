// Tags: JDK1.2

// Copyright (C) 2003 Red Hat, Inc.
// Copyright (C) 2004 Stephen Crawley.
// Copyright (C) 2005, 2006, 2010 Red Hat, Inc.
// Written by Tom Tromey <tromey@redhat.com>
// Extensively modified by Stephen Crawley <crawley@dstc.edu.au>
// Further modified by Gary Benson <gbenson@redhat.com>
// Further modified by Andrew John Hughes <ahughes@redhat.com>

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

package gnu.testlet.java.io.File;

import java.io.File;
import java.io.FilePermission;
import java.io.FilenameFilter;
import java.io.FileFilter;
import java.security.Permission;
import java.util.Date;
import java.util.PropertyPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test (TestHarness harness)
  {
    // Setup
    String tmp = harness.getTempDirectory();
    File tmpdir = new File(tmp + File.separator + "mauve-testdir");
    harness.check(tmpdir.mkdir() || tmpdir.exists(), "temp directory");
    File tmpdir2 = new File(tmpdir, "nested-dir");
    harness.check(tmpdir2.mkdir() || tmpdir2.exists(), "temp directory 2");
    File tmpdir3 = new File(tmpdir2, "nested-nested-dir");
    File tmpfile = new File(tmpdir, "testfile");
    harness.check(tmpfile.delete() || !tmpfile.exists(), "no temp file");
    File tmpfile2 = new File(tmpdir, "testfile2");
    harness.check(tmpfile2.delete() || !tmpfile2.exists());

    Permission tmpdirReadPerm =
      new FilePermission(tmpdir.toString(), "read");
    Permission tmpdirWritePerm =
      new FilePermission(tmpdir.toString(), "write");
    Permission tmpdirDeletePerm =
      new FilePermission(tmpdir.toString(), "delete");

    Permission tmpdir2ReadPerm =
      new FilePermission(tmpdir2.toString(), "read");
    Permission tmpdir2WritePerm =
      new FilePermission(tmpdir2.toString(), "write");
    Permission tmpdir2DeletePerm =
      new FilePermission(tmpdir2.toString(), "delete");

    Permission tmpdir3ReadPerm =
      new FilePermission(tmpdir3.toString(), "read");
    Permission tmpdir3WritePerm =
      new FilePermission(tmpdir3.toString(), "write");

    Permission tmpfileReadPerm =
      new FilePermission(tmpfile.toString(), "read");
    Permission tmpfileWritePerm =
      new FilePermission(tmpfile.toString(), "write");
    Permission tmpfileDeletePerm =
      new FilePermission(tmpfile.toString(), "delete");

    Permission tmpallWritePerm =
      new FilePermission(tmp + File.separator + "*", "write");
    Permission tmpdirallWritePerm =
      new FilePermission(tmpdir.toString() + File.separator + "*", "write");
    Permission tmpfile2WritePerm =
      new FilePermission(tmpfile2.toString(), "write");

    Permission rootReadPerm =
      new FilePermission(File.separator, "read");

    Permission tmpdirPropPerm =
      new PropertyPermission("java.io.tmpdir", "read");

    Permission modifyThreadGroup =
      new RuntimePermission("modifyThreadGroup");
    Permission shutdownHooks =
      new RuntimePermission("shutdownHooks");

    // Keep a record of created temp files so we can delete them later.
    File tf1 = null;
    File tf2 = null;

    TestSecurityManager sm = new TestSecurityManager(harness);
    try {
      sm.install();

      // throwpoint: java.io.File-canWrite-DIR
      harness.checkPoint("dir.canWrite");
      try {
        sm.prepareChecks(new Permission[]{tmpdirWritePerm});
        tmpdir.canWrite();
        sm.checkAllChecked();
      }
      catch (SecurityException ex) {
        harness.debug(ex);
        harness.check(false, "dir.canWrite - unexpected exception");
      }

      // throwpoint: java.io.File-canRead-DIR
      harness.checkPoint("dir.canRead");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.canRead();
        sm.checkAllChecked();
      }
      catch (SecurityException ex) {
        harness.debug(ex);
        harness.check(false, "dir.canRead - unexpected exception");
      }

      // throwpoint: java.io.File-createNewFile
      harness.checkPoint("file.createNewFile");
      try {
        sm.prepareChecks(new Permission[]{tmpfileWritePerm});
        tmpfile.createNewFile();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.createNewFile - unexpected exception");
      }

      // throwpoint: java.io.File-delete-FILE
      harness.checkPoint("file.delete");
      try {
        sm.prepareChecks(new Permission[]{tmpfileDeletePerm});
        tmpfile.delete();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.delete - unexpected exception");
      }

      // throwpoint: java.io.File-list(FilenameFilter)
      harness.checkPoint("dir.list(null)");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.list(null);
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.list(null) - unexpected exception");
      }

      // throwpoint: java.io.File-list
      harness.checkPoint("dir.list()");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.list();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.list() - unexpected exception");
      }

      // throwpoint: java.io.File-listFiles
      harness.checkPoint("dir.listFiles()");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.listFiles();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.listFiles() - unexpected exception");
      }

      // throwpoint: java.io.File-listFiles(FilenameFilter)
      harness.checkPoint("dir.listFiles(FilenameFilter)");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.listFiles((FilenameFilter) null);
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false,
                      "dir.listFiles(FilenameFilter) - unexpected exception");
      }

      // throwpoint: java.io.File-listFiles(FileFilter)
      harness.checkPoint("dir.listFiles(FileFilter)");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.listFiles((FileFilter) null);
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false,
                      "dir.listFiles(FileFilter) - unexpected exception");
      }

      // throwpoint: java.io.File-createTempFile(String, String)
      harness.checkPoint("createTempFile(2-args)");
      try {
        sm.prepareChecks(new Permission[]{tmpallWritePerm},
                         new Permission[]{tmpdirPropPerm});
        sm.setComparisonStyle(TestSecurityManager.IMPLIES);
        tf1 = File.createTempFile("pfx", "sfx");
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "createTempFile(2-args) - unexpected exception");
      }

      // throwpoint: java.io.File-createTempFile(String, String, File)
      harness.checkPoint("createTempFile(3-args)");
      try {
        sm.prepareChecks(new Permission[]{tmpdirallWritePerm});
        sm.setComparisonStyle(TestSecurityManager.IMPLIES);
        tf2 = File.createTempFile("pfx", "sfx", tmpdir);
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "createTempFile(3-args) - unexpected exception");
      }

      // throwpoint: java.io.File-setReadOnly-DIR
      harness.checkPoint("dir.setReadOnly");
      try {
        sm.prepareChecks(new Permission[]{tmpdir2WritePerm});
        tmpdir2.setReadOnly();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.setReadOnly - unexpected exception");
      }

      // throwpoint: java.io.File-delete-DIR
      // Make sure we remove the read only temp dir
      harness.checkPoint("dir.delete");
      try {
        sm.prepareChecks(new Permission[]{tmpdir2DeletePerm});
        tmpdir2.delete();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.delete - unexpected exception");
      }

      // throwpoint: java.io.File-listRoots
      harness.checkPoint("listRoots()");
      try {
        sm.prepareChecks(new Permission[]{rootReadPerm});
        File[] roots = File.listRoots();
        harness.check(roots.length >= 1, "File.listRoots()");
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "listRoots() - unexpected exception");
      }

      // throwpoint: java.io.File-renameTo
      harness.checkPoint("file.renameTo");
      try {
        sm.prepareChecks(new Permission[]{tmpfileWritePerm,
                                          tmpfile2WritePerm});
        tmpfile.renameTo(tmpfile2);
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.renameTo - unexpected exception");
      }

      // throwpoint: java.io.File-setLastModified-DIR
      harness.checkPoint("dir.setLastModified()");
      try {
        sm.prepareChecks(new Permission[]{tmpdirWritePerm});
        tmpdir.setLastModified(0);
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.setLastModified() - unexpected exception");
      }

      // throwpoint: java.io.File-deleteOnExit-DIR
      harness.checkPoint("dir.deleteOnExit()");
      try {
        sm.prepareChecks(new Permission[]{tmpdirDeletePerm},
                         new Permission[]{modifyThreadGroup, shutdownHooks});
        tmpdir.deleteOnExit();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.deleteOnExit() - unexpected exception");
      }

      // throwpoint: java.io.File-deleteOnExit-FILE
      harness.checkPoint("file.deleteOnExit()");
      try {
        sm.prepareChecks(new Permission[]{tmpfileDeletePerm},
                         new Permission[]{modifyThreadGroup, shutdownHooks});
        tmpfile.deleteOnExit();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.deleteOnExit() - unexpected exception");
      }

      // throwpoint: java.io.File-exists-DIR
      harness.checkPoint("file.exists");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.exists();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.exists - unexpected exception");
      }

      // throwpoint: java.io.File-exists-FILE
      harness.checkPoint("file.exists");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.exists();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.exists - unexpected exception");
      }

      // throwpoint: java.io.File-canRead-FILE
      harness.checkPoint("file.canRead");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.canRead();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.canRead - unexpected exception");
      }

      // throwpoint: java.io.File-isFile-FILE
      harness.checkPoint("file.isFile");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.isFile();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.isFile - unexpected exception");
      }

      // throwpoint: java.io.File-isFile-DIR
      harness.checkPoint("dir.isFile");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.isFile();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.isFile - unexpected exception");
      }

      // throwpoint: java.io.File-isDirectory-FILE
      harness.checkPoint("file.isDirectory");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.isDirectory();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.isDirectory - unexpected exception");
      }

      // throwpoint: java.io.File-isDirectory-DIR
      harness.checkPoint("dir.isDirectory");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.isDirectory();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.isDirectory - unexpected exception");
      }

      // throwpoint: java.io.File-isHidden-FILE
      harness.checkPoint("file.isHidden");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.isHidden();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.isHidden - unexpected exception");
      }

      // throwpoint: java.io.File-isHidden-DIR
      harness.checkPoint("dir.isHidden");
      try {
        sm.prepareChecks(new Permission[]{tmpdirReadPerm});
        tmpdir.isHidden();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.isHidden - unexpected exception");
      }

      // throwpoint: java.io.File-lastModified
      harness.checkPoint("file.lastModified");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.lastModified();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.lastModified - unexpected exception");
      }

      // throwpoint: java.io.File-length
      harness.checkPoint("file.length");
      try {
        sm.prepareChecks(new Permission[]{tmpfileReadPerm});
        tmpfile.length();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.length - unexpected exception");
      }

      // throwpoint: java.io.File-canWrite-FILE
      harness.checkPoint("file.canWrite");
      try {
        sm.prepareChecks(new Permission[]{tmpfileWritePerm});
        tmpfile.canWrite();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.canWrite - unexpected exception");
      }

      // throwpoint: java.io.File-mkdir
      harness.checkPoint("dir.mkdir");
      try {
        sm.prepareChecks(new Permission[]{tmpdirWritePerm});
        tmpdir.mkdir();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.mkdir - unexpected exception");
      }

      // throwpoint: java.io.File-mkdirs
      harness.checkPoint("dir.mkdirs");
      try {
        sm.prepareChecks(new Permission[]
          {tmpdir2WritePerm, tmpdir2ReadPerm, tmpdir3ReadPerm, tmpdir3WritePerm});
        tmpdir3.mkdirs();
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "dir.mkdirs - unexpected exception");
      }

      // throwpoint: java.io.File-setLastModified-FILE
      harness.checkPoint("file.setLastModified");
      try {
        sm.prepareChecks(new Permission[]{tmpfileWritePerm});
        tmpfile.setLastModified(new Date().getTime());
        sm.checkAllChecked();
      }
      catch (Exception ex) {
        harness.debug(ex);
        harness.check(false, "file.setLastModified - unexpected exception");
      }

    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "outer handler - unexpected exception");
    }
    finally {
      sm.uninstall();

      if (tmpfile != null) tmpfile.delete();
      if (tmpfile2 != null) tmpfile2.delete();
      if (tf1 != null) tf1.delete();
      if (tf2 != null) tf2.delete();
      if (tmpdir3 != null) tmpdir3.delete();
      if (tmpdir2 != null) tmpdir2.delete();
      if (tmpdir != null) tmpdir.delete();
    }
  }
}
