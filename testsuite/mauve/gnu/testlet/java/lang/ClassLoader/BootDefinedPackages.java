/* DefaultDefinedPackages.java -- Test which ensures that packages are defined by the boot classloader
 Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
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


package gnu.testlet.java.lang.ClassLoader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.Serializable;

/**
 * Test which ensures that the boot class loader is defining packages like the
 * URLClassLoader does.
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 * @see URLClassLoader#findClass(java.lang.String)
 */
public class BootDefinedPackages implements Testlet
{

  private static class TestLoader extends ClassLoader implements Serializable
  {

    /**
     * Dummy serialVersionUID used to appease warnings
     */
    private static final long serialVersionUID = 1L;

    /**
     * List of classes in each of the standard packages
     */
    static String standardPackagesClasses[] = {
                                               "java.applet.Applet",
                                               "java.awt.color.CMMException",
                                               "java.awt.datatransfer.Clipboard",
                                               "java.awt.dnd.peer.DragSourceContextPeer",
                                               "java.awt.dnd.Autoscroll",
                                               "java.awt.event.ActionEvent",
                                               "java.awt.font.FontRenderContext",
                                               "java.awt.geom.AffineTransform",
                                               "java.awt.im.spi.InputMethod",
                                               "java.awt.im.InputContext",
                                               "java.awt.image.renderable.ContextualRenderedImageFactory",
                                               "java.awt.image.AffineTransformOp",
                                               "java.awt.peer.ButtonPeer",
                                               "java.awt.print.Book",
                                               "java.awt.ActiveEvent",
                                               "java.beans.beancontext.BeanContext",
                                               "java.beans.AppletInitializer",
                                               "java.io.BufferedInputStream",
                                               "java.lang.annotation.AnnotationFormatError",
                                               "java.lang.ref.PhantomReference",
                                               "java.lang.reflect.AccessibleObject",
                                               "java.lang.AbstractMethodError",
                                               "java.math.BigDecimal",
                                               "java.net.Authenticator",
                                               "java.nio.channels.spi.AbstractInterruptibleChannel",
                                               "java.nio.channels.AlreadyConnectedException",
                                               "java.nio.charset.spi.CharsetProvider",
                                               "java.nio.charset.CharacterCodingException",
                                               "java.nio.Buffer",
                                               "java.rmi.activation.Activatable",
                                               "java.rmi.dgc.DGC",
                                               "java.rmi.registry.LocateRegistry",
                                               "java.rmi.server.ExportException",
                                               "java.rmi.AccessException",
                                               "java.security.acl.Acl",
                                               "java.security.cert.Certificate",
                                               "java.security.interfaces.DSAKey",
                                               "java.security.spec.AlgorithmParameterSpec",
                                               "java.security.AccessControlContext",
                                               "java.sql.Array",
                                               "java.text.Annotation",
                                               "java.util.jar.Attributes",
                                               "java.util.logging.ConsoleHandler",
                                               "java.util.prefs.AbstractPreferences",
                                               "java.util.regex.Matcher",
                                               "java.util.zip.Adler32",
                                               "java.util.AbstractCollection" };

    public TestLoader(ClassLoader parent)
    {
      super(parent);
    }

    /**
     * Real test method for package definition which can access the protected
     * getPackage method
     * @param harness
     *          the test harness
     * @see ClassLoader#getPackage(java.lang.String)
     */
    public void test(TestHarness harness)
    {
      harness.checkPoint("Checking basic packages");

      // This package must be defined since it is the one of the enclosing class
      harness.check(getPackage("gnu.testlet.java.lang.ClassLoader") != null);

      // This package must be defined since it is the one which contains Object
      harness.check(getPackage("java.lang") != null);

      // This package must be defined since we're implementing Serializable
      harness.check(getPackage("java.io") != null);

      // Instead of checking some packages, we loop over each standard package,
      // and if not already defined, it should be once we load a class in it.
      // Note that this loop may not produce the same result on different vms,
      // but it should be consistent across several runs on the same vm.
      for (int i = 0; i < standardPackagesClasses.length; i++)
        {
          String packageName;
          int lastDot = standardPackagesClasses[i].lastIndexOf('.');

          // Get the package name from the standard class name
          packageName = standardPackagesClasses[i].substring(0, lastDot);

          if (getPackage(packageName) == null)
            {
              // packageName is not yet defined, we should be able to make it
              // defined by trying to access a class in it
              try
                {
                  Class.forName(standardPackagesClasses[i]);
                  harness.check(getPackage(packageName) != null,
                                "Checking definition of " + packageName);
                }
              catch (ClassNotFoundException e)
                {
                  harness.debug("Unsuitable class to test on this vm");
                  harness.debug(e);
                }
            }
        }
    }
  }

  /*
   * (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    // Define a class loader for testing, with the system class loader as
    // parent, and starts the real test
    TestLoader loader = new TestLoader(getClass().getClassLoader());
    loader.test(harness);
  }

}
