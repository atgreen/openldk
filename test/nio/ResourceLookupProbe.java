// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.net.URL;
import java.net.URLClassLoader;

public class ResourceLookupProbe {
    public static void main(String[] args) throws Exception {
        try (URLClassLoader isolated = new URLClassLoader(new URL[0], null)) {
            if (isolated.findResource("java/lang/Object.class") != null)
                throw new AssertionError("Bootstrap resource leaked into findResource");
            if (isolated.findResource("ResourceLookupProbe.class") != null)
                throw new AssertionError("Application resource leaked into findResource");
        }
        if (ClassLoader.getSystemClassLoader().getResource("ResourceLookupProbe.class") == null)
            throw new AssertionError("System loader lost its application resources");
        System.out.println("URL loader own-resource lookup and system resource lookup passed");
    }
}
