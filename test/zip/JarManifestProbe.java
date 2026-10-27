// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.jar.JarFile;
import java.util.jar.Manifest;

public class JarManifestProbe {
    public static void main(String[] args) throws Exception {
        try (JarFile jar = new JarFile(args[0])) {
            Manifest manifest = jar.getManifest();
            if (manifest == null || !"1.0".equals(manifest.getMainAttributes().getValue("Manifest-Version")))
                throw new AssertionError("Manifest contents");
            manifest.getMainAttributes().putValue("OpenLDK-Probe", "cached");
            for (int i = 0; i < 100; i++) {
                Manifest again = jar.getManifest();
                if (again != manifest || !"cached".equals(again.getMainAttributes().getValue("OpenLDK-Probe")))
                    throw new AssertionError("Manifest cache lost identity or edits");
            }
        }
        System.out.println("JAR manifest cache identity and edits passed");
    }
}
