// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.ref.WeakReference;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.ArrayList;

public class SerializationLoaderProbe {
    public static void main(String[] args) throws Exception {
        ArrayList<WeakReference<ClassLoader>> references = new ArrayList<>();
        Thread worker = new Thread(() -> {
            try {
                for (int i = 0; i < 3; i++) {
                    try (URLClassLoader loader = new URLClassLoader(
                            new URL[] {Path.of(args[0]).toUri().toURL()}, null)) {
                        Class<?> payload = Class.forName("SerializationProbe$Payload", true, loader);
                        payload.getMethod("roundTrip").invoke(null);
                        references.add(new WeakReference<>(loader));
                    }
                }
            } catch (Exception e) { throw new RuntimeException(e); }
        });
        worker.start(); worker.join();
        if (references.size() != 3) throw new AssertionError("worker failed");
        for (int i = 0; i < 100; i++) {
            System.gc(); Thread.sleep(20);
            boolean cleared = true;
            for (WeakReference<ClassLoader> reference : references) cleared &= reference.get() == null;
            if (cleared) {
                System.out.println("serialization loaders collected");
                return;
            }
        }
        throw new AssertionError("serialization retained a loader");
    }
}
