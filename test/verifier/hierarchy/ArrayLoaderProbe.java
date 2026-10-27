// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.net.*;
import java.nio.file.*;

public class ArrayLoaderProbe {
    public static void main(String[] args) throws Exception {
        Path directory = Path.of(args[0]);
        try (URLClassLoader provider = new URLClassLoader(new URL[]{directory.toUri().toURL()}, null)) {
            ClassLoader owner = new ClassLoader(null) {
                @Override protected Class<?> findClass(String name) throws ClassNotFoundException {
                    if (name.equals("ArrayParent") || name.equals("ArrayContract"))
                        return provider.loadClass(name);
                    if (!name.equals("ArrayCaller") && !name.equals("ArrayChild"))
                        throw new ClassNotFoundException(name);
                    try {
                        byte[] bytes = Files.readAllBytes(directory.resolve(name + ".class"));
                        return defineClass(name, bytes, 0, bytes.length);
                    } catch (java.io.IOException error) {
                        throw new ClassNotFoundException(name, error);
                    }
                }
            };
            owner.loadClass("ArrayCaller").getMethod("check").invoke(null);
        }
        System.out.println("Array hierarchy passed");
    }
}
