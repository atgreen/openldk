// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.file.*;

public class ReflectionLoaderProbe {
    public static void main(String[] args) throws Throwable {
        StaticFieldPayload.value = 9;
        try (URLClassLoader loader = new URLClassLoader(new URL[]{Path.of(args[0]).toUri().toURL()}, null)) {
            Class<?> target = loader.loadClass("StaticFieldPayload");
            Object matrix = Array.newInstance(target, 2, 3);
            Class<?> matrixComponent = matrix.getClass().getComponentType().getComponentType();
            if (matrixComponent != target || Array.getLength(Array.get(matrix, 0)) != 3)
                throw new AssertionError("Reflective array crossed defining loaders");
            Field field = target.getField("value");
            if (field.getInt(null) != 41) throw new AssertionError("Reflection used another loader's storage");
            field.setInt(null, 73);
            MethodHandles.Lookup lookup = MethodHandles.publicLookup();
            MethodHandle getter = lookup.findStaticGetter(target, "value", int.class);
            MethodHandle setter = lookup.findStaticSetter(target, "value", int.class);
            if ((int) getter.invokeExact() != 73) throw new AssertionError("Method handle getter storage");
            setter.invokeExact(82);
            if (field.getInt(null) != 82 || StaticFieldPayload.value != 9)
                throw new AssertionError("Static field storage crossed defining loaders");
            Class<?> child = loader.loadClass("StaticFieldPayload$Inherited");
            MethodHandle inherited = lookup.findStatic(child, "compute", MethodType.methodType(int.class));
            if ((int) inherited.invokeExact() != 83)
                throw new AssertionError("Inherited static method crossed defining loaders");
            var factory = (java.util.function.Supplier<?>) target.getMethod("factory").invoke(null);
            if (factory.get().getClass() != target)
                throw new AssertionError("Constructor method reference crossed defining loaders");
        }
        System.out.println("Reflection and method-handle static fields preserve defining loaders");
    }
}
