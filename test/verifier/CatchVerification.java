// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;

// Check that guest Java can catch verifier failures, including defineClass(null,...).
public class CatchVerification extends ClassLoader {
    private Class<?> define(byte[] bytes) {
        return defineClass(null, bytes, 0, bytes.length);
    }

    public static void main(String[] args) throws Exception {
        try {
            Class<?> type = new CatchVerification().define(Files.readAllBytes(Path.of(args[0])));
            type.getMethod("main", String[].class).invoke(null, (Object) new String[0]);
        } catch (LinkageError error) {
            if (!error.getClass().getName().equals(args[1])) throw error;
            System.out.println("Caught " + args[1]);
            return;
        } catch (InvocationTargetException error) {
            if (!error.getCause().getClass().getName().equals(args[1])) throw error;
            System.out.println("Caught " + args[1]);
            return;
        }
        throw new AssertionError("Malformed class was accepted");
    }
}
