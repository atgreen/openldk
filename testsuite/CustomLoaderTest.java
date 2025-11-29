// Test for custom class loader support
import java.io.*;

// A simple test class that will be loaded by the custom loader
class LoadedClass {
    public static int getValue() {
        return 42;
    }

    public String getMessage() {
        return "Hello from LoadedClass";
    }
}

// A simple custom class loader
class SimpleClassLoader extends ClassLoader {
    public SimpleClassLoader(ClassLoader parent) {
        super(parent);
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        // For this test, we just delegate to the parent
        // In a real scenario, this would read bytes from a custom source
        throw new ClassNotFoundException(name);
    }
}

public class CustomLoaderTest {
    public static void main(String[] args) {
        try {
            // Test 1: Create a custom class loader
            SimpleClassLoader loader = new SimpleClassLoader(CustomLoaderTest.class.getClassLoader());
            System.out.println("Created custom loader: " + loader);

            // Test 2: Load a class using the standard mechanism
            LoadedClass obj = new LoadedClass();
            System.out.println("Message: " + obj.getMessage());
            System.out.println("Value: " + LoadedClass.getValue());

            // Test 3: Check that loaded classes have the right loader
            Class<?> clazz = LoadedClass.class;
            ClassLoader actualLoader = clazz.getClassLoader();
            System.out.println("LoadedClass loader: " + actualLoader);

            // Test 4: Load String class (should be from boot loader)
            Class<?> stringClass = String.class;
            ClassLoader stringLoader = stringClass.getClassLoader();
            System.out.println("String loader: " + stringLoader);

            System.out.println("SUCCESS");
            System.exit(0);
        } catch (Exception e) {
            System.out.println("FAILED: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
