import java.lang.invoke.*;

public class MethodHandleTest {

    // Simple static method to invoke
    public static String simpleMethod(String arg) {
        return "Hello, " + arg;
    }

    // Static method with no args
    public static int getValue() {
        return 42;
    }

    public static void main(String[] args) {
        try {
            System.out.println("=== MethodHandle Test ===");

            // Test 1: Get a MethodHandles.Lookup
            System.out.println("\n1. Getting Lookup...");
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            System.out.println("   Lookup obtained: " + lookup);
            assert lookup != null : "Lookup is null";

            // Test 2: Create a MethodType for getValue()
            System.out.println("\n2. Creating MethodType for getValue()I...");
            MethodType mt0 = MethodType.methodType(int.class);
            System.out.println("   MethodType created: " + mt0);
            assert mt0 != null : "MethodType is null";

            // Test 3: Find the static method getValue
            System.out.println("\n3. Finding static method getValue...");
            MethodHandle mh0 = lookup.findStatic(MethodHandleTest.class, "getValue", mt0);
            System.out.println("   MethodHandle obtained: " + mh0);
            assert mh0 != null : "MethodHandle is null";

            // Test 4: Invoke the method handle
            System.out.println("\n4. Invoking getValue()...");
            int result0 = (int) mh0.invokeExact();
            System.out.println("   Result: " + result0);
            assert result0 == 42 : "Expected 42, got " + result0;

            // Test 5: Create a MethodType for simpleMethod
            System.out.println("\n5. Creating MethodType for simpleMethod(String)String...");
            MethodType mt1 = MethodType.methodType(String.class, String.class);
            System.out.println("   MethodType created: " + mt1);
            assert mt1 != null : "MethodType is null";

            // Test 6: Find the static method simpleMethod
            System.out.println("\n6. Finding static method simpleMethod...");
            MethodHandle mh1 = lookup.findStatic(MethodHandleTest.class, "simpleMethod", mt1);
            System.out.println("   MethodHandle obtained: " + mh1);
            assert mh1 != null : "MethodHandle is null";

            // Test 7: Invoke with argument
            System.out.println("\n7. Invoking simpleMethod(\"World\")...");
            String result1 = (String) mh1.invokeExact("World");
            System.out.println("   Result: " + result1);
            assert "Hello, World".equals(result1) : "Expected 'Hello, World', got '" + result1 + "'";

            System.out.println("\n=== All tests passed! ===");

        } catch (AssertionError e) {
            System.err.println("ASSERTION FAILED: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        } catch (Throwable t) {
            System.err.println("ERROR: " + t);
            t.printStackTrace();
            System.exit(1);
        }
    }
}
