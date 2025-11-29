// Simple test for custom class loader support
class SimpleLoaderTest {
    public static void main(String[] args) {
        // Test 1: Check that we can get the class loader
        ClassLoader loader = SimpleLoaderTest.class.getClassLoader();
        System.out.println("Got loader");

        // Test 2: Check String class loader (should be null for boot loader)
        ClassLoader stringLoader = String.class.getClassLoader();
        if (stringLoader == null) {
            System.out.println("String has null loader (boot loader)");
        } else {
            System.out.println("String has non-null loader");
        }

        // Test 3: Check Integer class loader
        ClassLoader intLoader = Integer.class.getClassLoader();
        if (intLoader == null) {
            System.out.println("Integer has null loader (boot loader)");
        } else {
            System.out.println("Integer has non-null loader");
        }

        System.out.println("SUCCESS");
        System.exit(0);
    }
}
