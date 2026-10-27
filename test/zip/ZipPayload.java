// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class ZipPayload {
    public enum Kind { VALUE }
    public interface KindSupplier { Kind get(); }
    public static KindSupplier kindSupplier() { return () -> Kind.VALUE; }
    public static String resource() { return java.util.ResourceBundle.getBundle("ZipMessages").getString("message"); }
    static { System.setProperty("zip.payload.initialized", "yes"); }
    public static int answer() { return 42; }
    public static ZipCallback[] callbacks() {
        ZipCallback callback = value -> System.setProperty("zip.callback", Integer.toString(value));
        return new ZipCallback[]{callback};
    }
}
