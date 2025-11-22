import java.util.function.Function;

public class InvokeDynamicTest {
    public static void main(String[] args) {
        Function<String, String> f = (s) -> "Hello, " + s;
        System.out.println(f.apply("World"));
    }
}
