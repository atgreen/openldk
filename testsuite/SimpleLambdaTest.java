import java.util.function.Supplier;

public class SimpleLambdaTest {
    public static void main(String[] args) {
        // Simple lambda expression
        Supplier<String> supplier = () -> "Hello from lambda!";
        String result = supplier.get();
        System.out.println(result);
    }
}
