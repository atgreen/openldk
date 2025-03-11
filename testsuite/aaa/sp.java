package aaa;
import java.util.Properties;

public class sp {
    public static void main(String[] args) {
        Properties props = System.getProperties();
        for (String key : props.stringPropertyNames()) {
            String value = props.getProperty(key);
            System.out.println(key + "=" + value);
        }
    }
}
