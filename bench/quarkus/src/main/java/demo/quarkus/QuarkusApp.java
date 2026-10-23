package demo.quarkus;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.quarkus.arc.Arc;
import io.quarkus.logging.Log;
import io.quarkus.runtime.QuarkusApplication;
import io.quarkus.runtime.annotations.QuarkusMain;
import io.quarkus.runtime.annotations.RegisterForReflection;
import io.smallrye.config.ConfigMapping;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.inject.Any;
import jakarta.inject.Inject;
import java.util.List;
import org.eclipse.microprofile.config.inject.ConfigProperty;

// A small Quarkus application for the startup benchmark, the counterpart of
// bench/spring-boot's: CDI, a @ConfigMapping bound from
// application.properties, @ConfigProperty, the Quarkus-managed Jackson
// mapper, JBoss logging, run in command mode.
@QuarkusMain
public class QuarkusApp implements QuarkusApplication {
    @ConfigMapping(prefix = "demo.greeting")
    public interface Greeting {
        String text();
        List<String> names();
        int repeat();
    }

    // Jackson reads Message by reflection, which a native image must know.
    @RegisterForReflection
    public record Message(String to, String text, int length) {}

    @ApplicationScoped
    public static class Greeter {
        @Inject Greeting greeting;

        public List<Message> messages() {
            return greeting.names().stream()
                .map(n -> greeting.text().replace("{name}", n).repeat(greeting.repeat()))
                .map(t -> new Message(t.substring(0, t.indexOf(',')), t, t.length())).toList();
        }
    }

    @Inject Greeter greeter;
    @Inject ObjectMapper json;
    @ConfigProperty(name = "quarkus.application.name") String name;

    @Override
    public int run(String... args) throws Exception {
        Log.infof("Running %s", name);
        System.out.println(json.writerWithDefaultPrettyPrinter().writeValueAsString(greeter.messages()));
        System.out.println("roundtrip=" + json.readValue(json.writeValueAsString(greeter.messages().get(0)), Message.class));
        System.out.println("beans=" + Arc.container().beanManager().getBeans(Object.class, Any.Literal.INSTANCE).size());
        return 0;
    }
}
