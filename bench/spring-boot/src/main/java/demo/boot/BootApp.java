package demo.boot;

import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aot.hint.annotation.RegisterReflectionForBinding;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;
import tools.jackson.databind.json.JsonMapper;

// A small Spring Boot application for the startup benchmark: auto-configuration,
// a @ConfigurationProperties record bound from application.properties, @Value,
// the auto-configured Jackson mapper, a CommandLineRunner, Logback and the banner.
@SpringBootApplication
// Jackson reads Message by reflection, which Spring AOT cannot see; GraalVM
// native images need it registered.
@RegisterReflectionForBinding(BootApp.Message.class)
@EnableConfigurationProperties(BootApp.Greeting.class)
public class BootApp {
    private static final Logger log = LoggerFactory.getLogger(BootApp.class);

    @ConfigurationProperties("demo.greeting")
    public record Greeting(String text, List<String> names, int repeat) {}

    public record Message(String to, String text, int length) {}

    @Service
    public static class Greeter {
        private final Greeting greeting;
        Greeter(Greeting greeting) { this.greeting = greeting; }
        public List<Message> messages() {
            return greeting.names().stream()
                .map(n -> greeting.text().replace("{name}", n).repeat(greeting.repeat()))
                .map(t -> new Message(t.substring(0, t.indexOf(',')), t, t.length())).toList();
        }
    }

    @Bean
    CommandLineRunner run(Greeter greeter, JsonMapper json, ApplicationContext ctx,
                          @Value("${spring.application.name}") String name) {
        return args -> {
            log.info("Running {}", name);
            System.out.println(json.writerWithDefaultPrettyPrinter().writeValueAsString(greeter.messages()));
            System.out.println("roundtrip=" + json.readValue(json.writeValueAsString(greeter.messages().get(0)), Message.class));
            System.out.println("beans=" + ctx.getBeanDefinitionCount());
        };
    }

    public static void main(String[] args) {
        SpringApplication.run(BootApp.class, args);
    }
}
