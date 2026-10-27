// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class StaticFieldPayload {
    public static int value = 41;
    public static java.util.function.Supplier<StaticFieldPayload> factory() {
        return StaticFieldPayload::new;
    }
}
