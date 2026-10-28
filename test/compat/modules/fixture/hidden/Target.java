// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package fixture.hidden;
public class Target {
    public static String PUBLIC = "public";
    private static String SECRET = "secret";
    public Target() {}
    private Target(int ignored) {}
    public static String visible() { return PUBLIC; }
    private static String hidden() { return SECRET; }
}
