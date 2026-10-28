// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package fixture.api;
public class ReadAfterProbe {
    public static String logging() { return java.util.logging.Logger.getLogger("access").getName(); }
}
