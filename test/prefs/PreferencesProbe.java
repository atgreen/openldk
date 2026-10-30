// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.prefs.Preferences;
import java.util.prefs.PreferenceChangeListener;
import java.util.prefs.NodeChangeListener;
import java.util.prefs.NodeChangeEvent;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class PreferencesProbe {
    static void check(boolean value) {
        if (!value) throw new AssertionError();
    }

    public static void main(String[] args) throws Exception {
        Preferences root = Preferences.userRoot();
        Preferences node = root.node("openldk/probe");
        switch (args[0]) {
            case "write":
                node.put("unicode", "λ 雪 😀 & < > \"");
                node.putInt("integer", -123456);
                node.putLong("long", Long.MAX_VALUE);
                node.putBoolean("boolean", true);
                node.putByteArray("bytes", new byte[] {0, -1, 127, -128});
                node.node("child").put("key", "child value");
                CountDownLatch changes = new CountDownLatch(1);
                PreferenceChangeListener listener = event -> {
                    if (event.getKey().equals("notification") && event.getNewValue().equals("ready"))
                        changes.countDown();
                };
                node.addPreferenceChangeListener(listener);
                node.put("notification", "ready");
                check(changes.await(20, TimeUnit.SECONDS));
                node.removePreferenceChangeListener(listener);
                CountDownLatch added = new CountDownLatch(1);
                CountDownLatch removed = new CountDownLatch(1);
                NodeChangeListener nodes = new NodeChangeListener() {
                    public void childAdded(NodeChangeEvent event) { added.countDown(); }
                    public void childRemoved(NodeChangeEvent event) { removed.countDown(); }
                };
                node.addNodeChangeListener(nodes);
                Preferences transientNode = node.node("transient");
                check(added.await(20, TimeUnit.SECONDS));
                transientNode.removeNode();
                check(removed.await(20, TimeUnit.SECONDS));
                node.removeNodeChangeListener(nodes);
                node.flush();
                break;
            case "read":
                node.sync();
                check(node.get("unicode", "").equals("λ 雪 😀 & < > \""));
                check(node.getInt("integer", 0) == -123456);
                check(node.getLong("long", 0) == Long.MAX_VALUE);
                check(node.getBoolean("boolean", false));
                check(Arrays.equals(node.getByteArray("bytes", null),
                                    new byte[] {0, -1, 127, -128}));
                check(node.node("child").get("key", "").equals("child value"));
                ByteArrayOutputStream xml = new ByteArrayOutputStream();
                node.exportSubtree(xml);
                node.removeNode();
                root.flush();
                check(!root.nodeExists("openldk/probe"));
                Preferences.importPreferences(new ByteArrayInputStream(xml.toByteArray()));
                check(root.node("openldk/probe").get("unicode", "").equals("λ 雪 😀 & < > \""));
                root.node("openldk").removeNode();
                root.flush();
                break;
            case "removed":
                // This invocation created the empty probe node above.  The
                // previous process's keys and child must have disappeared.
                check(node.keys().length == 0);
                check(node.childrenNames().length == 0);
                root.node("openldk").removeNode();
                root.flush();
                break;
            default: throw new AssertionError(args[0]);
        }
        System.out.println("PASS " + args[0]);
    }
}
