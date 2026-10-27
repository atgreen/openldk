// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class ModuleLayerProbe {
 public static void main(String[] args) throws Exception {
  ModuleLayer layer = ModuleLayer.boot();
  System.out.println("boot modules=" + layer.modules().size());
  Module base = layer.findModule("java.base").orElseThrow();
  Module xml = layer.findModule("java.xml").orElseThrow();
  if (Object.class.getModule() != base || base.getDescriptor() == null)
   throw new AssertionError("java.base ownership");
  if (javax.xml.parsers.DocumentBuilderFactory.class.getModule() != xml
      || !xml.isExported("javax.xml.parsers") || !xml.getPackages().contains("javax.xml.parsers"))
   throw new AssertionError("XML module ownership and exports");
  if (int[].class.getModule() != base || Object[].class.getModule() != base)
   throw new AssertionError("Array module ownership");
  if (Class.forName(xml, "javax.xml.parsers.DocumentBuilderFactory")
      != javax.xml.parsers.DocumentBuilderFactory.class
      || Class.forName(xml, "java.lang.String") != null)
   throw new AssertionError("Named-module class lookup");
  if (ModuleLayerProbe.class.getModule() != ClassLoader.getSystemClassLoader().getUnnamedModule())
   throw new AssertionError("Application unnamed module");
  var document = javax.xml.parsers.DocumentBuilderFactory.newInstance()
      .newDocumentBuilder().parse(new java.io.ByteArrayInputStream("<hello>value</hello>".getBytes()));
  if (!document.getDocumentElement().getTagName().equals("hello"))
   throw new AssertionError("XML parsing");
  String zone = java.util.TimeZone.getTimeZone("Europe/Berlin")
      .getDisplayName(false, java.util.TimeZone.LONG, java.util.Locale.US);
  if (zone.isEmpty() || zone.equals("Europe/Berlin"))
   throw new AssertionError("Locale-data service provider lookup");
  System.out.println("Boot module layer, ownership, and exports passed");
 }
}
