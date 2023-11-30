// Test for method java.lang.Class.getClass().getDeclaredMethods()

// Copyright (C) 2012, 2013, 2014, 2015 Pavel Tisnovsky <ptisnovs@redhat.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

// Tags: JDK1.5

package gnu.testlet.java.lang.Class.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Class;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Class.getClass().getDeclaredMethods()
 */
public class getDeclaredMethods implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of declared methods which should exists
        Map<String, String> testedDeclaredMethods = null;

        // map of declared methods for (Open)JDK6
        Map<String, String> testedDeclaredMethods_jdk6 = new HashMap<String, String>();

        // map of declared methods for (Open)JDK7
        Map<String, String> testedDeclaredMethods_jdk7 = new HashMap<String, String>();

        // map for methods declared in (Open)JDK6
        testedDeclaredMethods_jdk6.put("private void java.lang.Class.checkPackageAccess(java.lang.ClassLoader,boolean)", "checkPackageAccess");
        testedDeclaredMethods_jdk6.put("public static java.lang.Class java.lang.Class.forName(java.lang.String,boolean,java.lang.ClassLoader) throws java.lang.ClassNotFoundException", "forName");
        testedDeclaredMethods_jdk6.put("public static java.lang.Class java.lang.Class.forName(java.lang.String) throws java.lang.ClassNotFoundException", "forName");
        testedDeclaredMethods_jdk6.put("private static java.lang.Class java.lang.Class.forName0(java.lang.String,boolean,java.lang.ClassLoader) throws java.lang.ClassNotFoundException", "forName0");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Class.toString()", "toString");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isAssignableFrom(java.lang.Class)", "isAssignableFrom");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isInstance(java.lang.Object)", "isInstance");
        testedDeclaredMethods_jdk6.put("public int java.lang.Class.getModifiers()", "getModifiers");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isInterface()", "isInterface");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isArray()", "isArray");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isPrimitive()", "isPrimitive");
        testedDeclaredMethods_jdk6.put("public java.lang.Class java.lang.Class.getSuperclass()", "getSuperclass");
        testedDeclaredMethods_jdk6.put("public java.lang.Class java.lang.Class.getComponentType()", "getComponentType");
        testedDeclaredMethods_jdk6.put("private static void java.lang.Class.registerNatives()", "registerNatives");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Class.getName()", "getName");
        testedDeclaredMethods_jdk6.put("java.util.Map java.lang.Class.enumConstantDirectory()", "enumConstantDirectory");
        testedDeclaredMethods_jdk6.put("public java.lang.Object java.lang.Class.newInstance() throws java.lang.InstantiationException,java.lang.IllegalAccessException", "newInstance");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isAnnotation()", "isAnnotation");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isSynthetic()", "isSynthetic");
        testedDeclaredMethods_jdk6.put("private java.lang.String java.lang.Class.getName0()", "getName0");
        testedDeclaredMethods_jdk6.put("public java.lang.ClassLoader java.lang.Class.getClassLoader()", "getClassLoader");
        testedDeclaredMethods_jdk6.put("native java.lang.ClassLoader java.lang.Class.getClassLoader0()", "getClassLoader0");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.TypeVariable[] java.lang.Class.getTypeParameters()", "getTypeParameters");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Type java.lang.Class.getGenericSuperclass()", "getGenericSuperclass");
        testedDeclaredMethods_jdk6.put("public java.lang.Package java.lang.Class.getPackage()", "getPackage");
        testedDeclaredMethods_jdk6.put("public java.lang.Class[] java.lang.Class.getInterfaces()", "getInterfaces");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Type[] java.lang.Class.getGenericInterfaces()", "getGenericInterfaces");
        testedDeclaredMethods_jdk6.put("public java.lang.Object[] java.lang.Class.getSigners()", "getSigners");
        testedDeclaredMethods_jdk6.put("native void java.lang.Class.setSigners(java.lang.Object[])", "setSigners");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Method java.lang.Class.getEnclosingMethod()", "getEnclosingMethod");
        testedDeclaredMethods_jdk6.put("private java.lang.Object[] java.lang.Class.getEnclosingMethod0()", "getEnclosingMethod0");
        testedDeclaredMethods_jdk6.put("private java.lang.Class$EnclosingMethodInfo java.lang.Class.getEnclosingMethodInfo()", "getEnclosingMethodInfo");
        testedDeclaredMethods_jdk6.put("private static java.lang.Class java.lang.Class.toClass(java.lang.reflect.Type)", "toClass");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Constructor java.lang.Class.getEnclosingConstructor()", "getEnclosingConstructor");
        testedDeclaredMethods_jdk6.put("public java.lang.Class java.lang.Class.getDeclaringClass()", "getDeclaringClass");
        testedDeclaredMethods_jdk6.put("public java.lang.Class java.lang.Class.getEnclosingClass()", "getEnclosingClass");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Class.getSimpleName()", "getSimpleName");
        testedDeclaredMethods_jdk6.put("private static boolean java.lang.Class.isAsciiDigit(char)", "isAsciiDigit");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Class.getCanonicalName()", "getCanonicalName");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isAnonymousClass()", "isAnonymousClass");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isLocalClass()", "isLocalClass");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isMemberClass()", "isMemberClass");
        testedDeclaredMethods_jdk6.put("private java.lang.String java.lang.Class.getSimpleBinaryName()", "getSimpleBinaryName");
        testedDeclaredMethods_jdk6.put("private boolean java.lang.Class.isLocalOrAnonymousClass()", "isLocalOrAnonymousClass");
        testedDeclaredMethods_jdk6.put("public java.lang.Class[] java.lang.Class.getClasses()", "getClasses");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Field[] java.lang.Class.getFields() throws java.lang.SecurityException", "getFields");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Method[] java.lang.Class.getMethods() throws java.lang.SecurityException", "getMethods");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Constructor[] java.lang.Class.getConstructors() throws java.lang.SecurityException", "getConstructors");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Field java.lang.Class.getField(java.lang.String) throws java.lang.NoSuchFieldException,java.lang.SecurityException", "getField");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Method java.lang.Class.getMethod(java.lang.String,java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getMethod");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Constructor java.lang.Class.getConstructor(java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getConstructor");
        testedDeclaredMethods_jdk6.put("public java.lang.Class[] java.lang.Class.getDeclaredClasses() throws java.lang.SecurityException", "getDeclaredClasses");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Field[] java.lang.Class.getDeclaredFields() throws java.lang.SecurityException", "getDeclaredFields");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Method[] java.lang.Class.getDeclaredMethods() throws java.lang.SecurityException", "getDeclaredMethods");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Constructor[] java.lang.Class.getDeclaredConstructors() throws java.lang.SecurityException", "getDeclaredConstructors");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Field java.lang.Class.getDeclaredField(java.lang.String) throws java.lang.NoSuchFieldException,java.lang.SecurityException", "getDeclaredField");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Method java.lang.Class.getDeclaredMethod(java.lang.String,java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getDeclaredMethod");
        testedDeclaredMethods_jdk6.put("public java.lang.reflect.Constructor java.lang.Class.getDeclaredConstructor(java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getDeclaredConstructor");
        testedDeclaredMethods_jdk6.put("public java.io.InputStream java.lang.Class.getResourceAsStream(java.lang.String)", "getResourceAsStream");
        testedDeclaredMethods_jdk6.put("public java.net.URL java.lang.Class.getResource(java.lang.String)", "getResource");
        testedDeclaredMethods_jdk6.put("public java.security.ProtectionDomain java.lang.Class.getProtectionDomain()", "getProtectionDomain");
        testedDeclaredMethods_jdk6.put("private java.security.ProtectionDomain java.lang.Class.getProtectionDomain0()", "getProtectionDomain0");
        testedDeclaredMethods_jdk6.put("native void java.lang.Class.setProtectionDomain0(java.security.ProtectionDomain)", "setProtectionDomain0");
        testedDeclaredMethods_jdk6.put("static java.lang.Class java.lang.Class.getPrimitiveClass(java.lang.String)", "getPrimitiveClass");
        testedDeclaredMethods_jdk6.put("private static boolean java.lang.Class.isCheckMemberAccessOverridden(java.lang.SecurityManager)", "isCheckMemberAccessOverridden");
        testedDeclaredMethods_jdk6.put("private void java.lang.Class.checkMemberAccess(int,java.lang.Class,boolean)", "checkMemberAccess");
        testedDeclaredMethods_jdk6.put("private java.lang.String java.lang.Class.resolveName(java.lang.String)", "resolveName");
        testedDeclaredMethods_jdk6.put("private void java.lang.Class.clearCachesOnClassRedefinition()", "clearCachesOnClassRedefinition");
        testedDeclaredMethods_jdk6.put("private java.lang.String java.lang.Class.getGenericSignature()", "getGenericSignature");
        testedDeclaredMethods_jdk6.put("private sun.reflect.generics.factory.GenericsFactory java.lang.Class.getFactory()", "getFactory");
        testedDeclaredMethods_jdk6.put("private sun.reflect.generics.repository.ClassRepository java.lang.Class.getGenericInfo()", "getGenericInfo");
        testedDeclaredMethods_jdk6.put("private byte[] java.lang.Class.getRawAnnotations()", "getRawAnnotations");
        testedDeclaredMethods_jdk6.put("native sun.reflect.ConstantPool java.lang.Class.getConstantPool()", "getConstantPool");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Field[] java.lang.Class.privateGetDeclaredFields(boolean)", "privateGetDeclaredFields");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Field[] java.lang.Class.privateGetPublicFields(java.util.Set)", "privateGetPublicFields");
        testedDeclaredMethods_jdk6.put("private static void java.lang.Class.addAll(java.util.Collection,java.lang.reflect.Field[])", "addAll");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Constructor[] java.lang.Class.privateGetDeclaredConstructors(boolean)", "privateGetDeclaredConstructors");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Method[] java.lang.Class.privateGetDeclaredMethods(boolean)", "privateGetDeclaredMethods");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Method[] java.lang.Class.privateGetPublicMethods()", "privateGetPublicMethods");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Field java.lang.Class.searchFields(java.lang.reflect.Field[],java.lang.String)", "searchFields");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Field java.lang.Class.getField0(java.lang.String) throws java.lang.NoSuchFieldException", "getField0");
        testedDeclaredMethods_jdk6.put("private static java.lang.reflect.Method java.lang.Class.searchMethods(java.lang.reflect.Method[],java.lang.String,java.lang.Class[])", "searchMethods");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Method java.lang.Class.getMethod0(java.lang.String,java.lang.Class[])", "getMethod0");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Constructor java.lang.Class.getConstructor0(java.lang.Class[],int) throws java.lang.NoSuchMethodException", "getConstructor0");
        testedDeclaredMethods_jdk6.put("private static boolean java.lang.Class.arrayContentsEq(java.lang.Object[],java.lang.Object[])", "arrayContentsEq");
        testedDeclaredMethods_jdk6.put("private static java.lang.reflect.Field[] java.lang.Class.copyFields(java.lang.reflect.Field[])", "copyFields");
        testedDeclaredMethods_jdk6.put("private static java.lang.reflect.Method[] java.lang.Class.copyMethods(java.lang.reflect.Method[])", "copyMethods");
        testedDeclaredMethods_jdk6.put("private static java.lang.reflect.Constructor[] java.lang.Class.copyConstructors(java.lang.reflect.Constructor[])", "copyConstructors");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Field[] java.lang.Class.getDeclaredFields0(boolean)", "getDeclaredFields0");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Method[] java.lang.Class.getDeclaredMethods0(boolean)", "getDeclaredMethods0");
        testedDeclaredMethods_jdk6.put("private java.lang.reflect.Constructor[] java.lang.Class.getDeclaredConstructors0(boolean)", "getDeclaredConstructors0");
        testedDeclaredMethods_jdk6.put("private java.lang.Class[] java.lang.Class.getDeclaredClasses0()", "getDeclaredClasses0");
        testedDeclaredMethods_jdk6.put("private static java.lang.String java.lang.Class.argumentTypesToString(java.lang.Class[])", "argumentTypesToString");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.desiredAssertionStatus()", "desiredAssertionStatus");
        testedDeclaredMethods_jdk6.put("private static boolean java.lang.Class.desiredAssertionStatus0(java.lang.Class)", "desiredAssertionStatus0");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isEnum()", "isEnum");
        testedDeclaredMethods_jdk6.put("private static sun.reflect.ReflectionFactory java.lang.Class.getReflectionFactory()", "getReflectionFactory");
        testedDeclaredMethods_jdk6.put("private static void java.lang.Class.checkInitted()", "checkInitted");
        testedDeclaredMethods_jdk6.put("public java.lang.Object[] java.lang.Class.getEnumConstants()", "getEnumConstants");
        testedDeclaredMethods_jdk6.put("java.lang.Object[] java.lang.Class.getEnumConstantsShared()", "getEnumConstantsShared");
        testedDeclaredMethods_jdk6.put("public java.lang.Object java.lang.Class.cast(java.lang.Object)", "cast");
        testedDeclaredMethods_jdk6.put("private java.lang.String java.lang.Class.cannotCastMsg(java.lang.Object)", "cannotCastMsg");
        testedDeclaredMethods_jdk6.put("public java.lang.Class java.lang.Class.asSubclass(java.lang.Class)", "asSubclass");
        testedDeclaredMethods_jdk6.put("public java.lang.annotation.Annotation java.lang.Class.getAnnotation(java.lang.Class)", "getAnnotation");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Class.isAnnotationPresent(java.lang.Class)", "isAnnotationPresent");
        testedDeclaredMethods_jdk6.put("public java.lang.annotation.Annotation[] java.lang.Class.getAnnotations()", "getAnnotations");
        testedDeclaredMethods_jdk6.put("public java.lang.annotation.Annotation[] java.lang.Class.getDeclaredAnnotations()", "getDeclaredAnnotations");
        testedDeclaredMethods_jdk6.put("private synchronized void java.lang.Class.initAnnotationsIfNecessary()", "initAnnotationsIfNecessary");
        testedDeclaredMethods_jdk6.put("void java.lang.Class.setAnnotationType(sun.reflect.annotation.AnnotationType)", "setAnnotationType");
        testedDeclaredMethods_jdk6.put("sun.reflect.annotation.AnnotationType java.lang.Class.getAnnotationType()", "getAnnotationType");
        testedDeclaredMethods_jdk6.put("static boolean java.lang.Class.access$100(java.lang.Object[],java.lang.Object[])", "access$100");
        testedDeclaredMethods_jdk6.put("static boolean java.lang.Class.access$202(boolean)", "access$202");
        testedDeclaredMethods_jdk6.put("static boolean java.lang.Class.access$302(boolean)", "access$302");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("private java.lang.Object java.lang.Class.newInstance0() throws java.lang.InstantiationException,java.lang.IllegalAccessException", "newInstance0");
        testedDeclaredMethods_jdk7.put("public static java.lang.Class java.lang.Class.forName(java.lang.String,boolean,java.lang.ClassLoader) throws java.lang.ClassNotFoundException", "forName");
        testedDeclaredMethods_jdk7.put("public static java.lang.Class java.lang.Class.forName(java.lang.String) throws java.lang.ClassNotFoundException", "forName");
        testedDeclaredMethods_jdk7.put("private static java.lang.Class java.lang.Class.forName0(java.lang.String,boolean,java.lang.ClassLoader) throws java.lang.ClassNotFoundException", "forName0");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Class.toString()", "toString");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isAssignableFrom(java.lang.Class)", "isAssignableFrom");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isInstance(java.lang.Object)", "isInstance");
        testedDeclaredMethods_jdk7.put("public int java.lang.Class.getModifiers()", "getModifiers");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isInterface()", "isInterface");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isArray()", "isArray");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isPrimitive()", "isPrimitive");
        testedDeclaredMethods_jdk7.put("public java.lang.Class java.lang.Class.getSuperclass()", "getSuperclass");
        testedDeclaredMethods_jdk7.put("public java.lang.Class java.lang.Class.getComponentType()", "getComponentType");
        testedDeclaredMethods_jdk7.put("private static void java.lang.Class.registerNatives()", "registerNatives");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Class.getName()", "getName");
        testedDeclaredMethods_jdk7.put("static boolean java.lang.Class.access$100(java.lang.Object[],java.lang.Object[])", "access$100");
        testedDeclaredMethods_jdk7.put("static boolean java.lang.Class.access$202(boolean)", "access$202");
        testedDeclaredMethods_jdk7.put("static boolean java.lang.Class.access$302(boolean)", "access$302");
        testedDeclaredMethods_jdk7.put("private static void java.lang.Class.addAll(java.util.Collection,java.lang.reflect.Field[])", "addAll");
        testedDeclaredMethods_jdk7.put("private static java.lang.String java.lang.Class.argumentTypesToString(java.lang.Class[])", "argumentTypesToString");
        testedDeclaredMethods_jdk7.put("private static boolean java.lang.Class.arrayContentsEq(java.lang.Object[],java.lang.Object[])", "arrayContentsEq");
        testedDeclaredMethods_jdk7.put("public java.lang.Class java.lang.Class.asSubclass(java.lang.Class)", "asSubclass");
        testedDeclaredMethods_jdk7.put("private java.lang.String java.lang.Class.cannotCastMsg(java.lang.Object)", "cannotCastMsg");
        testedDeclaredMethods_jdk7.put("public java.lang.Object java.lang.Class.cast(java.lang.Object)", "cast");
        testedDeclaredMethods_jdk7.put("private static void java.lang.Class.checkInitted()", "checkInitted");
        testedDeclaredMethods_jdk7.put("private void java.lang.Class.checkMemberAccess(int,java.lang.ClassLoader)", "checkMemberAccess");
        testedDeclaredMethods_jdk7.put("private void java.lang.Class.clearCachesOnClassRedefinition()", "clearCachesOnClassRedefinition");
        testedDeclaredMethods_jdk7.put("private static java.lang.reflect.Constructor[] java.lang.Class.copyConstructors(java.lang.reflect.Constructor[])", "copyConstructors");
        testedDeclaredMethods_jdk7.put("private static java.lang.reflect.Field[] java.lang.Class.copyFields(java.lang.reflect.Field[])", "copyFields");
        testedDeclaredMethods_jdk7.put("private static java.lang.reflect.Method[] java.lang.Class.copyMethods(java.lang.reflect.Method[])", "copyMethods");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.desiredAssertionStatus()", "desiredAssertionStatus");
        testedDeclaredMethods_jdk7.put("private static boolean java.lang.Class.desiredAssertionStatus0(java.lang.Class)", "desiredAssertionStatus0");
        testedDeclaredMethods_jdk7.put("java.util.Map java.lang.Class.enumConstantDirectory()", "enumConstantDirectory");
        testedDeclaredMethods_jdk7.put("public java.lang.annotation.Annotation java.lang.Class.getAnnotation(java.lang.Class)", "getAnnotation");
        testedDeclaredMethods_jdk7.put("sun.reflect.annotation.AnnotationType java.lang.Class.getAnnotationType()", "getAnnotationType");
        testedDeclaredMethods_jdk7.put("public java.lang.annotation.Annotation[] java.lang.Class.getAnnotations()", "getAnnotations");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Class.getCanonicalName()", "getCanonicalName");
        testedDeclaredMethods_jdk7.put("public java.lang.ClassLoader java.lang.Class.getClassLoader()", "getClassLoader");
        testedDeclaredMethods_jdk7.put("native java.lang.ClassLoader java.lang.Class.getClassLoader0()", "getClassLoader0");
        testedDeclaredMethods_jdk7.put("public java.lang.Class[] java.lang.Class.getClasses()", "getClasses");
        testedDeclaredMethods_jdk7.put("native sun.reflect.ConstantPool java.lang.Class.getConstantPool()", "getConstantPool");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Constructor java.lang.Class.getConstructor(java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getConstructor");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Constructor java.lang.Class.getConstructor0(java.lang.Class[],int) throws java.lang.NoSuchMethodException", "getConstructor0");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Constructor[] java.lang.Class.getConstructors() throws java.lang.SecurityException", "getConstructors");
        testedDeclaredMethods_jdk7.put("public java.lang.annotation.Annotation[] java.lang.Class.getDeclaredAnnotations()", "getDeclaredAnnotations");
        testedDeclaredMethods_jdk7.put("public java.lang.Class[] java.lang.Class.getDeclaredClasses() throws java.lang.SecurityException", "getDeclaredClasses");
        testedDeclaredMethods_jdk7.put("private java.lang.Class[] java.lang.Class.getDeclaredClasses0()", "getDeclaredClasses0");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Constructor java.lang.Class.getDeclaredConstructor(java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getDeclaredConstructor");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Constructor[] java.lang.Class.getDeclaredConstructors() throws java.lang.SecurityException", "getDeclaredConstructors");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Constructor[] java.lang.Class.getDeclaredConstructors0(boolean)", "getDeclaredConstructors0");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Field java.lang.Class.getDeclaredField(java.lang.String) throws java.lang.NoSuchFieldException,java.lang.SecurityException", "getDeclaredField");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Field[] java.lang.Class.getDeclaredFields() throws java.lang.SecurityException", "getDeclaredFields");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Field[] java.lang.Class.getDeclaredFields0(boolean)", "getDeclaredFields0");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Method java.lang.Class.getDeclaredMethod(java.lang.String,java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getDeclaredMethod");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Method[] java.lang.Class.getDeclaredMethods() throws java.lang.SecurityException", "getDeclaredMethods");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Method[] java.lang.Class.getDeclaredMethods0(boolean)", "getDeclaredMethods0");
        testedDeclaredMethods_jdk7.put("public java.lang.Class java.lang.Class.getDeclaringClass()", "getDeclaringClass");
        testedDeclaredMethods_jdk7.put("public java.lang.Class java.lang.Class.getEnclosingClass()", "getEnclosingClass");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Constructor java.lang.Class.getEnclosingConstructor()", "getEnclosingConstructor");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Method java.lang.Class.getEnclosingMethod()", "getEnclosingMethod");
        testedDeclaredMethods_jdk7.put("private java.lang.Object[] java.lang.Class.getEnclosingMethod0()", "getEnclosingMethod0");
        testedDeclaredMethods_jdk7.put("private java.lang.Class$EnclosingMethodInfo java.lang.Class.getEnclosingMethodInfo()", "getEnclosingMethodInfo");
        testedDeclaredMethods_jdk7.put("public java.lang.Object[] java.lang.Class.getEnumConstants()", "getEnumConstants");
        testedDeclaredMethods_jdk7.put("java.lang.Object[] java.lang.Class.getEnumConstantsShared()", "getEnumConstantsShared");
        testedDeclaredMethods_jdk7.put("private sun.reflect.generics.factory.GenericsFactory java.lang.Class.getFactory()", "getFactory");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Field java.lang.Class.getField(java.lang.String) throws java.lang.NoSuchFieldException,java.lang.SecurityException", "getField");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Field java.lang.Class.getField0(java.lang.String) throws java.lang.NoSuchFieldException", "getField0");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Field[] java.lang.Class.getFields() throws java.lang.SecurityException", "getFields");
        testedDeclaredMethods_jdk7.put("private sun.reflect.generics.repository.ClassRepository java.lang.Class.getGenericInfo()", "getGenericInfo");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Type[] java.lang.Class.getGenericInterfaces()", "getGenericInterfaces");
        testedDeclaredMethods_jdk7.put("private java.lang.String java.lang.Class.getGenericSignature()", "getGenericSignature");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Type java.lang.Class.getGenericSuperclass()", "getGenericSuperclass");
        testedDeclaredMethods_jdk7.put("public java.lang.Class[] java.lang.Class.getInterfaces()", "getInterfaces");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Method java.lang.Class.getMethod(java.lang.String,java.lang.Class[]) throws java.lang.NoSuchMethodException,java.lang.SecurityException", "getMethod");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Method java.lang.Class.getMethod0(java.lang.String,java.lang.Class[])", "getMethod0");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.Method[] java.lang.Class.getMethods() throws java.lang.SecurityException", "getMethods");
        testedDeclaredMethods_jdk7.put("private java.lang.String java.lang.Class.getName0()", "getName0");
        testedDeclaredMethods_jdk7.put("public java.lang.Package java.lang.Class.getPackage()", "getPackage");
        testedDeclaredMethods_jdk7.put("static java.lang.Class java.lang.Class.getPrimitiveClass(java.lang.String)", "getPrimitiveClass");
        testedDeclaredMethods_jdk7.put("public java.security.ProtectionDomain java.lang.Class.getProtectionDomain()", "getProtectionDomain");
        testedDeclaredMethods_jdk7.put("private java.security.ProtectionDomain java.lang.Class.getProtectionDomain0()", "getProtectionDomain0");
        testedDeclaredMethods_jdk7.put("private byte[] java.lang.Class.getRawAnnotations()", "getRawAnnotations");
        testedDeclaredMethods_jdk7.put("private static sun.reflect.ReflectionFactory java.lang.Class.getReflectionFactory()", "getReflectionFactory");
        testedDeclaredMethods_jdk7.put("public java.net.URL java.lang.Class.getResource(java.lang.String)", "getResource");
        testedDeclaredMethods_jdk7.put("public java.io.InputStream java.lang.Class.getResourceAsStream(java.lang.String)", "getResourceAsStream");
        testedDeclaredMethods_jdk7.put("public java.lang.Object[] java.lang.Class.getSigners()", "getSigners");
        testedDeclaredMethods_jdk7.put("private java.lang.String java.lang.Class.getSimpleBinaryName()", "getSimpleBinaryName");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Class.getSimpleName()", "getSimpleName");
        testedDeclaredMethods_jdk7.put("public java.lang.reflect.TypeVariable[] java.lang.Class.getTypeParameters()", "getTypeParameters");
        testedDeclaredMethods_jdk7.put("private synchronized void java.lang.Class.initAnnotationsIfNecessary()", "initAnnotationsIfNecessary");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isAnnotation()", "isAnnotation");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isAnnotationPresent(java.lang.Class)", "isAnnotationPresent");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isAnonymousClass()", "isAnonymousClass");
        testedDeclaredMethods_jdk7.put("private static boolean java.lang.Class.isAsciiDigit(char)", "isAsciiDigit");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isEnum()", "isEnum");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isLocalClass()", "isLocalClass");
        testedDeclaredMethods_jdk7.put("private boolean java.lang.Class.isLocalOrAnonymousClass()", "isLocalOrAnonymousClass");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isMemberClass()", "isMemberClass");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Class.isSynthetic()", "isSynthetic");
        testedDeclaredMethods_jdk7.put("public java.lang.Object java.lang.Class.newInstance() throws java.lang.InstantiationException,java.lang.IllegalAccessException", "newInstance");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Constructor[] java.lang.Class.privateGetDeclaredConstructors(boolean)", "privateGetDeclaredConstructors");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Field[] java.lang.Class.privateGetDeclaredFields(boolean)", "privateGetDeclaredFields");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Method[] java.lang.Class.privateGetDeclaredMethods(boolean)", "privateGetDeclaredMethods");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Field[] java.lang.Class.privateGetPublicFields(java.util.Set)", "privateGetPublicFields");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Method[] java.lang.Class.privateGetPublicMethods()", "privateGetPublicMethods");
        testedDeclaredMethods_jdk7.put("private java.lang.String java.lang.Class.resolveName(java.lang.String)", "resolveName");
        testedDeclaredMethods_jdk7.put("private java.lang.reflect.Field java.lang.Class.searchFields(java.lang.reflect.Field[],java.lang.String)", "searchFields");
        testedDeclaredMethods_jdk7.put("private static java.lang.reflect.Method java.lang.Class.searchMethods(java.lang.reflect.Method[],java.lang.String,java.lang.Class[])", "searchMethods");
        testedDeclaredMethods_jdk7.put("void java.lang.Class.setAnnotationType(sun.reflect.annotation.AnnotationType)", "setAnnotationType");
        testedDeclaredMethods_jdk7.put("native void java.lang.Class.setProtectionDomain0(java.security.ProtectionDomain)", "setProtectionDomain0");
        testedDeclaredMethods_jdk7.put("native void java.lang.Class.setSigners(java.lang.Object[])", "setSigners");
        testedDeclaredMethods_jdk7.put("private static java.lang.Class java.lang.Class.toClass(java.lang.reflect.Type)", "toClass");

        // get a runtime class of an object "o"
        final Class c = Class.class;

        // get the right map containing method signatures
        testedDeclaredMethods = getJavaVersion() < 7 ? testedDeclaredMethods_jdk6 : testedDeclaredMethods_jdk7;

        // get all declared methods for this class
        java.lang.reflect.Method[] declaredMethods = c.getDeclaredMethods();

        // expected number of declared methods
        final int expectedNumberOfDeclaredMethods = testedDeclaredMethods.size();

        // basic check for a number of declared methods
        harness.check(declaredMethods.length, expectedNumberOfDeclaredMethods);

        // check if all declared methods exist
        for (java.lang.reflect.Method declaredMethod : declaredMethods) {
            // method name should consists of package name + class name
            String methodName = declaredMethod.getName();
            // modifiers + package + method name + parameter types
            String methodString = declaredMethod.toString().replaceAll(" native ", " ");
            harness.check(testedDeclaredMethods.containsKey(methodString));
            harness.check(testedDeclaredMethods.get(methodString), methodName);
        }
    }

    /**
     * Returns version of Java. The input could have the following form: "1.7.0_06"
     * and we are interested only in "7" in this case.
     * 
     * @return Java version
     */
    protected int getJavaVersion() {
        String javaVersionStr = System.getProperty("java.version");
        String[] parts = javaVersionStr.split("\\.");
        return Integer.parseInt(parts[1]);
    }
}

