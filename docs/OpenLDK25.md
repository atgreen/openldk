# OpenLDK 25: Roadmap for OpenJDK 25 Support

This document outlines the changes required to upgrade OpenLDK from Java 8 (class file version 52) support to OpenJDK 25 (class file version 69) support.

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Class File Format Changes](#class-file-format-changes)
3. [Module System (JPMS) Support](#module-system-jpms-support)
4. [Constant Pool Changes](#constant-pool-changes)
5. [New Class File Attributes](#new-class-file-attributes)
6. [Bytecode Changes](#bytecode-changes)
7. [Runtime Library Changes](#runtime-library-changes)
8. [Reflection API Updates](#reflection-api-updates)
9. [Implementation Priorities](#implementation-priorities)
10. [Files Requiring Modification](#files-requiring-modification)
11. [Testing Strategy](#testing-strategy)
12. [References](#references)

---

## Executive Summary

OpenLDK currently supports only Java 8 (class file version 52.0). Upgrading to OpenJDK 25 (class file version 69.0) requires significant changes across multiple subsystems:

| Area | Complexity | Priority |
|------|------------|----------|
| Module System (JPMS) | **High** | **Critical** |
| Class File Version Handling | Low | Critical |
| CONSTANT_Dynamic | Medium | High |
| New Attributes (NestHost, Records, Sealed) | Medium | High |
| String Concatenation via invokedynamic | Medium | High |
| Runtime Library Replacement | **High** | **Critical** |
| System Properties Update | Low | High |

**Key Challenge**: The most significant change is the removal of `rt.jar` in Java 9+. OpenLDK currently loads bootstrap classes from `$JAVA_HOME/lib/rt.jar`, which no longer exists.

---

## Class File Format Changes

### Version Number Updates

**Current Implementation** (`src/classfile.lisp:494-495`):
```lisp
(read-u2) ;; minor-version
(read-u2) ;; major version
```

The class file version is read but ignored. For JDK 25 support:

**Required Changes**:

1. **Add version validation** in `read-classfile`:
```lisp
(let ((minor-version (read-u2))
      (major-version (read-u2)))
  (when (> major-version 69)
    (error "Unsupported class file version ~A.~A (max supported: 69.0)"
           major-version minor-version))
  (when (and (= minor-version 65535) (> major-version 55))
    ;; Preview features enabled - may need special handling
    (warn "Class file uses preview features")))
```

2. **Store version in `<class>`** for later reference:
```lisp
(defclass/std <class> ()
  ((major-version)      ; NEW
   (minor-version)      ; NEW
   (initialized-p)
   ...))
```

### Class File Major Versions Reference

| Java Version | Major Version |
|--------------|---------------|
| Java 8       | 52 |
| Java 9       | 53 |
| Java 10      | 54 |
| Java 11      | 55 |
| Java 12      | 56 |
| Java 13      | 57 |
| Java 14      | 58 |
| Java 15      | 59 |
| Java 16      | 60 |
| Java 17 (LTS)| 61 |
| Java 18      | 62 |
| Java 19      | 63 |
| Java 20      | 64 |
| Java 21 (LTS)| 65 |
| Java 22      | 66 |
| Java 23      | 67 |
| Java 24      | 68 |
| Java 25 (LTS)| 69 |

---

## Module System (JPMS) Support

### The Problem

Java 9 introduced the Java Platform Module System (JPMS), which fundamentally changed how the JDK runtime is organized:

**Java 8 (Current OpenLDK)**:
- Bootstrap classes in `$JAVA_HOME/lib/rt.jar`
- Extension classes in `$JAVA_HOME/lib/ext/*.jar`
- Single monolithic runtime

**Java 9+ (Target)**:
- No `rt.jar` - classes stored in JMOD files and a runtime image
- Classes organized into modules (e.g., `java.base`, `java.logging`, `java.sql`)
- JRT filesystem for accessing runtime classes

### Implementation Options

#### Option A: Use Extracted Class Files (Recommended for MVP)

Extract the JDK classes to a directory structure and load them as a classpath entry:

```bash
# Extract all modules to a directory
mkdir -p /path/to/extracted-jdk
cd $JAVA_HOME
for mod in jmods/*.jmod; do
  jmod extract --dir /path/to/extracted-jdk $mod
done
```

**Pros**: Minimal changes to OpenLDK classpath handling
**Cons**: Requires manual extraction step, loses module metadata

#### Option B: JRT Filesystem Support (Full Support)

Implement native JRT filesystem access to read classes directly from the runtime image:

1. **Add JMOD/JRT Reader** - New file `src/jrt.lisp`:
```lisp
(defclass jrt-classpath-entry (classpath-entry)
  ((java-home :initarg :java-home)
   (modules :initform (make-hash-table :test 'equal)))
  (:documentation "Classpath entry for JDK 9+ JRT filesystem"))

(defmethod open-java-classfile ((cpe jrt-classpath-entry) classname)
  "Read a class from the JRT filesystem."
  ;; The JRT filesystem is at $JAVA_HOME/lib/modules
  ;; It's a custom format that requires special parsing
  ...)
```

2. **Update `bootstrap.lisp`** to detect JDK version:
```lisp
(defun detect-jdk-version ()
  "Determine if JAVA_HOME points to JDK 8 or JDK 9+."
  (let ((rt-jar (merge-pathnames "lib/rt.jar" *java-home*)))
    (if (probe-file rt-jar)
        :jdk8
        :jdk9+)))

(defun initialize-boot-classpath ()
  (ecase (detect-jdk-version)
    (:jdk8
     (push (make-instance 'jar-classpath-entry
                          :jarfile (merge-pathnames "lib/rt.jar" *java-home*))
           *classpath*))
    (:jdk9+
     (push (make-instance 'jrt-classpath-entry
                          :java-home *java-home*)
           *classpath*))))
```

#### Option C: Use ct.sym for Compilation (Hybrid)

For compilation-only scenarios, JDK provides `lib/ct.sym` which contains stripped class files.

### Module Descriptor Handling

Java 9+ class files may contain `module-info.class` files. These should be recognized and handled:

**Required in `read-attributes`** (`src/classfile.lisp`):
```lisp
("Module"
 ;; Parse module descriptor
 (let* ((module-name-index (read-u2))
        (module-flags (read-u2))
        (module-version-index (read-u2))
        ...)
   (setf (gethash "Module" attributes)
         (make-instance '<module-info>
                        :name (emit (aref constant-pool module-name-index) constant-pool)
                        ...))))

("ModulePackages"
 (read-buffer attributes-length))

("ModuleMainClass"
 (read-buffer attributes-length))
```

---

## Constant Pool Changes

### New Constant Pool Tags

**Current Implementation** (`src/classfile.lisp:506-573`) supports tags 1-18.

**Required Additions**:

| Tag | Name | Introduced | Description |
|-----|------|------------|-------------|
| 17 | CONSTANT_Dynamic | JDK 11 | Dynamic computed constant |
| 19 | CONSTANT_Module | JDK 9 | Module descriptor reference |
| 20 | CONSTANT_Package | JDK 9 | Package descriptor reference |

### CONSTANT_Dynamic (Tag 17) - Critical for JDK 11+

CONSTANT_Dynamic is similar to CONSTANT_InvokeDynamic but for constants rather than method calls. It allows lazy constant initialization via bootstrap methods.

**Add to `src/classfile.lisp`**:
```lisp
(defclass/std constant-dynamic ()
  ((bootstrap-method-attr-index)
   (name-and-type-index)))

(define-print-object/std constant-dynamic)
```

**In constant pool parsing**:
```lisp
(17
 ;; CONSTANT_Dynamic - JDK 11+
 (let ((bootstrap-method-attr-index (read-u2))
       (name-and-type-index (read-u2)))
   (make-instance 'constant-dynamic
                  :bootstrap-method-attr-index bootstrap-method-attr-index
                  :name-and-type-index name-and-type-index)))
```

**In `bc-to-ir.lisp`** - handle LDC with CONSTANT_Dynamic:
```lisp
(defmethod emit ((cd constant-dynamic) cp)
  "Resolve a dynamic constant by invoking its bootstrap method."
  ;; Similar to invokedynamic but returns a constant value
  ...)
```

### CONSTANT_Module (Tag 19) and CONSTANT_Package (Tag 20)

These are used in module descriptors:

```lisp
(19
 ;; CONSTANT_Module - JDK 9+
 (let ((name-index (read-u2)))
   (make-instance 'constant-module-reference :name-index name-index)))

(20
 ;; CONSTANT_Package - JDK 9+
 (let ((name-index (read-u2)))
   (make-instance 'constant-package-reference :name-index name-index)))
```

---

## New Class File Attributes

### Attributes by Java Version

| Attribute | Introduced | Location | Purpose |
|-----------|------------|----------|---------|
| NestHost | JDK 11 | ClassFile | Points to nest host class |
| NestMembers | JDK 11 | ClassFile | Lists nest member classes |
| Record | JDK 16 | ClassFile | Record component metadata |
| PermittedSubclasses | JDK 17 | ClassFile | Sealed class constraints |
| MethodParameters | JDK 8 | method_info | Parameter names/flags |

### NestHost and NestMembers (JDK 11) - Nest-Based Access Control

Enables inner classes to access private members without synthetic bridge methods.

**Add to `read-attributes`** (`src/classfile.lisp`):
```lisp
("NestHost"
 ;; Points to the host class of this nest
 (let ((host-class-index (read-u2)))
   (setf (gethash "NestHost" attributes)
         host-class-index)))

("NestMembers"
 ;; Lists all members of the nest (only on host class)
 (let ((number-of-classes (read-u2)))
   (setf (gethash "NestMembers" attributes)
         (loop for i below number-of-classes
               collect (read-u2)))))
```

**Runtime Support** - Add to `<class>`:
```lisp
(defclass/std <class> ()
  ((nest-host)      ; NEW - class index or nil
   (nest-members)   ; NEW - list of class indices or nil
   ...))
```

**Reflection API** - Implement in `src/reflection.lisp` or `src/native.lisp`:
```lisp
(defun |java/lang/Class.getNestHost()| (this)
  "Return the nest host of this class."
  ...)

(defun |java/lang/Class.getNestMembers()| (this)
  "Return array of all nest members."
  ...)

(defun |java/lang/Class.isNestmateOf(Ljava/lang/Class;)| (this other)
  "Check if this class and other are nestmates."
  ...)
```

### Record Attribute (JDK 16)

Records are a special kind of class that acts as a transparent carrier for immutable data.

**Add to `read-attributes`**:
```lisp
("Record"
 ;; Parse record components
 (let ((components-count (read-u2)))
   (setf (gethash "Record" attributes)
         (loop for i below components-count
               collect (let* ((name-index (read-u2))
                             (descriptor-index (read-u2))
                             (attr-count (read-u2))
                             (attrs (read-attributes bitio constant-pool class attr-count)))
                        (make-instance '<record-component>
                                       :name (slot-value (aref constant-pool name-index) 'value)
                                       :descriptor (slot-value (aref constant-pool descriptor-index) 'value)
                                       :attributes attrs))))))
```

**New class for record components**:
```lisp
(defclass/std <record-component> ()
  ((name)
   (descriptor)
   (attributes)
   (generic-signature)))  ; From Signature attribute if present
```

**Runtime Support**:
```lisp
;; Class.isRecord()
(defun |java/lang/Class.isRecord()| (this)
  (let ((ldk-class (%get-ldk-class-by-java-class this)))
    (not (null (gethash "Record" (attributes ldk-class))))))

;; Class.getRecordComponents()
(defun |java/lang/Class.getRecordComponents()| (this)
  ...)
```

### PermittedSubclasses (JDK 17) - Sealed Classes

Controls which classes can extend a sealed class.

**Add to `read-attributes`**:
```lisp
("PermittedSubclasses"
 (let ((number-of-classes (read-u2)))
   (setf (gethash "PermittedSubclasses" attributes)
         (loop for i below number-of-classes
               collect (let ((class-index (read-u2)))
                        (emit-name (aref constant-pool class-index) constant-pool))))))
```

**Runtime Enforcement** - In class loading:
```lisp
(defun validate-permitted-subclass (subclass superclass)
  "Verify that SUBCLASS is permitted to extend SUPERCLASS."
  (when-let (permitted (gethash "PermittedSubclasses" (attributes superclass)))
    (unless (member (name subclass) permitted :test #'string=)
      (error 'java-lang-incompatible-class-change-error
             :message (format nil "~A is not permitted to extend sealed class ~A"
                             (name subclass) (name superclass))))))
```

**Reflection API**:
```lisp
(defun |java/lang/Class.isSealed()| (this)
  (let ((ldk-class (%get-ldk-class-by-java-class this)))
    (not (null (gethash "PermittedSubclasses" (attributes ldk-class))))))

(defun |java/lang/Class.getPermittedSubclasses()| (this)
  ...)
```

---

## Bytecode Changes

### String Concatenation (JDK 9+)

**Before JDK 9**: String concatenation used `StringBuilder`:
```java
String s = "Hello " + name + "!";
// Compiled to: new StringBuilder().append("Hello ").append(name).append("!").toString()
```

**JDK 9+**: Uses `invokedynamic` with `StringConcatFactory`:
```
invokedynamic #X:makeConcatWithConstants:(Ljava/lang/String;)Ljava/lang/String; [
  "\u0001Hello \u0001!"
]
```

**Required**: The existing `INVOKEDYNAMIC` handler in `bc-to-ir.lisp` should already support this, but ensure `StringConcatFactory` bootstrap methods are handled:

```lisp
;; In codegen.lisp, add fast path for StringConcatFactory
(defun string-concat-bootstrap-p (bootstrap-method-name)
  "Check if this is a string concatenation bootstrap."
  (or (search "StringConcatFactory" (string bootstrap-method-name))
      (search "makeConcatWithConstants" (string bootstrap-method-name))))

;; Fast path implementation
(defun %fast-string-concat (recipe &rest args)
  "Efficient string concatenation without CallSite overhead."
  (with-output-to-string (s)
    (loop for char across recipe
          with arg-index = 0
          do (if (char= char #\x01)  ; Argument placeholder
                 (progn
                   (princ (elt args arg-index) s)
                   (incf arg-index))
                 (write-char char s)))))
```

### ConstantDynamic in LDC (JDK 11+)

The `LDC` instruction can now load `CONSTANT_Dynamic` entries:

**Update `bc-to-ir.lisp`** LDC transpiler:
```lisp
(define-bytecode-transpiler :LDC (context code)
  (with-slots (pc class) context
    (let* ((index (aref code (1+ pc)))
           (constant-pool (constant-pool class))
           (entry (aref constant-pool index)))
      (typecase entry
        (constant-dynamic
         ;; Resolve dynamic constant via bootstrap method
         (%transpile-constant-dynamic context entry))
        ;; ... existing cases ...
        ))))
```

### No New Bytecode Instructions

JDK 9-25 did not introduce any new bytecode opcodes. The existing opcode set in `src/opcodes.lisp` is complete.

---

## Runtime Library Changes

### System Properties Update

**Update `src/native.lisp`** (~line 838):

```lisp
;; Replace hardcoded Java 8 properties
("java.specification.version" . "25")
("java.specification.name" . "Java Platform API Specification")
("java.vm.specification.version" . "25")
("java.version" . "25")
("java.class.version" . "69.0")
("java.specification.vendor" . "Oracle Corporation")
("java.version.date" . "2025-09-16")
("java.runtime.version" . "25+36")  ; Match actual JDK version
```

Consider making these dynamic based on detected JDK:
```lisp
(defun get-java-version-properties ()
  "Return Java version properties based on JAVA_HOME."
  (let ((version (detect-jdk-version-string)))
    `(("java.specification.version" . ,version)
      ("java.version" . ,version)
      ("java.class.version" . ,(java-version-to-class-version version))
      ...)))
```

### Thread and Concurrency Changes

**Virtual Threads (JDK 21)** - Project Loom introduced virtual threads. While full support is complex, basic compatibility requires:

1. Recognize `Thread.Builder` and related APIs
2. Handle `Thread.isVirtual()` returning `false` for all OpenLDK threads
3. Support `Thread.startVirtualThread()` by falling back to platform threads

### New Core APIs

Several APIs added since Java 8 will need native method implementations:

| API | Since | Priority | Notes |
|-----|-------|----------|-------|
| `java.lang.StackWalker` | JDK 9 | Medium | Alternative to `Throwable.getStackTrace()` |
| `java.lang.invoke.VarHandle` | JDK 9 | High | Memory access operations |
| `java.lang.Runtime.version()` | JDK 9 | Low | Version parsing |
| `java.util.Optional` enhancements | JDK 9+ | Low | `ifPresentOrElse`, `stream`, etc. |
| `String` methods | JDK 11+ | Medium | `isBlank`, `lines`, `strip`, `repeat` |
| `Pattern` enhancements | JDK 11 | Low | `asMatchPredicate` |
| `Files` enhancements | JDK 11+ | Medium | `readString`, `writeString` |
| `java.net.http.HttpClient` | JDK 11 | Low | New HTTP client |

---

## Reflection API Updates

### New Reflection Methods Required

**Class methods**:
```lisp
;; JDK 11
(defun |java/lang/Class.getNestHost()| (this) ...)
(defun |java/lang/Class.getNestMembers()| (this) ...)
(defun |java/lang/Class.isNestmateOf(Ljava/lang/Class;)| (this other) ...)

;; JDK 12
(defun |java/lang/Class.descriptorString()| (this) ...)

;; JDK 16
(defun |java/lang/Class.isRecord()| (this) ...)
(defun |java/lang/Class.getRecordComponents()| (this) ...)

;; JDK 17
(defun |java/lang/Class.isSealed()| (this) ...)
(defun |java/lang/Class.getPermittedSubclasses()| (this) ...)
```

**Module-related**:
```lisp
(defun |java/lang/Class.getModule()| (this)
  "Return the module this class belongs to."
  ;; For now, return the unnamed module
  (%get-unnamed-module))
```

---

## Implementation Priorities

### Phase 1: Core Compatibility (Minimum Viable)

1. **Class file version handling** - Accept versions up to 69
2. **Runtime detection** - Detect JDK 8 vs JDK 9+ and adjust classpath
3. **System properties** - Update to report JDK 25
4. **CONSTANT_Dynamic** - Required for many JDK 11+ classes
5. **NestHost/NestMembers** - Required for inner class access

### Phase 2: Full Feature Support

1. **Record support** - Full Record attribute parsing and reflection
2. **Sealed classes** - PermittedSubclasses validation
3. **String concatenation** - Optimize StringConcatFactory path
4. **Module stubs** - Basic module API returning unnamed module

### Phase 3: Enhanced Support

1. **Full module support** - Module descriptors, exports, requires
2. **Virtual thread stubs** - Fallback implementation
3. **New String methods** - Native implementations
4. **StackWalker** - Alternative stack walking API

---

## Files Requiring Modification

### Critical Files

| File | Changes Required |
|------|------------------|
| `src/classfile.lisp` | Version validation, new constant pool tags (17, 19, 20), new attributes |
| `src/classpath.lisp` | JRT filesystem support, JDK version detection |
| `src/native.lisp` | System properties, new reflection methods, String methods |
| `src/bootstrap.lisp` | Boot classpath initialization for JDK 9+ |
| `src/bc-to-ir.lisp` | CONSTANT_Dynamic handling in LDC |
| `src/codegen.lisp` | StringConcatFactory fast path |

### New Files to Create

| File | Purpose |
|------|---------|
| `src/jrt.lisp` | JRT filesystem reader for JDK 9+ |
| `src/modules.lisp` | Module system support |

### Configuration

| File | Changes |
|------|---------|
| `openldk.asd` | Add new source files |
| `README.md` | Update requirements for JDK 25 |

---

## Testing Strategy

### Incremental Testing

1. **Version Detection Test**
   - Create test classes compiled with different JDK versions
   - Verify OpenLDK correctly identifies and loads each version

2. **Constant Pool Tests**
   - Test CONSTANT_Dynamic with simple bootstrap methods
   - Test module-related constants (if module support added)

3. **Attribute Tests**
   - Create classes with NestHost/NestMembers, verify reflection works
   - Create record classes, verify `isRecord()` and `getRecordComponents()`
   - Create sealed classes, verify `isSealed()` and `getPermittedSubclasses()`

4. **Runtime Library Tests**
   - Test string concatenation from JDK 9+ compiled classes
   - Test new String methods (`isBlank`, `lines`, etc.)

### Compatibility Matrix

Test with classes compiled by:
- JDK 8 (baseline)
- JDK 11 (first LTS with most changes)
- JDK 17 (LTS with records, sealed)
- JDK 21 (LTS)
- JDK 25 (target)

---

## References

### JVM Specifications
- [JVM Specification (Java SE 25)](https://docs.oracle.com/javase/specs/jvms/se25/html/index.html)
- [Class File Format](https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html)

### JEPs by Feature
- [JEP 181: Nest-Based Access Control (JDK 11)](https://openjdk.org/jeps/181)
- [JEP 309: Dynamic Class-File Constants (JDK 11)](https://openjdk.org/jeps/309)
- [JEP 280: Indify String Concatenation (JDK 9)](https://openjdk.org/jeps/280)
- [JEP 395: Records (JDK 16)](https://openjdk.org/jeps/395)
- [JEP 409: Sealed Classes (JDK 17)](https://openjdk.org/jeps/409)
- [JEP 261: Module System (JDK 9)](https://openjdk.org/jeps/261)

### JDK 25 Specific
- [JDK 25 Release Notes](https://www.oracle.com/java/technologies/javase/25-relnote-issues.html)
- [JDK 25 JEP List](https://openjdk.org/projects/jdk/25/)
- [Class File Versions Reference](https://javaalmanac.io/bytecode/versions/)

### OpenLDK Internals
- `TRANSPILER-ARCHITECTURE.md` - Compilation pipeline documentation
- `README.md` - Current status and limitations
