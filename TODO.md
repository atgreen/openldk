# OpenLDK Development TODO

## Immediate Priorities

### Test Suite Fixes
- [ ] Investigate and fix 11 unexpected test failures
  - Run `make check` and analyze failure output
  - Compare against `testsuite/expected-failures.txt`
  - Fix regressions in core functionality

### Code Quality
- [ ] Address undefined function warnings from build
  - `|<init>()|` - called before definition
  - `|java/lang/System.initializeSystemClass()|` - undefined
  - `|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)|` - undefined
- [ ] Fix undefined variable warning: `|+static-java/lang/System+|`
- [ ] Resolve style warning: `&OPTIONAL` and `&KEY` in same lambda list (main function)

## Core Features (Missing/Incomplete)

### High Priority
- [ ] Implement dynamic method invocation (invokedynamic)
  - Required for Java 7+ features
  - Currently blocking some modern Java libraries
- [ ] Add bytecode verification
  - Security and correctness requirement
  - Currently all bytecode is trusted
- [ ] Improve exception handling
  - Ensure all Java exceptions properly map to CL conditions
  - Test exception propagation across Java/Lisp boundary

### Medium Priority
- [ ] Expand native method implementations
  - Review `src/native.lisp` for stubs
  - Implement commonly-used native methods
- [ ] Improve reflection support
  - Test with complex reflection use cases
  - Ensure all reflection APIs work correctly
- [ ] Enhanced classpath handling
  - Better error messages for missing classes
  - Support for more classpath formats

### Low Priority
- [ ] Performance optimizations
  - Profile hot paths in JIT compilation
  - Optimize frequently-used native methods
  - Cache compiled methods more aggressively
- [ ] Support for Java versions beyond 8
  - Update bytecode parser for newer class file formats
  - Implement new bytecode instructions

## Documentation
- [ ] Add examples of using Java libraries from Common Lisp
- [ ] Document the compilation process (bytecode → IR → Lisp)
- [ ] Create troubleshooting guide for common errors
- [ ] Add API documentation for embedding OpenLDK

## Testing
- [ ] Reduce expected failures count (currently 1,712)
  - Categorize by feature area
  - Prioritize based on real-world usage
- [ ] Add tests for new features
- [ ] Create integration tests for Java library usage

## Known Limitations to Address
- [ ] Support for class files beyond Java 8
- [ ] Thread safety improvements
- [ ] Better memory management for Java objects
- [ ] Support for non-Linux platforms (macOS, Windows)
- [ ] Support for non-SBCL implementations

## Infrastructure
- [ ] Set up continuous integration
- [ ] Automate test suite runs
- [ ] Create release process
- [ ] Package for common Lisp package managers

---

## Completed
- [x] Fix linting issues throughout codebase
- [x] Add SPDX license identifiers
- [x] Remove unnecessary package prefixes
- [x] Enhance AGENTS.md with development guide
- [x] Add graceful CLI error handling
