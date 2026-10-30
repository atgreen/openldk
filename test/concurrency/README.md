# Concurrency checks

`make check-concurrency` compares the scoped-value and structured-concurrency
probes with JDK 25.

`make check-class-publication` runs an SBCL-level regression against the
current Lisp sources. It requires JDK 25 to compile temporary Java fixtures.
The test pauses a writer both before mirror creation and after mirror
publication, then starts readers through the class-loading, metadata,
initiating-loader, hierarchy, and Java-mirror lookup paths. Readers must wait
for preparation; the writer must still resolve its own provisional class.
Injected failures must wake every reader, retract the definition, and permit
a subsequent successful load, including when closing the class-file stream
also throws. Temporary fixtures are removed automatically.
