test 1
caught java.lang.ArithmeticException: / by zero
java.lang.ArithmeticException: / by zero
	at Test.testHardwareException(Test.java:23)
	at Test.trouble(Test.java:53)
	at Test.main(Test.java:66)
test 2
caught java.lang.NumberFormatException: abc
java.lang.NumberFormatException: abc
	at java.lang.Float.valueOf(Float.java:125)
	at Test.testSoftwareException(Test.java:31)
	at Test.trouble(Test.java:54)
	at Test.main(Test.java:66)
test 3
caught java.io.IOException
java.io.IOException
	at Test.testUserException(Test.java:39)
	at Test.trouble(Test.java:55)
	at Test.main(Test.java:66)
test 4
caught java.lang.NumberFormatException: abc
java.lang.NumberFormatException: abc
	at java.lang.Float.valueOf(Float.java:125)
	at Test.testSoftwareException(Test.java:31)
	at Test.<init>(Test.java:11)
	at Test.testRethrownException(Test.java:46)
	at Test.trouble(Test.java:56)
	at Test.main(Test.java:66)
