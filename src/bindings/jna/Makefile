#
#javac -classpath /usr/share/java/jna-3.2.7.jar:. Elektra/*.java HelloElektra.java
#
#ltrace -f java -classpath /usr/share/java/jna-3.2.7.jar:. HelloElektra
#javac -classpath /usr/share/java/jna-3.2.7.jar:. Elektra/*.java HelloElektra.java
#java -classpath /usr/share/java/jna-3.2.7.jar:. HelloElektra

HelloElektra:HelloElektra.java
	/opt/jdk1.8.0_25/bin/javac -classpath /usr/share/java/jna.jar:. elektra/plugin/*.java elektra/*.java HelloElektra.java
	/opt/jdk1.8.0_25/bin/java -classpath /usr/share/java/jna.jar:. HelloElektra

clean:
	rm *.class Elektra/*.class
