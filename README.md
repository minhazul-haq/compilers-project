# compilers-project
Design and Construction of Compilers (CSE 5317)

Project#1: Scanner  
Project#2: Parsing  
Project#3: Abstract Syntax  
Project#4: Type Checking  
Project#5: Code Generation  
Project#6: Instruction Selection  


To install the project on your PC, do the following**:
1. Install Java SE 8 JDK.
2. Setup JAVA_HOME variable. On Microsoft Windows 8/10, click Start, then Control Panel, then System, then Advanced, then Environment Variables. Insert a new System variable JAVA_HOME to be C:\Java\jdk1.8.0_111 (change the version to match).
3. Install Apache maven from https://maven.apache.org/install.html.
4. Install Scala 2.11.* from http://www.scala-lang.org/download/ (do not install Scala 2.12.*).
5. Change your PATH. On Microsoft Windows 8/10, click Start, then Control Panel, then System, then Advanced, then Environment Variables. Edit the System variable Path and insert C:\Java\jdk1.8.0_111\bin, C:\apache-maven-3.3.9\bin, and C:\Program Files (x86)\scala\bin at the top (change the versions to match).
6. Download the project from GitHub.
7. Install the solution into your system: Open a Command Prompt, go to the downloaded pcat directory, and do:

    mvn install:install-file -Dfile=pcat-solution.jar -DgroupId=edu.uta.pcat -DartifactId=pcat-solution -Dversion=0.1 -Dpackaging=jar


To compile the pcat sources, go to the pcat directory, and do:

    mvn clean install

To run, for example, project #4 on the test file tests/hanoi.pcat, run the script:

    scala lib/pcat.jar 4 tests/hanoi.pcat

To run the solution for the same example:

    scala pcat-solution.jar 4 tests/hanoi.pcat


** Courtesy: https://lambda.uta.edu/cse5317/
