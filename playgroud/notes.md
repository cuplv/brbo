# Run program analysis or model checking tools

## Run tools outside docker

### On Windows

#### Run CPAChecker

```
PS C:\Users\tianh\Desktop\test> docker run -v ${pwd}:/workdir -u $UID':'$GID registry.gitlab.com/sosy-lab/software/cpachecker -default -preprocess -timelimit 60 -64 test.c
```

## Run tools within docker

### Load image (from Windows)

```
winpty docker run -it -v //c/Users/tianh/:/home/tianh --privileged debian-java-11
```

### Run UAutomizer

#### Install Java 11

To let `Ultimate.py` find Java 11 (installed by SDK MAN), add line `/root/.sdkman/candidates/java/current/bin/java` to `Ultimate.py` at line 225 in function `get_java`.

```
root@b55c3f6c2cfb:/home/tianh/Desktop/UAutomizer-linux#  ./Ultimate.py --spec ../test/unreach-call.prp --architecture 64bit --file ../test/test.c
root@b55c3f6c2cfb:/home/tianh/Desktop/UAutomizer-linux#  ./Ultimate.py --spec ../test/unreach-call.prp --architecture 64bit --file /home/tianh/Documents/workspace/brbo-impl/playgroud/examples/test.c
```

### Run VeriAbs

```
root@b55c3f6c2cfb:/home/tianh/Desktop/VeriAbs# apt-get install libgetopt-complete-perl
root@b55c3f6c2cfb:/home/tianh/Desktop/VeriAbs# ./scripts/veriabs --property-file ../test/unreach-call.prp ../test/test.c
```