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
winpty docker run -it -v //c/Users/tianh/Documents/:/home/Documents --privileged debian-java-11
```

### Run UAutomizer

```
root@b55c3f6c2cfb:/home/tianh/Desktop/UAutomizer-linux#  ./Ultimate.py --spec ../test/unreach-call.prp --file ../test/test.c --archit
ecture 64bit
```

### Run VeriAbs

```
root@b55c3f6c2cfb:/home/tianh/Desktop/VeriAbs# apt-get install libgetopt-complete-perl
root@b55c3f6c2cfb:/home/tianh/Desktop/VeriAbs# ./scripts/veriabs --property-file ../test/unreach-call.prp ../test/test.c
```