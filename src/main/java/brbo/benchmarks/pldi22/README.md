# How to play with benchmarks

## Setup the environment

Run the docker:
```
docker run -it -v ~/Documents:/home/Documents --privileged sas-artifact-41 
```

Run ICRA:
```
./icra -cra-split-loops -cra-prsd /home/Documents/workspace/brbo-impl/src/main/java/brbo/benchmarks/pldi22/
```