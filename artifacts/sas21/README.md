# Artifact evaluation for Submission 41 (SAS'21)

Everything needed for artifact evaluation is under directory `/home/sas-artifact-41` inside the provided docker image.

Contents in the docker image:

- `/home/sas-artifact-41/brbo-impl`: the source code of brbo tool.
    - `src`: Source code of brbo.
    - `scripts`: Scripts for running experimental evaluation.
- `/home/sas-artifact-41/icra`: the source code and binaries of ICRA.

## Setup the stage for using the provided docker image

1. Download the docker image.
2. Import the image.
3. Run `docker run -it --privileged sas-artifact-41`.

## Validate the experimental results (Table 1)

### Step 1: Run the experiments

1. Inside the docker. Run `cd /home/sas-artifact-41/brbo-impl`.
2. Run `./scripts/benchmarks/both_all.sh`. The expected output is something like
    ```
    root@20189c0ba200:/home/sas-artifact-41/brbo-impl# ./scripts/benchmarks/both_all.sh
    + ./scripts/benchmarks/all.sh
    + ./scripts/benchmarks/string.sh
    ++ date +%m%d-%H%M
    + today=0505-2337
    + timeout=60
    + mostPrecise=mostPrecise
    + ./scripts/run.sh -d src/main/java/brbo/benchmarks/string -a all -t 60 --icra-path /home/sas-artifact-41/icra/icra
    + ./scripts/benchmarks/containers.sh
    ++ date +%m%d-%H%M
    + today=0506-0041
    + timeout=60
    + mostPrecise=mostPrecise
    + ./scripts/run.sh -d src/main/java/brbo/benchmarks/containers -a all -t 60 --icra-path /home/sas-artifact-41/icra/icra
    + ./scripts/benchmarks/synthetic.sh
    ++ date +%m%d-%H%M
    + today=0506-0046
    + timeout=60
    + mostPrecise=mostPrecise
    + ./scripts/run.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t 60 --icra-path /home/sas-artifact-41/icra/icra
    + ./scripts/benchmarks/less-precise/all.sh
    + ./scripts/benchmarks/less-precise/string.sh
    ++ date +%m%d-%H%M
    + today=0506-0259
    + timeout=60
    + lessPrecise=lessPrecise
    + ./scripts/run.sh -d src/main/java/brbo/benchmarks/string -a all -t 60 --less-precise --icra-path /home/sas-artifact-41/icra/icra
    + ./scripts/benchmarks/less-precise/containers.sh
    ++ date +%m%d-%H%M
    + today=0506-0415
    + timeout=60
    + lessPrecise=lessPrecise
    + ./scripts/run.sh -d src/main/java/brbo/benchmarks/containers -a all -t 60 --icra-path /home/sas-artifact-41/icra/icra --less-precise
    + ./scripts/benchmarks/less-precise/synthetic.sh
    ++ date +%m%d-%H%M
    + today=0506-0419
    + timeout=60
    + lessPrecise=lessPrecise
    + ./scripts/run.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t 60 --icra-path /home/sas-artifact-41/icra/icra --less-precise
    ```

### Step 2: Generate Table 1 in the paper

1. Run `cd /home/sas-artifact-41/brbo-impl/output`. There should be 6 folders:
    - `DATE-TIME-allAmortize-60s-mostPrecise`: Results for `guava`, `lang3`, `stringutils` when verifying the most precise bounds.
    - `DATE-TIME-allAmortize-60s-mostPrecise`: Results for `stac` when verifying the most precise bounds.
    - `DATE-TIME-allAmortize-60s-mostPrecise`: Results for `generated` when verifying the most precise bounds.
    - `DATE-TIME-allAmortize-60s-lessPrecise`: Results for `guava`, `lang3`, `stringutils` when verifying constant-weakened bounds.
    - `DATE-TIME-allAmortize-60s-lessPrecise`: Results for `stac` when verifying constant-weakened bounds.
    - `DATE-TIME-allAmortize-60s-lessPrecise`: Results for `generated` when verifying constant-weakened bounds.
2. Open `table.xlsx`.
3. Collect results for configuration `Most Precise Bounds` in Table 1.
    1. Copy all contents in `guava-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `A2` in Sheet `Most precise` of `table.xlsx`.
    2. Copy all contents in `guava-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `A13` in Sheet `Most precise`.
    3. Copy all contents in `lang3-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `J2` in Sheet `Most precise`.
    4. Copy all contents in `lang3-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `J13` in Sheet `Most precise`.
    5. Copy all contents in `stringutils-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `S2` in Sheet `Most precise`.
    6. Copy all contents in `stringutils-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `S13` in Sheet `Most precise`.
    7. Copy all contents in `stac-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `AB2` in Sheet `Most precise`.
    8. Copy all contents in `stac-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `AB13` in Sheet `Most precise`.
    9. Copy all contents in `synthetic-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `AT2` in Sheet `Most precise`.
    10. Copy all contents in `synthetic-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` into cell `AT13` in Sheet `Most precise`.
    11. Copy all contents in `synthetic-summary-DATE-TIME-allAmortize-60s-mostPrecise-100-199.csv` into cell `BC2` in Sheet `Most precise`.
    12. Copy all contents in `synthetic-individual-DATE-TIME-allAmortize-60s-mostPrecise-100-199.csv` into cell `BC13` in Sheet `Most precise`.
4. Collect results for configuration `Constant-Weakened Bounds` in Table 1 in a similar way as the above step into Sheet `Less precise` in `table.xlsx`.
5. Now Sheet `Summary` in `table.xlsx` should have similar results as Table 1.
    - Note that, the number of verified programs in `Summary` may differ from that of Table 1, depending on the performance of the machine that conducted the experiment and other unknown factors. Such numbers are highly related with if ICRA can successfully infer invariants, which is non-deterministic and depends on the machine performance.

## Extend the brbo tool

1. Inside the docker, run `cd /home/sas-artifact-41/brbo-impl`.
2. Make edits to the source code in any way that extends the tool.
3. Run `sbt package`.
4. Use brbo to verify an upper bound in some Java program(s) to see how the changes in the code (i.e., Step 2) affect the experimental results.
    - Example 1: You may run `./script/run_with_deps.sh --help` to see how to run brbo on a `*.java` file or all `*.java` files under a directory. Note that, for simpler implementation, we enforce syntactic restriction on the `*.java` files that brbo accepts. brbo will throw exceptions (with hopefully useful information about which syntactic restriction is violated), if a `*.java` is not acceptable. See examples of acceptable `*.java` files under `/home/sas-artifact-41/brbo-impl/src/main/java/brbo/benchmarks` (and also see how to specify upper bounds).
    - Example 2: You may simply run the changed tool on the benchmarks via `./scripts/benchmarks/both_all.sh`.
