# Artifact evaluation for Submission 41 (SAS'21)

Docker image: https://drive.google.com/file/d/18JNcHZ62OsvqQeccU4C4H5Yfft4Y3n3h/view?usp=sharing.
- MD5 value: 32ff53f93b6eb5fe3a243a40ee8a3508.

## Setup using the docker image

Everything needed for artifact evaluation is under directory `/home/sas-artifact-41` inside the docker image, including:

- `/home/sas-artifact-41/brbo-impl`: the source code of brbo tool.
    - `src`: Source code of brbo.
    - `artifacts/sas21`: `table.xlsx` (which is used for generating Table 1 in the paper), and scripts for running the experiments.
- `/home/sas-artifact-41/icra`: the source code and binaries of ICRA, which is an invariant verification tool that is used by brbo.

### How to run the docker image

1. Download the docker image `sas-artifact-41.tar`.
2. Import the image via `docker load < sas-artifact-41.tar`.
3. Run the container via `docker run -it --privileged sas-artifact-41`.

## Validate the experimental results (Table 1 in the paper)

### Step 1: Run the experiments

1. Inside the docker, run `cd /home/sas-artifact-41/brbo-impl`.
2. Run `./artifacts/sas21/benchmarks/both_all.sh`.
3. Wait until the script finishes executing. This process typically takes 5+ hours.

### Step 2: Generate Table 1 in the paper

1. Inside the docker, run `cd /home/sas-artifact-41/brbo-impl/output`, which should contain 6 folders:
    - `DATE-TIME-allAmortize-60s-mostPrecise`: Results for `guava`, `lang3`, `stringutils` when verifying the most precise bounds.
    - `DATE-TIME-allAmortize-60s-mostPrecise`: Results for `stac` when verifying the most precise bounds.
    - `DATE-TIME-allAmortize-60s-mostPrecise`: Results for `generated` when verifying the most precise bounds.
    - `DATE-TIME-allAmortize-60s-lessPrecise`: Results for `guava`, `lang3`, `stringutils` when verifying constant-weakened bounds.
    - `DATE-TIME-allAmortize-60s-lessPrecise`: Results for `stac` when verifying constant-weakened bounds.
    - `DATE-TIME-allAmortize-60s-lessPrecise`: Results for `generated` when verifying constant-weakened bounds.
2. Open `table.xlsx` under `/home/sas-artifact-41/brbo-impl/artifacts/sas21`.
3. Collect results for configuration `Most Precise Bounds` in Table 1. Note that, it is required to open the following `*.csv` files under `/home/sas-artifact-41/brbo-impl/output` with Microsoft Excel.
    1. Copy all contents in `guava-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `A2` in Sheet `Most precise` of `table.xlsx`.
    2. Copy all contents in `guava-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `A13` in Sheet `Most precise`.
    3. Copy all contents in `lang3-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `J2` in Sheet `Most precise`.
    4. Copy all contents in `lang3-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `J13` in Sheet `Most precise`.
    5. Copy all contents in `stringutils-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `S2` in Sheet `Most precise`.
    6. Copy all contents in `stringutils-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `S13` in Sheet `Most precise`.
    7. Copy all contents in `stac-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `AB2` in Sheet `Most precise`.
    8. Copy all contents in `stac-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `AB13` in Sheet `Most precise`.
    9. Copy all contents in `synthetic-summary-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `AT2` in Sheet `Most precise`.
    10. Copy all contents in `synthetic-individual-DATE-TIME-allAmortize-60s-mostPrecise-000-099.csv` at cell `AT13` in Sheet `Most precise`.
    11. Copy all contents in `synthetic-summary-DATE-TIME-allAmortize-60s-mostPrecise-100-199.csv` at cell `BC2` in Sheet `Most precise`.
    12. Copy all contents in `synthetic-individual-DATE-TIME-allAmortize-60s-mostPrecise-100-199.csv` at cell `BC13` in Sheet `Most precise`.
4. Collect results for configuration `Constant-Weakened Bounds` in Table 1 in a similar way as the above step into Sheet `Less precise` in `table.xlsx`.
5. Now Sheet `Summary` in `table.xlsx` should have similar results as Table 1.
    - Note that, it is observed that the number of verified programs in `Summary` may slightly differ from that of Table 1 *when running experiments from different machines*, due to our time out setting of ICRA. Since we hardcoded the time out of running ICRA, machines with better (or poorer) performance may allow ICRA to infer more (or less) invariants within the fixed timeout, which may contribute to a higher (or lower) number of verified programs in Table 1.

## Extend the brbo tool

1. Inside the docker, run `cd /home/sas-artifact-41/brbo-impl`.
2. Make edits to the source code in any way that extends the tool.
3. Run `sbt package`.
4. Use brbo to verify an upper bound in some Java program(s) to see how the changes in the code affect the experimental results.
    - Example 1: You may run `./script/run_with_deps_artifact.sh --help` to see how to run brbo on `*.java` files.
    - Example 2: You may simply run the changed tool on the existing benchmarks.

### Run brbo on custom `*.java` files

We enforce syntactic restriction on the `*.java` files that brbo accepts for simpler implementation. Thus, brbo will throw exceptions (with hopefully useful information about which syntactic restriction is violated), if a `*.java` file is not acceptable.

See examples of acceptable `*.java` files under `/home/sas-artifact-41/brbo-impl/src/main/java/brbo/benchmarks` (and also see how to specify upper bounds).