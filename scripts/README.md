# Run brbo

1. Run `sbt package`.
2. Run `./scripts/run_with_deps.sh --help` to see how to run brbo on `*.java` files.

## Useful commands

- Regex to search for YNN: `Y,\d*.*,N,\d*.*,N`
- Why was a process killed? `dmesg -T| grep -E -i -B100 'killed process'` ([Link](https://stackoverflow.com/questions/726690/what-killed-my-process-and-why))