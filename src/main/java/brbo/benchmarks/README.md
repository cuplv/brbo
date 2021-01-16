# Benchmarks

## Specifications on benchmarks

1. We use comments at the end of each method to express the expected experiment results.

    - For example, `No; Yes; No` means expecting no amortization to not work, selective amortization to work, and full amortization to not work. 
2. 

## Syntactic restriction on benchmarks

Input numeric programs conform to the following grammar:

```
s ::= s;...;s | while(e) s | if(e) s else s | do s while(e) | for(s; e; s) s | label: s
    | continue | break | assert(e) | return e | T x | R = R + e
    | assume(e) | asssert(e) | ndInt() | ndInt(e, e) | ndBool()
    | T x = e | x = e
e ::= x | R | e ⋈ e | -e | !e
⋈ ::= + | - | * | <= | < | == | != | > | >=
T ::= Int | Bool
```