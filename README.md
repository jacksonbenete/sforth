# sforth
Forth implementation in Scala language.

This project has currently no license file,
but you can contribute, use, extend and reproduce the source code
as long as you refer to the original author or GitHub repo
in your own README and/or license file and
as long as you know that this code has no warranties
and that you agree that the author or any other contributors
have no responsibility for any damages or misuse of the Software
and ideas contained.

## Timeline

- TSForth v0.1
- SForth v0.1

## TSForth
TSForth was the first implementation attempt.

It's an over-engineered implementation trying to keep track of types on
the stack with a lot of type signatures that makes the code quite painful,
confuse and doesn't quite deliver the type-safety in the current version,
as it requires a lot of defensive programming that doesn't quite help.

## SForth

SForth is both a refactoring and a new implementation.

Written from scratch, but with lessons learned from TSForth v0.1.

The initial idea is to have to write less type parameters all the type which
made everything quite troublesome in TSForth without really helping
on safety.

## Ideas

- Use automate Scala tests to rerun tests all the time
- Implement test cases to cover regressions
- Implement "Starting Forth" exercises as additional test cases

