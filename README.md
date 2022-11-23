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
- SForth v0.2

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

### Sforth v0.2
This version started being implemented right after I started reading "Starting Forth".

v0.1 had a State designed this way:
```scala
State(dictionary: Dictionary,
                   stack: List[DataItem],
                   namespace: Map[String, Dictionary],
                   mark: String, // mark current namespace
                   input: List[String],
                   io: IO,
                   status: Status)
```

Were the Stack was a List of "DataItem", and DataItem is the data structure following:
```scala
object Data {
  trait DataType
  object Number extends DataType
  object Literal extends DataType
  object Word extends DataType
  object Empty extends DataType
  object Invalid extends DataType

  case class DataItem(dataType: DataType, item: Any)
  object DataItem {
    def empty(): DataItem = DataItem(Empty, None)
  }
}
```

I had this first idea of a Stack were we could persist data of different types.

This is an over-engineering and makes things difficult without really helping with anything.

In Forth the Stack represents only a LIFO structure of Numbers. Variables, Strings and other Data Types aren't included on the Stack.


## Ideas

- Use automate Scala tests to rerun tests all the time
- Implement test cases to cover regressions
- Implement "Starting Forth" exercises as additional test cases

