# ldfi-akka

Lineage Driven Fault Injection (LDFI) is a fault injection technique for finding bugs in distributed systems. Its current state-of-the-art implementation, [Molly](https://github.com/palvaro/molly), performs LDFI analysis on Dedalus programs. Ldfi-akka uses the LDFI approach to find bugs in distributed systems written in Akka.

This project is still a **heavy** work in progress and will be subjected to big changes.

## How it works?

The objective is to be able to perform LDFI analysis on Akka programs. More concretely, given an Akka program, run it and extract the lineage from the execution traces, perform LDFI and determine whether such a program is fault tolerant or else provide the fault injections that proves otherwise.

In Akka, all of the communication between the actors is done by explicit message passing: an actor can therefore not be influenced (e.g, mutate its internal state or change its behaviour) by a different mean. Thus, the behavior of a given Akka program can be deduced by analyzing the messages sent within the actor system after it has finished executing. Therefore, in order to successfully perform LDFI on a given Akka program, the following sub problems need to be resolved: 
 * Log the execution traces of an Akka program and extract the data lineage from them. 
 * Control the run-time execution of an Akka program. 
 * Given an arbitrary Akka program, find a way to rewrite it so that it is possible to perform LDFI.

Ldfi-akka solves the first sub problem by using a SLF4J to log the execution of an Akka program, and parses the logs to make inferences about the behaviour exhibited in the logs. To solve the second sub-problem, ldfi-akka employs an external controller to interact with the Akka program. The third problem is resolved by using [ScalaFix](https://github.com/scalacenter/scalafix), a rewrite tool for Scala programs, to rewrite Akka programs such that they can interact with such controller, in addition to adding logging.
 
## Simple Deliv

The current implementation can use the LDFI approach on an Akka "equivalent" Dedalus [simple-deliv](https://github.com/palvaro/molly/blob/master/src/test/resources/examples_ft/delivery/simplog.ded). Naturally, as a result of Scala as a language, and therefore Akka by extension, being fundamentally different from Dedalus: the discrepancy in the number of ways to write a given program is large. Therefore, it is unclear whether this would work if simple-deliv was written in a different and unbiased way. With that being said, "ldfi-akka" will encode simple-deliv in a boolean formula, pass it to a SAT-solver and find the solutions that would violate its correctness specification.

To execute the Simple Deliv example so that Ldfi-akka finds counterexamples to simple-deliv:

    git clone https://github.com/KTH/ldfi-akka/
    sbt run

## Running the tests
To run the tests, run `"sbt tests"`.

## Contributing

### License
This project uses the MIT license.

### Issues

This project is still under work in progress and has many known [issues](https://github.com/KTH/ldfi-akka/issues). With that being said, all kinds of issues or pull requests are more than welcome!

## Authors

* Yonas Ghidei

## Acknowledgements

* Inspired by https://github.com/palvaro/molly
