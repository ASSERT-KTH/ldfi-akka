# ldfi-akka

Lineage Driven Fault Injection (LDFI) is a fault injection technique for finding bugs in distributed systems. Its current state-of-the-art implementation, [Molly](https://github.com/palvaro/molly), perform its analysis on Dedalus programs. Ldfi-akka uses the LDFI approach to find bugs in distributed systems written in Akka.  

## Getting Started
To use ldfi-akka, simply clone the repo and run `"sbt run"`. Ldfi-akka will find counterexamples to a simple best effort broadcast protocol written in Akka.

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
