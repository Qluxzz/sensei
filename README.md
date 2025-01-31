# Elm template

Elm template with the most things I use when creating a new Elm thing

## Effects instead of Cmds

Using effects instead of the built in Cmd type allow you to test your program end to end, simulate Cmds like HTTP calls or getting system time.
Here's more documentation on the benefits: https://github.com/avh4/elm-program-test/blob/main/docs/cmds.md

This template also includes an initial test which tests the initial document structure

## Pipeline

The included pipeline include three steps:

- A test step that runs all tests and reports if all tests succeeded
- A build step that compiles the code and optimizes/minimizes it according to [Simon Lydell's findings](https://gist.github.com/lydell/b92ec8b6c7ae91945da10c814e565d5e)
- A publishing step that publishes it to Github pages
