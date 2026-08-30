# Sensei (teacher in japanese)

Sensei is an online app made in order to make you learn japanese words and their usages

# How does it work?

You get a word in hiragana/katakana and then write it out in romaji to train your understanding of hiragana/katakana.

Then you convert the romaji back to hiragana


The last question is what does the word mean, one of the glossary words is enough to get it right

# Elm template

## Service worker for offline access

Since most of my Elm apps are client side only, we can also make them work offline by adding a service worker.
The service worker caches the resources used by the template and makes it so it can load even if offline.

## Pipeline

The included pipeline include three steps:

- A test step that runs all tests and reports if all tests succeeded
- A build step that compiles the code and optimizes/minimizes it
- A publishing step that publishes it to Github pages

## Keeping a template created repo up to date

```sh

git remote add upstream https://github.com/Qluxzz/elm-template.git
git fetch upstream
git config rerere.enabled true    # remembers conflict resolutions between merge

# First merge needs --allow-unrelated-histories to allow merging the two repos together
git merge upstream

```
