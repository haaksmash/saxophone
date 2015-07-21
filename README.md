[![Build Status](https://travis-ci.org/haaksmash/saxophone.svg?branch=master)](https://travis-ci.org/haaksmash/saxophone)

# `saxophone`
Ugh, what? ANOTHER preprocessor language? YES.

Deal with it.

## Opinions!
This is `saxophone`, a markup language (ML) with opinions.

### The same symbol should not mean different things.
In some MLs, `*` has a lot of jobs. `*word*` is generally rendered as emphasized text (*thus*) while `**word**` is rendered as strong text (**thus**).

Not so in `saxophone`! use `/` to denote text that should be read with *emphasis*, while `*` is back to its natural role of denoting text that should be read **with strength**.

What's that? `*` also denotes a line item in a list without any particular ordering? Well, that's true in `saxophone` as well. Just like a real person, `saxophone` is a bit of a hypocrite.

### Not everything is meant to be nested.
`saxophone` will NOT recursively parse a number of elements. For example, **/emphasis/**  should probably only render as `<strong>/emphasis/</strong>` --- the number of times you actually need to combine them is greatly outweighed by the number of times you *think* you should combine them but really shouldn't because it's tacky.

### Semantic tags should be used where possible.
This is a more narrow opinion, specifically targeting the HTML result of processing an ML. In short: do not output presentational markup.

I believe that most ML processors have switched from outputting `<i>` in favor of `<em>` (etc), but those that support "underlining" mostly spit out `<u>`. `saxophone` spits out `<mark>`.

### Glyphs should resemble the semantic they're trying to achieve.
None of this `_` means `<em>` (which is generally rendered in *italics*); everyone who reads `the _quick_ brown fox` is thinking in the back of their mind that `quick` is supposed to be underlined --- or the equivalent.

In `saxophone`, form follows function. The slant of `/` resembles the classic italic lean; the `_` reminds us of the highlighted passages of our youth. Even the `*` looks thicker than its brethren, a gentle reminder that this text will be strong.

## I AGREE WITH YOU
Awesome! You must want to use `saxophone` (**RIGHT?**)!

Here's how!

### SBT
add

```scala
libraryDependencies += "com.haaksmash" %% "saxophone" % "1.3.0"
```
to one of your project's sbt files.

### Usage
Pretty straightforward, choose the appropriate implementation of `BaseIntake` and send its output to the appropriate implementation of `BaseTranslator`, which will give you a String. Then, do what you want!

```scala
val input: Option[Document] = FileIntake(someFilename)
val output: Option[String] = input map {
  HTMLTranslator.translate(_)
}
```
### Syntax
`saxophone` is a lot like Markdown, structurally; the syntax is what's different. All these examples use the HTML output, because that's pretty easy to understand.

#### Blocks
* headers are any line preceded by a number of `#`s, up to 6 of them.
* code listings are started with `{{{` and ended with `}}}`. The initial `{{{` can optionally include some directives that may have meaning to the translators: `{{{lang:saxophone`
* lists are either unordered (preceded by `*`) or ordered (preceded by a number+period, e.g. `1.`). You may also have an (un)ordered list by leading with a `-`.
* blockquotes are introduced with `>>>`, and may optionally have a source after them, which should be in brackets.

Everything else is a paragraph; separate paragraphs with a newline and you'll come out just fine.

#### Inline styles
Within a block:

* `/words/` becomes `<em>words</em>`
* `{words}` will create a footnote
* `[link text](link source)` will create a hyperlink
* `*words*` becomes `<strong>words</strong>`
* `_words_` becomes `<mark>words</mark>`
* ``words`` becomes `<code>words</code>`
* `~words~` becomes `<s>words</s>`

