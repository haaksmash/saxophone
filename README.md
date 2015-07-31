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
libraryDependencies += "com.haaksmash" %% "saxophone" % "1.4.0"
```
to one of your project's sbt files.

### Usage
Pretty straightforward; choose the appropriate implementation of `BaseIntake` and `BaseTranslator`, specify a source string, and let the `Pipeline` `process` everything for you!

```scala
val output:Try[String] = saxophone.Pipeline
  .from(new FileIntake)
  // this string will have different meanings depending on your chosen intake;
  // in this case, it will be interpreted as a file path.
  .on("filename")
  .via(new HTMLTranslator)
  .to(new FileEmitter("output file")) // optional!
  .process()
```
It doesn't matter what order you call `on`/`via`/`to`/`from`; they each return a brand-new `Pipeline`. `Pipeline` even tries to choose sensible defaults for you if you process a bit more eagerly:

```scala
saxophone.Pipeline
  // on() is the only required builder method
  .on("source")
  .process()


// equivalent to the more-verbose
saxophone.Pipeline
  .on("source")
  .from(new StringIntake)
  .via(new HTMLTranslator)
  .process()
```
### Syntax
`saxophone` is a lot like Markdown, structurally; the syntax is what's different. All these examples use the HTML output, because that's pretty easy to understand.

#### Blocks
* code listings are started with `{{{` and ended with `}}}`. The initial `{{{` can optionally include some directives that may have meaning to the translators: `{{{lang:saxophone`
* lists are either unordered (preceded by `*`) or ordered (preceded by a number+period, e.g. `1.`). You may also have an (un)ordered list by leading with a `-`.
* embedded elements (images, videos) have a slightly different syntax from the other blocks; their format is `::<type> <arguments for the type>::`. For an image, this looks like `::image /a/source/image.jpg`. For a video, it's `::video <arguments> <for> <video>::`, which will intelligently render if the first argument is `youtube` or `vimeo`, otherwise it will emit a `<video>` element, treating the rest of the arguments as different `<source>` elements.
* blockquotes are introduced with `> ` and may optionally have a source after them, which should be in brackets.
* headers are any line preceded by a number of `#`s, up to 6 of them.

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

