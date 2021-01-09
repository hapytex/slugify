# slugify
[![Build Status of the package by Travis](https://travis-ci.com/hapytex/slugify.svg?branch=master)](https://travis-ci.com/hapytex/slugify)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/slugify/badge)](https://matrix.hackage.haskell.org/#/package/slugify)
[![Hackage version badge](https://img.shields.io/hackage/v/slugify.svg)](https://hackage.haskell.org/package/slugify)

Make slugs in Haskell. This package is based on the
[**`slugify`** function of Django [Django-doc]](https://docs.djangoproject.com/en/3.1/ref/utils/#django.utils.text.slugify).

## Usage

One can make use of `slugify`, `slugifyUnicode` and `slugifyWith` from
the `Text.Slugify` module to convert a `Text` object to an equivalent
`slug`, for example:

```
*Text.Slugify Text.Slugify> slugify "Haskell 98 Language and Libraries"
"haskell-98-language-and-libraries"
```

## `slugify` is not *safe* Haskell

`slugify` depends on [**`unicode-transforms`**](https://hackage.haskell.org/package/unicode-transforms), which is not safe Haskell.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/slugify).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).
