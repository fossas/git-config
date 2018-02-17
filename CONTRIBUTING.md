# Contributing

Pull requests are welcome from anyone in the community!

Contributions come in many forms, from bug reports to new features.

## Reporting Bugs

Before filing an [issue][is] make sure you're concern hasn't already been
reported.

Otherwise, report a bug via the [issues][is] page with the following
information:

  - Version of the library you are using
  - The behavior you saw in the library
  - The behavior you _expected_ to see in the library
  - Tag the issue as a `bug`

## Requesting Features

Before filing a [feature request][is] make sure you're request hasn't already
been reported!

Otherwise, report your feature request on the [issues][is] page with the
following information:

  - Brief description of feature request
  - Concerte use case for requesting the feature
  - Tag the issue as `enhancement`

## Contributing a Fix or New Feature

Fork, then clone the repo:

    git clone git@github.com:your-username/git-config.git

Perform an initial project build and check the tests:

    stack test

Make your change. Add tests for your change. Make the tests pass:

    stack test

Make sure you've complied with `stylish-haskell` and `hlint` checks:

    make stylish_haskell_check
    make hlint

Push to your fork and [submit a pull request].

At this point you're waiting on us. We like to at least comment on pull requests
within three business days (and, typically, one business day). We may suggest
some changes or improvements or alternatives.

Some things that will increase the chance that your pull request is accepted:

* Write tests.
* Make sure code complies with `stylish-haskell` and `hlint` checks
* Write a [good commit message][commit].

[style]: https://github.com/thoughtbot/guides/tree/master/style
[commit]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[submit a pull request]: https://github.com/dogonthehorizon/git-config/compare/
[is]: https://github.com/dogonthehorizon/git-config/issues
