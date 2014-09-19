CONTRIBUTING
============

This documents serves as guide on contributing source code and documentation to
the eDFS project.

## Contribution guidelines

### Pull requests are always welcome

Pull requests are always welcome. eDFS strives to be a simple but durable file
system, but we are always open to new ideas especially improvements to the codebase
and documentation. Please feel free to fork this project to implement your
feature or fix. Remember to do this under a new branch that is a descriptive of your changes.
When you are done, make the pull request.

Pull requests descriptions should be as clear as possible and the relevant
documentation and a passing test suite.

### Create issues...

Any significant improvement should be documented as [a github
issue](https://github.com/edfs/eDFS/issues) before anybody starts
working on it.

### ...but check for existing issues first!

Before filling your issue, please take a moment to check that an issue doesn't already exist.
If your issue has already being filed, you can "+1" it or add a comment.
This will help prioritize the most common problems and requests.

### Conventions

Fork the repo and make changes on your fork in a feature branch:

- If it's a bugfix branch, name it XXX-something where XXX is the number
  of the issue
- If it's a feature branch, create an enhancement issue to announce your
  intentions, and name it XXX-something where XXX is the number of the
issue.

Write clean code.

Remember to always include tests with your code. Run the full
test suite on your branch before submitting a pull request.

Commits that fix or close an issue should include a reference like
`Closes #XXX` or `Fixes #XXX`, which will automatically close the issue
when merged.

Add your name to the AUTHORS file, but make sure the list is sorted and
your name and email address match your git configuration.

## Decision process

The decision making process usually starts with a pull request or a new issue.
As an Open Source project, eDFS strives to be open so all decisions are held in
the open either on the WiKi, issues or pull requests. The people ultimately
responsible for making the final call are the core developers.
