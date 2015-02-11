Contribution
------------
The *Ichi-go project* is developed with a
[Scrum](http://en.wikipedia.org/wiki/Scrum_%28software_development%29)
software engineering methodology. Here are our development guidelines,
outlining how we write code, contribute to the project, and the tools
we use. This document exists for the benefit of the developers.

# Process

All have write access to this repository, but we should refrain from
committing to master. We will branch for every
feature/bug-ticket, work on that branch while pulling from upstream,
and then merge in (maybe even with a pull request).

The ticket tracker will be used to track our bugs and needed
features. Milestones will represent Sprints. Pre-sprint meetings will
involve the placement of tickets.

Remember, the code comes first! If there appears to be to much of "a
process", then we can water things down!

## Issue Tracker

All tickets will be in the ichi-go-project-tracker repository. This
makes our [ZenHub](zenhub.io) work-flow much nicer.

## Sprints
| Sprint   | Dates      |
|----------|------------|
| Sprint 1 | 2/8 - 2/22 |
| Sprint 2 | 2/22 - 3/8 |
| Sprint 3 | 3/8 - 3/22 |
| Sprint 4 | 3/22 - 4/5 |
| Sprint 5 | 4/5 - 4/19 |
| Sprint 6 | 4/19 - 5/3 |

## Branch Based Workflow
In order to add code to the project there must be a ticket documenting
the task at hand. For example, a ticket may say

> 13: "File issue" button needs to actually send information to
> github.

This ticket would be in the current sprint, under the "To-Do" column
on the [ZenHub](https://www.zenhub.io/) board. To start work, make a
new branch called `13-a-short-descript` (note the number corresponds
to the issue number). Then move the ticket to the "In Progress"
column.

When work is done, submit a pull request and move the ticket to the
"Needs Review" column. Another developer will read through the pull
request, check the build status, and make sure that any added code is
tested appropriately.

##Comit Messages
Commit messages should all have the following form:
```TAG: A small explanation of what happened (under 80 char).```
If more content is necessary, it can be added to lines bellow. `TAG` can
be any one of the following tags:
* `ADD`: Added to the API.
* `API`: Changed the existing API (other than adding).
* `CHANGE`: Changed source code (not API)
* `DOC` : Documented code or API.
* `CLEAN`: Cleaned source code.
* `FIX`: Fixed a bug.
* `TEST`: Added unit tests, or changed existing tests.
* `MOVE`: Moved files or code.
* `PROJ`: Commits pertaining to project structure (cabal/maven/ect...)

This does not apply to merge commits. Be sure to use
[closing messages](https://help.github.com/articles/closing-issues-via-commit-messages/).

## Test Driven Development

* Java code will be unit tested with the
[JUnit framework](http://junit.org/).
* Haskell code will be unit tests with the
[HUnit framework](http://hackage.haskell.org/package/HUnit).
* Continuous Integration will be done on
[Travis-Ci](https://travis-ci.org)

We will strive for high test coverage.

# Code

## Style

Coding style is vary important. For Java code, we will use the
[Google Java Style Guide](https://google-styleguide.googlecode.com/svn/trunk/javaguide.html).
Some highlights are:

| Thing        | Standard |
|--------------|----------|
| Indentation  | 2 space  |
| Curly braces | In line (one true brace style) |
| Line length  | 80 characters |

Google provides settings files for most major Java IDEs, as well as
emacs.

For haskell code, see the
[haskell programming guidelines](https://wiki.haskell.org/Programming_guidelines).
For those who are unfamiliar with haskell I would suggest reading
[Real World Haskell](http://book.realworldhaskell.org/).

## File Structure

To be discussed.

# Tools

| Task       | Tool             |
|------------|------------------|
| Haskell Project | cabal       |
| Build Server    | Travis-CI   |
| Java IDE | IntelliJ / Eclipse |
| Java Project | maven / ant    |
| SCM | GitHub                  |
| Issue Management | GitHub     |
| Scrum Board | [ZenHub] (https://www.zenhub.io/) |
| Project Conversation | [Slack] (https://ichi-go.slack.com/messages)|

Do remember that the Slack Chat will be used heavily to maintain
constant team communication. Be sure to check the chat often, and
consider using the mobile app.

# Schedule

A
[Google Calendar](https://www.google.com/calendar/embed?src=8542vsvk5t04na3e0gf13nmam4%40group.calendar.google.com&ctz=America/Denver)
exists to keep track of any meetings/deadlines we have planned.

In general we will meet *every other Sunday at 10:30* and *every
other Thursday at 5:00*. Information will also be found on the Slack
channel.
