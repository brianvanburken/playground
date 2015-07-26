# Publisher
[![Code Climate](https://codeclimate.com/github/brianvanburken/publisher/badges/gpa.svg)](https://codeclimate.com/github/brianvanburken/publisher)
[![Build Status](https://travis-ci.org/brianvanburken/publisher.svg?branch=master)](https://travis-ci.org/brianvanburken/publisher)
[![Test Coverage](https://codeclimate.com/github/brianvanburken/publisher/badges/coverage.svg)](https://codeclimate.com/github/brianvanburken/publisher/coverage)

## Assignment
Please write an application that enables storing poems and short stories (with their authors) and will automatically tag the content. Each story has a title, content, short description, publish date and an author. A poem has the same, but instead of short description it has a metaphor. Each author has a name and a biography. The content of poems, stories and biographies should be automatically tagged if it contains some of the predefined keywords (you can define 20 keywords yourself and store them however you wish). Tags can be shared between the entities.

In short:
- Create basic models Poem, Short Story, Author and Tag.
- Create tagging method for poems, short stories and author's biography.
- Create the show view for poem and short story that has the title, content, author name and tags.
- View for poem contains methaphore.
- View for short story contains short description.

## Setup
Run the following commands in order:
```bash
rake db:create
rake db:migrate
rake db:seed
rails s
```
After this visit ```http://localhost:3000/```.
