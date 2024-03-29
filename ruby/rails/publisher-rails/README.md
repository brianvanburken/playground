# Publisher
[![Code Climate](https://codeclimate.com/github/brianvanburken/publisher/badges/gpa.svg)](https://codeclimate.com/github/brianvanburken/publisher)
[![Build Status](https://travis-ci.org/brianvanburken/publisher-rails.svg?branch=master)](https://travis-ci.org/brianvanburken/publisher-rails)
[![Test Coverage](https://codeclimate.com/github/brianvanburken/publisher/badges/coverage.svg)](https://codeclimate.com/github/brianvanburken/publisher/coverage)

Small Ruby on Rails application to showcase my skills. The focus of this application
is on the server side. No styling has been applied to the HTML. See the Highlights
section of this README for important points in this application.

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

## Highlights
### STI (Single Table Inheritance)
[Poem](app/models/poem.rb) and [Short Story](app/models/short_story.rb)
both inherit from [Literature](app/models/literature.rb)
. This is done for the reason that Poem and Short
Story both share common fields like title, content and published date.

### Polymorphic association
The assignment states that authors, poems and short stories must be tagged. To
achieve a more generic solution I've used a polymorphic association. The model
that uses this technique is [Tagging](app/models/tagging.rb).
To enable tagging on other models a simple include of the module [Taggable](app/models/concerns/taggable.rb)
is all that is needed.

### Service objects
I've extracted the tagging of models into a service object called [TaggingService](app/services/tagging_service.rb).
This way duplicated code is prevented and the logic of tagging is moved away from the models.
