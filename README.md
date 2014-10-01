# CUFP Tutorial

Practice problems for the Elm tutorial at CUFP 2014.

## Setup

Clone this repo, navigate into `cufp-tutorial/todo`, compile
`Todo.elm`, and start Elm Reactor:

```bash
git clone https://github.com/evancz/cufp-tutorial.git
cd cufp-tutorial

cd todo
elm-get install
elm --make --only-js Todo.elm
cd ..

elm-reactor
```

Now you can edit `todo/Todo.elm` in your favorite editor and use
the time traveling debugger at [localhost:8000/todo/todo.html][debug].

[debug]: http://localhost:8000/todo/todo.html
