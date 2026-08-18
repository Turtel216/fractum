# Task Board

A complete front end written in Fractum: a task board with filtering, keyed
reordering, a session clock, keyboard shortcuts, persisted preferences and an
HTTP request. There is no hand written JavaScript anywhere in it. The five
modules below compile to a single `out.js` that runs against
`runtime/fractum_runtime.js`.

Where `examples/counter` shows the smallest possible Elm-architecture program,
this one is meant to show what the language looks like when an application is
big enough to need structure.

## Build and run

```bash
fractumc examples/taskboard/Main.fr -o examples/taskboard/out.js
```

Then open `taskboard.html`. Serving the directory (`python3 -m http.server`)
also makes the **Load tips** button succeed; opening the file directly is fine
too, the request just fails and the failure is rendered as a failure.

## Modules

The compiler resolves the whole import graph itself and emits one file, so the
split below is purely for the benefit of the reader.

| File | Responsibility |
| :--- | :--- |
| `Util.fr` | Generic helpers: list to array bridging, clock and percentage formatting. Depends on nothing. |
| `Task.fr` | The domain. What a task is, and every pure operation over a board of them. Knows nothing about messages or the DOM. |
| `Model.fr` | The application. `Model`, `Msg`, `init` and `update`. Describes effects, never performs them. |
| `View.fr` | The presentation. `Model` in, `Html<Msg>` out, with no way to reach the document. |
| `Main.fr` | The wiring: subscriptions plus the call to `app`. |

The dependency edges run one way (`Main -> View -> Model -> Task -> Util`),
which is what keeps `Task.fr` testable on its own and stops the view from
sneaking state changes into a render.

## What it demonstrates

**Types and inference**

- Records and parametric type aliases (`Task`, `Model`, `Step`) with structural
  field access checked at compile time.
- Enums with and without payloads (`Priority`, `Filter`, `Notice`, `Sync`,
  `Msg`), including the 29 variant `Msg` enum that is the application's entire
  vocabulary of events.
- Exhaustive pattern matching: adding a variant to `Msg` turns every unhandled
  case in `update` into a compile error.
- `Option<T>` and `Result<T, E>` instead of `null` and exceptions, at every
  boundary with the outside world.
- Generic functions over `List<T>`, and the `Int` / `Float` split with explicit
  `as` conversions for percentages and truncating division.

**The Elm architecture**

- A pure `update` returning `{ model, cmd }`, with the model split into four
  small records so that rebuilding one of them stays a single line.
- `Cmd` effects: `Delay` for self dismissing notices, `RandomInt` for the random
  pick, `FocusElement` after adding a task, `StorageGet` / `StorageSet` for the
  theme, `ConsoleLog`, `HttpGet` for the tips panel, and `cmd_batch` to fire
  several at once.
- `Sub` subscriptions: a one second `Every` that exists only while the clock is
  running, a global `OnKeyPress` for shortcuts, and a curried `OnWindowResize`.
- Every event the runtime sanitises: `OnClick`, `OnInput`, `OnSubmit`,
  `OnChange`, `OnKeyDown`, `OnFocus`, `OnBlur`, `OnMouseEnter`, `OnMouseLeave`.
  A handler never sees a raw DOM event, only a `String` or an `Int`.
- A keyed list (`Html::Keyed`), so that moving a row with the Up and Down
  buttons moves the existing DOM node instead of rebuilding the row.
- Conditional rendering with `Html::Empty`, and an inline `Style` attribute
  driven by a `Float` for the progress bar.

## Notes on working within the language today

Three things in the source are shaped by current limitations rather than by
taste, and are worth knowing about before copying the style:

- **Arrays have no length, lists do.** The model stores tasks as `List<T>` from
  `stdlib/Functional`, because arrays only support literals, indexing and index
  assignment. `list_to_array` in `Util.fr` is the one bridge between the two,
  and it exists because the `Html` builders take their children as arrays.
- **Records have no update syntax.** Every change rebuilds the record, which is
  why `Model` is four small records rather than one wide one, and why
  `Model.fr` has a block of `with_*` helpers near the top.
- **Lambdas over records need an annotation.** Object types are not row
  polymorphic yet, so `(t: Task) => t.done` is written with the annotation while
  `(code) => Msg::KeyPressed(code)` is not.

Two smaller ones: enum values compile to fresh objects, so `Filter` values are
compared through `filter_index` rather than with `==`; and string literals have
no escape sequences, so no message in the source contains a quotation mark.
