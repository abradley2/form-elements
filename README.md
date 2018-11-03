# Form Elements

To include in your project, add `FormFields.css` to your document.

To change variables in these styles, go into _styl/Variables.styl_, edit the appropriate
variables, and run `npm install && npm run build-styles` to get an updated `FormFields.css` file.

# TEA

Elm doesn't have a traditional concept of "components": stateful mutable bits in your UI
that change over time, like you have in libraries such as React. Instead Elm encourages reusable functions.

Every module here is some group of related helpers that allow you to represent a certain
form control in your application. Some form controls require a decent number of functions
to maintain a working element- like the "Super Select". Others like the simple "Checkbox"
just need a single `view` function.

**Important**  
The best way to figure out how to use these elements is probably just to check out
the demonstration source code.

## Config

If a form element's module exports a type called `Config` then it expects this to be passed
as an argument to the `view` function. If you are coming from React, than you can think of
`Config` as something very similar to props. The purpose of this type is pretty straight forward.
Any configuration parameters useful to the view are here. Like what the text input's "value" should be,
or what message the radio button group should send out to the Elm runtime when an option is selected.

## Model

In addition to a `Config`, a form element might export a type called `Model`. While the
`Config` object contains much of the dynamic data that the `view` function uses to display- there's
some data that is inconvenient to k.

You might wonder, why not extend this convenience to data in the `Config` as well? Why must I manually
update the text input's value when things like a text inputs hover state

## view

The view function returns type `Html Msg` or `Html a` depending on whether the element's module
also includes an `update` that handles specific types of messages unique to that element.

Views also take an associated `Config` record which has parameters that tell it to
display or certain way, or emit certain messages in response to an event.

The `RadioButtonGroup.view` for example, returns `Html a` where the `onSelect` field in the `Config` record
is type `b -> a` and the options field is a list of type `(String, b)`

```
RadioButton.view
  { options =
      [ ( "Senate", Senate )
      , ( "House", House )
      ]
  , onSelect = SelectChamber
  , selected = model.chamber
  }
```

The `TextInput.view` on the other hand returns `Html TextInput.Msg` as the views `Html`
emits certain `TextInput.Msg` events which the `TextInput.update` function needs to handle.

You will need to use `Html.map`

```
TextInput.view
    model.textInputData
    (textInputSettings model props)
    |> Html.map TextInputMsg
```
