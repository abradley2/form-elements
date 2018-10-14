# Elm Form Controls

To include in your project, add `FormFields.css` to your document.

To change variables in these styles, go into _styl/Variables.styl_, edit the appropriate
variables, and run `npm install && npm run build-styles` to get an updated `FormFields.css` file.

### TEA

The modules in this repo all use similarly grouped functions to achieve their behavior.

Each will have at most the following exports

1. `init` - Get an initial model representation for the element  
2. `view` - Html representation of the element that returns `Html Msg`  
3. `Msg` - outgoing messages that will be emitted by the view, map these to your own `Msg` type!  
4. `update` - used to get a new representation of the model based on emitted messages. For more complex controls like the SuperSelect, there may be additional information,  
5. `Config` - a type representing a record that the `view` function should be passed on render. Much of the data in this type lives in the model you manage as opposed to the opaque `Model` you store in your own for each element
6. `Model` - generally an opaque type for an element's datastructure that it will concern itself with via `update` and you don't need to care about. For example, you are expected to store things like the selected option and the current value of the text field for the SuperSelect in the managed part of your model, and pass it to the `view` in `Config`. But things like hover state, focus state, and whatnot that live in the element's model can have that managed abstracted away by it's corresponding `update` function
