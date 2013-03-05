# Caramel

Enlive like template system in Common Lisp

## Usage

### html-resource

build dom from html file

    (html-resource #p"/path/to/your/html/file")

### select

search node by css selector

    (select "#id" dom-node)
    => Matching node list 

### Translator

Every translator returns a function 
which takes a node and return translated node or nodes.

#### set-attr

Set attributes to node

    (set-attr :foo "baz" :bar "wow")

#### remove-attr

Remove attributes from node

    (remove-attr :foo :bar)


#### add-class

Add css classes to node

    (add-class "cls-foo" "cls-bar")

#### remove-class

Remove css classes from node

    (remove-class "cls-foo" "cls-bar")


#### content

Set content of node

    (content "foo" a-node "foo")

#### html-content

Build html from specified string and set

    (html-content "<p>Foo</p>")


#### wrap

Wrap node with specified tag

    (wrap "p")

#### unwrap

Get content of node

    (unwrap)

#### do->

Cascade transform to node

    (do-> (content "foo") (set-attr :color "green") (add-class "cls-foo"))

#### before

Insert nodes before node
    
    (before "foo" a-node "baz")

#### after

Insert nodes after node

    (after "foo" a-node "baz")

#### substitute
    
Replace node with nodes

    (substitute "foo" a-node "baz")

#### clone-for

Clone nodes

    (clone-for x '(1 2 3) (content x))
    
    (clone-for x '(1 2 3)
     "p" (content x)
     "h1" (content "foo"))

## Snippet & Template 

### defsnippet

Define snippet from file.

    (defsnippet bar #p"/path/to/your/file" "div#baz" ()
      "p" (content "foo"))

    (bar)
    => node-list
      
### deftemplate

Define template from file.

    (deftemplate foo #p"/path/to/your/base/file" (&optional foo)
      "#bar" (do-> 
              (content "fuge") 
              (set-attr :color "green") 
              (add-class "cls-foo"))
      "p#para" (if foo
                 (content foo)
                 (content "defaul")))

## Author

* Masato Sogame (poketo7878@gmail.com)

## Copyright

Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)

# License

        Licensed under the LLGPL License.


