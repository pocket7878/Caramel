# Caramel

Enlive like template system in Common Lisp

## Usage

#### html-resource

build dom from html file

    (html-resource #p"/path/to/your/html/file")

#### select

search node by css selector

    (select "#id" dom-node)
    => Matching node list 


#### set-attr

Set attributes to node

    (set-attr node :foo "baz" :bar "wow")

#### remove-attr

Remove attributes from node

    (remove-attr node :foo :bar)


#### add-class

Add css classes to node

    (add-class node "cls-foo" "cls-bar")

#### remove-class

Remove css classes from node

    (remove-class node "cls-foo" "cls-bar")


#### content

Set content of node

    (content node "foo")


#### do->

Cascade transform to node

    (do-> node (content "foo") (set-attr :color "green") (add-class "cls-foo"))

#### clone-for

Clone nodes

    (clone-for node x '(1 2 3) (content x))
    
    (clone-for node x '(1 2 3)
                   "p" (content x)
                   "h1" (content "foo"))

#### deftemplate

Define template from file.

    (deftemplate foo #p"/path/to/your/base/file" (bar)
      "#bar" (do-> (content "fuge") (set-attr :color "green") (add-class "cls-foo"))
      "p#para" (content "Wow"))

## Author

* Masato Sogame (poketo7878@gmail.com)

## Copyright

Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)

# License

        Licensed under the LLGPL License.


