=============
Try-Scalameta
=============

This project is about getting to know `Scalameta`_ better. It also offers some utility methods which
can be useful when using Scalameta in (Ammonite) REPL sessions to analyse Scala source code trees.

In no way is it suggested that this is optimal use of Scalameta. It is quite likely that higher-level APIs
that use Scalameta under the hood would be more appropriate than what is explored here. Even Scalameta
itself has probably far much more to offer than what is used here. On the other hand, it is still good
to explore the basics of Scalameta (its Tree API), and that's what is being done here.

Important documentation pages to get started are the `Tree Guide`_ and the corresponding `Tree API documentation`_.

Although `quasiquotes`_ are very powerful and useful, they are not really used in the examples in this project.

It is hoped that this project can help in quickly scripting some Scala code analysis, using Ammonite REPL sessions.
Some of the code in this project could first be copied into those REPL sessions.

.. _`Scalameta`: https://scalameta.org/docs/trees/guide.html
.. _`Tree Guide`: https://scalameta.org/docs/trees/guide.html
.. _`Tree API documentation`: https://www.javadoc.io/doc/org.scalameta/trees_2.13/latest/scala/meta/Tree.html
.. _`quasiquotes`: https://scalameta.org/docs/trees/quasiquotes.html

