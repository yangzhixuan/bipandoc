BiPandoc Meeting 2
====================

## Our Previous Plan

1. Markdown/HTML CST Parser
2. Determine the structure of AST
3. Markdown/HTML simple BX ([aligning elements]())
4. Aligning by text similarity
5. Evaluation
6. Extend it to BiEditor


## Current Progress

A prototype aligning elements sequentially:

* Markdown: A completed markdown CST parser and BX

* HTML: A HTML CST parser and BX

* DOCX: [chuanhao]()

Summary: Now we have a document converter between HTML and Markdown. The transformation is *consistent*.

## Current Problems

1. Element correspondence can be destroyed by insertion or deletion

   Solution: aligning by text-similarity.
   
2. (**New**) How to deal with elements that doesn't have correspondent structure in the target format? For example: `<table>` in HTML, `<div>` in HTML.

    - **Pandoc's solution**: creating some approximation if possible, or simply discarding it.

    - **For our BiPandoc**: In general, we don't want to sychronize such structures into (weaker) target format by approximation to ensure the `GetPut` property. But then:
    
      * **Conflicts in the Structure of AST** if we want to support more than HTML and Markdown: HTML and LaTeX both support table, while markdown doesn't. If we want to sychronize between 3 formats at the same time?
    
         **Possible solution**: A view type supports *ALL* possible structures in *ALL* formats (called *super view*) is used in the low level. Every concrete format has its own view (called *private view*) which **only** needs to include the structures in format B. There is a BX between concrete format and its private view. And there is also a BX between the private view and the super view. By this way, the complex structure in format A does exist in the super view but is filtered by the BX between the private view of B and the super view. 
         
         [Josh]()'s suggestion: Make the BX between the super view and private views reusable. For example, constructing from small components.
         
     * **Container-like structure**: for some structures, the most appropriate strategy is not to skip the whole structure, e.g. `div`. When we are transforming `<div>...</div>` into markdown, we want to ignore the `div` tag and transform its children, rather than skip the whole subtree. Similar to `BiGUL (Tree a) [a]`. But how to perform `put`?

       - Solution 0 (work around): We add `div`-like structure into Markdown to avoid tree flattening, e.g. exploit raw html tags in markdown.

       - Solution 1: match the elements in the view (a list) and elements in the source (a tree) to recover the tree structure. 

       - Solution 2: zirun's solution
         
### New Plan?

1. Markdown/HTML CST Parser

2. Determine the structure of AST

3. Markdown/HTML simple BX ([aligning elements]())
   
   Now:

4. Deal with `div` in a reasonable way.

5. Align by text similarity

6. Extend to more formats and the super-view framework

7. Evaluation

8. Extend it to BiEditor
