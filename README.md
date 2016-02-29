# Merry-Widow-Murderer
### HW 1
Basic methods for manipulating *syntax trees*. 

1. FILLER -- returns the branch of the *syntax tree*.

2. PATH-SL -- search by a *path* in a *syntax tree*.

3. UNGAP -- recursively replaces all *marcos* by its values in a *syntax tree*

4. ADD-SF -- add a *slot* (sub-branch of sentence) with *filler* (contents) to a *frame* (syntax tree)

5. SAME-SF -- returns true if two *syntax trees* have the same structure.

### HW 2
Maintaining 4 global variables as *"memory"* by functions.

1. LEXMEM -- Lexical memory, stores words/phrases, and their associated frames and demons.

LEXMEM's structure:

LEXMEM = ( ( (phrase) frame (dem-inst*) )

( (phrase) frame (dem-inst*) )

…

( (phrase) frame (dem-inst*) ) )

where phrase → word+

where dem-inst → (demon-name arguments*)

(remember, * = 0 or more, + = 1 or more, and +2 = 2 or more)

So a phrase consists of a list of one or more words. A dem-inst is a list with a demon name, followed by zero or more arguments.

1.1.ADD-LEX -- Adds a phrase, frame and demons to LEXMEM.

