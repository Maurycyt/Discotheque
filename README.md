
# Discotheque

The name stands for "Discovery of Theorems using Queries".
 
## Overview

The main file of the project is [`src/main/scala/main.scala`](src/main/scala/main.scala). It runs the search engine on a selected dataset. For usage instructions, see [Usage](#usage).

Top-level files other than `main.scala` and files in the [`util`](src/main/scala/util) directory are only utilities, not part of the main logic.

Files describing the CNF and FOF grammars are in [`src/main/resources`](src/main/resources), and classes which define how to parse them are in [`cnf`](src/main/scala/cnf), [`fof`](src/main/scala/fof), and [`cnfNfof`](src/main/scala/cnfNfof).

The representation of the parsed formulas is in [`src/main/scala/common`](src/main/scala/common), together with the function elimination algorithm, which converts function occurrences to relational judgements, thus flattening formulae to sets of literals.

The matching algorithm is in [`src/main/scala/matching`](src/main/scala/matching). It's complicated for implementation reasons, some of them historical.

## Usage

Should you wish to play around with the search engine, simply run `sbt "runMain main <dataset>"`, where `<dataset>` is a compatible file from [`src/test/resources`](src/test/resources). The compatible files are:
- all which start with `test`, and
- all which end in `.cnf`, `.fof`, or `.cnfNfof`.

The recommended dataset is `SET.cnfNfof`, as it contains a variety of formulae in both CNF and FOF formats, most of which are comprehensible for humans. This is also the file on which evaluation of the engine was performed.

After running the `sbt` command, the program will tell you how many formulae it has loaded, and then prompt you to enter a query. You can enter any formula in CNF or FOF format, terminated with a double new line (you have to hit Enter twice to confirm the query). The program will return the best matching formulae from the dataset, along with their scores, and some data about the best found matching.

The matching data contains the specific score components (total literals, valid literals, invalid literals, variable unifications, quantification clashes). It also contains the formulae after function elimination and renaming of variables according to each variable's equivalence class. Finally, associations between variable equivalence classes are shown, as well as their quantifications and the quantifications of the associations. The quantifications are `!` for universal quantification, `?` for existential quantification, `@` for ambivalent, and a lightning bolt for clashing.
