# pmap 0.6.0

Besides the count of the edge on the label, the duration stats are also calculated to be shown. Add `distinct_repeated_events` to be able to expand the repeated events to have better internal understanding. Add ability to use external `dot` program to render the graph, it will be faster in some cases.

* af20391 Fix 'UQ()' issue on dependencies.
* 0953e15 Fix figures naming issue.
* e1daa8c Add term schema relation graph
* 6523f17 Rename `activity_category` to just `category`
* 1929113 Change the term from `event` to `activity`.
* 384de85 Rename term `customer` to `case`
* 0c63555 Update the graph files and add more readme content to reflect latest update
* 702c526 Add `adjust_edge_label()` function
* a35ec17 Add huge process map warning for rendering
* 1fba54b Add `use_external_dot` argument to `render_pmap_file()`
* 089a958 Add more ignores to `.gitignore` and `.Rbuildignore`
* 1c17eb4 Add `dot` file support for `render_pmap_file()`
* 934d5d4 Add `distinct_repeated_events` argument to `create_pmap()`
* 095444c Remove node's color gradient as there is a compatibility issue in Viz.js
* de905fc Enhance color palette for event categories to 80 colours, and won't fail if ask more
* 64a089e Rename event_type to event_category to avoid confusion
* fa527d7 Add `*_prune_percentage` to `render_pmap_shiny()` for initial prune value settings.
* 84e656c Add ability to `render_pmap_file()` to guess file format
* 15ae4fa Handle the SPACE padding case
* 1530337 Convert `eventlog$timestamp` to `POSIXct` automatically if it's not `POSIXct` yet.
* c67f6c8 Add `median_duration` for edge label and more test cases.
* 8702685 Fix the edge label too close to the edge problem by padding some spaces
* dc79c7c Add duration support in edge label
* d26c5a5 Add Github version and license badges


# pmap 0.5.0

This update is mainly about performance improvements. With many of these tuning/refactoring, `0.5.0` is much faster than `0.4.0`. It can be thousands times faster for large dataset, such as event log contains several millions records.

Although the `create_pmap()` is pretty fast, the rendering part is still on V8's Javascript engine, which can be pretty slow for a graph with many nodes and edges. `prune_nodes()` and `prune_edges()` can be used to reduce the final graph's complexity.

* 94b1dd2 Improve slicing performance
* a4c72a0 Improve sorting performance by 2x or more.
* 073c2a4 Rewrite `generate_edges()` to improve performance
* e899f7d Add Installation section in `README.md` and change the example back for better compatibility.
* 8a4a096 Improve `get_colors()` performance by remove `ggsci` from dependencies
* 395cc42 Improve `create_pmap_graph()` and `generate_edges()` performance
* 3c98ac5 Remove data.table from required dependencies
* e7f4f14 Avoid running slow example
* a3788ec Refactor `get_colors()` function to make it more efficient.

# pmap 0.4.0

* 5e22bd8 Add `render_pmap_file()` function.
* 64d4a08 Use `match.arg()` for `prune_nodes()` argument `rank`
* c7537b6 Remove internal functions from the docs
* c2e74bc Reorder the prune sequence in `README.md` example
* b0d0880 Remove the dependency of eventdataR which is not necessary
* 0f97cf8 Update the `README.md` to use sepsis dataset as the example
* 63de70d Add `clean_graph()` to remove nodes without edge
* 63d6760 Explicitly using package name in the external function call.
* b26447d Remove the uncertainty by explicitly using `round()`.
* 6624434 Add ability to handle the `event_type` missing `eventlog` case.
* 12f1319 Add `render_pmap_shiny()` function
* 43fa193 Add `weight` attribute to the edge with projected `amount` value.
* ed365e3 Add test case with prune for `create_pmap()`
* dacdbdd Clean up `apply_node_color.R`, extract color-related code.
* 8423941 Add `adjust_node_style()` function
* a9cc7e6 Adjust edge style by simply project the `edges$amount` value to expected range.
* 76094cd Adjust the node size based on the amount within size range from 10 to 20
* d05d8fb Add randomness to the weight of each event names
* d91c3e0 Add `amount` in the node label.
* d70731a Remove the randomness of color selection, use predefined sequence.
* 1ea5a05 Add `apply_node_color()` function to dynamic assign the node's color based on the type
* 714dadd Add `CODE_OF_CONDUCT.md` and fix typos in `DESCRIPTION`.

# pmap 0.3.2

* Added a `NEWS.md` file to track changes.
* Add bug report url and more test environments in `cran-comments.md`

# pmap 0.3.1

* Replace PNG format with SVG format.
* Fix figures file namepackage.

# pmap 0.3.0

* Fix typos in `README.md` example
* Rename `create_pmap_from_eventlog()` to `create_pmap()`
* Rename `create_pmap()` to `create_pmap_graph()`
* Rename `generate_random_eventlog()` to `generate_eventlog()` and remove the `helper.R`
* Add more examples to the documents and fix typos
* Add release target to Makefile

# pmap 0.2.0

* Add `cran-comments.md`
* Fix typos
* Add example code to `README.md`
* Add more docs for `prune_edges()` and others
* Add more docs for `create_pmap_from_eventlog()`
* Add random eventlog generator function `generate_random_eventlog()`
* Add random eventlog generator function `generate_random_eventlog()`
* Add `create_pmap_from_eventlog()` example with graph
* Fix a bug that the target event shouldn't be the start point
* Add ability to handle the empty `target_types`, for that case, every paths count, rather than only the paths reaches the target

# pmap 0.1.0

* First release.