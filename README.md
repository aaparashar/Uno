# UNO
Authors: Aarushi Parashar, Aaron Peng and Christina Li
## System Description:
A fun, multi-level implementation of the popular card game Uno in the command line.
### Description:
An OCaml terminal implementation of online/card game Uno/Crazy 8's supporting multiple levels. Our UI displays cards for the hand and a color coded description of the playing stack.
### Key Features:
* Fully implemented logic for Uno (including Power Cards)
* Terminal Based UI
* Multilevel Game Play
## System Design:
We used a Model-View-Controller framework. There are three core modules:
* Model - state.mli
  * Encapsulates the current state of the game, maintains information about the map, user turns, scores and other information.
  * Checks the validity of user actions passed to it from the controller, handling errors by catching them and passing an unmodified state and log messages to the controller
  * Applies changes to produce a new state structure to return to the controller
* Controller - command.mli
  * An intermediary module between the state and the display.
  * Interprets user actions generated by the Terminal, calls appropriate update methods in the state, updates the Terminal to reflect state changes, and propagates error information if an action fails.
  * Enforces separation between display operations and the State, ensuring that neither directly interact or share internal information.
* View - UI.mli
  * Displays game state (for example, number of troops and owner of each territory) on the map and in the informational panel.
  * Provides user access to gameplay data (available cards, troop counts, etc.).
  * Accepts user input and does some logic before passing to the controller.
 ## Module Design
 Our .mli files contain interfaces describing our module design.
 ## Data
 Our program maintains game state data and user action data.
* Game state: as in A2, we used record types to represent the game state cleanly, with a player hand, ai hand, current card and the remaining deck. All of this came from the Deck module which mmaintained a list of Cards and supports basic card operations. 
* User action data: we defined an Command variant type exposed to all modules. When the game receives a user cpmmand, its parsed, which the controller passes to the State, and upon receiving an updated state, updates the Display.
## Testing
* Interactive play-testing: we conducted extensive play-testing of the game in both a non-malicious average user use case, and deliberately attempting to break the game through illegal actions. A detailed rep_ok function was also run on each state during play-testing. Most of our testing occurred this way, as results are easily verifiable.
* Unit testing: used to demonstrate the correctness of our State implementation. Tests that state is correctly initialized and updated in a variety of cases. Unit testing was limited in both scope and usefulness due to encapsulation of state, and was mostly used to verify simple updates to state.
* Utop: we used during development to test State operations during programming.
* Ocamldebug: for specific error cases, we used Ocamldebug to identify the locations of problems and to verify that they were corrected after corrections were made.
