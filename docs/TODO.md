# TODOs

## Client

 - [x] Add timers to every board action and store the times. Will be useful for the following TODOs
 - [x] ~~Change direction of the board's Y axis from (0,0) being at top left corner to bottom left corner~~
 - [x] Add "invisible" top 20 lines for cases when garbage is over the board's top border
 - [x] Fix initial Y position of the incoming pieces
 - [ ] Fix rotation tests for 180 degrees rotation
 - [x] ~~Change the way pieces handle their position. Now it is handled by relative coordinates and you need to manually add offsets to them which is relatively slow~~
 - [ ] Fix rendering of the pieces: render into `RenderTarget2D` on demand:
	- [x] Render the Board itself
	- [x] Render Board Grid
	- [ ] Render Queue
	- [ ] Render individual pieces
 - [x] Optimize the way Pieces calculate it's position: find nearest Y for the piece ghost only on board change, etc
 - [ ] Move SRS tables to a JSON file
 - [x] Add skins
 - [ ] Implement correct input handling
   - [x] Handling simultaneous key presses
   - [x] Simple version with `ARR = 0` `SDF = Infinity`
   - [ ] Correct ARR (Automatic Repeat Rate)
   - [x] Correct DAS (Delayed Auto Shift)
   - [ ] Correct DCD (DAS Cut Delay)
   - [ ] Correct SDF (Soft Drop Factor)
 - [x] Add Piece Queue
 - [x] Implement Bag-7 piece generator
 - [ ] Create a shared bag-7 (same seed for the pieces)
 - [ ] Add gravity for the pieces
 - [ ] Add win/lose conditions
 - [ ] Add garbage sending/receiving mechanisms 
 - [ ] Add stats on the screen
 - [ ] Implement a scoring system (combos, quads, etc)
	- [x] Handle different type of line clears: quads, triples, all clears, etc
	- [x] Handle B2B's and Combos
	- [ ] Handle T-Spins correctly (check for overhang and find diffs between minis and regular)
 - [ ] Add ColdClearNet bot:
	- [x] Synchronous version (block)
	- [ ] Asynchronous version (poll)
 - [ ] Add Unit Tests
 - [ ] Add effects (particles)
 - [ ] Add sounds
 - [ ] Add background music
 - [ ] Add menus and game modes
 - [ ] Create a good UI
 - [ ] `Client <-> Server <-> WebAPI` interaction

## Server
 - [ ] Do something