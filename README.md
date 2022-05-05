# Quader

[![.NET Client Application](https://github.com/lunacys/Quader/actions/workflows/client-app.yml/badge.svg?branch=master)](https://github.com/lunacys/Quader/actions/workflows/client-app.yml)

Online stacker puzzle game.

## TODOs

### Client

 - [x] Add timers to every board action and store the times. Will be useful for the following TODOs
 - [x] ~~Change direction of the board's Y axis from (0,0) being at top left corner to bottom left corner~~
 - [x] Add "invisible" top 20 lines for cases when garbage is over the board's top border
 - [x] Fix initial Y position of the incoming pieces
 - [ ] Fix rotation tests for 180 degrees rotation
 - [x] ~~Change the way pieces handle their position. Now it is handled by relative coordinates and you need to manually add offsets to them which is relatively slow~~
 - [ ] Check if there's a faster variant of `TestRotation` and `MoveDown` methods of the `Board` class:
   - [ ] `HardDrop()` and `Rotate()` are relatively slow (0.02ms and 0.05ms respectively)
 - [ ] Optimize the way Pieces calculate it's position: find nearest Y for the piece ghost only on board change, etc
 - [ ] Move SRS tables to a JSON file
 - [ ] Add correct input handling
   - [ ] Handling simultaneous key presses
   - [ ] Correct ARR (Automatic Repeat Rate)
   - [ ] Correct DAS (Delayed Auto Shift)
   - [ ] Correct DCD (DAS Cut Delay)
   - [ ] Correct SDF (Soft Drop Factor)
 - [x] Add Piece Queue
 - [x] Implement Bag-7 piece generator
 - [ ] Implement a scoring system (combos, quads, etc)
 - [ ] Handle T-Spins (minis, singles, doubles, triples)
 - [ ] Add ColdClearNet bot
 - [ ] Add Unit Tests

## Ideas

 - Add a way to choose which player you want to get blocks from, adding a negative multiplier to attack sent
 - 2v2
 - Lines received should not be greater than lines sent

## Game Comparison (pros and cons)

### Overall (exp in all the games)

#### Pros

 - Exists

#### Cons

 - No balance changes whatsoever: same rules staying unchanged for years
 - SRS (Super Rotation System)
 - Is really hard to get into competetive scene
 - Only 4 damage tagets: even, random, backfire and eliminations

### [TETR.IO](https://tetr.io)

#### Pros

 - Good feedback (attacks, damage taken, etc.)
 - Custom skin support (via TETR.IO PLUS) (graphics and sounds)
 - Really good sound effects (attack, damage, t-spins, etc.)
 - Good attack indicator (numbers on the field)
 - Good effects
 - TETRA LEAGUE (ranked matches)
 - TETRA CHANNEL 
 - MMR is really informative when ranked is actively played
 - Good spectating support
 - Good replay support

#### Cons

 - No bots to practice with
 - Bad attack system in games with 3+ people:
   - Unbearable attack when using an opener
   - Huge spikes from all players which results in huge cheese on the board
   - Often 10+ damage taken in matter of milliseconds or vice versa zero damage taken in minutes
 - Very long useless and annoying animations/transitions: 
   - When entering Zen (countdown timer)
   - In the break between 1v1 matches in tetra league
   - Uneven lines sent and received
   - You can die easily in the first 15 seconds of the game
 - Bad keyboard interface interaction in all the places (main menu, pvp, etc.). Escape key works inconsistently
 - Lack of practice modes and overall performance info (in comparison with jstris)
 - Very slow development
 - A lot of smurfs and stupid room handling (people can just stay in B rank even though they play like high SS rank)
 - Passthrough
 - Bad music choise (at least some of them): they are really good as separate tracks, but quickly become annoying when playing often
 - Bad overall game performance (low fps on macbook pro)
 - No tutorials nor guides
 - Chat is a mess with a lot of useless info (players connected/disconnects)

### [Jstris](https://jstris.jezevec10.com/)

#### Pros

 - Minimalistic in a good way
 - A lot of game modes
 - AI (bots)
 - Player progression

#### Cons

 - Bad UI
 - A little of customization - only sounds and pieces apperance

### [Techmino](https://github.com/26F-Studio/Techmino)

#### Pros

 - Full cross-platform support: PC, Mac, Android, etc

#### Cons

 - Written in Lua with bad English support

### [Puyo Puyo Tetris 2](https://store.steampowered.com/app/1259790/Puyo_Puyo_Tetris_2/)

#### Pros

 - Anime with pretty girls

#### Cons

 - Feels slow and clunky to play

### [Tetris Effect: Connected](https://store.steampowered.com/app/1003590/Tetris_Effect_Connected/)

#### Pros

 - Great one of the best visuals and music in all the tetris games
 - Zone feature

#### Cons

 - Feels slow and clunky to play
