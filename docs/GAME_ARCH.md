# Quader Architecture

## Project Overview

### Libraries and Frameworks

 - `FNA` - main game framework.
 - `Nez` - a lot of useful stuff that comes on top of `FNA`.
 - `lunge` - my own library, a collection of useful utils that were cherry-picked from various sources.
 - `ColdClearNet` - .NET bindings of the ColdClear bot.

#### Plans on new libs

 - `Quader.Bot.Api` - a simple API used for creating custom bots for the game.

### Game's Engine

 - `quader_engine` - a new engine written from scratch in Rust. Replaces old `Quader.Engine` .NET project.

### Clients

 - `Quader.Client` - the main client of the game.
 - `Quader.WebApi.Client` - frontend for `Quader.WebApi` written using `Angular` framework. Shows all the player's stats as well as replays, online players, etc.

### Web

 - `Quader.WebApi` - provides RESTful API methods that are used by all projects, such as autorization, DB connection, replay persistance, etc.

### Server

 - `quader_server` - handles multiplayer games. Will be written in Rust.

## Quader Engine

The Quader Engine is written in Rust and is used by the client and the server.

### Engine API Overview

`Quader` is the main class that holds all the necessary handlers:

 - Game State Manager
   - Board:
     - Layout:
       - Stores all cells
       - Checks for piece collisions
     - Piece:
       - Rotation (clockwise, counter-clockwise and 180 degrees)
       - Movement (left/right, soft drop, hard drop)
       - Stores its own layout in form of a 2D array
       - Stores its bounds
     - Damage/Garbage Handler
       - Calculates incoming and outgoing damage
       - Pushes garbage when necessary
   - Random Manager
     - Creates a seed that is shared across all boards
   - Time Manager
     - Stores time spent in matches
   - Replay Manager
     - Stores replays of played matches
   - ColdClear bot

### Engine TODO

 - [ ] **BOARD:**
   - [x] Create Board
   - [x] Destroy Board
   - [x] Get/Set Current Piece
   - [ ] Get/Set Current Piece Generator
   - [ ] Get Current Bag
   - [ ] Start/Stop Game (+Replay)
   - [ ] Hold/Unhold Piece
   - [ ] Update (Gravity, Lock, etc.)
   - [x] Find Nearest Y
   - [ ] Get Damage/Garbage Handler
   - [x] Reset
   - [ ] Clear
   - [x] Get/Set Cell at Position
   - [ ] Serialize/Deserialize
 - [ ] **PIECE:**
   - [x] Create
   - [x] Move Left/Right
   - [x] Rotate Clockwise/Counter-Clockwise/180 degrees
   - [x] Soft Drop
   - [x] Hard Drop
   - [x] Reset
   - [x] Test Movement
   - [x] Test Rotation
   - [x] Get/Set Default Colors
   - [x] Get/Set Default Wall Kick Data
   - [x] Get/Set Default Positioning
   - [x] Get Bounds
   - [x] Get Current Position
   - [x] Get Current Rotation
 - [ ] **DAMAGE/GARBAGE:**
   - [ ] Push Garbage
   - [ ] Push Solid Row
   - [ ] Calculate Damage
   - [ ] Predict Damage?
 - [ ] **RANDOM MANAGER:**
   - [ ] Get/Set Seed
 - [ ] **TIME MANAGER:**
   - [ ] Get Time Spent in Current Game/Match
   - [ ] Get Total Time Spent from Engine Start