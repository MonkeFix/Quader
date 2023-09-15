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

## Engine Public API TODO:

 - [ ] **BOARD:**
   - [ ] Create Board
   - [ ] Destroy Board
   - [ ] Get/Set Current Piece
   - [ ] Get/Set Current Piece Generator
   - [ ] Get Current Bag
   - [ ] Start/Stop Game (+Replay)
   - [ ] Hold/Unhold Piece
   - [ ] Update (Gravity, Lock, etc.)
   - [ ] Find Nearest Y
   - [ ] Get Damage/Garbage Handler
   - [ ] Reset
   - [ ] Clear
   - [ ] Get/Set Cell at Position
   - [ ] Serialize/Deserialize
 - [ ] **PIECE:**
   - [ ] Create
   - [ ] Move Left/Right
   - [ ] Rotate Clockwise/Counter-Clockwise/180 degrees
   - [ ] Soft Drop
   - [ ] Hard Drop
   - [ ] Reset
   - [ ] Test Movement
   - [ ] Test Rotation
   - [ ] Get/Set Default Colors
   - [ ] Get/Set Default Wall Kick Data
   - [ ] Get/Set Default Positioning
   - [ ] Get Bounds
   - [ ] Get Current Position
   - [ ] Get Current Rotation
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