# Quader

A Cross-Platform Online stacker puzzle game written in Rust.

**Work in progress.**

## Project overview

All crates are located in `src` subdirectory. This includes:

 - `assets` - all the game's assets: textures, sound effects, etc.
 - `client-prototype` - an example client written using [macroquad](https://github.com/not-fl3/macroquad).
 - `engine` - the main Quader engine which handles all the gameplay stuff - mostly done.
 - `server` - the game server. Handles multiplayer games.
 - `server-prototype` - an example server, kind of a playground for testing stuff.
 - `skynet` - [cold-clear](https://github.com/MinusKelvin/cold-clear) bot wrapper.
 - `web-api` - a RESTful web API which directly works with the DB.
